defmodule Db do
  @moduledoc """
  Umbrella-wide Repo and legacy LTM facade.

  Boundary role:
  • `Db` is the Ecto repo module for the umbrella's single database.
  • The LTM helpers in this module are the current DB context for `brain_cells`
    retrieval used by Core's semantic pipeline.
  • New persistence areas should prefer named context modules (`Db.Episodes`,
    `Db.AgencyEvents`, `Db.SelfSnapshots`, `Db.BrainCellCorpus`) instead of
    adding unrelated query helpers here.

  Long-term memory (LTM) helpers:
  • Collect normalized `norm`s from `si.tokens` while preserving token indices.
  • Load matching `Db.BrainCell` rows from long-term storage.
  • Annotate returned rows with `token_index` so downstream LIFG bucketing works.
  • Report which norms are missing so callers can decide what to do.
  • Return loaded rows along with a set of DB hits.

  Important boundary rule:
  LTM retrieval does not filter by `status == "active"`. A brain cell can be
  inactive from a runtime/selection perspective and still exist in long-term
  memory. STM/LIFG should decide which retrieved cells are currently relevant,
  active, salient, or usable.

  ## Options

    * `:limit_per_norm` — `pos_integer | :all`, default: `:all`.
      Caps rows returned per normalized token.

  ## Return shape

  `ltm/2` returns rows as plain maps, not Ecto structs, so we can include
  token-local metadata:

    * `:token_index` — the token position in the input SI
    * `:id`
    * `:lemma`
    * `:norm`
    * `:word`
    * `:pos`
    * `:definition`
    * `:example`
    * `:synonyms`
    * `:antonyms`
    * `:semantic_atoms`
    * `:score`
    * `:source`

  This is intentional: DB BrainCell rows are lexicon senses and do not carry
  token indices. LIFG Stage1 buckets candidates by token index.
  """

  use Ecto.Repo,
    otp_app: :db,
    adapter: Ecto.Adapters.Postgres,
    priv: "priv/db"

  import Ecto.Query

  alias Db.BrainCell

  @type norm :: String.t()

  @brain_cell_embedding_dim Application.compile_env(:db, :brain_cell_embedding_dim, 768)

  # Closed-class inventory pruning for candidate hygiene.
  #
  # This is not an active/inactive memory gate. It prevents currently observed
  # closed-class troublemakers from flooding downstream candidate buckets with
  # irrelevant senses.
  @closed_class_allowed_pos_by_norm %{
    "is" => ["verb", "aux", "auxiliary", "copula"],
    "my" => [
      "det",
      "determiner",
      "possessive",
      "possessive_determiner",
      "pronoun",
      "adj",
      "adjective"
    ],
    "what" => ["pronoun", "det", "determiner", "interrogative", "particle"]
  }

  @doc """
  Returns all BrainCell rows for a given word's normalized form.

  Used by fuzzy repair to check if a word is obsolete (all senses inactive)
  and retrieve synonyms for correction suggestions.

  Unlike `word_exists?/2`, this returns the full rows so callers can inspect
  `status`, `synonyms`, and other fields.
  """
  @spec brain_cells_for_norm(String.t()) :: [BrainCell.t()]
  def brain_cells_for_norm(word) when is_binary(word) do
    case norm(word) do
      "" ->
        []

      n ->
        from(b in BrainCell,
          where: b.norm == ^n,
          select: b
        )
        |> Db.all()
    end
  end

  def brain_cells_for_norm(_word), do: []

  @doc """
  Look up BrainCell rows for the tokens inside an SI-like map.

  `si.tokens` may contain:

    * maps with `:phrase`, preferred
    * maps with `:norm`
    * maps with string keys, such as `"phrase"` or `"norm"`
    * plain strings

  Returns:

      {:ok, %{rows: rows, missing_norms: missing, db_hits: MapSet.t()}}

  Rows are returned as plain maps with a required `:token_index` key.

  This is long-term-memory retrieval. It intentionally does not filter by
  `status == "active"`.
  """
  @spec ltm(map(), keyword()) ::
          {:ok,
           %{
             rows: [map()],
             missing_norms: [binary()],
             db_hits: MapSet.t()
           }}
  def ltm(si, opts \\ [])

  def ltm(%{tokens: tokens}, opts) when is_list(tokens) do
    limit_per_norm = Keyword.get(opts, :limit_per_norm, :all)

    token_norms =
      tokens
      |> Enum.with_index()
      |> Enum.map(fn {tok, fallback_idx} ->
        idx = token_index(tok, fallback_idx)
        phrase = token_to_phrase(tok)

        {idx, norm(phrase)}
      end)
      |> Enum.reject(fn {_idx, n} -> n == "" end)

    uniq_norms =
      token_norms
      |> Enum.map(fn {_idx, n} -> n end)
      |> Enum.uniq()

    if uniq_norms == [] do
      {:ok, %{rows: [], missing_norms: [], db_hits: MapSet.new()}}
    else
      rows0 =
        uniq_norms
        |> brain_cells_for_norms_query(limit_per_norm)
        |> Db.all()

      token_meta_by_idx = token_meta_by_idx(tokens)
      token_idxs_by_norm = token_idxs_by_norm(token_norms)

      rows =
        rows0
        |> Enum.flat_map(fn row ->
          row
          |> token_indices_for_row(token_idxs_by_norm)
          |> Enum.flat_map(fn idx ->
            token_meta = Map.get(token_meta_by_idx, idx, %{})

            if keep_ltm_row_for_token?(row, token_meta) do
              [braincell_to_candidate_map(row, idx)]
            else
              []
            end
          end)
        end)

      kept_norms = MapSet.new(for row <- rows, do: row.norm)
      missing_norms = Enum.reject(uniq_norms, &MapSet.member?(kept_norms, &1))

      {:ok, %{rows: rows, missing_norms: missing_norms, db_hits: kept_norms}}
    end
  end

  def ltm(_si, _opts), do: {:ok, %{rows: [], missing_norms: [], db_hits: MapSet.new()}}

  @doc """
  Returns `true` if a word exists by normalized form in `brain_cells`.

  This is an LTM existence check only. It does not filter by runtime activation
  or `status`. STM/LIFG should decide whether a known word is currently
  relevant.

  The second argument is kept for temporary compatibility with older call sites
  that may still pass options such as `only_active: true`; those options are now
  intentionally ignored.
  """
  @spec word_exists?(term()) :: boolean()
  @spec word_exists?(term(), keyword()) :: boolean()
  def word_exists?(term, _opts \\ [])

  def word_exists?(term, _opts) when is_binary(term) do
    case norm(term) do
      "" ->
        false

      n ->
        from(b in BrainCell,
          where: b.norm == ^n,
          select: true
        )
        |> Db.exists?()
    end
  end

  def word_exists?(_term, _opts), do: false

  @doc """
  Returns active BrainCell word candidates that are spelling-near `term`.

  This is a candidate generator for fuzzy repair. It intentionally uses pg_trgm
  and shape filters, not wildcard phrase searches.

  Unlike `ltm/2` and `word_exists?/2`, this helper may still filter by
  `status == "active"` because it is a candidate-pruning path, not pure LTM
  retrieval.
  """
  @spec fuzzy_word_candidates(term(), keyword()) :: [map()]
  def fuzzy_word_candidates(term, opts \\ [])

  def fuzzy_word_candidates(term, opts) when is_binary(term) do
    only_active? = Keyword.get(opts, :only_active, true)
    limit = Keyword.get(opts, :limit, 12)
    min_similarity = Keyword.get(opts, :min_similarity, 0.35)
    max_length_delta = Keyword.get(opts, :max_length_delta, 2)

    case norm(term) do
      "" ->
        []

      n ->
        initial = String.first(n) || ""
        n_len = String.length(n)

        status_filter =
          if only_active? do
            dynamic([b], b.status == "active")
          else
            dynamic([_b], true)
          end

        from(b in BrainCell,
          where: ^status_filter,
          where: b.norm != ^n,
          where: fragment("left(?::text, 1) = ?", b.norm, ^initial),
          where:
            fragment(
              "abs(char_length(?::text) - ?) <= ?",
              b.norm,
              ^n_len,
              ^max_length_delta
            ),
          where: fragment("similarity(?::text, ?) >= ?", b.norm, ^n, ^min_similarity),
          order_by: [
            desc: fragment("similarity(?::text, ?)", b.norm, ^n),
            asc: fragment("abs(char_length(?::text) - ?)", b.norm, ^n_len)
          ],
          limit: ^limit,
          select: %{
            id: b.id,
            norm: type(b.norm, :string),
            word: type(b.word, :string),
            pos: b.pos,
            spelling_score: fragment("similarity(?::text, ?)", b.norm, ^n),
            source: "db_trigram"
          }
        )
        |> Db.all()
    end
  end

  def fuzzy_word_candidates(_term, _opts), do: []

  @doc """
  Reranks spelling candidates with pgvector context evidence from `brain_cells`.

  `query_embedding` must match the `brain_cells.embedding` dimension
  configured by `:brain_cell_embedding_dim`.

  Vector evidence is soft evidence: callers should still keep exact norm checks
  and spelling gates.
  """
  @spec rerank_word_candidates([String.t()] | [map()], [number()] | Pgvector.t(), keyword()) :: [
          map()
        ]
  def rerank_word_candidates(candidates, query_embedding, opts \\ [])

  def rerank_word_candidates(candidates, query_embedding, opts)
      when is_list(candidates) and is_list(query_embedding) do
    with {:ok, query_vec} <- brain_cell_pgvector(query_embedding) do
      rerank_word_candidates_with_vector(candidates, query_vec, opts)
    else
      {:error, _reason} -> []
    end
  end

  def rerank_word_candidates(candidates, %Pgvector{} = query_embedding, opts)
      when is_list(candidates) do
    rerank_word_candidates_with_vector(candidates, query_embedding, opts)
  end

  def rerank_word_candidates(_candidates, _query_embedding, _opts), do: []

  @doc """
  Generates spelling candidates for `term`, then reranks those candidates by
  contextual pgvector similarity when a query embedding is available.
  """
  @spec context_word_candidates(term(), [number()] | Pgvector.t() | nil, keyword()) :: [map()]
  def context_word_candidates(term, query_embedding, opts \\ [])

  def context_word_candidates(term, nil, opts), do: fuzzy_word_candidates(term, opts)

  def context_word_candidates(term, query_embedding, opts) do
    candidates = fuzzy_word_candidates(term, opts)

    case rerank_word_candidates(candidates, query_embedding, opts) do
      [] -> candidates
      reranked -> reranked
    end
  end

  # Legacy compatibility wrapper. Prefer `insert_all/3` directly in new code.
  def insrt_all(table, rows, opts), do: insert_all(table, rows, opts)

  # -- query helpers ----------------------------------------------------------

  defp brain_cells_for_norms_query(uniq_norms, limit_per_norm) when is_list(uniq_norms) do
    base_q =
      from(b in BrainCell,
        where: b.norm in ^uniq_norms
      )

    case limit_per_norm do
      :all ->
        from(b in base_q, select: b)

      n when is_integer(n) and n > 0 ->
        ranked_q =
          from(b in base_q,
            select: %{b: b, rn: over(row_number(), :norm_part)},
            windows: [norm_part: [partition_by: b.norm, order_by: [desc: b.updated_at]]]
          )

        from(s in subquery(ranked_q),
          where: s.rn <= ^n,
          select: s.b
        )

      _invalid ->
        from(b in base_q, select: b)
    end
  end

  defp rerank_word_candidates_with_vector(candidates, query_vec, opts) do
    norms = candidate_norms(candidates)

    if norms == [] do
      []
    else
      spelling_scores = candidate_spelling_scores(candidates)
      limit = Keyword.get(opts, :limit, length(norms))
      only_active? = Keyword.get(opts, :only_active, true)

      status_filter =
        if only_active? do
          dynamic([b], b.status == "active")
        else
          dynamic([_b], true)
        end

      from(b in BrainCell,
        where: ^status_filter,
        where: b.norm in ^norms,
        where: not is_nil(b.embedding),
        order_by: fragment("? <-> ?", b.embedding, type(^query_vec, Pgvector.Ecto.Vector)),
        limit: ^limit,
        select: %{
          id: b.id,
          norm: type(b.norm, :string),
          word: type(b.word, :string),
          pos: b.pos,
          distance: fragment("? <-> ?", b.embedding, type(^query_vec, Pgvector.Ecto.Vector)),
          context_score:
            fragment(
              "1.0 / (1.0 + (? <-> ?))",
              b.embedding,
              type(^query_vec, Pgvector.Ecto.Vector)
            ),
          source: "db_pgvector"
        }
      )
      |> Db.all()
      |> Enum.map(fn row ->
        Map.put(row, :spelling_score, Map.get(spelling_scores, norm(row.norm)))
      end)
    end
  end

  # -- vector helpers ---------------------------------------------------------

  defp brain_cell_pgvector(list) when is_list(list) do
    if length(list) == @brain_cell_embedding_dim do
      {:ok, Pgvector.new(list)}
    else
      {:error, {:wrong_embedding_size, length(list)}}
    end
  end

  # -- candidate helpers ------------------------------------------------------

  defp candidate_norms(candidates) do
    candidates
    |> Enum.map(&candidate_norm/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp candidate_norm(candidate) when is_binary(candidate), do: norm(candidate)
  defp candidate_norm(%{norm: value}) when is_binary(value), do: norm(value)
  defp candidate_norm(%{"norm" => value}) when is_binary(value), do: norm(value)
  defp candidate_norm(%{word: value}) when is_binary(value), do: norm(value)
  defp candidate_norm(%{"word" => value}) when is_binary(value), do: norm(value)
  defp candidate_norm(_candidate), do: ""

  defp candidate_spelling_scores(candidates) do
    Enum.reduce(candidates, %{}, fn candidate, acc ->
      candidate_norm = candidate_norm(candidate)
      score = candidate_score(candidate)

      if candidate_norm == "" or is_nil(score) do
        acc
      else
        Map.put(acc, candidate_norm, score)
      end
    end)
  end

  defp candidate_score(%{spelling_score: score}) when is_number(score), do: score * 1.0
  defp candidate_score(%{"spelling_score" => score}) when is_number(score), do: score * 1.0
  defp candidate_score(_candidate), do: nil

  defp braincell_to_candidate_map(%BrainCell{} = row, token_index) when is_integer(token_index) do
    %{
      id: row.id,
      token_index: token_index,
      lemma: to_string(row.norm || row.word || ""),
      norm: to_string(row.norm || ""),
      word: to_string(row.word || ""),
      pos: row.pos,
      definition: row.definition,
      example: row.example,
      synonyms: List.wrap(row.synonyms),
      antonyms: List.wrap(row.antonyms),
      semantic_atoms: List.wrap(row.semantic_atoms),
      score: 0.5,
      source: :ltm
    }
  end

  # -- token helpers ----------------------------------------------------------

  defp token_idxs_by_norm(token_norms) when is_list(token_norms) do
    token_norms
    |> Enum.reduce(%{}, fn {idx, token_norm}, acc ->
      Map.update(acc, token_norm, [idx], fn idxs -> [idx | idxs] end)
    end)
    |> Enum.into(%{}, fn {token_norm, idxs} ->
      {token_norm, Enum.uniq(idxs)}
    end)
  end

  defp token_indices_for_row(%BrainCell{} = row, token_idxs_by_norm)
       when is_map(token_idxs_by_norm) do
    Map.get(token_idxs_by_norm, row.norm, [])
  end

  defp token_meta_by_idx(tokens) when is_list(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {tok, fallback_idx}, acc ->
      idx = token_index(tok, fallback_idx)
      surface = token_to_phrase(tok)
      token_norm = norm(surface)

      Map.put(acc, idx, %{
        surface: surface,
        norm: token_norm
      })
    end)
  end

  defp token_meta_by_idx(_tokens), do: %{}

  defp keep_ltm_row_for_token?(%BrainCell{} = row, %{norm: token_norm, surface: surface}) do
    row_pos = normalize_pos(row.pos)
    row_word = normalize_word(row.word)

    cond do
      lowercase_surface?(surface) and acronymish_or_titlecase_noun_row?(row_word, row_pos) ->
        false

      token_norm in Map.keys(@closed_class_allowed_pos_by_norm) ->
        row_pos in Map.fetch!(@closed_class_allowed_pos_by_norm, token_norm)

      true ->
        true
    end
  end

  defp keep_ltm_row_for_token?(%BrainCell{}, _token_meta), do: true

  # Accept token map or string, extract phrase text.
  defp token_to_phrase(%{phrase: phrase}) when is_binary(phrase), do: phrase
  defp token_to_phrase(%{"phrase" => phrase}) when is_binary(phrase), do: phrase
  defp token_to_phrase(%{norm: token_norm}) when is_binary(token_norm), do: token_norm
  defp token_to_phrase(%{"norm" => token_norm}) when is_binary(token_norm), do: token_norm
  defp token_to_phrase(value) when is_binary(value), do: value
  defp token_to_phrase(_value), do: ""

  # Prefer explicit token index when present; fall back to enumeration index.
  defp token_index(%{index: idx}, _fallback) when is_integer(idx) and idx >= 0, do: idx
  defp token_index(%{"index" => idx}, _fallback) when is_integer(idx) and idx >= 0, do: idx
  defp token_index(%{token_index: idx}, _fallback) when is_integer(idx) and idx >= 0, do: idx
  defp token_index(%{"token_index" => idx}, _fallback) when is_integer(idx) and idx >= 0, do: idx

  defp token_index(_tok, fallback_idx) when is_integer(fallback_idx) and fallback_idx >= 0 do
    fallback_idx
  end

  # -- normalization helpers --------------------------------------------------

  defp normalize_pos(nil), do: ""

  defp normalize_pos(pos) do
    pos
    |> to_string()
    |> String.trim()
    |> String.downcase()
    |> String.replace(~r/\s+/, "_")
    |> String.replace("-", "_")
  end

  defp normalize_word(nil), do: ""

  defp normalize_word(word) do
    word
    |> to_string()
    |> String.trim()
  end

  defp lowercase_surface?(surface) when is_binary(surface) do
    surface = String.trim(surface)

    surface != "" and surface == String.downcase(surface)
  end

  defp lowercase_surface?(_surface), do: false

  defp acronymish_or_titlecase_noun_row?(row_word, row_pos) do
    row_pos == "noun" and row_word != "" and row_word != String.downcase(row_word)
  end

  # P-213: Unicode-punctuation-safe normalization for lookups.
  defp norm(value) when is_binary(value) do
    value
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp norm(_value), do: ""
end
