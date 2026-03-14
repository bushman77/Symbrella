defmodule Db do
  @moduledoc """
  Umbrella-wide Repo (single database).

  Long-term memory (LTM) helpers:
  • Collect normalized `norm`s from `si.tokens` (preserving token indices).
  • Load matching `Db.BrainCell` rows (active-only by default).
  • Annotate returned rows with `token_index` so downstream LIFG bucketing works.
  • Report which norms are missing so callers can decide what to do (no auto-enrich).
  • Return loaded rows along with a set of DB hits.

  ## Options
    * `:only_active` (boolean, default: `true`) — filter rows with `status == "active"`.
    * `:limit_per_norm` (pos_integer | :all, default: `:all`) — cap rows returned per norm.

  ## Return shape
  `ltm/2` returns rows as **plain maps** (not Ecto structs) so we can include:

    * `:token_index` — the token position in the input SI
    * `:id`, `:lemma`, `:norm`, `:pos`, `:definition`, `:example`, `:synonyms`, `:antonyms`, `:score`, `:source`

  This is intentional: DB BrainCell rows are lexicon senses and do not carry token indices;
  LIFG Stage1 buckets candidates by token index.
  """

  use Ecto.Repo,
    otp_app: :db,
    adapter: Ecto.Adapters.Postgres,
    priv: "priv/db"

  import Ecto.Query
  alias Db.BrainCell

  @type norm :: String.t()

  # Item 1: closed-class inventory pruning at LTM retrieval time.
  # This intentionally handles only the currently observed troublemakers.
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
  Look up BrainCell rows for the tokens inside an SI-like map.

  `si.tokens` may contain:
  • maps with `:phrase` (preferred) or `:norm` (or string keys)
  • plain strings

  Returns `{:ok, %{rows: rows, missing_norms: missing, db_hits: MapSet.t()}}`.

  Rows are returned as plain maps with a required `:token_index` key.

  See module docs for options.
  """
  @spec ltm(map(), keyword()) ::
          {:ok,
           %{
             rows: [map()],
             missing_norms: [binary()],
             db_hits: MapSet.t()
           }}
  def ltm(si, opts \\ [])

  def ltm(%{tokens: tokens} = _si, opts) when is_list(tokens) do
    only_active? = Keyword.get(opts, :only_active, true)
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
      token_meta_by_idx = token_meta_by_idx(tokens)

      where_dyn =
        if only_active? do
          dynamic([b], b.status == "active" and b.norm in ^uniq_norms)
        else
          dynamic([b], b.norm in ^uniq_norms)
        end

      base_q =
        from(b in BrainCell,
          where: ^where_dyn
        )

      q =
        case limit_per_norm do
          :all ->
            from(b in base_q, select: b)

          n when is_integer(n) and n > 0 ->
            q1 =
              from(b in base_q,
                select: %{b: b, rn: over(row_number(), :norm_part)},
                windows: [norm_part: [partition_by: b.norm, order_by: [desc: b.updated_at]]]
              )

            from(s in subquery(q1),
              where: s.rn <= ^n,
              select: s.b
            )

          _ ->
            from(b in base_q, select: b)
        end

      rows0 = Db.all(q)

      token_idxs_by_norm =
        token_norms
        |> Enum.reduce(%{}, fn {idx, n}, acc ->
          Map.update(acc, n, [idx], fn lst -> [idx | lst] end)
        end)
        |> Enum.into(%{}, fn {n, idxs} -> {n, Enum.uniq(idxs)} end)

      rows =
        rows0
        |> Enum.flat_map(fn row ->
          idxs = Map.get(token_idxs_by_norm, row.norm, [])

          Enum.flat_map(idxs, fn idx ->
            token_meta = Map.get(token_meta_by_idx, idx, %{})

            if keep_ltm_row_for_token?(row, token_meta) do
              [braincell_to_candidate_map(row, idx)]
            else
              []
            end
          end)
        end)

      kept_norms = MapSet.new(for r <- rows, do: r.norm)
      missing = Enum.reject(uniq_norms, &MapSet.member?(kept_norms, &1))
      hits = kept_norms

      {:ok, %{rows: rows, missing_norms: missing, db_hits: hits}}
    end
  end

  def ltm(_si, _opts), do: {:ok, %{rows: [], missing_norms: [], db_hits: MapSet.new()}}

  @doc """
  Returns `true` if a *word* exists by `norm` in `brain_cells`.

  Guards invalid/blank inputs without querying.
  When `:only_active` is `true` (default), requires `status == "active"`.
  """
  @spec word_exists?(term(), keyword()) :: boolean()
  def word_exists?(term, opts \\ [])

  def word_exists?(term, opts) when is_binary(term) do
    only_active? = Keyword.get(opts, :only_active, true)

    case norm(term) do
      "" ->
        false

      n ->
        where_dyn =
          if only_active? do
            dynamic([b], b.status == "active" and b.norm == ^n)
          else
            dynamic([b], b.norm == ^n)
          end

        from(b in BrainCell, where: ^where_dyn, select: true)
        |> Db.exists?()
    end
  end

  def word_exists?(_, _opts), do: false

  def insrt_all(table, rows, opt) do
    insert_all(table, rows, opt)
  end

  # -- helpers ----------------------------------------------------------------

  defp braincell_to_candidate_map(%BrainCell{} = r, token_index) when is_integer(token_index) do
    %{
      id: r.id,
      token_index: token_index,
      lemma: to_string(r.norm || r.word || ""),
      norm: to_string(r.norm || ""),
      word: to_string(r.word || ""),
      pos: r.pos,
      definition: r.definition,
      example: r.example,
      synonyms: List.wrap(r.synonyms),
      antonyms: List.wrap(r.antonyms),
      score: 0.5,
      source: :ltm
    }
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

  defp token_meta_by_idx(_), do: %{}

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

  defp keep_ltm_row_for_token?(%BrainCell{}, _), do: true

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
    s = String.trim(surface)
    s != "" and s == String.downcase(s)
  end

  defp lowercase_surface?(_), do: false

  defp acronymish_or_titlecase_noun_row?(row_word, row_pos) do
    row_pos == "noun" and row_word != "" and row_word != String.downcase(row_word)
  end

  # Accept token map or string, extract phrase text
  defp token_to_phrase(%{phrase: p}) when is_binary(p), do: p
  defp token_to_phrase(%{"phrase" => p}) when is_binary(p), do: p
  defp token_to_phrase(%{norm: n}) when is_binary(n), do: n
  defp token_to_phrase(%{"norm" => n}) when is_binary(n), do: n
  defp token_to_phrase(s) when is_binary(s), do: s
  defp token_to_phrase(_), do: ""

  # Prefer explicit token index when present; fall back to enumeration index.
  defp token_index(%{index: i}, _fallback) when is_integer(i) and i >= 0, do: i
  defp token_index(%{"index" => i}, _fallback) when is_integer(i) and i >= 0, do: i
  defp token_index(%{token_index: i}, _fallback) when is_integer(i) and i >= 0, do: i
  defp token_index(%{"token_index" => i}, _fallback) when is_integer(i) and i >= 0, do: i

  defp token_index(_tok, fallback_idx) when is_integer(fallback_idx) and fallback_idx >= 0,
    do: fallback_idx

  # P-213: Unicode-punctuation-safe normalization for lookups
  defp norm(nil), do: ""

  defp norm(s) when is_binary(s) do
    s
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp norm(_), do: ""
end
