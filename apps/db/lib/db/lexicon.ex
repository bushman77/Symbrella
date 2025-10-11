# apps/db/lib/db/lexicon.ex
defmodule Db.Lexicon do
  @moduledoc """
  DB helpers for Lexicon/Brain integration.

  ## RETURN SHAPES

  - fetch_by_norms/1 → [sense_map, ...] (flat list; Core-safe)
  - fetch_by_ids/1 → [sense_map, ...] (flat list)
  - fetch_by_norms_grouped/1 → %{norm => [sense_map, ...]}
  - ensure_pos_variants_from_tokens/2 → [sense_map, ...] (flat list of seeded rows)
  - lookup/2 and definitions_for/2 → [pmg_evidence_map, ...] (PMTG-friendly subset)
  - lookup_synonyms/3 → {:ok, [entry], meta} (Core.Recall.Synonyms external provider target)

  ## SENSE MAP FIELDS (Core/LIFG friendly)

    id :: String.t()
    word :: String.t()
    lemma :: String.t()
    norm :: String.t()
    pos :: String.t()
    type :: String.t()   # "lexicon" | "word" | "phrase"
    definition :: String.t() | nil
    example :: String.t() | nil
    synonyms :: [String.t()]
    antonyms :: [String.t()]
    features :: %{lemma: String.t(), pos: String.t()}

  ## Invariants (align with schema & DB constraints)

  - POS must be in allowed set; never "unk".
  - ID shape: `text|pos` or `text|pos|suffix` (alnum/underscore); no trailing pipe.
  - ID POS must equal `pos` column.
  - ID must not contain `|unk|` or `|seed|`.
  - We **never** fabricate single-word placeholder rows.
  - For phrases with no senses yet, we may create conservative fallbacks elsewhere;
    here we only upsert concrete senses or explicit POS variants requested by caller.
  """

  import Ecto.Query, warn: false
  alias Db
  alias Db.BrainCell

  # External tag inventory some callers may pass (UD-style short tags).
  @default_pos_inventory ~w(noun verb adj adv pron det adp num cconj sconj part propn punct intj sym x)

  # Our canonical allowed POS (matches Db.BrainCell / Brain.Cell / DB CHECKs)
  @allowed_pos ~w(
    noun verb adjective adverb interjection phrase proper_noun pronoun
    determiner preposition conjunction numeral particle
  )

  # ID regex (two or three segments; third is alnum/_ only)
  @id_regex ~r/^[^|]+?\|(noun|verb|adjective|adverb|interjection|phrase|proper_noun|pronoun|determiner|preposition|conjunction|numeral|particle)(\|[A-Za-z0-9_]+)?$/

  @upsert_chunk 500

  # ---------- Public Reads ----------

  @doc """
  Fetch lexicon senses whose norm is in `norms`.

  Returns a flat list of sense maps.
  """
  @spec fetch_by_norms([String.t()]) :: [map()]
  def fetch_by_norms(norms) when is_list(norms) do
    norms =
      norms
      |> Enum.map(&normize/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.uniq()

    if norms == [] do
      []
    else
      Db.all(
        from b in BrainCell,
          where: b.norm in ^norms and b.status == "active",
          select: b
      )
      |> Enum.map(&map_for_core/1)
    end
  end

  @doc """
  Same as fetch_by_norms/1 but grouped by norm.

  Returns a map %{norm => [sense_map, ...]}.
  """
  @spec fetch_by_norms_grouped([String.t()]) :: %{optional(String.t()) => [map()]}
  def fetch_by_norms_grouped(norms) when is_list(norms) do
    fetch_by_norms(norms) |> Enum.group_by(& &1.norm)
  end

  @doc """
  Fetch senses by exact IDs. Returns a flat list of sense maps.
  """
  @spec fetch_by_ids([String.t()]) :: [map()]
  def fetch_by_ids(ids) when is_list(ids) do
    ids = ids |> Enum.map(&to_string/1) |> Enum.uniq()

    if ids == [] do
      []
    else
      Db.all(
        from b in BrainCell,
          where: b.id in ^ids,
          select: b
      )
      |> Enum.map(&map_for_core/1)
    end
  end

  # ---------- Seeding / Upserts ----------

  @doc """
  Insert/Update lexicon senses as BrainCell rows.

  Accepted row keys (string or atom keys are both OK):

    :id (optional; if absent we generate a stable `norm|pos|lex_<hash>`)
    :word
    :pos
    :type ("lexicon" | "word" | "phrase")
    :definition (optional)
    :example (optional)
    :synonyms (list)
    :antonyms (list)
    :semantic_atoms (list)

  Upsert policy:
    - Conflict on :id
    - On conflict, replace: :word, :norm, :pos, :type, :definition, :example, :synonyms, :antonyms, :updated_at

  All rows are sanitized:
    - POS normalized to canonical set (UD tags get mapped).
    - Rows with unknown/unsupported POS are dropped.
    - Provided `id` must pass shape/consistency checks; otherwise we generate one.
    - Any `|unk|`/`|seed|` in ID are rejected.
  """
  @spec bulk_upsert_senses(list()) :: :ok
  def bulk_upsert_senses([]), do: :ok

  def bulk_upsert_senses(rows) when is_list(rows) do
    now = now_naive()

    rows1 =
      rows
      |> Enum.map(&normalize_row(&1, now))
      |> Enum.reject(&is_nil/1)

    rows1
    |> Enum.chunk_every(@upsert_chunk)
    |> Enum.each(fn chunk ->
      _ =
        Db.insert_all(
          BrainCell,
          chunk,
          on_conflict:
            {:replace,
             [:word, :norm, :pos, :type, :definition, :example, :synonyms, :antonyms, :updated_at]},
          conflict_target: [:id]
        )

      :ok
    end)

    :ok
  end

  def bulk_upsert_senses(_), do: :ok

  @doc """
  Seed POS variants for tokens. Pass `only_mw: true` to restrict to MWEs.

  Accepts tokens shaped like `%{phrase: String.t(), mw: boolean}` or any map/struct
  that has a :phrase key. No compile-time dependency on Core.Token.

  Returns a flat list of the seeded rows as sense maps.

  Notes:
    - POS inventory accepts UD-style tags and canonical strings; we map to canonical and filter.
    - IDs are shaped `norm|pos|<type>` (e.g., "hello there|phrase|phrase") to dedupe naturally.
  """
  @spec ensure_pos_variants_from_tokens(list(), keyword()) :: [map()]
  def ensure_pos_variants_from_tokens(tokens, opts \\ []) do
    only_mw?      = !!opts[:only_mw]
    gram_function = opts[:gram_function] || ""
    type_override = opts[:type]
    pos_list =
      pos_inventory(opts)
      |> Enum.map(&normalize_pos/1)
      |> Enum.filter(&(&1 in @allowed_pos))
      |> Enum.uniq()

    now_naive = now_naive()

    selected =
      tokens
      |> Enum.map(&coerce_token/1)
      |> Enum.reject(&is_nil/1)
      |> then(fn toks -> if only_mw?, do: Enum.filter(toks, & &1.mw), else: toks end)

    rows =
      for %{phrase: phrase, mw: mw?} <- selected, pos <- pos_list do
        norm = normize(phrase)
        type = type_override || if(mw?, do: "phrase", else: "word")
        id   = variant_id(norm, pos, type)

        %{
          id: id,
          word: phrase,
          norm: norm,
          pos: pos,
          type: type,
          gram_function: gram_function,
          status: "active",
          synonyms: [],
          antonyms: [],
          semantic_atoms: [],
          activation: 0.0,
          modulated_activation: 0.0,
          dopamine: 0.0,
          serotonin: 0.0,
          connections: [],
          inserted_at: now_naive,
          updated_at: now_naive
        }
      end
      |> Enum.uniq_by(& &1.id)

    # Insert if not present (composite natural key across norm/pos/type/gram_function)
    _ =
      Db.insert_all(
        BrainCell,
        rows,
        on_conflict: :nothing,
        conflict_target:
          {:unsafe_fragment,
           " (norm, COALESCE(pos,''), COALESCE(type,''), COALESCE(gram_function,'')) "}
      )

    ids = Enum.map(rows, & &1.id)

    _ =
      from(b in BrainCell, where: b.id in ^ids)
      |> Db.update_all(set: [status: "active", updated_at: now_naive])

    Db.all(from(b in BrainCell, where: b.id in ^ids))
    |> Enum.map(&to_plain_map/1)
  end

  # ---------- PMTG compatibility ----------

  @doc """
  Return up to `limit` senses for a lemma, shaped for PMTG evidence.
  Prefers rows with type = 'lexicon'.
  """
  @spec lookup(String.t(), non_neg_integer()) :: [map()]
  def lookup(lemma, limit) when is_binary(lemma) and is_integer(limit) do
    norm = normize(lemma)

    Db.all(
      from b in BrainCell,
        where: b.norm == ^norm and b.status == "active",
        order_by: [
          asc: fragment("CASE WHEN ? = 'lexicon' THEN 0 ELSE 1 END", b.type),
          desc: b.updated_at
        ],
        limit: ^max(limit, 0),
        select: %{
          id: b.id,
          lemma: b.word,
          pos: b.pos,
          type: b.type,
          gloss: b.definition,
          example: b.example,
          synonyms: b.synonyms,
          antonyms: b.antonyms
        }
    )
  end

  @doc "Alias kept for compatibility with other callers."
  @spec definitions_for(String.t(), non_neg_integer()) :: [map()]
  def definitions_for(lemma, limit), do: lookup(lemma, limit)

  # ---------- Synonyms Provider (External MFA Target) ----------

  @doc """
  Provide synonyms for `word` (and optional `pos`) for Core.Recall.Synonyms.

  Returns:
    {:ok, [entry], %{source: \"db_lexicon\", took_ms: integer, counts: map()}}

  Each entry:
    %{lemma: String.t(), pos: String.t() | nil, prior: float, source: String.t(), meta: map()}
  """
  @spec lookup_synonyms(String.t(), String.t() | atom() | nil, keyword()) ::
          {:ok, [map()], map()} | {:ok, [map()]} | {:error, term()}
  def lookup_synonyms(word, pos, opts \\ []) when is_binary(word) do
    t0 = System.monotonic_time(:millisecond)

    norm        = normize(word)
    pos         = normalize_pos(pos)
    limit       = Keyword.get(opts, :limit, 64)
    include_id? = Keyword.get(opts, :include_identity, true)

    identity =
      if include_id? do
        [%{lemma: norm, pos: pos, prior: 1.0, source: "db/identity", meta: %{}}]
      else
        []
      end

    # Direct rows for this norm (optional pos filter)
    direct_rows =
      Db.all(
        from b in BrainCell,
          where:
            b.norm == ^norm and b.status == "active" and
              (is_nil(^pos) or b.pos == ^pos),
          select: %{pos: b.pos, synonyms: b.synonyms, word: b.word, type: b.type}
      )

    direct_entries =
      direct_rows
      |> Enum.flat_map(fn %{pos: p, synonyms: syns} ->
        syns = List.wrap(syns)
        for s <- syns, is_binary(s) do
          s_norm = normize(s)
          %{lemma: s_norm, pos: p, prior: 0.85, source: "db/direct", meta: %{}}
        end
      end)

    lw = String.downcase(word)

    reverse_rows =
      Db.all(
        from b in BrainCell,
          where:
            b.status == "active" and
              (is_nil(^pos) or b.pos == ^pos) and
              (fragment("? = ANY(?)", ^word, b.synonyms) or
               fragment("? = ANY(?)", ^lw,   b.synonyms)),
          select: %{pos: b.pos, word: b.word, type: b.type}
      )

    reverse_entries =
      for %{pos: p, word: w} <- reverse_rows do
        %{lemma: normize(w), pos: p, prior: 0.75, source: "db/reverse", meta: %{}}
      end

    entries =
      identity ++ direct_entries ++ reverse_entries
      |> Enum.reduce(%{}, fn e = %{lemma: l, pos: p}, acc ->
        Map.update(acc, {l, p}, e, fn prev ->
          if (prev[:prior] || 0.0) >= (e[:prior] || 0.0), do: prev, else: e
        end)
      end)
      |> Map.values()
      |> Enum.sort_by(&{-(&1[:prior] || 0.0), &1.lemma})
      |> Enum.take(limit)

    took_ms = System.monotonic_time(:millisecond) - t0
    meta = %{
      source: "db_lexicon",
      took_ms: took_ms,
      counts: %{
        identity: length(identity),
        direct_rows: length(direct_rows),
        reverse_rows: length(reverse_rows),
        returned: length(entries)
      }
    }

    {:ok, entries, meta}
  rescue
    e -> {:error, e}
  end

  # ---------- Helpers ----------

  # Normalize BrainCell to a Core-friendly shape.
  defp map_for_core(%BrainCell{} = b) do
    %{
      id: b.id,
      type: b.type,
      word: b.word,
      lemma: b.word,
      norm: b.norm,
      pos: b.pos,
      definition: b.definition,
      example: b.example,
      synonyms: b.synonyms || [],
      antonyms: b.antonyms || [],
      features: %{
        lemma: b.word,
        pos: b.pos
      }
    }
  end

  defp to_plain_map(%BrainCell{} = b), do: map_for_core(b)

  defp normize(p),
    do: p |> to_string() |> String.downcase() |> String.trim() |> String.replace(~r/\s+/, " ")

  # -- POS normalization: map UD tags to our canonical POS set ----------------
  defp normalize_pos(nil), do: nil
  defp normalize_pos(p) when is_atom(p), do: normalize_pos(Atom.to_string(p))
  defp normalize_pos(p) when is_binary(p) do
    p =
      p
      |> String.trim()
      |> String.downcase()

    case p do
      "n"     -> "noun"
      "v"     -> "verb"
      "adj"   -> "adjective"
      "adv"   -> "adverb"
      "pron"  -> "pronoun"
      "det"   -> "determiner"
      "adp"   -> "preposition"
      "num"   -> "numeral"
      "cconj" -> "conjunction"
      "sconj" -> "conjunction"
      "part"  -> "particle"
      "propn" -> "proper_noun"
      "intj"  -> "interjection"
      "punct" -> nil        # ignore punctuation in lexicon rows
      "sym"   -> nil
      "x"     -> nil
      # already canonical?
      c when c in @allowed_pos -> c
      _other -> nil
    end
  end

  defp get(m, k, alt \\ nil), do: Map.get(m, k, Map.get(m, to_string(k), alt))

  # Build a stable lexicon id (exactly 3 segments: norm|pos|lex_<hash>)
  defp build_lex_id(norm, pos, r) do
    base = :erlang.phash2({norm, pos, get(r, :definition)})
    "#{norm}|#{pos}|lex_#{base}"
  end

  # Sanitize an incoming row for upsert.
  # Returns a map ready for Db.insert_all/3, or nil to drop.
  defp normalize_row(r, now) when is_map(r) do
    word = get(r, :word, "")
    norm = normize(word)
    pos0 = get(r, :pos)
    pos  = normalize_pos(pos0)
    type = get(r, :type, "lexicon")

    cond do
      norm == "" ->
        nil

      is_nil(pos) or pos not in @allowed_pos ->
        # Unknown/unsupported POS → drop row
        nil

      true ->
        # ID handling
        id0 = get(r, :id)
        id1 =
          case id0 do
            nil -> build_lex_id(norm, pos, r)
            s when is_binary(s) ->
              s
              |> String.trim()
              |> String.replace(~r/\|+$/u, "")
          end

        # Reject placeholders or bad shapes; otherwise ensure id/pos consistent.
        cond do
          String.contains?(id1, "|unk|") or String.contains?(id1, "|seed|") ->
            nil

          not Regex.match?(@id_regex, id1) ->
            # If the provided ID was malformed, generate a safe one
            build_lex_id(norm, pos, r)

          split_pos(id1) != pos ->
            # Override with consistent id
            build_lex_id(norm, pos, r)

          true ->
            id1
        end
        |> case do
          nil -> nil
          id ->
            %{
              id: id,
              status: "active",
              type: type,
              norm: norm,
              pos: pos,
              word: word,
              definition: get(r, :definition),
              example: get(r, :example),
              synonyms: clean_str_list(get(r, :synonyms, [])),
              antonyms: clean_str_list(get(r, :antonyms, [])),
              semantic_atoms: clean_str_list(get(r, :semantic_atoms, [])),
              inserted_at: now,
              updated_at: now
            }
        end
    end
  end

  defp normalize_row(_other, _now), do: nil

  # Accept any map/struct that has a :phrase key (matches Core.Token at runtime).
  defp coerce_token(%{phrase: p} = m) when is_binary(p) do
    %{phrase: p, mw: !!Map.get(m, :mw)}
  end
  defp coerce_token(_), do: nil

  # Variant ID used for ensure_pos_variants_from_tokens/2 (dedupe by norm/pos/type)
  defp variant_id(norm, pos, type) do
    suffix =
      type
      |> to_string()
      |> String.trim()
      |> String.replace(~r/[^A-Za-z0-9_]/u, "_")
      |> case do
        "" -> "var"
        s  -> s
      end

    "#{norm}|#{pos}|#{suffix}"
  end

  defp clean_str_list(list) when is_list(list) do
    list
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end
  defp clean_str_list(_), do: []

  defp split_pos(id) when is_binary(id) do
    id
    |> String.split("|", trim: true)
    |> Enum.at(1)
  end
  defp split_pos(_), do: nil

  defp pos_inventory(opts) do
    inv = opts[:pos_inventory] || Application.get_env(:symbrella, :pos_inventory) || @default_pos_inventory
    List.wrap(inv)
  end

  defp now_naive do
    NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)
  end
end

