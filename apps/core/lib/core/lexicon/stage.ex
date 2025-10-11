# apps/core/lib/core/lexicon/stage.ex
defmodule Core.Lexicon.Stage do
  @moduledoc """
  Collect lexicon senses for tokens and UPSERT them into DB (definitions, examples, synonyms, antonyms).
  Adds a trace event with counts for quick debugging.

  Design goals:
  - Never crash the pipeline if the backing lexicon or DB layer is unavailable.
  - Avoid compile-time coupling to a specific storage module; prefer soft checks.
  - Accept flexible token shapes as long as they include a :phrase string and optional :n.
  - **No placeholders**: never emit `pos: "unk"` and never fabricate `|unk|`/`|seed|` IDs.
    We let the DB layer validate invariants.

  New in this refactor:
  - Seed fallbacks from tokens first (phrases + unigrams w/ known POS).
  - For **senses**, we now assign **stable indexed IDs** per (norm, POS): `norm|pos|1`, `norm|pos|2`, …
    (order is the order returned by the external lexicon, grouped per POS).
  """

  alias Core.SemanticInput, as: SI
  alias Core.Lexicon, as: CoreLex
  alias Db.Lexicon, as: DbLex

  @allowed_pos ~w(
    noun verb adjective adverb interjection phrase proper_noun pronoun
    determiner preposition conjunction numeral particle
  )

  @spec run(SI.t()) :: SI.t()
  def run(%SI{} = si) do
    tokens = si.tokens || []

    # 0) Seed from tokens first (phrases + unigrams with known POS); no-ops if nothing applicable.
    _ = safe_seed_from_tokens(tokens)

    # 0.1) Trace a quick POS coverage snapshot (helps explain why unigrams may or may not be seeded)
    {pos_known, pos_unknown} = pos_coverage(tokens)

    # 1) choose which phrases to look up (unigrams vs MWEs)
    {unigrams, mwes} = partition_tokens(tokens)

    words =
      (unigrams ++ mwes)
      |> Enum.map(&String.downcase/1)
      |> Enum.uniq()

    # 2) collect **indexed** rows from the Lexicon client (resilient to provider errors)
    raw_rows = Enum.flat_map(words, &rows_for_word/1)

    # 3) sanitize: drop unknown/unsupported POS, coerce list fields
    {rows, dropped} = sanitize_rows(raw_rows)

    # 4) upsert immediately, but never crash if storage is missing
    _ = safe_upsert(rows)

    # 5) trace for introspection
    put_trace(si, :lexicon_upserts, %{
      looked_up: length(words),
      senses_raw: length(raw_rows),
      senses_kept: length(rows),
      dropped_unknown_pos: dropped,
      pos_known_tokens: pos_known,
      pos_unknown_tokens: pos_unknown,
      words: words
    })
  end

  # ——— helpers ———

  # Partition into unigrams vs multiword expressions by token metadata.
  # Accepts maps/structs that expose :phrase (binary) and optional :n (integer n-gram length).
  defp partition_tokens(tokens) do
    tokens
    |> Enum.reduce({[], []}, fn
      %{phrase: p, n: 1}, {uns, mws} when is_binary(p) ->
        {[p | uns], mws}

      %{phrase: p, n: n}, {uns, mws} when is_binary(p) and is_integer(n) and n > 1 ->
        {uns, [p | mws]}

      # If :n is absent, treat as unigram to err on the side of dictionary coverage.
      %{phrase: p}, {uns, mws} when is_binary(p) ->
        {[p | uns], mws}

      _, acc ->
        acc
    end)
  end

  # Build **indexed** candidate rows for a single word.
  # We group senses by normalized POS, then enumerate 1..N per POS to build IDs like `norm|pos|1`.
  defp rows_for_word(word) when is_binary(word) and word != "" do
    case safe_lookup(word) do
      %{senses: senses} when is_list(senses) and senses != [] ->
        norm = normalize(word)

        senses
        |> Enum.map(fn s ->
          pos = normalize_pos(get(s, :pos))
          %{
            pos: pos,
            definition: get(s, :definition, get(s, :def)),
            example: get(s, :example, get(s, :ex)),
            synonyms: List.wrap(get(s, :synonyms, get(s, :syns, []))),
            antonyms: List.wrap(get(s, :antonyms, get(s, :ants, [])))
          }
        end)
        |> Enum.filter(&(&1.pos in @allowed_pos))
        |> Enum.group_by(& &1.pos)
        |> Enum.flat_map(fn {pos, list_for_pos} ->
          list_for_pos
          |> Enum.with_index(1)
          |> Enum.map(fn {s, idx} ->
            %{
              id: "#{norm}|#{pos}|#{idx}",
              word: word,
              norm: norm,
              pos: pos,
              type: "lexicon",
              definition: s.definition,
              example: s.example,
              synonyms: s.synonyms,
              antonyms: s.antonyms
            }
          end)
        end)

      _ ->
        []
    end
  end

  defp rows_for_word(_), do: []

  # Sanitize:
  # - drop rows with unknown/unsupported POS (incl. nil/"unk")
  # - keep only allowed POS (canonical set)
  # - ensure lists are lists and strings are strings
  defp sanitize_rows(rows) when is_list(rows) do
    rows
    |> Enum.reduce({[], 0}, fn r, {acc, dropped} ->
      pos = normalize_pos(get(r, :pos))

      cond do
        is_nil(pos) ->
          {acc, dropped + 1}

        pos not in @allowed_pos ->
          {acc, dropped + 1}

        true ->
          row =
            r
            |> Map.put(:pos, pos)
            |> Map.put(:word, to_string(get(r, :word, "")))
            |> Map.update(:synonyms, [], &List.wrap/1)
            |> Map.update(:antonyms, [], &List.wrap/1)
            |> Map.put_new(:type, "lexicon")

          {[row | acc], dropped}
      end
    end)
    |> then(fn {kept, d} -> {Enum.reverse(kept), d} end)
  end

  defp get(m, k, alt \\ nil), do: Map.get(m, k, Map.get(m, to_string(k), alt))

  # POS normalization to our canonical set (mirrors Db.* normalization)
  defp normalize_pos(nil), do: nil
  defp normalize_pos(pos) when is_atom(pos), do: normalize_pos(Atom.to_string(pos))
  defp normalize_pos(pos) when is_binary(pos) do
    pos =
      pos
      |> String.trim()
      |> String.downcase()

    case pos do
      # UD-ish tags & aliases
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
      "conj"  -> "conjunction"
      "part"  -> "particle"
      "propn" -> "proper_noun"
      "intj"  -> "interjection"
      # skip categories we never persist
      "punct" -> nil
      "sym"   -> nil
      "x"     -> nil
      # canonical already?
      c when c in @allowed_pos -> c
      # any other/unknown
      _ -> nil
    end
  end

  defp normalize(s) when is_binary(s) do
    s
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/, " ")
  end

  defp normalize(_), do: ""

  # Look up word through CoreLex without ever crashing.
  defp safe_lookup(word) when is_binary(word) do
    try do
      CoreLex.lookup(word)
    rescue
      _ -> %{word: word, senses: []}
    catch
      _, _ -> %{word: word, senses: []}
    end
  end

  # Perform upserts defensively: prefer CoreLex if it exposes a bulk_upsert,
  # then fallback to Db.Lexicon if present. No-ops for empty lists.
  defp safe_upsert(rows) when is_list(rows) do
    if rows == [] do
      :ok
    else
      try do
        cond do
          function_exported?(CoreLex, :bulk_upsert_senses, 1) ->
            CoreLex.bulk_upsert_senses(rows)

          Code.ensure_loaded?(DbLex) and function_exported?(DbLex, :bulk_upsert_senses, 1) ->
            DbLex.bulk_upsert_senses(rows)

          true ->
            :ok
        end
      rescue
        _ -> :ok
      catch
        _, _ -> :ok
      end
    end
  end

  # Seed from tokens using CoreLex's guarded seeding function.
  # This writes phrase fallbacks and unigrams with known allowed POS straight into Db.BrainCell
  # using canonical IDs (…|pos|fallback) with on_conflict: :nothing.
  defp safe_seed_from_tokens(tokens) do
    if is_list(tokens) do
      try do
        if function_exported?(CoreLex, :ensure_cells_from_tokens, 1) do
          CoreLex.ensure_cells_from_tokens(tokens)
        else
          :ok
        end
      rescue
        _ -> :ok
      catch
        _, _ -> :ok
      end
    else
      :ok
    end
  end

  # Simple coverage stat to aid debugging: how many tokens appear to have a usable POS?
  defp pos_coverage(tokens) do
    Enum.reduce(tokens, {0, 0}, fn tok, {yes, no} ->
      pos =
        Map.get(tok, :pos) ||
          Map.get(tok, "pos") ||
          get_in(tok, [:features, :pos]) ||
          get_in(tok, ["features", "pos"]) ||
          Map.get(tok, :tag) ||
          Map.get(tok, "tag") ||
          Map.get(tok, :upos) ||
          Map.get(tok, "upos") ||
          Map.get(tok, :xpos) ||
          Map.get(tok, "xpos")

      case normalize_pos(to_string_safe(pos)) do
        nil -> {yes, no + 1}
        "punct" -> {yes, no + 1}
        _ -> {yes + 1, no}
      end
    end)
  end

  defp to_string_safe(nil), do: nil
  defp to_string_safe(v) when is_binary(v), do: v
  defp to_string_safe(v) when is_atom(v), do: Atom.to_string(v)
  defp to_string_safe(v), do: to_string(v)

  defp put_trace(%SI{trace: tr} = si, stage, meta) do
    evt = %{stage: stage, ts_ms: System.monotonic_time(:millisecond), meta: Map.new(meta)}
    # Keep newest-first order for quick inspection (matches prior Core style used here).
    %{si | trace: [evt | (tr || [])]}
  end
end

