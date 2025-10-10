defmodule Core.Lexicon.Stage do
  @moduledoc """
  Collect lexicon senses for tokens and UPSERT them into DB (definitions, examples, synonyms, antonyms).
  Adds a trace event with counts for quick debugging.

  Design goals:
  - Never crash the pipeline if the backing lexicon or DB layer is unavailable.
  - Avoid compile-time coupling to a specific storage module; prefer soft checks.
  - Accept flexible token shapes as long as they include a :phrase string and optional :n.
  """

  alias Core.SemanticInput, as: SI
  alias Core.Lexicon, as: CoreLex

  @spec run(SI.t()) :: SI.t()
  def run(%SI{} = si) do
    # 1) choose which phrases to look up
    {unigrams, mwes} = partition_tokens(si.tokens || [])

    words =
      (unigrams ++ mwes)
      |> Enum.map(&String.downcase/1)
      |> Enum.uniq()

    # 2) collect rows from the Lexicon client (resilient to provider errors)
    rows = Enum.flat_map(words, &rows_for_word/1)

    # 3) upsert immediately, but never crash if storage is missing
    _ = safe_upsert(rows)

    # 4) trace for introspection
    put_trace(si, :lexicon_upserts, %{
      looked_up: length(words),
      upserts: length(rows),
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

  defp rows_for_word(word) when is_binary(word) and word != "" do
    case safe_lookup(word) do
      %{senses: senses} when is_list(senses) ->
        senses
        |> Enum.with_index()
        |> Enum.map(fn {s, i} ->
          pos = normalize_pos(get(s, :pos))

          %{
            id: word <> "|" <> pos <> "|" <> Integer.to_string(i),
            word: word,
            pos: pos,
            type: "lexicon",
            definition: get(s, :definition, get(s, :def)),
            example: get(s, :example, get(s, :ex)),
            synonyms: List.wrap(get(s, :synonyms, get(s, :syns, []))),
            antonyms: List.wrap(get(s, :antonyms, get(s, :ants, [])))
          }
        end)

      _ ->
        []
    end
  end

  defp rows_for_word(_), do: []

  defp get(m, k, alt \\ nil), do: Map.get(m, k, Map.get(m, to_string(k), alt))

  defp normalize_pos(nil), do: "unk"
  defp normalize_pos(pos) when is_binary(pos), do: String.downcase(pos)
  defp normalize_pos(pos) when is_atom(pos), do: pos |> Atom.to_string() |> String.downcase()

  # Look up word through CoreLex without ever crashing.
  # CoreLex.lookup/1 already handles external client arity differences and fallbacks.
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
          function_exported?(CoreLex, :bulk_upsert_senses, 1) -> CoreLex.bulk_upsert_senses(rows)
          Code.ensure_loaded?(Db.Lexicon) and function_exported?(Db.Lexicon, :bulk_upsert_senses, 1) ->
            Db.Lexicon.bulk_upsert_senses(rows)
          true -> :ok
        end
      rescue
        _ -> :ok
      catch
        _, _ -> :ok
      end
    end
  end

  defp put_trace(%SI{trace: tr} = si, stage, meta) do
    evt = %{stage: stage, ts_ms: System.monotonic_time(:millisecond), meta: Map.new(meta)}
    # Keep newest-first order for quick inspection (matches prior Core style used here).
    %{si | trace: [evt | (tr || [])]}
  end
end

