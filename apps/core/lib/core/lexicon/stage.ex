defmodule Core.Lexicon.Stage do
  @moduledoc """
  Collect lexicon senses for tokens and UPSERT them into DB (defs, examples, synonyms, antonyms).
  Adds a trace event with counts for quick debugging.
  """

  alias Core.SemanticInput, as: SI
  alias Core.Lexicon, as: CoreLex

  @spec run(SI.t()) :: SI.t()
  def run(%SI{} = si) do
    # 1) choose which phrases to look up
    {unigrams, mwes} = partition_tokens(si.tokens || [])

    # prefer unigrams for dictionaries; include MWEs in case you have entries
    words =
      (unigrams ++ mwes)
      |> Enum.map(&String.downcase/1)
      |> Enum.uniq()

    # 2) collect rows (defs/examples/syns/ants) from your Lexicon client
    rows =
      words
      |> Enum.flat_map(&rows_for_word/1)

    # 3) upsert immediately (no-op if rows == [])
    if rows != [], do: CoreLex.bulk_upsert_senses(rows)

    # 4) trace for introspection
    put_trace(si, :lexicon_upserts, %{
      looked_up: length(words),
      upserts: length(rows),
      words: words
    })
  end

  # ——— helpers ———

  # Partition tokens: unigrams (n==1) first; MWEs later.
  # Your Token struct already has `n` and `phrase`.
  defp partition_tokens(tokens) do
    tokens
    |> Enum.reduce({[], []}, fn
      %{phrase: p, n: 1}, {uns, mws} when is_binary(p) ->
        {[p | uns], mws}

      %{phrase: p, n: n}, {uns, mws} when is_binary(p) and is_integer(n) and n > 1 ->
        {uns, [p | mws]}

      %{phrase: p}, acc when is_binary(p) ->
        acc

      _, acc ->
        acc
    end)
  end


  defp normalize_pos(nil), do: "unk"
  defp normalize_pos(pos) when is_binary(pos), do: String.downcase(pos)
  defp normalize_pos(pos) when is_atom(pos), do: pos |> Atom.to_string() |> String.downcase()

  # Wrap the lexicon client so failures never crash the pipeline
  defp safe_lookup(word) do
    try do
      # Your app’s client. Should return %{word: ..., senses: [%{pos, definition, example, synonyms, antonyms}, ...]}
      CoreLex.lookup(word)
    rescue
      _ -> %{word: word, senses: []}
    catch
      _, _ -> %{word: word, senses: []}
    end
  end

  defp put_trace(%SI{trace: tr} = si, stage, meta) do
    evt = %{stage: stage, ts_ms: System.monotonic_time(:millisecond), meta: Map.new(meta)}
    %{si | trace: [evt | tr || []]}
  end

defp get(map, k, alt \\ nil), do: Map.get(map, k, Map.get(map, to_string(k), alt))

defp rows_for_word(word) do
  case safe_lookup(word) do
    %{senses: senses} when is_list(senses) ->
      senses
      |> Enum.with_index()
      |> Enum.map(fn {s, i} ->
        pos = normalize_pos(get(s, :pos))
        %{
          id: "#{word}|#{pos}|#{i}",
          word: word,
          pos: pos,
          type: "lexicon",
          definition: get(s, :definition, get(s, :def)),
          example: get(s, :example, get(s, :ex)),
          synonyms: get(s, :synonyms, get(s, :syns, [])),
          antonyms: get(s, :antonyms, get(s, :ants, []))
        }
      end)

    _ -> []
  end
end

end

