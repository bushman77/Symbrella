defmodule Core.Lexicon.Stage do
  @moduledoc """
  Collect lexicon senses for tokens and UPSERT them into DB (defs, examples, synonyms, antonyms).
  Adds a trace event with counts for quick debugging.

  BrainCell `id` format (for lexicon rows):
    "<norm>|<pos>|lexicon|<index>"
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
      |> Enum.map(&norm_text/1)     # normalize once for de-dupe
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
  defp partition_tokens(tokens) do
    tokens
    |> Enum.reduce({[], []}, fn
      %{phrase: p, n: 1}, {uns, mws} when is_binary(p) ->
        {[p | uns], mws}

      %{phrase: p, n: n}, {uns, mws} when is_binary(p) and is_integer(n) and n > 1 ->
        {uns, [p | mws]}

      # ignore non-binary phrases / malformed tokens
      _, acc ->
        acc
    end)
  end

  # Build one BrainCell row per lexicon sense.
  defp rows_for_word(word) when is_binary(word) do
    case safe_lookup(word) do
      %{senses: senses} when is_list(senses) ->
        now   = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)
        norm  = norm_text(word)
        src   = "lexicon"

        senses
        |> Enum.with_index()
        |> Enum.map(fn {s, i} ->
          pos = normalize_pos(get(s, :pos))

          %{
            # 4-part id prevents collisions with seeds/other sources:
            #   "<norm>|<pos>|lexicon|<i>"
            id: [norm, pos, src, Integer.to_string(i)] |> Enum.join("|"),

            word: word,
            norm: norm,
            pos: pos,
            type: "lexicon",
            status: "active",

            definition: blank_to_nil(get(s, :definition, get(s, :def))),
            example:    blank_to_nil(get(s, :example,    get(s, :ex))),
            synonyms:   listify(get(s, :synonyms, get(s, :syns, []))),
            antonyms:   listify(get(s, :antonyms, get(s, :ants, []))),

            gram_function: nil,
            semantic_atoms: [],
            activation: 0.0,
            modulated_activation: 0.0,
            dopamine: 0.0,
            serotonin: 0.0,
            connections: [],
            inserted_at: now,
            updated_at: now
          }
        end)
        # protect id uniqueness if upstream sends dup senses
        |> Enum.uniq_by(& &1.id)

      _ ->
        []
    end
  end

  # POS normalization
  defp normalize_pos(nil), do: "unk"
  defp normalize_pos(pos) when is_binary(pos), do: String.downcase(pos)
  defp normalize_pos(pos) when is_atom(pos), do: pos |> Atom.to_string() |> String.downcase()

  # Wrap the lexicon client so failures never crash the pipeline
  defp safe_lookup(word) do
    try do
      # Should return %{word: ..., senses: [%{pos, definition, example, synonyms, antonyms}, ...]}
      CoreLex.lookup(word)
    rescue
      _ -> %{word: word, senses: []}
    catch
      _, _ -> %{word: word, senses: []}
    end
  end

  # Map/string key helper
  defp get(map, k, alt \\ nil), do: Map.get(map, k, Map.get(map, to_string(k), alt))

  # Lists: accept string or list; normalize strings to single-element list
  defp listify(nil), do: []
  defp listify(v) when is_binary(v), do: [v]
  defp listify(v) when is_list(v), do: Enum.map(v, &to_string/1)
  defp listify(v), do: [to_string(v)]

  # Text normalization for norms
  defp norm_text(nil), do: ""
  defp norm_text(v) when is_binary(v),
    do: v |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()
  defp norm_text(v),
    do:
      v
      |> Kernel.to_string()
      |> String.downcase()
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()

  # Definition/example trimming
  defp blank_to_nil(nil), do: nil
  defp blank_to_nil(s) when is_binary(s) do
    trimmed = s |> String.replace(~r/\s+/u, " ") |> String.trim()
    if trimmed == "", do: nil, else: trimmed
  end

  defp put_trace(%SI{trace: tr} = si, stage, meta) do
    evt = %{stage: stage, ts_ms: System.monotonic_time(:millisecond), meta: Map.new(meta)}
    %{si | trace: [evt | tr || []]}
  end
end

