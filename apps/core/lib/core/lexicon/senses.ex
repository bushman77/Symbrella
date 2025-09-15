defmodule Core.Lexicon.Senses do
  @moduledoc false
  alias Core.LexID

  @spec senses([map()]) :: [map()]
  def senses(entries) do
    # collect all defs, keep original order
    defs =
      for entry <- entries,
          meaning <- List.wrap(entry["meanings"] || []),
          {defn, d_idx} <- List.wrap(meaning["definitions"] || []) |> Enum.with_index() do
        %{
          word: entry["word"] || "",
          pos: meaning["partOfSpeech"],
          defn: defn["definition"],
          example: defn["example"]
        }
      end

    # re-index per (word,pos) so idx is dense and stable
    defs
    |> Enum.group_by(fn d -> {LexID.normalize_word(d.word), LexID.normalize_pos(d.pos)} end)
    |> Enum.flat_map(fn {{w, p}, group} ->
      group
      |> Enum.with_index()
      |> Enum.map(fn {d, idx} ->
        %{
          id: LexID.build(w, p, idx),
          word: w,
          pos: p,
          idx: idx,
          definition: d.defn,
          example: d.example
        }
      end)
    end)
  end
end

