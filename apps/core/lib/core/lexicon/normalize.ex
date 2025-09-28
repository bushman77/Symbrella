defmodule Core.Lexicon.Normalize do
  @moduledoc false

  @spec defs([map()]) :: [map()]
  def defs(entries) do
    for entry <- entries,
        meaning <- List.wrap(entry["meanings"] || []),
        defn <- List.wrap(meaning["definitions"] || []) do
      %{
        word: entry["word"],
        pos: meaning["partOfSpeech"],
        definition: defn["definition"],
        example: defn["example"],
        synonyms: defn["synonyms"] || [],
        antonyms: defn["antonyms"] || []
      }
    end
  end
end
