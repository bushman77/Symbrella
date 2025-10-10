defmodule Core.Lexicon.Senses do
  @moduledoc """
  Helpers for building/upserting lexicon senses from meanings data.
  """

  @type meaning :: map()
  @type sense_row :: map()

  @doc """
  Extract sense rows from a meaning map (structure may vary).
  Safe on missing keys; trims empty definitions.
  """
  @spec senses(meaning()) :: [sense_row()]
  def senses(meaning) when is_map(meaning) do
    defs = List.wrap(meaning["definitions"] || [])

    for {defn, _d_idx} <- defs |> Enum.with_index(),
        is_binary(defn) and String.trim(defn) != "" do
      %{
        definition: String.trim(defn),
        lemma: meaning["lemma"] || meaning["word"] || "",
        pos: meaning["pos"] || meaning["part_of_speech"] || nil,
        provider: meaning["provider"] || meaning["source"] || "unknown"
      }
    end
  end
end
