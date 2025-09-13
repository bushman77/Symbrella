defmodule Core.Segmenter do
  @moduledoc "Barebones segmenter stub: whitespace chunks → segments."

  @spec segment_phrases(String.t()) :: [%{text: String.t(), start: non_neg_integer(), stop: non_neg_integer()}]
  def segment_phrases(sentence) when is_binary(sentence) do
    # naive segmentation on non-space runs, preserving character spans
    Regex.scan(~r/\S+/, sentence, return: :index)
    |> Enum.map(fn [{start, len}] ->
      text = binary_part(sentence, start, len)
      %{text: text, start: start, stop: start + len}
    end)
  end
end

