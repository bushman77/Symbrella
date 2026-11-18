defmodule Core.Text.PassageTest do
  use ExUnit.Case, async: true

  alias Core.Text.Passage
  alias Core.Pipeline.Input

  @passage "John went to the store. He bought some bread. " <>
             "When he returned home, he made a sandwich. " <>
             "What did John buy?"

  test "segments a passage without losing source alignment" do
    assert {:ok, segments} = Passage.segment(@passage)

    assert Enum.map(segments, & &1.text) == [
             "John went to the store.",
             "He bought some bread.",
             "When he returned home, he made a sandwich.",
             "What did John buy?"
           ]

    assert Enum.map(segments, & &1.terminator) ==
             [:period, :period, :period, :question_mark]

    # Spans are UTF-8 byte offsets with an exclusive end.
    assert Enum.all?(segments, fn
             %{text: text, span: {first, last}} ->
               binary_part(@passage, first, last - first) == text

             _ ->
               false
           end)
  end

  test "does not split a decimal number into separate sentences" do
    passage = "The price was 3.50. What was the price?"

    assert {:ok, segments} = Passage.segment(passage)

    assert Enum.map(segments, & &1.text) == [
             "The price was 3.50.",
             "What was the price?"
           ]
  end

  test "preserves UTF-8 byte offsets across sentences" do
    passage = "José visited Montréal. He bought bread."

    assert {:ok, segments} = Passage.segment(passage)
    assert length(segments) == 2

    assert Enum.map(segments, & &1.text) == [
             "José visited Montréal.",
             "He bought bread."
           ]

    assert Enum.all?(segments, fn segment ->
             {first, last} = segment.span

             binary_part(passage, first, last - first) ==
               segment.text
           end)
  end

  test "aligns each sentence with canonical word indices" do
    si = Input.prepare(@passage)

    assert {:ok, segments} = Passage.align(si)

    assert Enum.map(segments, & &1.word_range) == [
             {0, 4},
             {5, 8},
             {9, 16},
             {17, 20}
           ]

    assert Enum.at(segments, 1).word_indices == [5, 6, 7, 8]

    all_indices =
      Enum.flat_map(segments, & &1.word_indices)

    assert all_indices == Enum.map(si.tokens, & &1.index)
  end

  test "rejects damaged token alignment" do
    si = Input.prepare(@passage)

    first = hd(si.tokens)
    damaged = %{first | span: {1, 5}}

    altered = %{
      si
      | tokens: List.replace_at(si.tokens, 0, damaged)
    }

    assert {:error, :invalid_alignment} = Passage.align(altered)
  end

  test "aligns Unicode words using original byte offsets" do
    si = Input.prepare("José bought bread. He ate it.")

    assert {:ok, segments} = Passage.align(si)

    assert Enum.map(segments, & &1.word_range) == [
             {0, 2},
             {3, 5}
           ]
  end
end
