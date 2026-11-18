defmodule Core.Semantic.PassageTest do
  use ExUnit.Case, async: true

  alias Core.Pipeline.Input
  alias Core.Semantic.Passage

  @passage "John went to the store. He bought some bread. " <>
             "When he returned home, he made a sandwich. " <>
             "What did John buy?"

  test "projects local sentence tokens with original provenance" do
    si = Input.prepare(@passage)

    assert {:ok, projection} = Passage.project(si)

    assert projection.status == :segmented_not_interpreted
    assert projection.original_text == @passage
    assert projection.factual_authority? == false

    segments = projection.segments

    assert length(segments) == 4

    assert Enum.map(segments, & &1.source_word_range) == [
             {0, 4},
             {5, 8},
             {9, 16},
             {17, 20}
           ]

    second = Enum.at(segments, 1)

    assert second.text == "He bought some bread."

    assert Enum.map(second.tokens, & &1.phrase) ==
             ~w(He bought some bread)

    assert Enum.map(second.tokens, & &1.index) ==
             [0, 1, 2, 3]

    assert Enum.map(second.token_provenance, & &1.source_index) ==
             [5, 6, 7, 8]

    assert Enum.at(second.tokens, 1).phrase == "bought"

    assert Enum.at(second.token_provenance, 1).source_index == 6

    assert List.last(segments).terminator == :question_mark

    refute Map.has_key?(second, :sentence_meaning)

    # The original canonical token stream remains unchanged.
    assert Enum.at(si.tokens, 6).index == 6
  end

  test "all local tokens retain exact source-aligned spans" do
    passage = "José visited Montréal. He bought bread."
    si = Input.prepare(passage)

    assert {:ok, projection} = Passage.project(si)

    for segment <- projection.segments,
        {token, provenance} <-
          Enum.zip(segment.tokens, segment.token_provenance) do
      {local_start, local_stop} = token.span
      {source_start, source_stop} = provenance.source_span

      assert binary_part(
               segment.text,
               local_start,
               local_stop - local_start
             ) == token.phrase

      assert binary_part(
               passage,
               source_start,
               source_stop - source_start
             ) == token.phrase
    end
  end

  test "rejects corrupted canonical token spans" do
    si = Input.prepare(@passage)

    first = hd(si.tokens)
    damaged = %{first | span: {1, 5}}

    altered = %{
      si
      | tokens: List.replace_at(si.tokens, 0, damaged)
    }

    assert {:error, :invalid_alignment} = Passage.project(altered)
  end
end
