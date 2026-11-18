defmodule Core.Semantic.NamedMentionsTest do
  use ExUnit.Case, async: true

  alias Core.Pipeline.Input
  alias Core.Semantic.NamedMentions

  @passage "John went to the store. " <>
             "He bought some bread. " <>
             "When he returned home, he made a sandwich. " <>
             "What did John buy?"

  defp input do
    cells =
      for index <- [0, 19] do
        %{
          id: "john|proper_noun|1",
          norm: "john",
          pos: "proper_noun",
          token_index: index,
          definition: "A male given name."
        }
      end

    choices =
      for index <- [0, 19] do
        %{token_index: index, id: "john|noun|7"}
      end

    %{
      Input.prepare(@passage)
      | active_cells: cells,
        lifg_choices: choices,
        atl_slate: %{winners: choices}
    }
  end

  test "detects both names despite incorrect noun winners" do
    si = input()

    assert {:ok, %{mentions: [first, second]}} =
             NamedMentions.observe(si)

    assert first.source_index == 0
    assert first.sentence_index == 1
    assert first.sentence_position == :initial

    assert second.source_index == 19
    assert second.sentence_index == 4
    assert second.sentence_position == :internal

    for mention <- [first, second] do
      assert mention.status == :candidate
      assert mention.kind == :possible_proper_name

      assert mention.dictionary_witnesses ==
               ["john|proper_noun|1"]

      assert mention.lifg_winner == "john|noun|7"
      assert mention.atl_winner == "john|noun|7"
      assert mention.referent == :unresolved
      assert mention.factual_authority? == false
    end
  end

  test "capitalization without dictionary evidence is insufficient" do
    si = Input.prepare(@passage)

    assert {:ok, %{mentions: []}} =
             NamedMentions.observe(si)
  end

  test "observation cannot rewrite lexical decisions" do
    si = input()

    assert {:ok, _} = NamedMentions.observe(si)

    assert Enum.map(si.lifg_choices, & &1.id) ==
             ["john|noun|7", "john|noun|7"]

    assert si.atl_slate.winners == si.lifg_choices
  end

  test "rejects damaged source alignment" do
    si = input()
    first = hd(si.tokens)

    altered = %{
      si
      | tokens:
          List.replace_at(
            si.tokens,
            0,
            %{first | span: {1, 5}}
          )
    }

    assert {:error, _reason} =
             NamedMentions.observe(altered)
  end
end
