defmodule Core.Semantic.PassageAntecedentsTest do
  use ExUnit.Case, async: true

  alias Core.Pipeline.Input
  alias Core.Semantic.PassageAntecedents

  @passage "John went to the store. " <>
             "He bought some bread. " <>
             "When he returned home, he made a sandwich. " <>
             "What did John buy?"

  defp input do
    name_cells =
      for index <- [0, 19] do
        %{
          id: "john|proper_noun|1",
          norm: "john",
          pos: "proper_noun",
          token_index: index
        }
      end

    verb_cells = [
      %{id: "he|pronoun|1", norm: "he", pos: "pronoun"},
      %{
        id: "bought|verb|1",
        norm: "bought",
        pos: "verb",
        semantic_atoms: [
          "tag:form-of",
          "tag:past",
          "tag:participle"
        ],
        definition: "(form-of, participle, past) simple past and past participle of buy."
      },
      %{
        id: "some|determiner|1",
        norm: "some",
        pos: "determiner"
      },
      %{id: "bread|noun|2", norm: "bread", pos: "noun"}
    ]

    choices = [
      %{token_index: 5, id: "he|pronoun|1"},
      %{token_index: 6, id: "bought|verb|1"},
      %{token_index: 7, id: "some|determiner|1"},
      %{token_index: 8, id: "bread|noun|2"}
    ]

    %{
      Input.prepare(@passage)
      | source: :test,
        active_cells: name_cells ++ verb_cells,
        lifg_choices: choices,
        atl_slate: %{winners: choices}
    }
  end

  test "finds an earlier named-mention candidate" do
    assert {:ok, %{observations: [observation]}} =
             PassageAntecedents.observe(input())

    assert observation.status == :candidate_only
    assert observation.binding == :unresolved

    assert observation.pronoun.source_index == 5
    assert observation.pronoun.surface == "he"

    assert [
             %{
               surface: "John",
               source_index: 0,
               status: :possible_antecedent
             }
           ] = observation.antecedent_candidates

    assert observation.factual_authority? == false
  end

  test "does not use later name mentions as antecedents" do
    si = input()

    cells =
      Enum.reject(si.active_cells, fn cell ->
        cell.pos == "proper_noun" and
          cell.token_index == 0
      end)

    reduced = %{si | active_cells: cells}

    assert {:ok, %{observations: [observation]}} =
             PassageAntecedents.observe(reduced)

    assert observation.status == :no_candidate
    assert observation.antecedent_candidates == []
    assert observation.binding == :unresolved
  end

  test "does not mutate the original input" do
    si = input()
    original = si

    assert {:ok, _} = PassageAntecedents.observe(si)

    assert si == original
  end

  test "multiple earlier names preserve pronoun ambiguity" do
    text = "John met Alex. He bought some bread."

    names = [
      %{
        id: "john|proper_noun|1",
        norm: "john",
        pos: "proper_noun",
        token_index: 0
      },
      %{
        id: "alex|proper_noun|1",
        norm: "alex",
        pos: "proper_noun",
        token_index: 2
      }
    ]

    lexical_cells = [
      %{id: "he|pronoun|1", norm: "he", pos: "pronoun"},
      %{
        id: "bought|verb|1",
        norm: "bought",
        pos: "verb",
        semantic_atoms: [
          "tag:form-of",
          "tag:past",
          "tag:participle"
        ],
        definition: "(form-of, participle, past) simple past and past participle of buy."
      },
      %{
        id: "some|determiner|1",
        norm: "some",
        pos: "determiner"
      },
      %{
        id: "bread|noun|2",
        norm: "bread",
        pos: "noun"
      }
    ]

    choices = [
      %{token_index: 3, id: "he|pronoun|1"},
      %{token_index: 4, id: "bought|verb|1"},
      %{token_index: 5, id: "some|determiner|1"},
      %{token_index: 6, id: "bread|noun|2"}
    ]

    prepared = Input.prepare(text)

    si = %{
      prepared
      | source: :test,
        active_cells: names ++ lexical_cells,
        lifg_choices: choices,
        atl_slate: %{winners: choices}
    }

    assert {:ok, %{observations: [observation]}} =
             PassageAntecedents.observe(si)

    assert observation.status == :ambiguous
    assert observation.binding == :unresolved
    assert observation.pronoun.source_index == 3

    assert Enum.map(
             observation.antecedent_candidates,
             &{&1.surface, &1.source_index}
           ) == [
             {"John", 0},
             {"Alex", 2}
           ]

    assert observation.factual_authority? == false
  end
end
