defmodule Core.Semantic.PassageAnalysisTest do
  use ExUnit.Case, async: true

  alias Core.Pipeline.Input
  alias Core.Semantic.Passage
  alias Core.Integration.Coverage

  @passage "John went to the store. " <>
             "He bought some bread. " <>
             "When he returned home, he made a sandwich. " <>
             "What did John buy?"

  defp input do
    choices = [
      %{token_index: 5, id: "he|pronoun|1"},
      %{token_index: 6, id: "bought|verb|1"},
      %{token_index: 7, id: "some|determiner|1"},
      %{token_index: 8, id: "bread|noun|2"}
    ]

    Input.prepare(@passage)
    |> Map.put(:source, :test)
    |> Map.put(:active_cells, [
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
      %{id: "some|determiner|1", norm: "some", pos: "determiner"},
      %{id: "bread|noun|2", norm: "bread", pos: "noun"}
    ])
    |> Map.put(:lifg_choices, choices)
    |> Map.put(:atl_slate, %{winners: choices})
  end

  test "projects existing lexical evidence into sentence-local meaning" do
    si = input()

    assert {:ok, analysis} = Passage.analyze(si)
    assert length(analysis.segments) == 4

    second = Enum.at(analysis.segments, 1)

    assert second.source_word_range == {5, 8}

    assert %{
             kind: :structural_hypothesis,
             status: :provisional,
             source_frame: :finite_past_svo_candidate,
             participants: %{
               actor: %{referent: :unresolved},
               predicate: %{lemma: "buy", tense: :past},
               patient: %{surface: "some bread"}
             }
           } = second.sentence_meaning

    assert second.factual_authority? == false
    assert si.sentence == @passage
    assert si.episodic_candidates == []
  end

  test "missing ATL evidence cannot establish participant meaning" do
    si = input()

    reduced = %{
      si
      | atl_slate:
          Map.put(
            si.atl_slate,
            :winners,
            [%{token_index: 5, id: "he|pronoun|1"}]
          )
    }

    assert {:ok, analysis} = Passage.analyze(reduced)

    second = Enum.at(analysis.segments, 1)

    refute Map.has_key?(
             second.sentence_meaning,
             :participants
           )
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

    assert {:error, :invalid_alignment} =
             Passage.analyze(altered)
  end

  test "coverage reports partial passage interpretation" do
    si = input()

    coverage = Coverage.assess(si)

    assert coverage.version == 2
    assert coverage.status == :partially_interpreted
    assert coverage.sentence_count == 4
    assert coverage.question_count == 1
    assert coverage.factual_authority? == false

    assert %{
             status: :read_only_observed,
             provisional_count: 1,
             unresolved_count: 3
           } = coverage.per_sentence_semantics

    assert Enum.map(
             coverage.per_sentence_semantics.segments,
             & &1.status
           ) == [
             :not_yet_interpreted,
             :provisional,
             :not_yet_interpreted,
             :not_yet_interpreted
           ]

    assert coverage.discourse_resolution == :not_evaluated
    assert coverage.question_answer_binding == :not_evaluated
  end
end
