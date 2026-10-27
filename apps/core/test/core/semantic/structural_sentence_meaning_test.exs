defmodule Core.Semantic.StructuralSentenceMeaningTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.SentenceMeaning
  alias Core.Syntax.{Signatures, StructuralCompetition}

  defp input do
    words = ~w(The cat is sleeping under the table.)
    ids = [
      "the|determiner|1", "cat|adjective|1", "is|verb|1",
      "sleeping|adjective|1", "under|preposition|1",
      "the|determiner|1", "table|noun|1"
    ]

    %Core.SemanticInput{
      sentence: "The cat is sleeping under the table.",
      source: :user,
      tokens: words
        |> Enum.with_index()
        |> Enum.map(fn {phrase, index} ->
          %{phrase: phrase, index: index, n: 1}
        end),
      active_cells: [
        %{id: "the|determiner|1", norm: "the", pos: "determiner"},
        %{id: "cat|noun|1", norm: "cat", pos: "noun"},
        %{id: "cat|adjective|1", norm: "cat", pos: "adjective"},
        %{id: "is|verb|1", norm: "is", pos: "verb"},
        %{id: "sleeping|adjective|1", norm: "sleeping", pos: "adjective"},
        %{id: "sleeping|verb|1", norm: "sleeping", pos: "verb",
          semantic_atoms: ["tag:form-of", "tag:present", "tag:participle"]},
        %{id: "under|preposition|1", norm: "under", pos: "preposition"},
        %{id: "table|noun|1", norm: "table", pos: "noun"}
      ],
      lifg_choices: ids
        |> Enum.with_index()
        |> Enum.map(fn {id, token_index} ->
          %{id: id, token_index: token_index}
        end)
    }
  end

  defp observed(si) do
    si |> Signatures.attach() |> StructuralCompetition.attach()
  end

  test "semantic composition can inspect structural preference without asserting event" do
    si = input() |> observed()
    assert si.structural_competition.status == :preferred

    meaning = SentenceMeaning.build(si)
    assert meaning.status == :provisional
    assert meaning.kind == :structural_hypothesis
    assert meaning.source_frame == :progressive_locative_candidate
    assert meaning.roles.subject_head == 1
    assert meaning.roles.predicate == 3
    assert meaning.grounding.factual_authority? == false
    assert :event_grounding in meaning.unresolved
    assert meaning.definition == nil
    assert si.episodic_candidates == []
  end

  test "altered result or lexical selections cannot bypass semantic revalidation" do
    si = input() |> observed()

    forged = put_in(si.structural_competition.preferred.construction, :arbitrary_claim)
    refute match?({:ok, _}, StructuralCompetition.verified(forged))
    assert SentenceMeaning.build(forged).status == :not_yet_interpreted

    changed = %{si | lifg_choices: [
      %{token_index: 1, id: "cat|adjective|1"},
      %{token_index: 3, id: "sleeping|adjective|1"},
      %{token_index: 4, id: "under|noun|1"},
      %{token_index: 6, id: "table|verb|1"}
    ]}
    assert StructuralCompetition.verified(changed) == :not_applicable
    assert SentenceMeaning.build(changed).status == :not_yet_interpreted
  end

  test "without participial morphology the sentence does not gain a preferred reading" do
    si = input()
    stripped = %{si | active_cells:
      Enum.reject(si.active_cells, &(&1.id == "sleeping|verb|1"))}
    out = observed(stripped)

    assert out.structural_competition.status == :no_complete_candidate
    assert SentenceMeaning.build(out).status == :not_yet_interpreted
  end
end
