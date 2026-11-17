defmodule Core.Semantic.ProvisionalPastPronounTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}
  alias Core.Syntax.{Signatures, StructuralCompetition}

  defp input do
    words = ~w(He bought some bread.)

    choices = [
      %{token_index: 0, id: "he|pronoun|1"},
      %{token_index: 1, id: "bought|verb|1"},
      %{token_index: 2, id: "some|determiner|1"},
      %{token_index: 3, id: "bread|noun|2"}
    ]

    cells = [
      %{
        id: "he|pronoun|1",
        norm: "he",
        pos: "pronoun"
      },
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
        id: "bought|noun|1",
        norm: "bought",
        pos: "noun",
        definition: "(obsolete) A bend; flexure; curve."
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

    %{
      sentence: "He bought some bread.",
      source: :user,
      intent: :unknown,
      trace: [],
      tokens:
        words
        |> Enum.with_index()
        |> Enum.map(fn {phrase, index} ->
          %{phrase: phrase, index: index, n: 1}
        end),
      active_cells: cells,
      lifg_choices: choices,
      atl_slate: %{winners: choices}
    }
    |> Signatures.attach()
    |> StructuralCompetition.attach()
  end

  test "past-tense pronoun clause retains an unresolved actor" do
    si = input()

    assert %{
             status: :preferred,
             preferred: %{
               construction: :finite_past_svo_candidate
             }
           } = si.structural_competition

    meaning = SentenceMeaning.build(si)

    assert meaning.status == :provisional
    assert meaning.kind == :structural_hypothesis

    assert meaning.participants == %{
             actor: %{
               surface: "he",
               token_index: 0,
               sense_id: "he|pronoun|1",
               referent: :unresolved
             },
             predicate: %{
               surface: "bought",
               lemma: "buy",
               tense: :past,
               token_index: 1,
               sense_id: "bought|verb|1"
             },
             patient: %{
               surface: "some bread",
               span: {2, 3},
               determiner_sense_id: "some|determiner|1",
               head_sense_id: "bread|noun|2"
             }
           }

    assert :actor_referent in meaning.unresolved
    assert :event_grounding in meaning.unresolved
    assert meaning.definition == nil
    assert meaning.grounding.factual_authority? == false

    # An unidentified actor cannot become a stored event.
    assert EpisodicProjection.project(si) == []
  end

  test "rejects an ATL winner that disagrees with LIFG" do
    si = input()

    winners =
      Enum.map(si.atl_slate.winners, fn
        %{token_index: 1} = winner ->
          %{winner | id: "bought|noun|1"}

        winner ->
          winner
      end)

    altered = put_in(si.atl_slate.winners, winners)

    meaning = SentenceMeaning.build(altered)

    refute Map.has_key?(meaning, :participants)
    assert EpisodicProjection.project(altered) == []
  end

  test "rejects a final semantic selection that disagrees with ATL" do
    si = input()

    choices =
      Enum.map(si.lifg_choices, fn
        %{token_index: 1} = choice ->
          %{choice | id: "bought|noun|1"}

        choice ->
          choice
      end)

    altered = Map.put(si, :semantic_choices, choices)

    meaning = SentenceMeaning.build(altered)

    refute Map.has_key?(meaning, :participants)
    assert EpisodicProjection.project(altered) == []
  end

  test "rejects unsupported past-tense morphology" do
    si = input()

    cells =
      Enum.map(si.active_cells, fn
        %{id: "bought|verb|1"} = cell ->
          %{cell | definition: "(form-of, participle, past) past participle of buy."}

        cell ->
          cell
      end)

    altered = %{si | active_cells: cells}

    meaning = SentenceMeaning.build(altered)

    refute Map.has_key?(meaning, :participants)
    assert EpisodicProjection.project(altered) == []
  end

  test "rejects altered sentence surfaces and assistant-origin input" do
    si = input()

    altered_sentence = %{si | sentence: "He bought some cheese."}
    assistant_source = %{si | source: :assistant}

    for altered <- [altered_sentence, assistant_source] do
      meaning = SentenceMeaning.build(altered)

      refute Map.has_key?(meaning, :participants)
      assert EpisodicProjection.project(altered) == []
    end
  end

  test "provisional past action produces a cautious symbolic response" do
    si = input() |> SentenceMeaning.attach()

    assert %{
             kind: :structural_hypothesis,
             status: :provisional,
             participants: %{
               actor: %{referent: :unresolved},
               predicate: %{lemma: "buy", tense: :past},
               patient: %{surface: "some bread"}
             }
           } = si.sentence_meaning

    assert %{
             operation: :acknowledge_provisional_action,
             status: :eligible,
             realizer: :symbolic_generation,
             missing: missing
           } = Core.Response.SemanticStrategy.select(si)

    assert :actor_referent in missing

    assert {:ok, reply, metadata} =
             Core.Response.SymbolicGeneration.generate(
               si.sentence,
               %{risk_bucket: :low, intent: :unknown},
               %{action: :offer_options},
               si
             )

    assert reply ==
             "You're saying that he bought some bread. " <>
               "I can't tell who \"he\" refers to."

    assert metadata.provenance ==
             :revalidated_provisional_past_svo

    assert metadata.semantic_verified? == false
    assert metadata.event_occurrence_verified? == false
    assert metadata.referent_verified? == false
    assert metadata.factual_claims? == false

    assert EpisodicProjection.project(si) == []
  end
end
