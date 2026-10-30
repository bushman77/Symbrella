defmodule Core.Semantic.CompositionFinitePastLocativeTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition.FinitePastLocative

  defp input do
    %{
      signature: %{
        id: :finite_past_locative_candidate,
        source: :core_syntax,
        span: {0, 5},
        roles: %{subject_determiner: 0, subject_head: 1, predicate: 2,
                 preposition: 3, location_determiner: 4, location_head: 5},
        evidence: [:contiguous_surface, :attested_nominal_subject,
                   :attested_simple_past_verb, :attested_preposition,
                   :attested_nominal_location]
      },
      predicate_surface: "sat",
      actor: "bird",
      preposition: "on",
      landmark: "fence",
      grounding: %{
        status: :predicate_feature_candidates,
        surface: "sat",
        selected_lifg_id: "sat|noun|1",
        forms: [%{
          surface: "sat", lemma: "sit",
          inflection: "simple past and past participle",
          source: :attested_scored_form_of_dictionary_sense,
          morphological_only?: true,
          inflected_sense_id: "sat|verb|1", score: 0.2
        }],
        feature_hypotheses: [
          %{feature: :seated_posture, source_lemma: "sit",
            source_lemma_sense_id: "sit|verb|10",
            source: :lemma_dictionary_definition,
            epistemic_status: :candidate_only},
          %{feature: :seated_posture, source_lemma: "sit",
            source_lemma_sense_id: "sit|verb|1",
            source: :lemma_dictionary_definition,
            epistemic_status: :candidate_only}
        ]
      }
    }
  end

  test "derives a replayable conditional past locative property without changing the winner" do
    assert {:ok, proof} = FinitePastLocative.compose(input())
    assert :ok = FinitePastLocative.verify(proof)
    assert proof.result_type == {:arrow, :event, :truth}
    assert proof.premises.selected_lifg_id == "sat|noun|1"
    assert proof.premises.lemma_feature_candidate.source_lemma_sense_id == "sit|verb|1"
    assert proof.event_semantics.tense == :past
    assert proof.event_semantics.predicate == :sit
    assert proof.event_semantics.spatial.relation == :on
    assert proof.event_semantics.event_existence == :not_asserted
    refute proof.factual_authority?
    refute proof.semantic_compatibility_proven?
  end

  test "abstains without independent morphological or dictionary feature evidence" do
    base = input()
    assert :unsupported = FinitePastLocative.compose(
      put_in(base, [:grounding, :forms], []))
    assert :unsupported = FinitePastLocative.compose(
      put_in(base, [:grounding, :feature_hypotheses], []))
    assert :unsupported = FinitePastLocative.compose(
      put_in(base, [:grounding, :forms, Access.at(0), :inflection], "past participle"))
    assert :unsupported = FinitePastLocative.compose(
      put_in(base, [:grounding, :forms, Access.at(0), :lemma], "sleep"))
  end

  test "abstains for invalid construction, surface mismatch, and unsupported lexical terms" do
    base = input()
    assert :unsupported = FinitePastLocative.compose(
      put_in(base, [:signature, :span], {1, 6}))
    assert :unsupported = FinitePastLocative.compose(
      %{base | predicate_surface: "sitting"})
    assert :unsupported = FinitePastLocative.compose(
      %{base | actor: "fisherman"})
    assert :unsupported = FinitePastLocative.compose(
      %{base | preposition: "into"})
    assert :unsupported = FinitePastLocative.compose(
      %{base | landmark: "bank"})
  end

  test "proof verification rejects altered semantic claims or reduction steps" do
    assert {:ok, proof} = FinitePastLocative.compose(input())
    assert {:error, :invalid_derivation} =
             FinitePastLocative.verify(put_in(proof, [:event_semantics, :event_existence], :asserted))
    assert {:error, :invalid_derivation} =
             FinitePastLocative.verify(%{proof | steps: []})
  end
end
