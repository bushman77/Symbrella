defmodule Core.Social.SemanticProbeReadOnlyHandoffTest do
  use ExUnit.Case, async: true

  alias Core.Social.SemanticProbe

  test "external language traverses pure structural and composition stages without stateful brain admission" do
    scorer = fn _si, opts ->
      assert opts[:cerebellum_learn?] == false
      assert opts[:sense_focus?] == false
      {:ok, %{choices: [], audit: %{missing_candidates: 0}}}
    end

    assert {:ok, result} =
      SemanticProbe.run("The bird is on the fence.",
        ltm?: false,
        scorer: scorer,
        emit_telemetry?: false,
        brain_observation?: false
      )

    assert result.stage == :social_semantic_shadow
    assert %{
      pipeline: :read_only_external_projection,
      lexical_stage: :scored,
      atl_stage: :read_only_reduced,
      structural_competition: structural,
      typed_composition: typed,
      sentence_interpretation: meaning,
      external_source_isolation: :enforced,
      runtime_lifg_ingest: :not_performed,
      runtime_atl_ingest: :not_performed,
      working_memory_admission: :not_performed,
      episodic_persistence: :not_performed
    } = result.stage_diagnostic

    assert is_atom(structural)
    assert typed in [:derived, :not_derived]
    assert meaning == result.sentence_meaning.status

    assert result.brain_observation.status == :disabled
    assert result.brain_observation.stimuli_issued == 0
    assert result.stage_diagnostic.comprehension_received_meaning == true
    assert result.stage_ledger.lifg_options[:semantic_control?] == false
    assert result.stage_ledger.lifg_options[:sense_focus?] == false
    assert result.stage_ledger.lifg == :scored
    assert result.stage_ledger.weak_choices == 0
    assert result.stage_ledger.neuron_observation == :disabled
    assert result.stage_ledger.structural_scope.scope ==
      :observed_signatures_not_authoritative_eligibility
    assert result.stage_ledger.structural_scope.reason in [
      :no_base_tokens, :noncontiguous_token_indices, :no_sentence_signatures,
      :no_full_span_signature, :complete_signature_eligibility_unknown,
      :not_applicable, :structural_token_budget_exceeded
    ]
    assert is_list(result.comprehension.recognized_topics)
  end

  test "external shadow retains lexical and semantic uncertainty rather than asserting events" do
    scorer = fn _si, _opts ->
      {:ok, %{choices: [], audit: %{missing_candidates: 0}}}
    end

    assert {:ok, result} =
      SemanticProbe.run("If the bird were free, it might fly.",
        ltm?: false, scorer: scorer,
        emit_telemetry?: false,
        brain_observation?: false
      )

    assert result.sentence_meaning.provenance == :external_unverified
    refute Enum.any?(result.sentence_meaning, fn {key, value} ->
      key == :factual_authority? and value not in [nil, false]
    end)
    assert result.reported_proposition.verification == :external_unverified
    assert result.stage_diagnostic.working_memory_admission == :not_performed
    assert result.stage_diagnostic.episodic_persistence == :not_performed
  end
end
