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
    assert result.sentence_meaning.status == :not_yet_interpreted
    assert result.stage_diagnostic.working_memory_admission == :not_performed
    assert result.stage_diagnostic.episodic_persistence == :not_performed
  end
end
