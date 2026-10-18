defmodule Core.Response.ProjectedMemoryReplyPolicyTest do
  use ExUnit.Case, async: true

  alias Core.Response.Memory
  alias Core.Semantic.EpisodicProjection

  @evidence [
    :contiguous_surface, :attested_nominal_subject,
    :attested_nominal_object, :attested_finite_present_verb
  ]

  @form_tags [
    "tag:form-of", "tag:indicative", "tag:present",
    "tag:singular", "tag:third-person"
  ]

  # Structural fixture avoids a false assumption that all test LTM options
  # admit a full real-dictionary episodic candidate.
  defp proven_event do
    choices = [
      %{token_index: 1, id: "baker|noun|1"},
      %{token_index: 2, id: "cooks|verb|1"},
      %{token_index: 4, id: "meal|noun|1"}
    ]

    %{
      source: :user, sentence: "The baker cooks a meal.",
      intent: :unknown, trace: [],
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: "baker", n: 1},
        %{index: 2, phrase: "cooks", n: 1},
        %{index: 3, phrase: "a", n: 1},
        %{index: 4, phrase: "meal.", n: 1}
      ],
      sentence_signatures: [
        %{id: :finite_present_svo_candidate, span: {0, 4},
          source: :core_syntax, evidence: @evidence,
          roles: %{subject_determiner: 0, subject_head: 1,
            predicate: 2, object_determiner: 3, object_head: 4}}
      ],
      lifg_choices: choices,
      atl_slate: %{winners: choices},
      active_cells: [
        %{id: "cooks|verb|1", norm: "cooks", pos: "verb",
          definition: "An inflected form of cook.",
          semantic_atoms: @form_tags}
      ]
    }
    |> EpisodicProjection.attach()
  end

  test "plain structurally validated SVO is not a memory acknowledgment request" do
    si = proven_event()
    assert [%{source_frame: :finite_present_svo}] = si.episodic_candidates
    assert Memory.acknowledge_projected(
      si, "policy-test", si.sentence, :unknown, 0.4) == nil
    assert Memory.acknowledge_projected(
      si, "policy-test", si.sentence, :statement, 0.8) == nil
  end

  test "even an erroneously classified memory_write intent needs overt request text" do
    si = proven_event()
    assert [%{source_frame: :finite_present_svo}] = si.episodic_candidates
    assert Memory.acknowledge_projected(
      si, "policy-test", si.sentence, :memory_write, 1.0) == nil
  end

  test "projected event mismatch still cannot trigger acknowledgment" do
    si = proven_event()
    forged = %{si | sentence: "Remember what the baker cooks."}
    assert Memory.acknowledge_projected(
      forged, "policy-test", si.sentence, :memory_write, 1.0) == nil
  end
end
