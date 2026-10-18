defmodule Core.Response.ProjectedMemoryReplyPolicyTest do
  use ExUnit.Case, async: false

  alias Core.Response.Memory

  @read_only_opts [
    mode: :prod,
    experience_origin: :user_turn,
    fuzzy: false,
    response: :off,
    episodes: false,
    persist_episodes: false,
    hippocampus_encode?: false,
    self_model: :off,
    comprehension_wm?: false,
    notify_brain?: false,
    synapse_shadow?: false,
    semantic_sense_shadow?: false,
    lifg_opts: [grammar_weight: 1.0]
  ]

  test "plain validated SVO is not an instruction to acknowledge memory recording" do
    utterance = "The baker cooks a meal."
    si = Core.resolve_input(utterance, @read_only_opts)

    assert is_list(si.episodic_candidates)
    assert si.episodic_candidates != []

    # The utterance is a structurally valid user-reported event but says
    # nothing about the desired conversational action.
    assert Memory.acknowledge_projected(
      si, "policy-test", utterance, :unknown, 0.4) == nil

    assert Memory.acknowledge_projected(
      si, "policy-test", utterance, :statement, 0.8) == nil
  end

  test "an explicit memory cue is necessary, not just a memory_write atom" do
    utterance = "The baker cooks a meal."
    si = Core.resolve_input(utterance, @read_only_opts)
    assert si.episodic_candidates != []

    # Even a misclassified intent must not silently authorize an
    # acknowledgment that implies the user asked for persistence.
    assert Memory.acknowledge_projected(
      si, "policy-test", utterance, :memory_write, 1.0) == nil
  end
end
