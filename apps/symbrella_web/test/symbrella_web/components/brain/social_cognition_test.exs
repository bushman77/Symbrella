defmodule SymbrellaWeb.Components.Brain.SocialCognitionTest do
  use ExUnit.Case, async: true

  alias SymbrellaWeb.Components.Brain.SocialCognition

  @colony %{
    kind: :telemetry,
    event: [:core, :social, :cognitive_shadow],
    at_ms: 1_792_693_000_000,
    measurements: %{lifg_choices: 31, atl_winners: 31, syntax_frames: 0},
    meta: %{
      source: :colony_external,
      post_id: "test-colony-post",
      reported_relation: :reported_refutation,
      sentence_status: :not_yet_interpreted,
      provenance: :author_claim_not_independently_verified
    }
  }

  test "latest read-only cognition is projected from direct blackboard history" do
    assert result = SocialCognition.latest([@colony])
    assert result.lifg_choices == 31
    assert result.atl_winners == 31
    assert result.syntax_frames == 0
    assert result.sentence_status == :not_yet_interpreted
    assert result.relation == :reported_refutation
    assert result.provenance == :author_claim_not_independently_verified
  end

  test "accepts events as wrapped by the live Brain blackboard" do
    wrapped = [%{env: @colony, tag: "core/social/cognitive_shadow", at_ms: 1}]
    assert SocialCognition.latest(wrapped).post_id == "test-colony-post"
  end

  test "does not mislabel regular neuron or WM events as Colony cognition" do
    events = [
      %{kind: :telemetry, event: [:brain, :wm, :update], measurements: %{size: 1}},
      %{kind: :telemetry, event: [:brain, :lifg, :stage1, :winner], measurements: %{count: 31}}
    ]

    assert SocialCognition.latest(events) == nil
    assert SocialCognition.latest([]) == nil
  end

  test "does not accept a spoofed external source with the same event name" do
    assert SocialCognition.latest([
             Map.put(@colony, :meta, Map.put(@colony.meta, :source, :private_chat))
           ]) == nil
  end
end
