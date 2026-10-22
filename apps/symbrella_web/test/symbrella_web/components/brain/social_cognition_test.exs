defmodule SymbrellaWeb.Components.Brain.SocialCognitionTest do
  use ExUnit.Case, async: true

  alias SymbrellaWeb.Components.Brain.SocialCognition

  @colony %{
    kind: :telemetry,
    event: [:core, :social, :cognitive_shadow],
    at_ms: 1_792_693_000_000,
    measurements: %{
      lifg_choices: 31, atl_winners: 31, syntax_frames: 0,
      dictionary_rows: 130, pos_covered_tokens: 25, processed_tokens: 32
    },
    meta: %{
      source: :colony_external,
      post_id: "test-colony-post",
      reported_relation: :reported_refutation,
      analysis_scope: :post_title,
      sentence_status: :not_yet_interpreted,
      grammar_outcome: :no_supported_grammar_pattern,
      proposition_status: :attributed_structural_interpretation,
      provenance: :author_claim_not_independently_verified
    }
  }

  test "latest read-only cognition is projected from direct blackboard history" do
    assert result = SocialCognition.latest([@colony])
    assert result.lifg_choices == 31
    assert result.atl_winners == 31
    assert result.syntax_frames == 0
    assert result.dictionary_rows == 130
    assert result.pos_covered_tokens == 25
    assert result.processed_tokens == 32
    assert result.grammar_outcome == :no_supported_grammar_pattern
    assert result.proposition_status == :attributed_structural_interpretation
    assert result.sentence_status == :not_yet_interpreted
    assert result.analysis_scope == :post_title
    assert result.relation == :reported_refutation
    assert result.provenance == :author_claim_not_independently_verified
  end

  test "shows lexical role disagreements separately from dictionary and neuron counts" do
    %{meta: metadata, measurements: counts} = @colony

    event = %{@colony |
      meta: Map.merge(metadata, %{
        proposition_status: :not_interpreted,
        proposition_reason: :lifg_atl_role_disagreement,
        probe_mode: :standalone_read_only
      }),
      measurements: Map.put(counts, :disputed_roles, 2)
    }

    result = SocialCognition.latest([event])
    assert result.proposition_status == :not_interpreted
    assert result.proposition_reason == :lifg_atl_role_disagreement
    assert result.disputed_roles == 2
    assert result.dictionary_rows == 130
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
    %{meta: metadata} = @colony
    spoofed_event = %{@colony | meta: %{metadata | source: :private_chat} }

    assert SocialCognition.latest([spoofed_event]) == nil
  end
end
