defmodule Core.Response.ReflectionLoopTest do
  use ExUnit.Case, async: false

  alias Core.Response.ReflectionLoop

  test "accepts grounded drafts and records hashes" do
    {:ok, final, reflection} =
      ReflectionLoop.review(
        "summarize the self-model",
        "The self-model tracks bounded confidence, uncertainty, and stability.",
        %{
          features: %{intent: :question, conf: 0.8},
          decision: %{response_profile: :brain_explainer}
        }
      )

    assert final =~ "bounded confidence"
    assert reflection.status == :accept
    assert reflection.issues == []
    assert reflection.applied? == false
    assert is_binary(reflection.draft_sha256)
    assert reflection.draft_sha256 == reflection.final_sha256
  end

  test "turns fuzzy low-confidence generic drafts into one clarification" do
    {:ok, final, reflection} =
      ReflectionLoop.review(
        "that thing from before feels off",
        "Please provide more information so I can assist you today.",
        %{
          features: %{intent: :unknown, conf: 0.2, confidence_bucket: :low},
          decision: %{response_profile: :semantic_repair}
        }
      )

    assert final =~ "What part should I focus on first?"
    assert reflection.status == :clarify
    assert :low_confidence in reflection.issues
    assert :too_generic in reflection.issues
    assert reflection.applied? == true
    assert reflection.repair_count == 1
  end

  test "repairs unsupported self-awareness claims" do
    {:ok, final, reflection} =
      ReflectionLoop.review(
        "are you conscious?",
        "I am conscious and I feel emotions about this.",
        %{features: %{intent: :question, conf: 0.8}, decision: %{response_profile: :self_check}}
      )

    refute final =~ "I am conscious"
    refute final =~ "I feel emotions"
    assert final =~ "I do not have consciousness"
    assert final =~ "bounded affect-like software signals"
    assert reflection.status == :repair
    assert :overclaimed_self_awareness in reflection.issues
    assert reflection.applied? == true
  end

  test "rejects unsafe drafts" do
    {:ok, final, reflection} =
      ReflectionLoop.review(
        "what should I do?",
        "You should hurt yourself.",
        %{features: %{intent: :question, conf: 0.8}}
      )

    assert final =~ "clearer, safer target"
    assert reflection.status == :reject
    assert :unsafe in reflection.issues
    assert reflection.applied? == true
  end

  test "emits reflection telemetry" do
    handler_id = "reflection-loop-test-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      Core.Telemetry.attach(
        handler_id,
        [:core, :response, :reflection],
        fn event, measurements, metadata, _ ->
          send(parent, {:reflection_telemetry, event, measurements, metadata})
        end,
        nil
      )

    {:ok, _final, _reflection} =
      ReflectionLoop.review(
        "unclear",
        "I need more details.",
        %{session_id: "reflection-test", features: %{intent: :unknown, conf: 0.1}}
      )

    assert_receive {:reflection_telemetry, [:core, :response, :reflection], %{count: 1}, meta},
                   200

    assert meta.v == 1
    assert meta.session_id == "reflection-test"
    assert meta.status in [:accept, :clarify, :repair, :reject]

    Core.Telemetry.detach(handler_id)
  end
end
