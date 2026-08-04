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

  test "repairs leaked internal response-state labels" do
    leaked = """
    Uncertain: Good afternoon Symbrella, afternoon Symbrella, need help fixing.
    Temperament: careful
    Assertiveness: 0.3
    Curiosity: 0.25
    Restraint: 0.8
    Warmth: 0.4
    Self-check: 0.8
    Abstraction: 0.25
    Depth: brief
    Reasons: LIFG degraded, comprehension degraded
    Personality state: Temperament=careful; Assert
    da 0.89 5ht 0.61 glu 0.82 ne 0.86
    """

    {:ok, final, reflection} =
      ReflectionLoop.review(
        "good afternoon symbrella, i need help fixing my bad credit",
        leaked,
        %{features: %{intent: :help, conf: 0.7}, decision: %{response_profile: :careful}}
      )

    refute final =~ "Temperament:"
    refute final =~ "Personality state:"
    refute final =~ "da 0.89"
    assert final =~ "pulling your credit reports"
    assert reflection.status == :repair
    assert :leaked_hidden_context in reflection.issues
    assert reflection.applied? == true
  end

  test "repairs alien answer that leaks internal policy wording" do
    draft =
      "I understand that you are uncertain about whether aliens might exist. My internal state suggests that I should offer options to clarify your intent. However, based on my current understanding, I cannot provide a definitive answer."

    {:ok, final, reflection} =
      ReflectionLoop.review(
        "do you belive aliens might exist?",
        draft,
        %{features: %{intent: :question, conf: 0.4}, decision: %{response_profile: :social_chat}}
      )

    assert final =~ "Alien life might exist"
    assert final =~ "plausible, not proven"
    refute final =~ "internal state"
    refute final =~ "clarify your intent"
    assert reflection.status == :repair
    assert :leaked_hidden_context in reflection.issues
    assert reflection.applied? == true
  end

  test "hidden-context fallback does not mention exposing internal state" do
    draft =
      "The response policy says I should answer directly instead of exposing internal state."

    {:ok, final, reflection} =
      ReflectionLoop.review(
        "hey ou",
        draft,
        %{features: %{intent: :unknown, conf: 0.4}, decision: %{response_profile: :social_chat}}
      )

    assert final == "I can help. What outcome are you trying to get first?"
    refute final =~ "exposing internal state"
    refute final =~ "response policy"
    assert reflection.status == :repair
    assert :leaked_hidden_context in reflection.issues
  end

  test "repairs generic topic offer on alien follow-up" do
    draft =
      "Great! So, given our understanding and agreement on this topic, do you have any specific questions or topics you'd like to explore further?<|im_end"

    {:ok, final, reflection} =
      ReflectionLoop.review(
        "whether or not are they going to be domineers or treat us with our own soverenty",
        draft,
        %{
          features: %{
            intent: :question,
            conf: 0.4,
            context_status: %{topics: %{alien_life?: true}}
          },
          decision: %{response_profile: :social_chat}
        }
      )

    assert final =~ "alien-life thread"
    assert final =~ "Sovereignty"
    refute final =~ "specific questions"
    refute final =~ "<|im_end"
    assert reflection.status == :repair
    assert :topic_dead_end in reflection.issues
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
