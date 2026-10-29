defmodule Core.Response.AttentionFallbackTest do
  use ExUnit.Case, async: false

  alias Core.Brain.WM.Comprehension, as: WMComprehension
  alias Core.Response
  alias Core.Response.AttentionFallback

  setup do
    previous = Application.get_env(:core, :llm_inference_enabled?, :unset)
    Application.put_env(:core, :llm_inference_enabled?, false)

    on_exit(fn ->
      if previous == :unset,
        do: Application.delete_env(:core, :llm_inference_enabled?),
        else: Application.put_env(:core, :llm_inference_enabled?, previous)
    end)

    :ok
  end

  defp focused_si(session, sentence) do
    WMComprehension.store(%{
      source: :user, session_id: session, sentence: sentence, intent: :ask_info,
      comprehension: %{
        recognized_topics: ["ocean"],
        understood: [], uncertain: ["ocean"], degraded?: true
      },
      trace: []
    }, comprehension_wm?: true)
  end

  test "no-LLM continuation uses the admitted same-session focus, not a fabricated answer" do
    sid = "attention-response-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    si = focused_si(sid, "Tell me about the Pacific Ocean?")
    assert si.wm_context.topic == "the Pacific Ocean"
    assert si.wm_context.attention.scope == :session
    assert si.wm_context.attention.salience == 1.0

    assert {:ok, reply, metadata} =
      AttentionFallback.generate(si.sentence,
        %{risk_bucket: :low, intent: :ask_info}, %{action: :answer}, si)

    assert reply =~ "the Pacific Ocean"
    assert reply =~ "don't have a grounded answer"
    assert metadata.provenance == :admitted_same_session_comprehension
    assert metadata.factual_claims? == false
    assert metadata.llm_called? == false

    {_tone, planned, plan_meta} =
      Response.plan(Map.merge(si, %{text: si.sentence, confidence: 0.7}), %{})
    assert planned == reply
    assert plan_meta.response_source == :wm_attention_fallback
    assert plan_meta.curiosity_probe == nil

    attached = Core.Response.Attach.maybe_build_response_plan(
      Map.merge(si, %{confidence: 0.7}), [])
    assert attached.response_text == reply
    assert attached.response_meta.response_source == :wm_attention_fallback
  end

  test "unadmitted, stale, other-session, and safety-blocked focus cannot be voiced" do
    sid = "attention-private-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    si = focused_si(sid, "Tell me about the Pacific Ocean?")
    features = %{risk_bucket: :low, intent: :ask_info}
    action = %{action: :answer}

    assert :not_applicable ==
      AttentionFallback.generate(si.sentence, features, action,
        %{si | session_id: "another-session"})

    assert :not_applicable ==
      AttentionFallback.generate(si.sentence, features, action,
        Map.put(si, :wm_context, %{topic: "private context"}))

    assert :not_applicable ==
      AttentionFallback.generate(si.sentence,
        %{features | risk_bucket: :high}, action, si)

    assert :not_applicable ==
      AttentionFallback.generate(si.sentence, features,
        %{action: :ask_clarifying_question}, si)

    Brain.defocus(WMComprehension.frame_id(sid))
    assert :not_applicable ==
      AttentionFallback.generate(si.sentence, features, action, si)
  end

  defp repair_si(session, sentence, opts \\ []) do
    verified? = Keyword.get(opts, :verified?, true)
    recognized? = Keyword.get(opts, :recognized?, false)
    focus_subject = Keyword.get(opts, :focus_subject, "bird")
    uncertain = Keyword.get(opts, :uncertain, ["flarped", "blorp"])

    lexical =
      if verified? do
        [%{dictionary_verified?: true, id: "bird|noun|1",
          phrase: "bird", definition: "an animal", context_score: 0.9}]
      else
        []
      end

    WMComprehension.store(%{
      source: :user,
      session_id: session,
      sentence: sentence,
      intent: :statement,
      lexical_constructions: lexical,
      symbolic_frame: if(recognized?, do: %{subject: focus_subject}, else: %{}),
      comprehension: %{
        recognized_topics: [],
        understood: if(verified?, do: ["bird"], else: []),
        uncertain: uncertain,
        degraded?: true
      },
      trace: []
    }, comprehension_wm?: true)
  end

  test "unknown term and verified lexical anchor yield a targeted partial-comprehension repair" do
    sid = "attention-repair-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    sentence = "The bird flarped beside the blorp."
    si = repair_si(sid, sentence)

    assert si.wm_context.attention.goal_terms == ["bird", "flarped", "blorp"]
    assert si.wm_context.attention.salience > 0.0

    assert {:ok, reply, metadata} =
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, si)

    assert reply ==
      "I understand \"bird\", but I'm not sure how you're using \"flarped\" here. Could you clarify that part?"
    assert metadata.status == :attention_comprehension_repair
    assert metadata.recognized_level == :dictionary_verified
    assert metadata.unresolved_term == "flarped"
    assert metadata.attention_selected? == true
    assert metadata.provenance == :admitted_same_session_comprehension
    assert metadata.evidence_level == :conversation_context_only
    assert metadata.factual_claims? == false
    assert metadata.llm_called? == false

    {_tone, planned, meta} =
      Response.plan(Map.merge(si, %{text: sentence, confidence: 0.7}), %{})
    assert planned == reply
    assert meta.response_source == :wm_attention_fallback
    assert meta.curiosity_probe == nil

    attached = Core.Response.Attach.maybe_build_response_plan(
      Map.merge(si, %{confidence: 0.7}), [])
    assert attached.response_text == reply
    assert attached.response_meta.response_source == :wm_attention_fallback
  end

  test "recognized focus is distinguished from verified lexical understanding" do
    sid = "attention-recognized-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    sentence = "The bird flarped."
    si = repair_si(sid, sentence, verified?: false, recognized?: true,
      uncertain: ["flarped"])

    assert {:ok, reply, meta} =
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, si)

    assert reply ==
      "I'm following \"bird\", but I'm not sure how you're using \"flarped\" here. Could you clarify that part?"
    assert meta.recognized_level == :recognized_focus
    assert meta.factual_claims? == false
  end

  test "an unresolved word can be selected without inventing a known referent" do
    sid = "attention-unresolved-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    sentence = "Flarped."
    si = repair_si(sid, sentence, verified?: false,
      uncertain: ["Flarped"])

    assert si.wm_context.entities == []
    assert si.wm_context.attention.goal_terms == ["Flarped"]

    assert {:ok, reply, meta} =
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :unknown}, %{action: :ask_first}, si)

    assert reply ==
      "I'm not sure how you're using \"Flarped\" here. Could you clarify what you mean?"
    assert meta.recognized_level == :none
    assert meta.recognized_term == nil
    assert meta.unresolved_term == "Flarped"
  end

  test "repair refuses a forged unknown, missing witness, and already-interpreted meaning" do
    sid = "attention-boundary-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    sentence = "The bird flarped."
    si = repair_si(sid, sentence, uncertain: ["flarped"])

    # An unknown must occur on the current surface, not only in cached WM.
    ungrounded = %{si | sentence: "The bird arrived."}
    assert :not_applicable ==
      AttentionFallback.generate(ungrounded.sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, ungrounded)

    # A dictionary-verified lexical witness is needed before using
    # the stronger "I understand" wording.
    unverified = Map.put(si, :lexical_constructions, [])
    assert {:ok, reply, meta} =
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, unverified)
    refute String.contains?(reply, "I understand")
    assert meta.recognized_level == :none

    completed = Map.put(si, :sentence_meaning, %{kind: :explicit_temporal_report})
    assert :not_applicable ==
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, completed)

    assert :not_applicable ==
      AttentionFallback.generate(sentence,
        %{risk_bucket: :high, intent: :statement},
        %{action: :ask_first}, si)
    assert :not_applicable ==
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :safe_redirect}, si)
  end


  test "attention selects a later unresolved term over mere lexical list order" do
    sid = "attention-priority-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    sentence = "The bird flarped beside the blorp."
    si = repair_si(sid, sentence, recognized?: true, focus_subject: "blorp")

    assert si.wm_context.attention.goal_terms == ["blorp"]

    assert {:ok, reply, meta} =
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, si)

    assert String.contains?(reply, "I understand \"bird\"")
    assert String.contains?(reply, "using \"blorp\"")
    assert meta.unresolved_term == "blorp"
    assert meta.attention_selected? == true
    assert meta.recognized_level == :dictionary_verified
    assert meta.llm_called? == false
  end

end
