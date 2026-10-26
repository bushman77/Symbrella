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
end
