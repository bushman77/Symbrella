defmodule Core.Brain.ComprehensionWMTest do
  use ExUnit.Case, async: false

  alias Core.Brain.WM.Comprehension, as: WMComprehension
  alias Core.Comprehension.Summary
  alias Core.Semantic.EventFrames

  defp session do
    "comprehension-wm-#{System.unique_integer([:positive])}"
  end

  defp frame_id(session) do
    hash =
      :crypto.hash(:sha256, :erlang.term_to_binary({:symbrella_wm_session, session}))
      |> Base.encode16(case: :lower)
      |> binary_part(0, 32)

    "comprehension:#{hash}"
  end

  defp prior_turn(session) do
    %{
      session_id: session,
      sentence: "How far is Kamloops BC from Maple Ridge BC?",
      intent: :ask,
      symbolic_frame: %{
        type: :location_question,
        relation: :proximity,
        subject: "Kamloops BC",
        target: "Maple Ridge BC",
        grounding_status: :unverified
      },
      sentence_meaning: %{status: :not_yet_interpreted},
      comprehension: %{
        recognized_topics: ["distance"],
        understood: [],
        uncertain: ["distance"],
        degraded?: true
      },
      trace: []
    }
  end

  test "admit completed comprehension despite degraded lexical certainty, and reuse referents" do
    assert is_pid(Process.whereis(Brain))
    sid = session()
    on_exit(fn -> Brain.defocus(frame_id(sid)) end)

    original_gate = Brain.snapshot_wm().cfg
    stored = WMComprehension.store(prior_turn(sid), comprehension_wm?: true)
    assert [%{stage: :wm_comprehension, decision: :admitted, meta: meta}] = stored.trace
    assert meta.entity_count == 2
    assert meta.degraded? == true
    assert meta.fact_verified? == false

    %{wm: wm} = Brain.snapshot_wm()
    assert item = Enum.find(wm, &(&1.id == frame_id(sid)))
    assert item.source == :comprehension
    assert item.payload.context.verification == :conversation_context_only
    assert item.payload.context.entities == ["Kamloops BC", "Maple Ridge BC"]

    # Global prompt snapshots must not expose other sessions' context frames.
    runtime = Core.Response.RuntimeContext.snapshot()
    refute Enum.any?(runtime.wm_items, &(&1.id == frame_id(sid)))

    # No global WM policy was relaxed: only the focus call for the temporary
    # frame receives permissive opts.
    assert Brain.snapshot_wm().cfg == original_gate

    next = %{session_id: sid, sentence: "What are the populations of each?", trace: []}
    recalled = WMComprehension.recall(next, comprehension_wm?: true)
    assert recalled.wm_context.entities == ["Kamloops BC", "Maple Ridge BC"]
    assert recalled.discourse_reference.antecedents == ["Kamloops BC", "Maple Ridge BC"]
    assert recalled.discourse_reference.source == :wm_comprehension

    framed = EventFrames.attach(recalled)
    assert framed.symbolic_frame.relation == :population
    assert framed.symbolic_frame.subjects == ["Kamloops BC", "Maple Ridge BC"]
    assert framed.symbolic_frame.grounding_status == :unverified

    understood = Summary.attach(framed)
    assert understood.comprehension.active_referents == ["Kamloops BC", "Maple Ridge BC"]

    next_complete =
      Map.put(understood, :comprehension,
        Map.put(understood.comprehension, :degraded?, true))

    assert [%{stage: :wm_comprehension, decision: :admitted} | _] =
             WMComprehension.store(next_complete, comprehension_wm?: true).trace

    %{wm: newer_wm} = Brain.snapshot_wm()
    assert Enum.count(newer_wm, &(&1.id == frame_id(sid))) == 1
    item2 = Enum.find(newer_wm, &(&1.id == frame_id(sid)))
    assert item2.payload.context.relation == :population
    assert item2.payload.context.entities == ["Kamloops BC", "Maple Ridge BC"]
  end

  test "never recall a different session's comprehension frame" do
    assert is_pid(Process.whereis(Brain))
    sid = session()
    other = session()
    on_exit(fn -> Brain.defocus(frame_id(sid)) end)

    WMComprehension.store(prior_turn(sid), comprehension_wm?: true)

    result = WMComprehension.recall(%{session_id: other,
      sentence: "What are the populations of each?", trace: []}, comprehension_wm?: true)

    refute Map.has_key?(result, :wm_context)
    refute Map.has_key?(result, :discourse_reference)
    assert WMComprehension.recall(%{sentence: "What about both?", trace: []}) ==
             %{sentence: "What about both?", trace: []}
  end

  test "existing discourse reference is not overwritten by WM plural binder" do
    sid = session()
    on_exit(fn -> Brain.defocus(frame_id(sid)) end)

    WMComprehension.store(prior_turn(sid), comprehension_wm?: true)
    known = %{source: :explicit, antecedent: "a specific entity"}

    recalled = WMComprehension.recall(%{session_id: sid,
      sentence: "What are the populations of each?",
      discourse_reference: known, trace: []}, comprehension_wm?: true)

    assert recalled.discourse_reference == known
  end

  test "option disables both sides of the comprehension WM loop" do
    sid = session()
    assert WMComprehension.store(prior_turn(sid), comprehension_wm?: false) ==
             prior_turn(sid)

    no_context = %{session_id: sid, sentence: "What about each?", trace: []}
    assert WMComprehension.recall(no_context, comprehension_wm?: false) == no_context
  end
end
