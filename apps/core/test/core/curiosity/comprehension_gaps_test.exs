defmodule Core.Curiosity.ComprehensionGapsTest do
  use ExUnit.Case, async: true
  alias Core.Brain.WM.Comprehension
  alias Core.Curiosity.ComprehensionGaps

  @time 1_792_598_267_855
  @session "uninterpreted-conversation"

  defp snapshot(session, attrs \\ %{}) do
    context = Map.merge(%{
      degraded?: true,
      interpretation_status: :not_yet_interpreted,
      intent: :ask,
      verification: :conversation_context_only,
      uncertain: ["iranian", "war", "u.s"],
      understood: [],
      updated_at_ms: @time,
      sentence: "not for curiosity reporting"
    }, attrs)

    %{wm: [%{
      id: Comprehension.frame_id(session),
      source: :comprehension,
      last_bump: @time,
      payload: %{kind: :comprehension_frame, context: context}
    }]}
  end

  test "a degraded, unverified same-session frame produces a typed review, not factual learning" do
    assert {:ok, gap} =
      snapshot(@session)
      |> ComprehensionGaps.from_snapshot(@session, @time + 30_000)

    assert gap.kind == :comprehension_gap
    assert gap.status == :review_required
    assert gap.reason == :uninterpreted_language
    assert gap.next_action == :inspect_comprehension
    assert gap.uncertain_count == 3
    assert gap.understood_count == 0
    assert gap.age_ms == 30_000
    assert gap.verification == :conversation_context_only
    assert gap.factual_authority? == false
    assert gap.investigation_authorized? == false
    refute Map.has_key?(gap, :sentence)
    refute Map.has_key?(gap, :uncertain)
    refute Map.has_key?(gap, :session_id)
  end

  test "never reads a different session or the global WM without a session identifier" do
    snap = snapshot(@session)
    assert :none == ComprehensionGaps.from_snapshot(snap, "someone-else", @time)
    assert :none == ComprehensionGaps.from_snapshot(snap, nil, @time)
    assert :none == ComprehensionGaps.for_session(nil)
  end

  test "does not revive stale context or accept invalid source, missing proof labels or clean comprehension" do
    snap = snapshot(@session)
    assert :none == ComprehensionGaps.from_snapshot(snap, @session, @time + 20 * 60 * 1_000 + 1)

    clean = snapshot(@session, %{degraded?: false})
    assert :none == ComprehensionGaps.from_snapshot(clean, @session, @time)

    no_terms = snapshot(@session, %{uncertain: []})
    assert :none == ComprehensionGaps.from_snapshot(no_terms, @session, @time)

    not_context = snapshot(@session, %{verification: :source_supported})
    assert :none == ComprehensionGaps.from_snapshot(not_context, @session, @time)

    [%{} = item] = snap.wm
    untrusted = %{wm: [%{item | source: :hippocampus}]}
    assert :none == ComprehensionGaps.from_snapshot(untrusted, @session, @time)
  end

  test "nonquestion uncertainty is eligible only for supervised clarification review" do
    snap = snapshot(@session, %{intent: :statement, interpretation_status: :partial})
    assert {:ok, %{next_action: :consider_user_clarification,
                   reason: :unresolved_lexical_meaning}} =
      ComprehensionGaps.from_snapshot(snap, @session, @time)
  end

  test "real comprehension frame is identified with the same hash used by WM storage" do
    sid = "real-comprehension-#{System.unique_integer([:positive])}"
    si = %{
      session_id: sid, sentence: "What about the unknown thing?", intent: :ask,
      comprehension: %{recognized_topics: [], understood: [],
        uncertain: ["unknown thing"], degraded?: true},
      sentence_meaning: %{status: :not_yet_interpreted}
    }
    context = Comprehension.context_from(si, si.comprehension)
    now = context.updated_at_ms
    snap = snapshot(sid, context)

    assert {:ok, %{kind: :comprehension_gap, uncertain_count: 1}} =
      ComprehensionGaps.from_snapshot(snap, sid, now)
  end
end
