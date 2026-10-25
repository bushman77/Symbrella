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

  test "Core comprehension uses BG admission without gaining factual authority" do
    sid = session()
    id = frame_id(sid)
    listener = "p9a03-core-#{System.unique_integer([:positive])}"

    :ok =
      :telemetry.attach(
        listener,
        [:brain, :gate, :decision],
        fn event, measurements, metadata, pid ->
          send(pid, {:core_gate, event, measurements, metadata})
        end,
        self()
      )

    on_exit(fn ->
      :telemetry.detach(listener)
      Brain.defocus(id)
    end)

    stored = WMComprehension.store(prior_turn(sid), comprehension_wm?: true)
    assert [%{stage: :wm_comprehension, decision: :admitted} | _] = stored.trace

    assert_receive {:core_gate, [:brain, :gate, :decision], %{score: score},
                    %{id: ^id, source: :comprehension, gate: :basal_ganglia,
                      decision: decision}}

    assert decision in [:allow, :boost]
    assert score >= 0.0 and score <= 1.0

    assert %{payload: %{context: context}} =
             Enum.find(Brain.snapshot_wm().wm, &(&1.id == id))

    assert context.verification == :conversation_context_only
    refute Map.get(context, :factual_authority?, false)
  end

  defp projected_action_si(sid) do
    choices = [
      %{token_index: 1, id: "artist|noun|1"},
      %{token_index: 2, id: "draws|verb|1"},
      %{token_index: 4, id: "picture|noun|1"}
    ]

    si = %{
      session_id: sid,
      source: :user,
      sentence: "The artist draws a picture.",
      intent: :unknown,
      confidence: 0.4,
      trace: [],
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: "artist", n: 1},
        %{index: 2, phrase: "draws", n: 1},
        %{index: 3, phrase: "a", n: 1},
        %{index: 4, phrase: "picture.", n: 1}
      ],
      sentence_signatures: [
        %{
          id: :finite_present_svo_candidate,
          source: :core_syntax,
          span: {0, 4},
          evidence: [:contiguous_surface, :attested_nominal_subject,
                     :attested_nominal_object, :attested_finite_present_verb],
          roles: %{subject_determiner: 0, subject_head: 1, predicate: 2,
                   object_determiner: 3, object_head: 4}
        }
      ],
      lifg_choices: choices,
      atl_slate: %{winners: choices},
      active_cells: [
        %{id: "draws|verb|1", norm: "draws", pos: "verb",
          definition: "An inflected form of draw.",
          semantic_atoms: ["tag:form-of", "tag:indicative", "tag:present",
                           "tag:singular", "tag:third-person"]}
      ],
      sentence_meaning: %{status: :not_yet_interpreted},
      comprehension: %{
        understood: [],
        uncertain: ["artist", "draws", "picture"],
        recognized_topics: [],
        degraded?: true
      }
    }

    Core.Semantic.EpisodicProjection.attach(si)
  end

  test "validated action participants become WM context without falsely upgrading lexical comprehension" do
    sid = session()
    id = frame_id(sid)
    on_exit(fn -> Brain.defocus(id) end)

    si = projected_action_si(sid)
    assert [%{relation: "verb_draw", participants: %{"actor" => "the artist",
              "patient" => "a picture"}}] = si.episodic_candidates

    context = WMComprehension.context_from(si, si.comprehension)
    assert context.entities == ["the artist", "a picture"]
    assert context.relation == "verb_draw"
    assert context.event_roles == %{
      "actor" => "the artist",
      "predicate" => "draw",
      "surface_predicate" => "draws",
      "patient" => "a picture",
      "tense" => "present"
    }
    assert context.event_source_frame == :finite_present_svo
    assert context.event_role_status == :validated_projection
    assert context.interpretation_status == :not_yet_interpreted
    assert context.degraded?
    assert context.understood == []
    assert context.verification == :conversation_context_only

    stored = WMComprehension.store(si, comprehension_wm?: true)
    assert Enum.any?(stored.trace, &(&1.stage == :wm_comprehension and &1.decision == :admitted))

    assert %{payload: %{context: wm_context}} =
             Enum.find(Brain.snapshot_wm().wm, &(&1.id == id))
    assert wm_context.event_roles == context.event_roles
    assert wm_context.entities == ["the artist", "a picture"]
    refute Map.get(wm_context, :factual_authority?, false)
  end

  test "WM does not admit forged or non-user participant roles as context" do
    si = projected_action_si(session())
    [candidate] = si.episodic_candidates

    poisoned = %{si | episodic_candidates:
      [%{candidate | participants: %{"actor" => "the painter"}}]}
    rejected = WMComprehension.context_from(poisoned, poisoned.comprehension)
    assert rejected.entities == []
    assert rejected.relation == nil
    assert rejected.event_roles == %{}
    assert rejected.event_role_status == :none

    assistant = %{si | source: :assistant}
    rejected_assistant = WMComprehension.context_from(assistant, assistant.comprehension)
    assert rejected_assistant.entities == []
    assert rejected_assistant.relation == nil
    assert rejected_assistant.event_roles == %{}
    assert rejected_assistant.event_role_status == :none
  end

  test "disabled Core comprehension never submits a WM candidate" do
    sid = session()
    id = frame_id(sid)
    listener = "p9a03-core-disabled-#{System.unique_integer([:positive])}"

    :ok =
      :telemetry.attach(
        listener,
        [:brain, :gate, :decision],
        fn _event, _measurements, metadata, pid ->
          send(pid, {:core_gate_id, metadata.id})
        end,
        self()
      )

    on_exit(fn -> :telemetry.detach(listener) end)

    si = prior_turn(sid)
    assert WMComprehension.store(si, comprehension_wm?: false) == si

    refute_receive {:core_gate_id, ^id}, 75
    refute Enum.any?(Brain.snapshot_wm().wm, &(&1.id == id))
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
  test "WM carries an established conversation subject into a year follow-up" do
    sid = session()
    id = frame_id(sid)
    on_exit(fn -> Brain.defocus(id) end)

    initial = %{
      session_id: sid,
      sentence: "can you give me te detils of the Ukranian - Russian war?",
      intent: :ask,
      comprehension: %{recognized_topics: [], understood: [], uncertain: [], degraded?: true},
      trace: []
    }
    assert %{topic: "the Ukranian - Russian war", verification: :conversation_context_only} =
             WMComprehension.context_from(initial, initial.comprehension)

    WMComprehension.store(initial, comprehension_wm?: true)
    recalled = WMComprehension.recall(
      %{session_id: sid, sentence: "how about in 2026?", trace: []},
      comprehension_wm?: true
    )

    assert recalled.wm_context.topic == "the Ukranian - Russian war"
    following = Map.put(recalled, :comprehension,
      %{recognized_topics: [], understood: [], uncertain: ["2026"], degraded?: true})
    context = WMComprehension.context_from(following, following.comprehension)
    assert context.topic == "the Ukranian - Russian war"
    assert context.topic_source == :same_session_wm
    assert context.temporal_reference.year == 2026
    assert context.verification == :conversation_context_only
  end
end
