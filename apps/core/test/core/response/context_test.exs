defmodule Core.Response.ContextTest do
  use ExUnit.Case, async: true

  alias Core.Response.Context

  test "from_si/1 compacts symbolic SI evidence into a bounded frame" do
    context =
      Context.from_si(%{
        sentence: "what are you tracking?",
        session_id: "s1",
        intent: :question,
        keyword: "tracking",
        confidence: 0.82,
        tokens: [
          %{payload: %{lemma: "symbolic"}},
          %{payload: %{lemma: "brain"}}
        ],
        active_cells: [%{id: "tracking|noun|0"}],
        sense_candidates: %{0 => [%{id: "symbolic|adj|0"}], 1 => [%{id: "brain|noun|0"}]},
        lifg_choices: [%{chosen_id: "brain|noun|0"}],
        acc_conflict: 0.62,
        perception: %{source: :chat},
        self_model: %{confidence: 0.7}
      })

    assert context.user_text == "what are you tracking?"
    assert context.session_id == "s1"
    assert context.symbolic_frame.intent == :question
    assert context.symbolic_frame.lexical.token_count == 2
    assert context.symbolic_frame.lexical.active_cells_count == 1
    assert context.symbolic_frame.lexical.sense_candidates_count == 2
    assert context.symbolic_frame.lexical.top_terms == ["symbolic", "brain", "tracking|noun|0"]
    assert context.symbolic_frame.lifg.choices_count == 1
    assert context.symbolic_frame.lifg.degraded? == true
    assert context.symbolic_frame.perception == :present
    assert context.symbolic_frame.self_model == :present
  end

  test "from_response_parts/5 preserves embedded turn context and live runtime state" do
    embedded = %{
      session_id: "turn-7",
      comprehension: %{understood: ["loop"]},
      symbolic_frame: %{intent: :command, lexical: %{token_count: 3}}
    }

    context =
      Context.from_response_parts(
        "close the loop",
        %{intent: :command, turn_context: embedded},
        %{tone: :warm},
        %{},
        %{wm_items: [%{id: "loop"}], runtime_state: %{source: :brain}}
      )

    assert context.session_id == "turn-7"
    assert context.comprehension == %{understood: ["loop"]}
    assert context.symbolic_frame.lexical.token_count == 3
    assert context.runtime_state.source == :brain
    assert [%{id: "loop"}] = context.wm_items
  end

  test "bounded hippocampal episode text survives SI to response context transport" do
    episode = fn session, sentence, score ->
      %{score: score, episode: %{meta: %{session_id: session, sentence: sentence},
                                  slate: %{winners: [%{lemma: "orchard"}]}}}
    end

    si = %{
      sentence: "What was the orchard nickname?",
      session_id: "active-session",
      evidence: %{
        episodes: [
          episode.("active-session", "We called it copper-fern-orbit.", 0.9),
          episode.("other-session", "Cross-session secret.", 0.85),
          episode.("active-session", "We called it copper-fern-orbit.", 0.8),
          episode.("active-session", String.duplicate("a", 300), 0.7),
          episode.("active-session", "Third eligible episode.", 0.6),
          episode.("active-session", "Unscored episode.", 0.0)
        ]
      }
    }

    embedded = Context.from_si(si)

    assert Enum.map(embedded.episodic_recall, & &1.text) ==
             ["We called it copper-fern-orbit.", String.duplicate("a", 220)]

    assert Enum.all?(embedded.episodic_recall,
             &(&1.source == :hippocampal_episode))

    refute Map.has_key?(embedded, :evidence)
    refute inspect(embedded) =~ "Cross-session secret"
    refute inspect(embedded) =~ "Third eligible episode"

    projected =
      Context.from_response_parts(si.sentence,
        %{session_id: si.session_id, turn_context: embedded},
        %{mode: :collaborator}, %{}, %{})

    assert projected.episodic_recall == embedded.episodic_recall
  end

  test "does not expose hippocampal content without a matching session and positive score" do
    si = %{
      sentence: "Do you remember?",
      session_id: "current",
      evidence: %{episodes: [
        %{score: 0.0, episode: %{meta: %{session_id: "current", sentence: "No match"}}},
        %{score: 0.95, episode: %{meta: %{session_id: "previous", sentence: "Wrong user"}}},
        %{score: 0.7, episode: %{meta: %{sentence: "Unscoped old episode"}}}
      ]}
    }

    refute Map.has_key?(Context.from_si(si), :episodic_recall)
    refute Map.has_key?(Context.from_si(%{si | session_id: nil}), :episodic_recall)
  end
end
