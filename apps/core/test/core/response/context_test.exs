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
end
