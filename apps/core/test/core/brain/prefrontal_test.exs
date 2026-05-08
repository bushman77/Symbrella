defmodule Core.Brain.PrefrontalTest do
  use ExUnit.Case, async: true

  alias Core.Brain.Prefrontal
  alias Core.SemanticInput

  test "attaches semantic control signals to a SemanticInput" do
    si = %SemanticInput{
      sentence: "remember the dangerous system event",
      tokens: [%{phrase: "remember"}, %{phrase: "dangerous"}],
      confidence: 0.42,
      acc_conflict: 0.72,
      sense_candidates: %{
        0 => [%{id: "remember|verb|0", score: 0.8}],
        1 => [%{id: "dangerous|adj|0", score: 0.6}, %{id: "danger|noun|0", score: 0.55}]
      },
      perception: %{salience: %{0 => 0.4, 1 => 0.8}},
      comprehension: %{degraded?: true},
      trace: []
    }

    out = Prefrontal.attach(si, wm_load: 0.5)

    assert %{signals: signals, ctx: ctx} = out.prefrontal
    assert out.control_signals == signals
    assert ctx.acc_conflict == 0.72
    assert ctx.novelty > 0.0
    assert is_atom(signals.policy)
    assert is_integer(signals.top_k)
    assert is_integer(signals.branch_budget)
    assert is_number(signals.salience_boost)

    assert [%{stage: :prefrontal_control, decision: :attached} | _] = out.trace
  end

  test "can be disabled by option" do
    si = %SemanticInput{sentence: "hello", trace: []}

    assert ^si = Prefrontal.attach(si, prefrontal_control?: false)
  end
end
