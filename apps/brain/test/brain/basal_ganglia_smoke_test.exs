# test/brain/basal_ganglia_smoke_test.exs
defmodule Brain.BasalGangliaSmokeTest do
  use ExUnit.Case, async: true
  alias Brain.BasalGanglia

  @cfg %{gate_threshold: 0.4, capacity: 7}

  test "attention lifts marginal item" do
    attn = %{goal_terms: ~w(hello)}
    {dec, s} = BasalGanglia.decide([], %{lemma: "hello", score: 0.2, source: :runtime}, attn, @cfg)
    assert dec in [:allow, :boost]
    assert s >= 0.4
  end

  test "duplicate within cooldown is a boost" do
    now = System.system_time(:millisecond)
    wm = [%{id: "hello", last_bump: now, activation: 0.7, payload: %{lemma: "hello"}}]
    attn = %{}
    {dec, _} = BasalGanglia.decide(wm, %{id: "hello", score: 0.3, source: :lifg}, attn, Map.merge(@cfg, %{cooldown_ms: 5000}))
    assert dec == :boost
  end
end

