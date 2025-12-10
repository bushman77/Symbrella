defmodule Brain.SelfPortrait.ModelTest do
  use ExUnit.Case, async: true

  alias Brain.SelfPortrait.Model

  test "tracks boundary drops and chargram violations" do
    p0 = Model.new(max_events: 3)

    p1 =
      Model.observe(p0, %{
        kind: :telemetry,
        event: [:brain, :lifg, :stage1, :boundary_drop],
        measurements: %{dropped: 1},
        meta: %{token_index: 2},
        at_ms: 1
      })

    p2 =
      Model.observe(p1, %{
        kind: :telemetry,
        event: [:brain, :lifg, :stage1, :chargram_violation],
        measurements: %{dropped: 1},
        meta: %{token_index: 2},
        at_ms: 2
      })

    assert p2.patterns.boundary_drops == 1
    assert p2.patterns.chargram_violations == 1
    assert length(p2.last_events) == 2
  end

  test "bounds last_events to max_events" do
    p0 = Model.new(max_events: 2)

    p1 = Model.observe(p0, %{kind: :telemetry, event: [:brain, :wm, :update], at_ms: 10})
    p2 = Model.observe(p1, %{kind: :telemetry, event: [:brain, :wm, :update], at_ms: 11})
    p3 = Model.observe(p2, %{kind: :telemetry, event: [:brain, :wm, :update], at_ms: 12})

    assert length(p3.last_events) == 2
    assert p3.patterns.wm_updates == 3
  end

  test "nudges curiosity when pMTG consult fires" do
    p0 = Model.new()
    c0 = p0.traits.curiosity_bias

    p1 = Model.observe(p0, %{kind: :telemetry, event: [:brain, :pmtg, :consult], at_ms: 1})

    assert p1.patterns.pmtg_consults == 1
    assert p1.traits.curiosity_bias >= c0
  end
end

