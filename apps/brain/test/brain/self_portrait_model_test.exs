# apps/brain/test/brain/self_portrait_model_test.exs
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

  test "tracks LIFG Stage1 summary payload gaps" do
    p0 = Model.new()

    p1 =
      Model.observe(p0, %{
        kind: :telemetry,
        event: [:brain, :pipeline, :lifg_stage1, :stop],
        measurements: %{kept: 2},
        meta: %{
          kept_tokens: 2,
          tokens: [],
          choices: [],
          finalists: []
        },
        at_ms: 1
      })

    assert p1.patterns.lifg_payload_gaps == 1

    p2 =
      Model.observe(p1, %{
        kind: :telemetry,
        event: [:brain, :pipeline, :lifg_stage1, :stop],
        measurements: %{kept: 1},
        meta: %{
          kept_tokens: 1,
          tokens: [%{index: 0, phrase: "hello"}],
          choices: [%{token_index: 0, chosen_id: "hello|interjection|0"}],
          finalists: [%{token_index: 0, ranking: [{"hello|interjection|0", 1.0}]}]
        },
        at_ms: 2
      })

    assert p2.patterns.lifg_payload_gaps == 1
  end

  test "tracks suspicious LIFG POS choices in greeting context" do
    p0 = Model.new()

    p1 =
      Model.observe(p0, %{
        kind: :telemetry,
        event: [:brain, :pipeline, :lifg_stage1, :stop],
        measurements: %{kept: 3},
        meta: %{
          intent: :greet,
          kept_tokens: 3,
          tokens: [
            %{index: 0, phrase: "Good"},
            %{index: 1, phrase: "morning"},
            %{index: 2, phrase: "Symbrella"}
          ],
          choices: [
            %{token_index: 0, chosen_id: "good|verb|1", margin: 0.05},
            %{token_index: 1, chosen_id: "morning|noun|0", margin: 1.0},
            %{token_index: 2, chosen_id: "symbrella|assistant|0", margin: 1.0}
          ],
          finalists: [%{token_index: 0, ranking: [{"good|verb|1", 0.05}]}]
        },
        at_ms: 1
      })

    assert p1.patterns.lifg_pos_anomalies == 1
  end
end
