defmodule Core.Response.MoodTurnPersistenceTest do
  use ExUnit.Case, async: false

  alias Core.Response

  @baseline %{da: 0.5, "5ht": 0.5, glu: 0.5, ne: 0.5}

  setup do
    case Process.whereis(Brain.MoodCore) do
      nil -> start_supervised!({Brain.MoodCore, []})
      _pid -> :ok
    end

    Brain.MoodCore.configure(
      clock: :self,
      baseline: @baseline,
      init: @baseline,
      half_life_ms: 60_000,
      max_delta_per_tick: 0.20,
      saturation_ticks: 10,
      shock_threshold: 1.0
    )

    Brain.MoodCore.reset()
    :ok
  end

  test "response planning nudges MoodCore so turn state is not stuck at baseline" do
    before = Brain.MoodCore.snapshot()

    {_tone, _text, _meta} =
      Response.plan(
        %{
          intent: :question,
          confidence: 0.9,
          text: "what is happening in your digital traces?"
        },
        %{mood: before.mood, tone_hint: before.tone_hint}
      )

    after_ = Brain.MoodCore.snapshot()

    assert after_.levels.da > before.levels.da
    assert after_.levels.ne > before.levels.ne
    assert after_.mood.exploration > before.mood.exploration
    assert after_.mood.vigilance > before.mood.vigilance
  end
end
