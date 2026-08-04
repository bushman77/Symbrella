defmodule Core.Response.MoodIndicesFlowTest do
  use ExUnit.Case, async: false

  alias Core.Response

  setup do
    case Process.whereis(Brain.MoodCore) do
      nil -> start_supervised!({Brain.MoodCore, []})
      _pid -> :ok
    end

    Brain.MoodCore.configure(
      clock: :self,
      baseline: %{da: 0.4, "5ht": 0.6, glu: 0.4, ne: 0.4},
      init: %{da: 0.4, "5ht": 0.6, glu: 0.4, ne: 0.4},
      half_life_ms: 60_000,
      max_delta_per_tick: 0.20
    )

    Brain.MoodCore.reset()

    Brain.MoodCore.configure(init: %{da: 0.41, "5ht": 0.52, glu: 0.41, ne: 0.53})

    :ok
  end

  test "routes mood index queries through model synthesis" do
    si = %{
      intent: :question,
      keyword: "mood indices",
      confidence: 0.9,
      text: "your mood indices"
    }

    {tone, text, meta} = Response.plan(si, %{})

    assert tone == :neutral
    assert meta.mode == :explainer
    assert meta.action == :answer
    assert meta.chosen_skill == :mood_indices
    assert :mood_indices_answer in meta.overrides

    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "Current mood indices:"
    refute text =~ "Vigilance: 0.53 (+0.13 from baseline)"

    refute text =~ "quick TODO list"
    refute text =~ "short outline"
    refute text =~ "reflect a calm"
  end

  test "routes feeling questions through model synthesis" do
    si = %{
      intent: :question,
      confidence: 0.9,
      text: "how are you feeling?"
    }

    {tone, text, meta} = Response.plan(si, %{})

    assert tone == :neutral
    assert meta.mode == :explainer
    assert meta.action == :answer
    assert meta.chosen_skill == :self_state_feeling
    assert :self_state_feeling_answer in meta.overrides

    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "software self-state"
    refute text =~ "Raw modulators:"

    refute text =~ "How can I assist you today"
  end
end
