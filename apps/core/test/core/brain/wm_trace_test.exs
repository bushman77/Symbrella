defmodule Core.Brain.WMTraceTest do
  use ExUnit.Case, async: true

  alias Core.Brain.WM

  def handle_focus(event, measurements, metadata, pid),
    do: send(pid, {:wm_trace_telemetry, event, measurements, metadata})

  test "geography conversation reports no focus submission, not a gate rejection" do
    handler = "core-wm-route-#{System.unique_integer([:positive])}"
    event = [:core, :wm, :focus]

    :ok = :telemetry.attach(handler, event, &__MODULE__.handle_focus/4, self())
    on_exit(fn -> :telemetry.detach(handler) end)

    si = %{
      sentence: "How far is Kamloops BC from Maple Ridge BC?",
      lifg_choices: [
        %{token_index: 3, id: "kamloops|proper_noun|1", score: 0.5},
        %{token_index: 6, id: "maple ridge|proper_noun|1", score: 0.5}
      ],
      trace: []
    }

    out = WM.focus_prompt_topics(si, [])
    assert [%{stage: :wm_focus, decision: :none, reason: :no_configured_prompt_topic,
      meta: meta}] = out.trace

    assert meta.candidate_count == 0
    assert meta.lifg_choices_available == 2
    assert meta.lifg_winners_submitted? == false
    assert meta.focus_attempted? == false
    assert meta.admission_path == :prompt_topics_only

    assert_receive {:wm_trace_telemetry, ^event,
      %{topic_candidates: 0, lifg_choices: 2},
      %{decision: :none, reason: :no_configured_prompt_topic}}
  end

  test "disabled topic focus is explicitly traced, not silently dropped" do
    out = WM.focus_prompt_topics(%{sentence: "working memory", trace: []},
      focus_prompt_topics?: false)

    assert [%{stage: :wm_focus, decision: :disabled,
      reason: :disabled_by_option, meta: %{focus_attempted?: false}}] = out.trace
  end
end
