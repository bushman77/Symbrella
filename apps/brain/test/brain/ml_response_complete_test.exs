defmodule Brain.MLResponseCompleteTest do
  use ExUnit.Case, async: false

  @pipeline_stop_event [:brain, :pipeline, :lifg_stage1, :stop]
  @response_complete_event [:core, :response, :complete]

  setup do
    if is_nil(Process.whereis(Brain.ML)) do
      start_supervised!({Brain.ML, []})
    end

    :ok = Brain.Bus.subscribe("brain:ml")
    :ok
  end

  test "response complete upgrades the matching turn and republishes it" do
    text = "close ml response loop #{System.unique_integer([:positive])}"
    pid = Process.whereis(Brain.ML)

    send(pid, {:intent, %{text: text, label: "ask", intent: :ask, confidence: 0.7}})
    send(pid, {:blackboard, stage1_stop_env(text)})

    _turn = wait_for_turn(text, &is_nil(Map.get(&1, :response)))

    send(pid, {:blackboard, response_complete_env(text)})

    turn =
      wait_for_turn(
        text,
        &match?(%{assistant_text: "The loop is closed."}, Map.get(&1, :response))
      )

    published = wait_for_published_response(text)

    assert turn.response.assistant_text == "The loop is closed."
    assert published.response.assistant_text == "The loop is closed."
    assert turn.response.assistant_chars == 19
    assert turn.response.user_chars == String.length(text)
    assert turn.response.tone == :warm
    assert turn.response.mode == :explainer
    assert turn.response.response_profile == :brain_explainer
    assert turn.response.prompt_response_profile == "brain_explainer"
    assert turn.response.simulated_affect == "label=steady_focus; intensity=0.62"
    assert turn.response.personality_state == "temperament=steady; depth=normal"
    assert turn.response.reflection.status == :accept
    assert turn.response.reflection.draft_sha256 == "draft-sha"
    assert turn.response.symbolic_frame == %{intent: :ask, lifg: %{choices_count: 1}}
    assert turn.response.metadata.user_text == text
    assert turn.response.metadata.prompt_response_profile == "brain_explainer"
    assert turn.response.metadata.system_sha256 == "fake-prompt-sha"
  end

  defp stage1_stop_env(text) do
    now = System.system_time(:millisecond)

    %{
      kind: :telemetry,
      event: @pipeline_stop_event,
      at_ms: now,
      measurements: %{kept: 1},
      meta: %{
        source: :run,
        frame_run_id: System.unique_integer([:positive]),
        frame_ts_ms: now,
        ts_ms: now,
        sentence: text,
        intent: :ask,
        confidence: 0.7,
        tokens: [%{index: 0, phrase: "loop", span: {0, 4}, n: 1, mw: false}],
        choices: [
          %{
            token_index: 0,
            chosen_id: "loop|noun|0",
            scores: %{"loop|noun|0" => 1.0},
            margin: 1.0,
            alt_ids: []
          }
        ],
        finalists: [%{token_index: 0, ranking: [{"loop|noun|0", 1.0}]}],
        kept_tokens: 1,
        dropped_tokens: 0,
        weak_decisions: 0,
        missing_candidates: 0,
        missing_candidate_tokens: [],
        boundary_drops: 0,
        chargram_violation: 0,
        guard_drops: 0,
        mwe_fallbacks: 0
      }
    }
  end

  defp response_complete_env(text) do
    %{
      kind: :telemetry,
      event: @response_complete_event,
      at_ms: System.system_time(:millisecond),
      measurements: %{
        assistant_chars: 19,
        user_chars: String.length(text)
      },
      meta: %{
        user_text: text,
        assistant_text: "The loop is closed.",
        tone: :warm,
        mode: :explainer,
        response_profile: :brain_explainer,
        prompt_response_profile: "brain_explainer",
        simulated_affect: "label=steady_focus; intensity=0.62",
        personality_state: "temperament=steady; depth=normal",
        reflection: %{
          v: 1,
          status: :accept,
          issues: [],
          applied?: false,
          repair_count: 0,
          draft_sha256: "draft-sha",
          final_sha256: "draft-sha"
        },
        system_sha256: "fake-prompt-sha",
        symbolic_frame: %{intent: :ask, lifg: %{choices_count: 1}}
      }
    }
  end

  defp wait_for_turn(text, predicate, timeout_ms \\ 500, step_ms \\ 20) do
    wait_until(
      fn ->
        Brain.ML.turns()
        |> Enum.filter(&(Map.get(&1, :text) == text))
        |> case do
          [%{} = turn] ->
            if predicate.(turn), do: turn, else: false

          _ ->
            false
        end
      end,
      timeout_ms,
      step_ms
    )
  end

  defp wait_until(fun, timeout_ms, step_ms) do
    started_at = System.monotonic_time(:millisecond)

    case fun.() do
      false ->
        if System.monotonic_time(:millisecond) - started_at > timeout_ms do
          flunk("timed out waiting for Brain.ML response-complete turn")
        else
          Process.sleep(step_ms)
          wait_until(fun, timeout_ms, step_ms)
        end

      nil ->
        Process.sleep(step_ms)
        wait_until(fun, timeout_ms, step_ms)

      value ->
        value
    end
  end

  defp wait_for_published_response(text) do
    receive do
      {:ml_turn, %{text: ^text, response: %{assistant_text: "The loop is closed."}} = turn} ->
        turn

      {:ml_turn, _other} ->
        wait_for_published_response(text)
    after
      500 -> flunk("timed out waiting for republished Brain.ML response turn")
    end
  end
end
