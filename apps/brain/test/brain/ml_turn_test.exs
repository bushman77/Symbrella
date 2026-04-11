defmodule Brain.MLTurnTest do
  use ExUnit.Case, async: false

  @pipeline_stop_event [:brain, :pipeline, :lifg_stage1, :stop]

  setup do
    if is_nil(Process.whereis(Brain.ML)) do
      start_supervised!({Brain.ML, []})
    end

    :ok
  end

  test "pipeline stop rerun replaces recent turn for the same text" do
    text = "ml turn collapse #{System.unique_integer([:positive])}"
    pid = Process.whereis(Brain.ML)

    send(pid, {:intent, %{text: text, label: "ask", intent: :ask, confidence: 0.7}})
    send(pid, {:blackboard, stage1_stop_env(text, 1, "alpha|noun|0")})

    _first = wait_for_turn(text, 1)

    send(pid, {:blackboard, stage1_stop_env(text, 2, "alpha|verb|1")})

    turn = wait_for_turn(text, 2)

    matching =
      Brain.ML.turns()
      |> Enum.filter(&(Map.get(&1, :text) == text))

    assert [^turn] = matching
    assert get_in(turn, [:lifg, :last_update, :last, :meta, :frame_run_id]) == 2
    assert [%{chosen_id: "alpha|verb|1"} | _] = get_in(turn, [:lifg, :winners])
  end

  defp stage1_stop_env(text, frame_run_id, chosen_id) do
    now = System.system_time(:millisecond)

    %{
      kind: :telemetry,
      event: @pipeline_stop_event,
      at_ms: now,
      measurements: %{kept: 1},
      meta: %{
        source: :run,
        frame_run_id: frame_run_id,
        frame_ts_ms: now,
        ts_ms: now,
        sentence: text,
        intent: :ask,
        confidence: 0.7,
        tokens: [%{index: 0, phrase: "alpha", span: {0, 5}, n: 1, mw: false}],
        choices: [
          %{
            token_index: 0,
            chosen_id: chosen_id,
            scores: %{chosen_id => 1.0},
            margin: 1.0,
            alt_ids: []
          }
        ],
        finalists: [%{token_index: 0, ranking: [{chosen_id, 1.0}]}],
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

  defp wait_for_turn(text, frame_run_id, timeout_ms \\ 500, step_ms \\ 20) do
    wait_until(
      fn ->
        Brain.ML.turns()
        |> Enum.filter(&(Map.get(&1, :text) == text))
        |> case do
          [%{} = turn] ->
            if get_in(turn, [:lifg, :last_update, :last, :meta, :frame_run_id]) == frame_run_id do
              turn
            else
              false
            end

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
          flunk("timed out waiting for Brain.ML turn")
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
end
