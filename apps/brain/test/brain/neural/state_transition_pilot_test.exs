defmodule Brain.Neural.StateTransitionPilotTest do
  use ExUnit.Case, async: true

  alias Brain.Neural.{StateTransitionBuilder, StateTransitionPilot}

  setup do
    dir = Path.join(System.tmp_dir!(), "symbrella_state_#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf(dir) end)
    %{dir: dir}
  end

  defp write_turns(dir, sessions) do
    rows =
      sessions
      |> Enum.flat_map(fn {session, confidences} ->
        Enum.with_index(confidences, 1)
        |> Enum.map(fn {conf, index} ->
          %{
            schema_version: 1,
            record_type: "completed_turn",
            session_id: session,
            turn_id: "#{session}-#{index}",
            recorded_at: "2026-10-13T12:00:00Z",
            user_text: "A conversation message #{index}",
            assistant_text: "An answer",
            cognition: %{
              confidence: conf,
              intent: "ask",
              mood: %{da: conf, "5ht": 0.5, glu: 0.3, ne: 0.2},
              tokens: [%{phrase: "hello"}, %{phrase: "world"}],
              lifg_choices: [%{id: "hello|noun|1"}],
              self_monitor: %{vigilance: 0.1}
            }
          }
        end)
      end)

    File.write!(Path.join(dir, "turns.jsonl"), Enum.map_join(rows, "", &(Jason.encode!(&1) <> "\n")))
  end

  test "all recorded consecutive states become self-supervised x -> next-state y", %{dir: dir} do
    write_turns(dir, [{"session-a", [0.2, 0.4, 0.6]}, {"session-b", [0.7, 0.8]}])

    assert {:ok, data} = StateTransitionBuilder.build(dir)
    assert data.metadata.input_turns == 5
    assert data.metadata.total_pairs == 3
    assert data.metadata.train_examples == 2
    assert data.metadata.eval_examples == 1
    assert data.metadata.train_sessions == ["session-a"]
    assert data.metadata.eval_sessions == ["session-b"]

    count = length(StateTransitionBuilder.feature_names())
    assert Nx.shape(data.train.x) == {2, count}
    assert Nx.shape(data.train.y) == {2, count}

    # Target is the ACTUAL next observed confidence, not a made-up "correct" label.
    [x1 | _] = Nx.to_list(data.train.x)
    [y1 | _] = Nx.to_list(data.train.y)
    assert_in_delta hd(x1), 0.2, 0.00001
    assert_in_delta hd(y1), 0.4, 0.00001

    assert {:ok, model} = StateTransitionPilot.train(data, epochs: 1)
    assert model.model_version == "state-transition-pilot-v1"
    assert model.evaluated? == true
    assert model.train_examples == 2
    assert model.eval_examples == 1
    assert model.train_mse >= 0.0
    assert model.eval_mse >= 0.0
    assert model.persistence_eval_mse >= 0.0
    assert model.deployment_status == :experimental_not_for_runtime
  end

  test "smoke mode uses every transition from one session but reports no holdout", %{dir: dir} do
    write_turns(dir, [{"only-session", [0.2, 0.4, 0.6]}])

    assert {:error, :insufficient_independent_sessions} = StateTransitionBuilder.build(dir)

    assert {:ok, data} = StateTransitionBuilder.build(dir, smoke: true)
    assert data.metadata.total_pairs == 2
    assert data.metadata.split_mode == :in_sample_smoke
    assert Nx.shape(data.train.x) == {2, length(StateTransitionBuilder.feature_names())}

    assert {:ok, model} = StateTransitionPilot.train(data, epochs: 1)
    assert model.evaluated? == false
    assert model.train_examples == 2
    assert model.eval_examples == 2
  end

  test "no consecutive observations means no fabricated transitions", %{dir: dir} do
    write_turns(dir, [{"one", [0.5]}, {"two", [0.7]}])

    assert {:error, :insufficient_consecutive_turns} =
             StateTransitionBuilder.build(dir, smoke: true)
  end

  test "truncated event trace is not used to learn state transitions", %{dir: dir} do
    write_turns(dir, [{"session-a", [0.2, 0.4]}])
    event = %{record_type: "memory_event", kind: "trace_truncated", session_id: "session-a", turn_id: "session-a-1"}
    File.write!(Path.join(dir, "memory_events.jsonl"), Jason.encode!(event) <> "\n")
    assert {:error, :truncated_hippocampal_trace} = StateTransitionBuilder.build(dir, smoke: true)
  end
end
