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
              mood: %{
                exploration: conf,
                inhibition: 0.55,
                vigilance: 0.4,
                plasticity: 0.8
              },
              self_model: %{
                mood: %{levels: %{da: conf, "5ht": 0.52, glu: 0.64, ne: 0.77}}
              },
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
    assert data.metadata.complete_modulator_turns == 5
    assert data.metadata.missing_modulator_turns == 0
    assert data.metadata.feature_schema_version == 2
    assert length(data.metadata.feature_names) == 24
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
    assert model.model_version == "state-transition-pilot-v2"
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
  test "observed hippocampal events become state features without relevance guesses", %{dir: dir} do
    write_turns(dir, [{"session-a", [0.2, 0.4]}])

    events = [
      %{record_type: "memory_event", kind: "episodic_encode", session_id: "session-a", turn_id: "session-a-1"},
      %{record_type: "memory_event", kind: "recall_attempt", session_id: "session-a", turn_id: "session-a-1", hit: true},
      %{record_type: "memory_event", kind: "episodic_recall", session_id: "session-a", turn_id: "session-a-1"}
    ]

    File.write!(
      Path.join(dir, "memory_events.jsonl"),
      Enum.map_join(events, "", &(Jason.encode!(&1) <> "\n"))
    )

    assert {:ok, data} = StateTransitionBuilder.build(dir, smoke: true)
    assert data.metadata.input_events == 3

    [row] = Nx.to_list(data.train.x)
    features = StateTransitionBuilder.feature_names()

    lookup = fn key -> Enum.at(row, Enum.find_index(features, &(&1 == key))) end
    assert_in_delta lookup.(:episodic_encode_count), 1 / 16, 0.00001
    assert_in_delta lookup.(:episodic_recall_count), 1 / 16, 0.00001
    assert_in_delta lookup.(:recall_attempt_count), 1 / 16, 0.00001
    assert_in_delta lookup.(:recall_hit_fraction), 1.0, 0.00001
  end


  test "archive-native mood derives raw levels from self_model and keeps derived mood separate", %{dir: dir} do
    write_turns(dir, [{"session-a", [0.2, 0.4]}])
    assert {:ok, data} = StateTransitionBuilder.build(dir, smoke: true)

    [x] = Nx.to_list(data.train.x)
    [y] = Nx.to_list(data.train.y)
    features = StateTransitionBuilder.feature_names()
    at = fn row, feature ->
      row |> Enum.at(Enum.find_index(features, &(&1 == feature)))
    end

    assert_in_delta at.(x, :mood_da), 0.2, 0.000001
    assert_in_delta at.(y, :mood_da), 0.4, 0.000001
    assert_in_delta at.(x, :mood_5ht), 0.52, 0.000001
    assert_in_delta at.(x, :mood_glu), 0.64, 0.000001
    assert_in_delta at.(x, :mood_ne), 0.77, 0.000001
    assert at.(x, :mood_da_present) == 1.0
    assert at.(x, :mood_5ht_present) == 1.0
    assert at.(x, :mood_glu_present) == 1.0
    assert at.(x, :mood_ne_present) == 1.0
    assert_in_delta at.(x, :mood_exploration), 0.2, 0.000001
    assert_in_delta at.(x, :mood_inhibition), 0.55, 0.000001
    assert_in_delta at.(x, :mood_vigilance), 0.4, 0.000001
    assert_in_delta at.(x, :mood_plasticity), 0.8, 0.000001
    assert at.(x, :mood_present) == 1.0
  end

  test "missing self_model mood is marked absent rather than treated as observed zero", %{dir: dir} do
    write_turns(dir, [{"session-a", [0.2, 0.4]}])
    path = Path.join(dir, "turns.jsonl")
    [first, second] =
      path
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.map(&Jason.decode!/1)

    second = update_in(second, ["cognition"], &Map.delete(&1, "self_model"))

    File.write!(path, Jason.encode!(first) <> "\n" <> Jason.encode!(second) <> "\n")
    assert {:ok, data} = StateTransitionBuilder.build(dir, smoke: true)
    assert data.metadata.complete_modulator_turns == 1
    assert data.metadata.missing_modulator_turns == 1

    [input] = Nx.to_list(data.train.x)
    [target] = Nx.to_list(data.train.y)
    features = StateTransitionBuilder.feature_names()
    at = fn row, name -> Enum.at(row, Enum.find_index(features, &(&1 == name))) end

    assert_in_delta at.(input, :mood_da), 0.2, 0.000001
    assert at.(input, :mood_da_present) == 1.0
    assert at.(target, :mood_da) == 0.0
    assert at.(target, :mood_da_present) == 0.0
    assert at.(target, :mood_5ht_present) == 0.0
    assert at.(target, :mood_glu_present) == 0.0
    assert at.(target, :mood_ne_present) == 0.0
    # Missing raw levels are not incorrectly inferred from the derived mood.
    assert_in_delta at.(target, :mood_exploration), 0.4, 0.000001
    assert at.(target, :mood_present) == 1.0
  end


end
