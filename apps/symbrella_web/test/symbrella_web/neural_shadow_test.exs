defmodule SymbrellaWeb.NeuralShadowTest do
  use ExUnit.Case, async: false

  alias Brain.Neural.{StateTransitionBuilder, StateTransitionResidual}
  alias SymbrellaWeb.NeuralShadow

  setup do
    dir =
      Path.join(System.tmp_dir!(), "symbrella_shadow_#{System.unique_integer([:positive])}")

    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf(dir) end)
    %{dir: dir}
  end

  defp turn(session, n, confidence) do
    %{
      "record_type" => "completed_turn",
      "schema_version" => 1,
      "recorded_at" => "2026-10-13T12:00:00Z",
      "session_id" => session,
      "turn_id" => "#{session}-#{n}",
      "user_text" => "private input #{n}",
      "assistant_text" => "private answer #{n}",
      "cognition" => %{
        "confidence" => confidence,
        "mood" => %{
          "exploration" => confidence, "inhibition" => 0.5,
          "vigilance" => 0.45, "plasticity" => 0.52
        },
        "self_model" => %{"mood" => %{"levels" => %{
          "da" => confidence, "5ht" => 0.5, "glu" => 0.6, "ne" => 0.48
        }}},
        "tokens" => [%{"norm" => "hello"}],
        "lifg_choices" => [],
        "semantic_choices" => [],
        "action_candidates" => []
      },
      "runtime_observation" => %{"status" => "captured", "levels" => %{
        "da" => confidence, "5ht" => 0.5, "glu" => 0.6, "ne" => 0.48
      }},
      "hippocampal_trace" => %{"status" => "complete", "recall_status" => "not_attempted"}
    }
  end

  defp valid_checkpoint(dir) do
    turns =
      [
        turn("session-a", 1, 0.2),
        turn("session-a", 2, 0.4),
        turn("session-a", 3, 0.45),
        turn("session-b", 1, 0.7),
        turn("session-b", 2, 0.75)
      ]

    File.write!(
      Path.join(dir, "turns.jsonl"),
      Enum.map_join(turns, "", &(Jason.encode!(&1) <> "\n"))
    )

    assert {:ok, data} = StateTransitionBuilder.build(dir)
    assert {:ok, result} = StateTransitionResidual.train(data, epochs: 1, hidden_units: 4)
    checkpoint = Path.join(dir, "state_transition_residual_v4_pilot.etf")
    assert :ok == StateTransitionResidual.save(result, checkpoint)
    checkpoint
  end

  test "default runtime remains disabled when no supervised shadow is started" do
    assert %{status: :disabled} = NeuralShadow.status()
  end

  test "loads compatible held-out checkpoint and rejects incompatible metadata", %{dir: dir} do
    checkpoint = valid_checkpoint(dir)

    assert {:ok, %{artifact: artifact, sha256: checksum}} =
             NeuralShadow.load_checkpoint(checkpoint)

    assert artifact.model_version == "state-transition-residual-v4"
    assert String.length(checksum) == 64

    bad = %{artifact | feature_schema_version: 3}
    path = Path.join(dir, "bad.etf")
    File.write!(path, :erlang.term_to_binary(bad))
    assert {:error, :incompatible_or_smoke_checkpoint} = NeuralShadow.load_checkpoint(path)

    smoke = %{artifact | split_mode: :in_sample_smoke, evaluated?: false}
    File.write!(path, :erlang.term_to_binary(smoke))
    assert {:error, :incompatible_or_smoke_checkpoint} = NeuralShadow.load_checkpoint(path)

    File.write!(path, "not an Erlang term")
    assert {:error, :unsafe_or_corrupt_checkpoint} = NeuralShadow.load_checkpoint(path)
  end

  test "portable v4 checkpoint safe-decodes without embedded Nx/Axon structs", %{dir: dir} do
    checkpoint = valid_checkpoint(dir)

    assert {:ok, bytes} = File.read(checkpoint)
    assert encoded = :erlang.binary_to_term(bytes, [:safe])

    assert encoded.format == :symbrella_residual_v4_safe_etf
    assert encoded.feature_schema_version == 4
    assert encoded.byte_order == System.endianness()
    refute Map.has_key?(encoded, :params)

    assert %{shape: {24, 4}, binary: kernel} =
             encoded.weights["dense_0"]["kernel"]

    assert byte_size(kernel) == 24 * 4 * 4
    assert %{shape: {4, 24}} = encoded.weights["dense_1"]["kernel"]

    assert {:ok, %{artifact: %{params: params}}} = NeuralShadow.load_checkpoint(checkpoint)
    assert is_struct(params, Axon.ModelState)
    assert Nx.shape(params.data["dense_0"]["kernel"]) == {24, 4}
    assert Nx.type(params.data["dense_0"]["kernel"]) == {:f, 32}
  end

  test "portable checkpoint rejects invalid tensor lengths and shapes", %{dir: dir} do
    checkpoint = valid_checkpoint(dir)
    encoded = checkpoint |> File.read!() |> :erlang.binary_to_term([:safe])

    kernel = encoded.weights["dense_0"]["kernel"]

    invalid_bytes =
      put_in(encoded, [:weights, "dense_0", "kernel", :binary],
        binary_part(kernel.binary, 0, byte_size(kernel.binary) - 4))

    bad_path = Path.join(dir, "broken_tensor.etf")
    File.write!(bad_path, :erlang.term_to_binary(invalid_bytes))

    assert {:error, :invalid_portable_weights} = NeuralShadow.load_checkpoint(bad_path)

    invalid_shape =
      put_in(encoded, [:weights, "dense_0", "kernel", :shape], {4, 24})

    File.write!(bad_path, :erlang.term_to_binary(invalid_shape))

    assert {:error, :invalid_portable_weights} = NeuralShadow.load_checkpoint(bad_path)
  end

  test "supervised mode predicts and scores only consecutive same-session turns", %{dir: dir} do
    checkpoint = valid_checkpoint(dir)
    path = Path.join(dir, "shadow_predictions.jsonl")
    pid = start_supervised!({NeuralShadow, checkpoint: checkpoint, dir: dir, log_path: path})

    send_record = fn record ->
      :telemetry.execute(
        [:core, :training_archive, :completed_turn],
        %{count: 1},
        %{turn: record, events: []}
      )
      GenServer.call(pid, :status)
    end

    first = send_record.(turn("live-a", 1, 0.25))
    assert first.status == :shadow_only
    assert first.predictions == 1
    assert first.evaluations == 0
    assert first.pending_sessions == 1

    other = send_record.(turn("live-b", 1, 0.4))
    assert other.predictions == 2
    assert other.evaluations == 0
    assert other.pending_sessions == 2

    next = send_record.(turn("live-a", 2, 0.35))
    assert next.predictions == 3
    assert next.evaluations == 1
    assert next.last_evaluation.valid_feature_pairs > 0
    assert is_number(next.last_evaluation.residual_mse)
    assert is_number(next.last_evaluation.persistence_mse)

    duplicate = send_record.(turn("live-a", 2, 0.35))
    assert duplicate.predictions == 3
    assert duplicate.evaluations == 1
    assert duplicate.skipped == 1

    rows = path |> File.read!() |> String.split("\n", trim: true) |> Enum.map(&Jason.decode!/1)
    assert Enum.count(rows, &(&1["record_type"] == "shadow_prediction")) == 3
    assert Enum.count(rows, &(&1["record_type"] == "shadow_evaluation")) == 1
    [evaluation] = Enum.filter(rows, &(&1["record_type"] == "shadow_evaluation"))
    assert evaluation["from_turn_id"] == "live-a-1"
    assert evaluation["to_turn_id"] == "live-a-2"
    refute File.read!(path) =~ "private input"
    refute File.read!(path) =~ "private answer"
    assert Bitwise.band(File.stat!(path).mode, 0o777) == 0o600
  end

  test "checkpoint failure leaves the evaluator unavailable and does not observe", %{dir: dir} do
    pid =
      start_supervised!(
        {NeuralShadow, checkpoint: Path.join(dir, "missing.etf"), dir: dir}
      )

    assert %{status: :unavailable, reason: {:checkpoint_unavailable, _}} =
             GenServer.call(pid, :status)
    assert not File.exists?(Path.join(dir, "shadow_predictions.jsonl"))
  end

  test "archive integration publishes persisted turns to shadow, not raw text to its audit", %{dir: dir} do
    checkpoint = valid_checkpoint(dir)
    pid = start_supervised!({NeuralShadow, checkpoint: checkpoint, dir: dir})
    assert %{status: :shadow_only, reason: nil} = GenServer.call(pid, :status)

    attrs = fn n, conf ->
      %{
        session_id: "archive-live",
        turn_id: "archive-#{n}",
        user_text: "sensitive private text #{n}",
        assistant_text: "private reply #{n}",
        si: %{
          confidence: conf,
          tokens: [],
          lifg_choices: [],
          semantic_choices: [],
          action_candidates: [],
          mood: %{
            exploration: conf, inhibition: 0.5, vigilance: 0.4, plasticity: 0.5
          },
          self_model: %{
            mood: %{levels: %{da: conf, "5ht": 0.5, glu: 0.6, ne: 0.48}}
          }
        },
        runtime_observation: %{"status" => "captured", "levels" => %{
          "da" => conf, "5ht" => 0.5, "glu" => 0.6, "ne" => 0.48
        }},
        hippocampal_trace: %{
          worker: %{status: "complete", recall_attempts: 0, recall_misses: 0},
          final: %{status: "complete", recall_attempts: 0, recall_misses: 0}
        }
      }
    end

    assert :ok = Core.TrainingArchive.record_turn(attrs.(1, 0.5), config: [enabled?: true, dir: dir])
    assert :ok = Core.TrainingArchive.record_turn(attrs.(2, 0.6), config: [enabled?: true, dir: dir])

    status = GenServer.call(pid, :status)
    assert status.predictions == 2
    assert status.evaluations == 1
    assert File.read!(status.log_path) =~ "shadow_evaluation"
    refute File.read!(status.log_path) =~ "sensitive private text"
    refute File.read!(status.log_path) =~ "private reply"
  end
end
