defmodule Brain.Neural.MemoryRelevancePilotTest do
  use ExUnit.Case, async: true

  alias Brain.Neural.{MemoryRelevancePilot, TrainingBuilder}

  defp row(score) do
    [0.7, 1.0, 0.4, 0.5, 0.6, 0.3, 0.125, 0.2, score, 1.0, 0.125, 1.0]
  end

  defp batch(scores, labels) do
    %{
      x: Nx.tensor(Enum.map(scores, &row/1), type: :f32),
      y: Nx.tensor(Enum.map(labels, &[&1]), type: :f32),
      feature_names: TrainingBuilder.feature_names(),
      label_names: [:relevant]
    }
  end

  defp dataset do
    %{
      train: batch([0.9, 0.1], [1.0, 0.0]),
      eval: batch([0.3], [1.0]),
      metadata: %{
        task: :memory_relevance,
        feature_schema_version: TrainingBuilder.feature_schema_version(),
        train_sessions: ["train-a", "train-b"],
        eval_sessions: ["eval-c"]
      }
    }
  end

  test "trains an advisory one-output Axon graph and evaluates against recall score" do
    assert {:ok, result} = MemoryRelevancePilot.train(dataset(), epochs: 1, hidden_units: 4)
    assert result.model_version == "memory-relevance-pilot-v1"
    assert result.train_examples == 2
    assert result.eval_examples == 1
    assert result.evaluated? == true
    assert result.split_mode == :session_holdout
    assert result.params != nil
    assert result.deployment_status == :experimental_not_for_runtime
    assert result.train_mse >= 0.0
    assert result.eval_mse >= 0.0
    assert result.baseline_eval_mse > 0.0
    assert result.labels.train_positive == 1
  end

  test "single-session smoke mode is explicitly not an evaluation" do
    base = dataset()
    data = %{
      base |
      eval: base.train,
      metadata: %{
        base.metadata |
        train_sessions: ["only-session"],
        eval_sessions: ["only-session"]
      } |> Map.put(:split_mode, :in_sample_smoke)
    }

    assert {:ok, result} = MemoryRelevancePilot.train(data, epochs: 1)
    assert result.evaluated? == false
    assert result.split_mode == :in_sample_smoke
    assert result.train_examples == 2
    assert result.eval_examples == 2
  end

  test "rejects wrong task, mismatched features and leaked sessions" do
    d = dataset()

    assert {:error, :incorrect_training_task} =
             MemoryRelevancePilot.train(put_in(d, [:metadata, :task], :uncertainty))

    assert {:error, :feature_or_label_mismatch} =
             MemoryRelevancePilot.train(put_in(d, [:train, :label_names], [:uncertainty]))

    assert {:error, :session_leakage} =
             MemoryRelevancePilot.train(put_in(d, [:metadata, :eval_sessions], ["train-a"]))

    assert {:error, :invalid_training_options} =
             MemoryRelevancePilot.train(d, epochs: 0)
  end

  test "saves a private artifact that contains weights but no conversation text" do
    assert {:ok, result} = MemoryRelevancePilot.train(dataset(), epochs: 1)

    dir = Path.join(System.tmp_dir!(), "symbrella_pilot_#{System.unique_integer([:positive])}")
    path = Path.join(dir, "pilot.etf")
    on_exit(fn -> File.rm_rf(dir) end)

    assert :ok == MemoryRelevancePilot.save(result, path)
    assert Bitwise.band(File.stat!(path).mode, 0o777) == 0o600

    artifact = path |> File.read!() |> :erlang.binary_to_term([:safe])
    assert artifact.model_version == result.model_version
    assert artifact.params != nil
    refute Map.has_key?(artifact, :user_text)
  end
end
