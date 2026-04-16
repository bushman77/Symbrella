defmodule Brain.SelfCalibration.LoggerTest do
  use ExUnit.Case, async: false

  alias Brain.SelfCalibration.Logger
  alias Brain.SelfCalibration.Sample

  setup do
    old_cfg = Application.get_env(:brain, Logger)

    path =
      Path.join(
        System.tmp_dir!(),
        "symbrella-self-calibration-#{System.unique_integer([:positive])}.jsonl"
      )

    on_exit(fn ->
      if old_cfg do
        Application.put_env(:brain, Logger, old_cfg)
      else
        Application.delete_env(:brain, Logger)
      end

      File.rm(path)
    end)

    %{path: path}
  end

  test "log appends a JSONL sample and emits telemetry", %{path: path} do
    Application.put_env(:brain, Logger, enabled?: true, path: path)

    id = "self-calibration-logger-test-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        id,
        [:brain, :self_calibration, :sample_logged],
        fn _event, measurements, metadata, pid ->
          send(pid, {:sample_logged, measurements, metadata})
        end,
        parent
      )

    on_exit(fn -> :telemetry.detach(id) end)

    sample = %Sample{
      features: %{appraisal_valence: -0.2},
      labels: %{confidence: 0.7},
      source: :test,
      meta: %{feature_schema_v: 1},
      v: 1
    }

    assert :ok = Logger.log(sample)

    assert_receive {:sample_logged, %{count: 1}, meta}, 500
    assert meta.source == :test
    assert meta.v == 1
    assert meta.feature_schema_v == 1

    [line] = path |> File.read!() |> String.split("\n", trim: true)
    assert {:ok, decoded} = Jason.decode(line)
    assert decoded["source"] == "test"
    assert decoded["features"]["appraisal_valence"] == -0.2
    assert decoded["labels"]["confidence"] == 0.7
    assert decoded["meta"]["feature_schema_v"] == 1
  end

  test "log is a no-op when disabled", %{path: path} do
    Application.put_env(:brain, Logger, enabled?: false, path: path)

    assert :ok = Logger.log(%Sample{})
    refute File.exists?(path)
  end

  test "rejects invalid samples" do
    assert Logger.log(%{}) == {:error, :invalid_sample}
  end
end
