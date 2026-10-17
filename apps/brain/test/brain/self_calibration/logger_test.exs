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

  test "rotates when the configured cap would be exceeded", %{path: path} do
    sample = %Sample{source: :rotation_test, labels: %{confidence: 0.8}}
    assert :ok = Logger.log(sample, enabled?: true, path: path)
    first = File.read!(path)
    size = byte_size(first)

    assert :ok = Logger.log(sample,
             enabled?: true, path: path, max_bytes: size + 1, max_files: 2)

    assert File.read!(path <> ".1") == first
    assert File.read!(path) == first
    assert {:ok, _} = Jason.decode(String.trim(first))
    File.rm(path <> ".1")
  end

  test "rejects oversized records without corrupting the active log", %{path: path} do
    sample = %Sample{source: :oversized, raw: %{text: String.duplicate("a", 200)}}

    assert {:error, :sample_too_large} =
             Logger.log(sample, enabled?: true, path: path, max_record_bytes: 100)

    refute File.exists?(path)
  end

  test "concurrent writes keep complete JSONL rows", %{path: path} do
    sample = %Sample{source: :concurrency_test, labels: %{stability: 0.9}}

    results =
      1..12
      |> Task.async_stream(fn _ ->
        Logger.log(sample, enabled?: true, path: path, max_bytes: 100_000)
      end, max_concurrency: 6, timeout: 15_000)
      |> Enum.to_list()

    assert Enum.all?(results, &(&1 == {:ok, :ok}))

    rows = path |> File.read!() |> String.split("\n", trim: true)
    assert length(rows) == 12
    assert Enum.all?(rows, fn row -> match?({:ok, %{}}, Jason.decode(row)) end)
  end

  test "log is a no-op when disabled", %{path: path} do
    Application.put_env(:brain, Logger, enabled?: false, path: path)

    assert :ok = Logger.log(%Sample{})
    refute File.exists?(path)
  end

  test "log sanitizes non-json runtime values", %{path: path} do
    Application.put_env(:brain, Logger, enabled?: true, path: path)

    sample = %Sample{
      raw: %{
        pid: self(),
        ref: make_ref(),
        tuple: {:ok, :runtime},
        nested: [%{source: :brain}]
      }
    }

    assert :ok = Logger.log(sample)

    [line] = path |> File.read!() |> String.split("\n", trim: true)
    assert {:ok, decoded} = Jason.decode(line)

    assert is_binary(decoded["raw"]["pid"])
    assert is_binary(decoded["raw"]["ref"])
    assert decoded["raw"]["tuple"] == "{:ok, :runtime}"
    assert decoded["raw"]["nested"] == [%{"source" => "brain"}]
  end

  test "rejects invalid samples" do
    assert Logger.log(%{}) == {:error, :invalid_sample}
  end
end
