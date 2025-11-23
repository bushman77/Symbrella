defmodule Brain.HippocampusTelemetryTest do
  use ExUnit.Case, async: false
  alias Brain.Hippocampus

  setup do
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    Brain.Hippocampus.reset()
    :ok
  end

  defp slate_with(word), do: %{winners: [%{id: "#{word}|noun|0", lemma: word}]}

  defp flush_telemetry do
    receive do
      {:telemetry, _event, _meas, _meta} -> flush_telemetry()
      _ -> flush_telemetry()
    after
      0 -> :ok
    end
  end

  test "write emits telemetry with window_size" do
    handler_id = "hippo-write-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        handler_id,
        [:brain, :hippocampus, :write],
        fn event, meas, meta, _ ->
          send(parent, {:telemetry, event, meas, meta})
        end,
        nil
      )

    Hippocampus.encode(slate_with("hello"), %{source: :test})

    assert_receive {:telemetry, [:brain, :hippocampus, :write], meas, meta}, 100
    assert is_integer(meas.window_size) and meas.window_size >= 1
# Keep the source check but allow additional keys
assert %{meta: %{source: :test} = episode_meta} = meta

# Optionally assert that we get emotional context now
assert Map.has_key?(episode_meta, :emotion)
assert Map.has_key?(episode_meta, :latents)
assert Map.has_key?(episode_meta, :tone_reaction)

    :telemetry.detach(handler_id)
    flush_telemetry()
  end

  test "recall emits telemetry with counts and top_score" do
    handler_id = "hippo-recall-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        handler_id,
        [:brain, :hippocampus, :recall],
        fn event, meas, meta, _ ->
          send(parent, {:telemetry, event, meas, meta})
        end,
        nil
      )

    Hippocampus.encode(slate_with("alpha"), %{})
    Hippocampus.encode(slate_with("beta"), %{})
    _ = Hippocampus.recall(["alpha"], limit: 2)

    assert_receive {:telemetry, [:brain, :hippocampus, :recall], meas, meta}, 100
    assert meas.cue_count == 1
    assert meas.window_size >= 2
    assert meas.returned >= 1
    assert is_float(meas.top_score)
    assert meta.limit == 2
    assert is_integer(meta.half_life_ms) and meta.half_life_ms > 0

    :telemetry.detach(handler_id)
    flush_telemetry()
  end
end
