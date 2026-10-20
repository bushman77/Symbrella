defmodule Brain.Visual.CortexTest do
  use ExUnit.Case, async: false

  alias Brain.Visual.Cortex

  test "extracts real grayscale brightness and pixel change without inventing meanings" do
    telemetry_id = "visual-cortex-features-#{System.unique_integer([:positive])}"
    curiosity_id = "visual-cortex-no-curiosity-#{System.unique_integer([:positive])}"

    :ok = :telemetry.attach(telemetry_id, [:brain, :visual, :features],
      fn _, measurements, features, pid ->
        send(pid, {:features, measurements, features})
      end, self())

    :ok = :telemetry.attach(curiosity_id, [:curiosity, :proposal],
      fn _, _, metadata, pid -> send(pid, {:proposal, metadata}) end, self())

    on_exit(fn ->
      :telemetry.detach(telemetry_id)
      :telemetry.detach(curiosity_id)
    end)

    first = :binary.copy(<<0>>, 64)
    second = :binary.copy(<<255>>, 64)
    {:ok, frames} = start_supervised({Agent, fn -> [first, second] end})

    sampler = fn "priv/latest.jpg" ->
      Agent.get_and_update(frames, fn
        [pixels | remaining] -> {{:ok, pixels}, remaining}
        [] -> {{:error, :no_more_frames}, []}
      end)
    end

    {:ok, cortex} = start_supervised({Cortex, name: nil, sampler: sampler})

    observation = %{
      kind: :visual_observation,
      source: :camera,
      decoder: :none,
      decoded?: false,
      description: nil,
      frame_path: "priv/latest.jpg",
      observed_at_ms: 100
    }

    :telemetry.execute([:brain, :visual_observation], %{count: 1}, observation)
    assert_receive {:features, %{count: 1}, first_features}, 500
    assert first_features.kind == :visual_features
    assert first_features.method == :grayscale_8x8
    assert first_features.mean_luminance == 0.0
    assert first_features.quadrants.top_left == 0.0
    assert first_features.change_fraction == nil
    assert first_features.description == nil
    assert first_features.labels == []
    assert first_features.decoded? == false
    assert first_features.semantic_status == :uninterpreted

    :telemetry.execute([:brain, :visual_observation], %{count: 2}, observation)
    assert_receive {:features, %{count: 2}, second_features}, 500
    assert second_features.mean_luminance == 1.0
    assert second_features.quadrants.bottom_right == 1.0
    assert second_features.change_fraction == 1.0
    assert second_features.change_basis == :pixel_difference
    assert Cortex.latest(cortex) == second_features
    assert Cortex.status(cortex).count == 2

    refute_receive {:proposal, %{source: :camera}}, 50
  end

  test "does not reprocess decoded or noncamera observations" do
    sampler = fn _path -> flunk("sampler must not inspect decoded observations") end
    {:ok, cortex} = start_supervised({Cortex, name: nil, sampler: sampler})

    for observation <- [
      %{kind: :visual_observation, source: :camera, decoder: :debug_stub,
        decoded?: true, frame_path: "priv/latest.jpg"},
      %{kind: :visual_observation, source: :other, decoder: :none,
        decoded?: false, frame_path: "priv/latest.jpg"},
      %{kind: :visual_features, source: :camera, decoder: :none,
        decoded?: false, frame_path: "priv/latest.jpg"}
    ] do
      :telemetry.execute([:brain, :visual_observation], %{}, observation)
    end

    # Force a mailbox barrier after all prior telemetry messages have arrived.
    assert Cortex.status(cortex).count == 0
    assert Cortex.latest(cortex) == nil
  end

  test "sampling errors do not claim visual understanding or crash the cortex" do
    error_id = "visual-cortex-errors-#{System.unique_integer([:positive])}"

    :ok = :telemetry.attach(error_id, [:brain, :visual, :error],
      fn _, _, metadata, pid -> send(pid, {:visual_error, metadata}) end, self())

    on_exit(fn -> :telemetry.detach(error_id) end)

    {:ok, cortex} = start_supervised({Cortex, name: nil,
      sampler: fn _ -> {:error, :frame_unavailable} end})

    observation = %{
      kind: :visual_observation,
      source: :camera,
      decoder: :none,
      decoded?: false,
      frame_path: "priv/latest.jpg"
    }

    :telemetry.execute([:brain, :visual_observation], %{}, observation)

    assert_receive {:visual_error, %{reason: :frame_unavailable}}, 500
    assert Cortex.status(cortex).failure_count == 1
    assert Cortex.latest(cortex) == nil
    assert Process.alive?(cortex)
  end
end
