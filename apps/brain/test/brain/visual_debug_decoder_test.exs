defmodule Brain.Visual.DebugDecoderTest do
  use ExUnit.Case, async: false

  alias Brain.Visual.DebugDecoder

  test "debug-decodes placeholder camera observations and republishes proposals" do
    visual_id = "visual-debug-decoder-visual-#{System.unique_integer([:positive])}"
    curiosity_id = "visual-debug-decoder-curiosity-#{System.unique_integer([:positive])}"

    :ok =
      :telemetry.attach(
        visual_id,
        [:brain, :visual_observation],
        fn _event, measurements, metadata, pid ->
          send(pid, {:visual_observation, measurements, metadata})
        end,
        self()
      )

    :ok =
      :telemetry.attach(
        curiosity_id,
        [:curiosity, :proposal],
        fn _event, measurements, metadata, pid ->
          send(pid, {:curiosity_proposal, measurements, metadata})
        end,
        self()
      )

    on_exit(fn ->
      :telemetry.detach(visual_id)
      :telemetry.detach(curiosity_id)
    end)

    {:ok, decoder} = start_supervised({DebugDecoder, name: nil})

    observation = %{
      kind: :visual_observation,
      source: :camera,
      decoder: :none,
      decoded?: false,
      description: nil,
      labels: [],
      confidence: nil,
      frame_path: "/tmp/symbrella_camera/latest.jpg",
      observed_at_ms: 1_780_173_422_354,
      novelty: 0.25,
      risk: 0.0,
      score: 0.35
    }

    :telemetry.execute(
      [:brain, :visual_observation],
      %{count: 1, score: 0.35, novelty: 0.25, risk: 0.0},
      observation
    )

    assert_receive {:visual_observation, %{decoded: 1, score: score}, decoded}, 500
    assert score >= 0.4
    assert decoded.decoded? == true
    assert decoded.decoder == :debug_stub
    assert decoded.description == "Camera frame available."
    assert decoded.labels == ["camera_frame"]
    assert decoded.confidence == 0.1
    assert decoded.frame_path == observation.frame_path

    assert_receive {:curiosity_proposal, %{score: proposal_score, risk: risk}, metadata}, 500
    assert proposal_score == decoded.score
    assert risk == 0.0
    assert metadata.probe.reason == :decoded_visual_observation
    assert metadata.probe.seed.decoder == :debug_stub
    assert metadata.visual_observation == decoded

    assert DebugDecoder.latest(decoder) == decoded
  end

  test "ignores observations that are already decoded" do
    visual_id = "visual-debug-decoder-ignore-#{System.unique_integer([:positive])}"

    :ok =
      :telemetry.attach(
        visual_id,
        [:brain, :visual_observation],
        fn _event, measurements, metadata, pid ->
          send(pid, {:visual_observation, measurements, metadata})
        end,
        self()
      )

    on_exit(fn -> :telemetry.detach(visual_id) end)

    {:ok, decoder} = start_supervised({DebugDecoder, name: nil})

    decoded = %{
      kind: :visual_observation,
      source: :camera,
      decoder: :debug_stub,
      decoded?: true,
      description: "Camera frame available.",
      labels: ["camera_frame"],
      confidence: 0.1,
      frame_path: "/tmp/symbrella_camera/latest.jpg",
      observed_at_ms: 1_780_173_422_354,
      novelty: 0.25,
      risk: 0.0,
      score: 0.4
    }

    :telemetry.execute(
      [:brain, :visual_observation],
      %{count: 1, score: 0.4, novelty: 0.25, risk: 0.0, decoded: 1},
      decoded
    )

    assert_receive {:visual_observation, %{decoded: 1}, ^decoded}, 500
    refute_receive {:visual_observation, %{decoded: 1}, _redoded}, 100
    assert DebugDecoder.latest(decoder) == nil
  end
end
