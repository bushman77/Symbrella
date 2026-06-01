defmodule Brain.Camera.ObservationBridgeTest do
  use ExUnit.Case, async: false

  alias Brain.Camera.ObservationBridge

  test "bridges camera frames into visual observations and curiosity proposals" do
    visual_id = "camera-observation-visual-#{System.unique_integer([:positive])}"
    curiosity_id = "camera-observation-curiosity-#{System.unique_integer([:positive])}"

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

    {:ok, bridge} = start_supervised({ObservationBridge, name: nil})

    frame = %{
      kind: :camera_frame,
      path: "/tmp/symbrella_camera/latest.jpg",
      size_bytes: 42_641,
      captured_at_ms: 1_780_173_422_354,
      duration_ms: 5_014,
      source: %{type: :mjpeg_url, url: "http://192.168.1.138:4747/video?640x480"}
    }

    :telemetry.execute(
      [:brain, :camera, :frame],
      %{count: 1, duration_ms: 5_014, size_bytes: 42_641},
      frame
    )

    assert_receive {:visual_observation, %{count: 1, score: score}, observation}, 500
    assert score > 0.0
    assert observation.kind == :visual_observation
    assert observation.source == :camera
    assert observation.decoded? == false
    assert observation.description == nil
    assert observation.labels == []
    assert observation.frame_path == "/tmp/symbrella_camera/latest.jpg"
    assert observation.frame == frame

    assert_receive {:curiosity_proposal, %{score: proposal_score, risk: risk}, metadata}, 500
    assert proposal_score == observation.score
    assert risk == 0.0
    assert metadata.source == :camera
    assert metadata.probe.source == :camera
    assert metadata.probe.reason == :visual_observation
    assert metadata.visual_observation == observation

    assert ObservationBridge.latest(bridge) == observation
  end
end
