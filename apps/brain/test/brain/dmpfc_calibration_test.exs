defmodule Brain.DmPFCCalibrationTest do
  use ExUnit.Case, async: true
  alias Brain.DmPFC

  test "overconfident failure reduces confidence_scale and boosts acc_conflict_gain" do
    out = DmPFC.calibrate_confidence(%{
      predicted_confidence: 0.9,
      actual_outcome: 0.0,
      gain: 0.6
    })

    assert is_number(out.confidence_scale)
    assert is_number(out.acc_conflict_gain)

    assert out.confidence_scale < 1.0
    assert out.acc_conflict_gain >= 1.0
  end

  test "underconfident success increases confidence_scale" do
    out = DmPFC.calibrate_confidence(%{
      predicted_confidence: 0.3,
      actual_outcome: 1.0,
      gain: 0.4
    })

    assert out.confidence_scale > 1.0
    assert out.acc_conflict_gain >= 0.5 and out.acc_conflict_gain <= 1.5
  end

  test "clamps outputs within safe ranges even with extreme inputs" do
    out = DmPFC.calibrate_confidence(%{
      predicted_confidence: 99.0,
      actual_outcome: -5.0,
      gain: 2.0
    })

    assert out.confidence_scale >= 0.5 and out.confidence_scale <= 1.5
    assert out.acc_conflict_gain >= 0.5 and out.acc_conflict_gain <= 1.5
  end
end

