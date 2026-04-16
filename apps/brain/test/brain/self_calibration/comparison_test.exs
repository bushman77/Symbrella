defmodule Brain.SelfCalibration.ComparisonTest do
  use ExUnit.Case, async: true

  alias Brain.SelfCalibration.Comparison
  alias Brain.SelfCalibration.Evaluation

  test "selects candidate when candidate has lower mae" do
    baseline = evaluation(:baseline, "baseline-v1", 0.12)
    candidate = evaluation(:tiny_model, "tiny-linear-v1", 0.09)

    assert {:ok, comparison} = Comparison.compare(baseline, candidate)

    assert comparison.baseline_mae == 0.12
    assert comparison.candidate_mae == 0.09
    assert Float.round(comparison.delta, 4) == -0.03
    assert comparison.winner == :candidate
    assert comparison.reason == :lower_mae
    assert comparison.advisory? == true
    assert comparison.baseline_source == :baseline
    assert comparison.candidate_source == :tiny_model
  end

  test "selects baseline when baseline has lower mae" do
    baseline = evaluation(:baseline, "baseline-v1", 0.05)
    candidate = evaluation(:tiny_model, "tiny-linear-v1", 0.09)

    assert {:ok, comparison} = Comparison.compare(baseline, candidate)

    assert Float.round(comparison.delta, 4) == 0.04
    assert comparison.winner == :baseline
    assert comparison.reason == :lower_mae
  end

  test "returns tie within tolerance" do
    baseline = evaluation(:baseline, "baseline-v1", 0.05)
    candidate = evaluation(:tiny_model, "tiny-linear-v1", 0.051)

    assert {:ok, comparison} = Comparison.compare(baseline, candidate, tolerance: 0.01)

    assert comparison.winner == :tie
    assert comparison.reason == :equal_mae
    assert comparison.meta.tolerance == 0.01
  end

  test "rejects invalid inputs" do
    assert Comparison.compare(%{}, %Evaluation{}) == {:error, :invalid_comparison_input}
  end

  defp evaluation(source, model_version, mae) do
    %Evaluation{
      confidence_error: mae,
      uncertainty_error: mae,
      stability_error: mae,
      mae: mae,
      source: source,
      model_version: model_version,
      feature_schema_v: 1
    }
  end
end
