# apps/brain/test/brain/thalamus_params_test.exs
defmodule Brain.ThalamusParams_Test do
  use ExUnit.Case, async: false

  @moduledoc """
  Contract tests for `Brain.Thalamus.get_params/1`.

  We assert that `get_params/1` exposes the full parameter set and that
  the **config-defaults path** agrees with the `:brain` application
  environment (including mood fields).
  """

  test "config-defaults path reflects Application env (including mood fields)" do
    # Use a sentinel atom so we always hit the config_defaults() path,
    # regardless of whatever runtime overrides other tests may have applied.
    params = Brain.Thalamus.get_params(:__config_defaults__)

    expected_ofc =
      Application.get_env(:brain, :thalamus_ofc_weight, 0.5)
      |> to_float()

    expected_acc =
      Application.get_env(:brain, :thalamus_acc_alpha, 0.35)
      |> to_float()

    expected_cap =
      Application.get_env(:brain, :thalamus_mood_cap, 0.15)
      |> to_float()

    expected_weights =
      Application.get_env(:brain, :thalamus_mood_weights, %{
        expl: 0.05,
        inhib: -0.07,
        vigil: -0.03,
        plast: 0.04
      })

    # Shape: we expect all four fields to be present
    assert Map.has_key?(params, :ofc_weight)
    assert Map.has_key?(params, :acc_alpha)
    assert Map.has_key?(params, :mood_cap)
    assert Map.has_key?(params, :mood_weights)

    # Values match the config defaults (after basic numeric normalization)
    assert_in_delta expected_ofc, params.ofc_weight, 1.0e-6
    assert_in_delta expected_acc, params.acc_alpha, 1.0e-6
    assert_in_delta expected_cap, params.mood_cap, 1.0e-6
    assert params.mood_weights == expected_weights
  end

  defp to_float(v) when is_integer(v), do: v * 1.0
  defp to_float(v) when is_float(v), do: v
  defp to_float(_), do: 0.0
end
