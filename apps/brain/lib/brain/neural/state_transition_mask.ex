defmodule Brain.Neural.StateTransitionMask do
  @moduledoc """
  Common *observed-only* objective and evaluation for cognitive-state Axon pilots.

  Loss and MSE use a float32 mask of the same shape as the state vector.
  A value contributes only when it was explicitly observed at both endpoints
  of a state transition. Presence/availability indicators are **inputs only**
  and never prediction targets. No missing reading is imputed.

  This mask is separate from the input vector, allowing the model to see an
  observation's availability without being rewarded for predicting logging gaps.
  """

  @doc "Masked elementwise MSE for Axon.Loop.trainer/3, target map first."
  def loss(%{values: truth, mask: mask}, prediction) do
    difference = Nx.subtract(prediction, truth)
    weighted = Nx.multiply(Nx.pow(difference, 2), mask)

    # A completely empty batch must be rejected by the dataset validator.
    # Keep the denominator safe for compilation and for partial mini-batches.
    Nx.divide(Nx.sum(weighted), Nx.max(Nx.sum(mask), Nx.tensor(1.0)))
  end

  @doc "No mutation of the original model inputs or the observed target."
  def targets(values, mask), do: %{values: values, mask: mask}

  @doc """
  Calculate model/persistence MSE over exactly the SAME observed feature cells.

  Returns :not_observed (not 0) for a feature with no valid transitions.
  Counts are numbers of contributing transition pairs, not conversation turns.
  """
  def metrics(names, predicted, current, observed, mask) do
    model_squared = Nx.pow(Nx.subtract(predicted, observed), 2) |> Nx.multiply(mask)
    baseline_squared = Nx.pow(Nx.subtract(current, observed), 2) |> Nx.multiply(mask)

    counts = mask |> Nx.sum(axes: [0]) |> Nx.to_list()
    model_sums = model_squared |> Nx.sum(axes: [0]) |> Nx.to_list()
    baseline_sums = baseline_squared |> Nx.sum(axes: [0]) |> Nx.to_list()

    breakdown =
      names
      |> Enum.zip(Enum.zip(counts, Enum.zip(model_sums, baseline_sums)))
      |> Enum.map(fn {name, {count, {model_sum, baseline_sum}}} ->
        if count > 0 do
          model_mse = model_sum / count
          baseline_mse = baseline_sum / count

          %{
            feature: name,
            valid_pairs: round(count),
            model_mse: model_mse,
            persistence_mse: baseline_mse,
            improvement: baseline_mse - model_mse
          }
        else
          %{
            feature: name,
            valid_pairs: 0,
            model_mse: :not_observed,
            persistence_mse: :not_observed,
            improvement: :not_observed
          }
        end
      end)

    valid = mask |> Nx.sum() |> Nx.to_number()

    if valid > 0 do
      %{
        model_mse: model_squared |> Nx.sum() |> Nx.to_number() |> Kernel./(valid),
        persistence_mse: baseline_squared |> Nx.sum() |> Nx.to_number() |> Kernel./(valid),
        valid_cells: round(valid),
        per_feature: breakdown
      }
    else
      %{
        model_mse: :not_observed,
        persistence_mse: :not_observed,
        valid_cells: 0,
        per_feature: breakdown
      }
    end
  end

  @doc "Verify numeric masks are nonempty, finite binary indicators with matching shapes."
  def validate_batch(batch, names) do
    shape = Nx.shape(batch.x)
    width = length(names)

    cond do
      batch[:feature_names] != names ->
        {:error, :feature_schema_mismatch}

      not match?({n, ^width} when n > 0, shape) ->
        {:error, :invalid_feature_tensor_shape}

      Nx.shape(batch.y) != shape or
          not is_map_key(batch, :mask) or Nx.shape(batch.mask) != shape ->
        {:error, :invalid_target_or_mask_shape}

      not binary_mask?(batch.mask) ->
        {:error, :invalid_observation_mask}

      Nx.sum(batch.mask) |> Nx.to_number() |> Kernel.==(0) ->
        {:error, :no_observed_training_targets}

      true ->
        :ok
    end
  end

  defp binary_mask?(tensor) do
    tensor
    |> Nx.to_flat_list()
    |> Enum.all?(&(&1 in [0, 0.0, 1, 1.0]))
  end
end
