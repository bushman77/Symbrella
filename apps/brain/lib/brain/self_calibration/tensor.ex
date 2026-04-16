defmodule Brain.SelfCalibration.Tensor do
  @moduledoc """
  Converts self-calibration dataset rows into Nx tensors.

  This module is intentionally small: it is the boundary between the symbolic
  calibration dataset and the future Axon model.
  """

  @type tensor_batch :: %{
          x: term(),
          y: term(),
          feature_names: [atom()],
          label_names: [atom()]
        }

  @spec from_rows(map()) :: {:ok, tensor_batch()} | {:error, term()}
  def from_rows(%{x: x_rows, y: y_rows, feature_names: feature_names, label_names: label_names})
      when is_list(x_rows) and is_list(y_rows) and is_list(feature_names) and is_list(label_names) do
    cond do
      length(x_rows) != length(y_rows) ->
        {:error, :row_count_mismatch}

      x_rows == [] ->
        {:error, :empty_rows}

      not rectangular?(x_rows) ->
        {:error, :invalid_feature_rows}

      not rectangular?(y_rows) ->
        {:error, :invalid_label_rows}

      not nx_available?() ->
        {:error, :nx_unavailable}

      true ->
        {:ok,
         %{
           x: apply(Nx, :tensor, [x_rows, [type: :f32]]),
           y: apply(Nx, :tensor, [y_rows, [type: :f32]]),
           feature_names: feature_names,
           label_names: label_names
         }}
    end
  end

  def from_rows(_rows), do: {:error, :invalid_rows}

  def nx_available? do
    Code.ensure_loaded?(Nx) and function_exported?(Nx, :tensor, 2)
  end

  defp rectangular?([first | rest]) when is_list(first) do
    width = length(first)

    width > 0 and
      Enum.all?(rest, fn row ->
        is_list(row) and length(row) == width
      end)
  end

  defp rectangular?(_), do: false
end
