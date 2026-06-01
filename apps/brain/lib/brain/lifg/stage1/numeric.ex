defmodule Brain.LIFG.Stage1.Numeric do
  @moduledoc """
  Numeric kernels for LIFG Stage1.

  This module is the boundary between symbolic candidate construction and
  tensor math. Callers pass plain feature maps in and receive plain floats back.
  """

  @feature_names [:lex_fit, :rel_prior, :activation, :intent_bias]

  @type feature_row :: %{
          optional(:lex_fit) => number(),
          optional(:rel_prior) => number(),
          optional(:activation) => number(),
          optional(:intent_bias) => number()
        }

  @doc "Returns the stable feature order used by the Stage1 weighted scorer."
  @spec feature_names() :: [atom()]
  def feature_names, do: @feature_names

  @doc """
  Scores feature rows using the configured Stage1 weights.

  Uses Nx when available unless `mode: :scalar` is passed or
  `config :brain, :lifg_stage1_numeric_mode, :scalar` is set.
  """
  @spec weighted_scores([feature_row()], map() | keyword(), keyword()) :: [float()]
  def weighted_scores(rows, weights, opts \\ []) when is_list(rows) do
    mode = Keyword.get(opts, :mode, Application.get_env(:brain, :lifg_stage1_numeric_mode, :nx))

    if mode == :scalar or not nx_available?() do
      scalar_weighted_scores(rows, weights)
    else
      nx_weighted_scores(rows, weights)
    end
  end

  @doc "Stable softmax. Returns a uniform distribution for degenerate input."
  @spec softmax([number()], keyword()) :: [float()]
  def softmax(xs, opts \\ [])
  def softmax([], _opts), do: []

  def softmax(xs, opts) when is_list(xs) do
    mode = Keyword.get(opts, :mode, Application.get_env(:brain, :lifg_stage1_numeric_mode, :nx))

    if mode == :scalar or not nx_available?() do
      scalar_softmax(xs)
    else
      nx_softmax(xs)
    end
  end

  @doc "True when Nx tensor functions used by this module are available."
  @spec nx_available?() :: boolean()
  def nx_available? do
    Code.ensure_loaded?(Nx) and
      function_exported?(Nx, :tensor, 2) and
      function_exported?(Nx, :to_flat_list, 1)
  end

  defp nx_weighted_scores(rows, weights) do
    x =
      rows
      |> Enum.map(fn row ->
        Enum.map(@feature_names, fn feature ->
          row
          |> get_num(feature, 0.0)
          |> clamp01()
        end)
      end)
      |> Nx.tensor(type: :f32)

    w =
      @feature_names
      |> Enum.map(&get_num(weights, &1, 0.0))
      |> Nx.tensor(type: :f32)

    x
    |> Nx.multiply(w)
    |> Nx.sum(axes: [1])
    |> Nx.clip(0.0, 1.0)
    |> Nx.to_flat_list()
    |> Enum.map(&(&1 * 1.0))
  rescue
    _ -> scalar_weighted_scores(rows, weights)
  catch
    _, _ -> scalar_weighted_scores(rows, weights)
  end

  defp scalar_weighted_scores(rows, weights) do
    Enum.map(rows, fn row ->
      @feature_names
      |> Enum.reduce(0.0, fn feature, acc ->
        acc + get_num(weights, feature, 0.0) * clamp01(get_num(row, feature, 0.0))
      end)
      |> clamp01()
    end)
  end

  defp nx_softmax(xs) do
    t = xs |> Enum.map(&to_float/1) |> Nx.tensor(type: :f32)
    shifted = Nx.subtract(t, Nx.reduce_max(t))
    exps = Nx.exp(shifted)
    denom = Nx.sum(exps) |> Nx.to_number()

    if denom == 0.0 do
      uniform(length(xs))
    else
      exps
      |> Nx.divide(denom)
      |> Nx.to_flat_list()
      |> Enum.map(&(&1 * 1.0))
    end
  rescue
    _ -> scalar_softmax(xs)
  catch
    _, _ -> scalar_softmax(xs)
  end

  defp scalar_softmax(xs) do
    xs = Enum.map(xs, &to_float/1)
    m = Enum.max(xs)
    exps = Enum.map(xs, fn x -> :math.exp(x - m) end)
    denom = Enum.sum(exps)

    if denom == 0.0 do
      uniform(length(xs))
    else
      Enum.map(exps, &(&1 / denom))
    end
  end

  defp uniform(0), do: []

  defp uniform(n) do
    u = 1.0 / n
    Enum.map(1..n, fn _ -> u end)
  end

  defp get_num(map, key, default) when is_map(map) do
    case Map.get(map, key, Map.get(map, to_string(key), default)) do
      v when is_number(v) -> v * 1.0
      _ -> default * 1.0
    end
  end

  defp get_num(list, key, default) when is_list(list) do
    case Keyword.get(list, key, default) do
      v when is_number(v) -> v * 1.0
      _ -> default * 1.0
    end
  end

  defp get_num(_, _, default), do: default * 1.0

  defp clamp01(v) when is_number(v), do: max(0.0, min(1.0, v * 1.0))
  defp clamp01(_), do: 0.0

  defp to_float(x) when is_float(x), do: x
  defp to_float(x) when is_integer(x), do: x * 1.0
  defp to_float(_), do: 0.0
end
