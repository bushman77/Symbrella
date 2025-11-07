defmodule Brain.Prefrontal do
  @moduledoc """
  Pure aggregator for prefrontal planners.

  It gathers partial control knobs from the *pure* planners and returns a unified
  control-signals structure (or a plain map). No processes, no timers—safe to call
  inside LIFG Stage-1 or anywhere in the decision path.

  Planners consulted (if loaded & exported):
    • Brain.DLPFC.Policy.choose_policy/1
    • Brain.VmPFC.compute_utility/1
    • Brain.FPC.plan_branches/1
    • Brain.DmPFC.calibrate_confidence/1
    • Brain.Salience.salience_nudge/1

  If `Brain.Utils.ControlSignals.combine/1` exists, it is used for merging/clamping.
  Otherwise this module falls back to a small, local right-biased merge + clamp.
  """

  @planners [
    {Brain.DLPFC.Policy, :choose_policy},
    {Brain.VmPFC, :compute_utility},
    {Brain.FPC, :plan_branches},
    {Brain.DmPFC, :calibrate_confidence},
    {Brain.Salience, :salience_nudge}
  ]

  @doc """
  Return a unified control-signals *struct* when `Brain.Utils.ControlSignals` is available,
  otherwise a plain map (clamped). Safe even if some planners aren’t compiled yet.
  """
  @spec signals(map()) :: map() | struct()
  def signals(ctx \\ %{}) when is_map(ctx) do
    parts = planner_parts(ctx)
    combine(parts)
  end

  @doc """
  Always return a *plain map* of control signals (structs are converted).
  """
  @spec signals_map(map()) :: map()
  def signals_map(ctx \\ %{}) do
    case signals(ctx) do
      %_{} = struct -> Map.from_struct(struct)
      %{} = map -> map
      other -> %{} |> Map.merge(normalize_kv(other))
    end
  end

  @doc """
  Return the individual planner outputs (as a list of maps) for debugging/telemetry correlation.
  """
  @spec planner_parts(map()) :: [map()]
  def planner_parts(ctx) when is_map(ctx) do
    Enum.reduce(@planners, [], fn {mod, fun}, acc ->
      part =
        if Code.ensure_loaded?(mod) and function_exported?(mod, fun, 1) do
          safe_apply(mod, fun, [ctx])
        else
          %{}
        end

      case part do
        %_{} = s -> [Map.from_struct(s) | acc]
        %{} = m -> [m | acc]
        _ -> acc
      end
    end)
    |> Enum.reverse()
  end

  # ───────────────────────── internal helpers ─────────────────────────

  defp safe_apply(mod, fun, args) do
    try do
      apply(mod, fun, args)
    rescue
      _ -> %{}
    catch
      _, _ -> %{}
    end
  end

  # Prefer external combiner if present; otherwise use local fallback.
  defp combine(parts) when is_list(parts) do
    if Code.ensure_loaded?(Brain.Utils.ControlSignals) and
         function_exported?(Brain.Utils.ControlSignals, :combine, 1) do
      apply(Brain.Utils.ControlSignals, :combine, [parts])
    else
      parts
      |> right_biased_merge()
      |> clamp_local()
    end
  end

  defp right_biased_merge(list) do
    Enum.reduce(list, %{}, fn
      # match any struct without binding the module var (avoids "unused variable" warning)
      %{__struct__: _} = s, acc -> Map.merge(acc, Map.from_struct(s))
      %{} = m, acc -> Map.merge(acc, m)
      kvs, acc when is_list(kvs) -> Map.merge(acc, normalize_kv(kvs))
      _, acc -> acc
    end)
  end

  defp normalize_kv(list) when is_list(list), do: Map.new(list)
  defp normalize_kv(_), do: %{}

  # Local clamp mirrors the common fields used by the planners.
  defp clamp_local(map) do
    map
    |> clamp01(:utility_prior)
    |> clamp01(:explore_rate)
    |> clamp_range(:confidence_scale, 0.5, 1.5)
    |> clamp_range(:acc_conflict_gain, 0.5, 1.5)
    |> clamp01(:salience_boost)
    |> clamp_int(:top_k, 1, 10)
    |> clamp_int(:max_retries, 1, 5)
    |> clamp_int(:branch_budget, 0, 5)
    |> clamp_int(:switch_after_ms, 0, 5_000)
  end

  defp clamp01(map, key) do
    case Map.fetch(map, key) do
      {:ok, v} when is_number(v) -> Map.put(map, key, min(1.0, max(0.0, v)))
      _ -> map
    end
  end

  defp clamp_range(map, key, lo, hi) do
    case Map.fetch(map, key) do
      {:ok, v} when is_number(v) -> Map.put(map, key, max(lo, min(hi, v)))
      _ -> map
    end
  end

  defp clamp_int(map, key, lo, hi) do
    case Map.fetch(map, key) do
      {:ok, v} when is_integer(v) -> Map.put(map, key, min(hi, max(lo, v)))
      _ -> map
    end
  end
end

