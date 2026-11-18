defmodule Brain.Prefrontal do
  @moduledoc """
  Pure aggregator for prefrontal planner recommendations.

  Brain.PFC remains the separate, supervised owner of persistent executive
  policy state. This module invokes existing stateless planners, validates
  their outputs through Brain.ControlSignals and reports their provenance.
  Computing a recommendation does not mean an action consumed or executed it.

  Planner order is deterministic. The last *valid, non-nil* opinion on a
  duplicated control key takes precedence. No process or WM mutations occur.
  """

  alias Brain.ControlSignals

  @planners [
    {Brain.DLPFC.Policy, :choose_policy},
    {Brain.VmPFC, :compute_utility},
    {Brain.FPC, :plan_branches},
    {Brain.DmPFC, :calibrate_confidence},
    {Brain.Salience, :salience_nudge}
  ]

  @doc "Return the combined, clamped canonical control-signal struct."
  @spec signals(map()) :: ControlSignals.t()
  def signals(ctx \\ %{}) when is_map(ctx) do
    ctx
    |> report()
    |> Map.fetch!(:signals)
    |> then(&struct!(ControlSignals, &1))
  end

  @doc "Always return a plain map of the validated control signals."
  @spec signals_map(map()) :: map()
  def signals_map(ctx \\ %{}) when is_map(ctx), do: report(ctx).signals

  @doc """
  Inspect each planner's output and keep only allowlisted, validated keys.

  The reported :accepted_keys denote which planner won the *merge*, not
  whether Core, the action selector or any downstream consumer used the value.
  Failures are represented as statuses rather than silently hidden.
  """
  @spec report(map()) :: map()
  def report(ctx \\ %{}) when is_map(ctx) do
    observations = Enum.map(@planners, fn {mod, fun} -> inspect_planner(mod, fun, ctx) end)
    parts = Enum.map(observations, & &1.values)

    signals =
      parts
      |> ControlSignals.combine()
      |> Map.from_struct()

    owners =
      Enum.reduce(observations, %{}, fn observation, acc ->
        Enum.reduce(Map.keys(observation.values), acc, fn key, owners ->
          Map.put(owners, key, observation.planner)
        end)
      end)

    planners =
      Enum.map(observations, fn observation ->
        proposed_keys = Map.keys(observation.values) |> Enum.sort()
        accepted_keys =
          Enum.filter(proposed_keys, fn key ->
            Map.get(owners, key) == observation.planner
          end)

        %{
          planner: observation.planner,
          function: observation.function,
          status: observation.status,
          reason: observation.reason,
          proposed_keys: proposed_keys,
          accepted_keys: accepted_keys,
          superseded_keys: proposed_keys -- accepted_keys
        }
      end)

    %{
      version: 1,
      signals: signals,
      provenance: owners,
      planners: planners
    }
  end

  @doc "Return individual validated planner outputs, without repeating calls."
  @spec planner_parts(map()) :: [map()]
  def planner_parts(ctx) when is_map(ctx) do
    @planners
    |> Enum.map(fn {mod, fun} -> inspect_planner(mod, fun, ctx).values end)
    |> Enum.reject(&(map_size(&1) == 0))
  end

  @doc "Compact dashboard status for the pure prefrontal aggregator."
  @spec status() :: map()
  def status do
    %{
      region: :prefrontal,
      status: :available,
      mode: :pure,
      function: :signals_map,
      children: [:dlpfc, :vmpfc, :dmpfc, :fpc, :ofc, :lifg, :salience]
    }
  end

  defp inspect_planner(mod, fun, ctx) do
    cond do
      not Code.ensure_loaded?(mod) ->
        observation(mod, fun, :unavailable, :module_not_loaded)

      not function_exported?(mod, fun, 1) ->
        observation(mod, fun, :unavailable, :function_not_exported)

      true ->
        try do
          result = apply(mod, fun, [ctx])
          values = ControlSignals.normalize_part(result)

          cond do
            map_size(values) > 0 ->
              observation(mod, fun, :produced, :validated, values)

            is_map(result) and map_size(result) == 0 ->
              observation(mod, fun, :no_opinion, :empty_result)

            true ->
              observation(mod, fun, :rejected, :no_valid_control_values)
          end
        rescue
          _ -> observation(mod, fun, :failed, :planner_exception)
        catch
          _, _ -> observation(mod, fun, :failed, :planner_failure)
        end
    end
  end

  defp observation(mod, fun, status, reason, values \\ %{}) do
    %{
      planner: mod,
      function: fun,
      status: status,
      reason: reason,
      values: values
    }
  end
end
