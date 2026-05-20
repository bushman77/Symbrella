defmodule Brain.MetaMonitor do
  @moduledoc """
  Phase 8 meta-monitoring policy for explicit self-state warnings.

  This module consumes the canonical `Brain.SelfModel` and reports inspectable
  warning states. It does not mutate the self-model or make recovery decisions.
  """

  alias Brain.SelfModel

  @event [:brain, :meta_monitor, :warning]
  @v 1

  @default_thresholds %{
    uncertainty: 0.75,
    stability: 0.25,
    cognitive_load: 0.85,
    recent_errors: 1,
    contradiction_delta: 0.65,
    stuck_loop_repeats: 3
  }

  @type warning_kind ::
          :high_uncertainty
          | :low_stability
          | :high_cognitive_load
          | :recent_errors
          | :contradiction
          | :stuck_loop

  @type warning :: %{
          required(:kind) => warning_kind(),
          required(:severity) => :info | :warning | :critical,
          required(:value) => number(),
          required(:threshold) => number()
        }

  @doc """
  Detect meta-monitor warnings without emitting telemetry.
  """
  @spec warnings(SelfModel.t(), keyword()) :: [warning()]
  def warnings(%SelfModel{} = model, opts \\ []) do
    thresholds = thresholds(opts)

    []
    |> maybe_warn(
      contradiction?(model, thresholds),
      :contradiction,
      abs(number(model.confidence) - number(model.uncertainty)),
      thresholds.contradiction_delta,
      :warning
    )
    |> maybe_warn(
      model.uncertainty >= thresholds.uncertainty,
      :high_uncertainty,
      model.uncertainty,
      thresholds.uncertainty,
      severity_high(model.uncertainty, 0.9)
    )
    |> maybe_warn(
      model.stability <= thresholds.stability,
      :low_stability,
      model.stability,
      thresholds.stability,
      severity_low(model.stability, 0.15)
    )
    |> maybe_warn(
      model.cognitive_load >= thresholds.cognitive_load,
      :high_cognitive_load,
      model.cognitive_load,
      thresholds.cognitive_load,
      severity_high(model.cognitive_load, 0.95)
    )
    |> maybe_warn(
      recent_error_count(model) >= thresholds.recent_errors,
      :recent_errors,
      recent_error_count(model),
      thresholds.recent_errors,
      :warning
    )
    |> maybe_warn(
      stuck_loop?(model, thresholds),
      :stuck_loop,
      repeated_action_count(model),
      thresholds.stuck_loop_repeats,
      :warning
    )
    |> Enum.reverse()
  end

  @doc """
  Returns bounded repair suggestions for warning states.
  """
  @spec recovery_suggestions([warning()] | SelfModel.t()) :: [atom()]
  def recovery_suggestions(%SelfModel{} = model),
    do: model |> warnings() |> recovery_suggestions()

  def recovery_suggestions(warnings) when is_list(warnings) do
    warnings
    |> Enum.flat_map(fn
      %{kind: :high_uncertainty} -> [:ask_clarifying_question]
      %{kind: :low_stability} -> [:slow_down, :prefer_evidence]
      %{kind: :high_cognitive_load} -> [:reduce_scope, :defer_memory_writes]
      %{kind: :recent_errors} -> [:run_self_check]
      %{kind: :contradiction} -> [:surface_uncertainty, :prefer_evidence]
      %{kind: :stuck_loop} -> [:change_strategy, :ask_for_target]
      _ -> []
    end)
    |> Enum.uniq()
  end

  @doc """
  Detect warnings and emit one telemetry event when warnings are present.

  Returns `{:ok, warnings}` when no warning exists and `{:warning, warnings}`
  when one or more warning states are detected.
  """
  @spec check(SelfModel.t(), keyword()) :: {:ok | :warning, [warning()]}
  def check(%SelfModel{} = model, opts \\ []) do
    warnings = warnings(model, opts)

    if warnings == [] do
      {:ok, []}
    else
      emit(warnings, model)
      {:warning, warnings}
    end
  end

  defp maybe_warn(acc, false, _kind, _value, _threshold, _severity), do: acc

  defp maybe_warn(acc, true, kind, value, threshold, severity) do
    [
      %{
        kind: kind,
        severity: severity,
        value: number(value),
        threshold: number(threshold)
      }
      | acc
    ]
  end

  defp thresholds(opts) do
    user = Keyword.get(opts, :thresholds, %{})

    @default_thresholds
    |> Map.merge(Map.new(user))
    |> Map.update!(:uncertainty, &number/1)
    |> Map.update!(:stability, &number/1)
    |> Map.update!(:cognitive_load, &number/1)
    |> Map.update!(:recent_errors, &number/1)
    |> Map.update!(:contradiction_delta, &number/1)
    |> Map.update!(:stuck_loop_repeats, &number/1)
  end

  defp recent_error_count(%SelfModel{} = model) do
    model.recent_errors
    |> List.wrap()
    |> Enum.reduce(0, fn
      %{count: count}, acc when is_number(count) -> acc + count
      %{"count" => count}, acc when is_number(count) -> acc + count
      _error, acc -> acc + 1
    end)
  end

  defp emit(warnings, %SelfModel{} = model) do
    :telemetry.execute(
      @event,
      %{
        count: length(warnings),
        critical: Enum.count(warnings, &(&1.severity == :critical))
      },
      %{
        v: @v,
        self_model_v: model.v,
        warning_kinds: Enum.map(warnings, & &1.kind),
        severities: Enum.map(warnings, & &1.severity),
        recovery_suggestions: recovery_suggestions(warnings)
      }
    )
  end

  defp contradiction?(%SelfModel{} = model, thresholds) do
    high_confidence_and_uncertainty? =
      model.confidence >= 0.75 and model.uncertainty >= 0.75

    divergent_confidence? =
      abs(number(model.confidence) - number(model.uncertainty)) >= thresholds.contradiction_delta and
        model.stability <= 0.35

    attribution_conflict? =
      case model.self_other_attribution do
        %{conflict?: true} -> true
        %{"conflict?" => true} -> true
        %{conflict: true} -> true
        %{"conflict" => true} -> true
        _ -> false
      end

    high_confidence_and_uncertainty? or divergent_confidence? or attribution_conflict?
  end

  defp stuck_loop?(%SelfModel{} = model, thresholds) do
    repeated_action_count(model) >= thresholds.stuck_loop_repeats
  end

  defp repeated_action_count(%SelfModel{} = model) do
    model.recent_actions
    |> List.wrap()
    |> Enum.map(&action_signature/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.frequencies()
    |> Map.values()
    |> case do
      [] -> 0
      counts -> Enum.max(counts)
    end
  end

  defp action_signature(%{event: event}), do: event
  defp action_signature(%{"event" => event}), do: event
  defp action_signature(%{action: action}), do: action
  defp action_signature(%{"action" => action}), do: action
  defp action_signature(action) when is_atom(action) or is_binary(action), do: action
  defp action_signature(_), do: nil

  defp severity_high(value, critical_at) when is_number(value) and value >= critical_at,
    do: :critical

  defp severity_high(_value, _critical_at), do: :warning

  defp severity_low(value, critical_at) when is_number(value) and value <= critical_at,
    do: :critical

  defp severity_low(_value, _critical_at), do: :warning

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0
end
