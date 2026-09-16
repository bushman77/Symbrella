defmodule Core.Agency.AutonomyLoop do
  @moduledoc """
  Core-side bridge from endogenous DriveLoop impulses to bounded agency commands.

  The loop is intentionally tiny:

    * create one internal uncertainty-reduction goal
    * ask `Brain.ActionSelector` for the next internal action
    * execute the resulting self-check command
    * complete the goal when that self-check succeeds

  External tools and speech are deliberately out of scope here.
  """

  alias Core.Agency.Command
  alias Core.Agency.Decision
  alias Core.Agency.Executor

  @handler_id "core-agency-autonomy-loop"
  @event [:core, :agency, :autonomy, :transition]

  @spec attach(keyword()) :: :ok | {:error, term()}
  def attach(opts \\ []) when is_list(opts) do
    case :telemetry.attach(
           @handler_id,
           [:brain, :drive_loop, :impulse],
           &__MODULE__.handle_impulse/4,
           Map.new(opts)
         ) do
      :ok -> :ok
      {:error, :already_exists} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @spec detach() :: :ok | {:error, term()}
  def detach do
    case :telemetry.detach(@handler_id) do
      :ok -> :ok
      {:error, :not_found} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  def handle_impulse(_event, measurements, metadata, cfg) do
    if enabled?(cfg) do
      _ = run_impulse(measurements, metadata, Map.to_list(cfg))
    end

    :ok
  end

  @spec run_impulse(map(), map(), keyword()) :: map()
  def run_impulse(measurements, metadata, opts \\ [])

  def run_impulse(measurements, metadata, opts)
      when is_map(measurements) and is_map(metadata) and is_list(opts) do
    trace_id = trace_id(metadata)
    session_id = session_id(metadata)
    salience_score = measurements |> map_get(:salience_score, 0.0) |> number() |> clamp01()
    uncertainty = max(salience_score, 0.65)

    emit_transition(trace_id, :salience_detected, %{salience_score: salience_score}, metadata)

    goal = goal(trace_id, uncertainty, metadata)
    seed_decision = seed_decision(trace_id, session_id, uncertainty, metadata)

    set_goal =
      Command.new(:set_goal, seed_decision, reason: :endogenous_salience, payload: %{goal: goal})

    set_goal_result =
      Executor.execute(set_goal,
        record?: record_commands?(opts),
        session_id: session_id
      )

    emit_transition(trace_id, :goal_created, %{salience_score: salience_score}, %{
      command_id: set_goal.id,
      result: set_goal_result
    })

    action_result =
      Brain.ActionSelector.select(%{
        intent: :internal_salience,
        confidence: 0.85,
        mood: %{},
        self_model: %{uncertainty: uncertainty, active_goals: [goal]},
        self_monitor: %{status: :watching, recovery_suggestions: [:run_self_check]},
        session_id: session_id
      })

    decision =
      action_result
      |> Decision.from_action_result(%{
        intent: :internal_salience,
        self_model: %{uncertainty: uncertainty, active_goals: [goal]},
        self_monitor: %{status: :watching, recovery_suggestions: [:run_self_check]},
        session_id: session_id
      })
      |> Map.put(:trace_id, trace_id)

    commands = Command.from_decision(decision)

    emit_transition(trace_id, :action_selected, %{confidence: decision.confidence}, %{
      selected_action: decision.selected_action,
      candidates: summarize_candidates(decision.candidates),
      command_types: Enum.map(commands, & &1.type)
    })

    command_results =
      Executor.execute_all(commands,
        permission?: false,
        record?: record_commands?(opts),
        session_id: session_id
      )

    emit_transition(trace_id, :command_executed, %{count: length(command_results)}, %{
      results: command_results
    })

    complete_result =
      maybe_complete_goal(goal.id, seed_decision, command_results, opts, session_id)

    %{
      trace_id: trace_id,
      goal: goal,
      set_goal_result: set_goal_result,
      action_result: action_result,
      decision: decision,
      commands: commands,
      command_results: command_results,
      complete_result: complete_result
    }
  end

  def run_impulse(_measurements, _metadata, _opts) do
    %{status: :rejected, reason: :invalid_impulse}
  end

  defp maybe_complete_goal(goal_id, decision, command_results, opts, session_id) do
    if self_check_executed?(command_results) do
      command =
        Command.new(:complete_goal, decision,
          reason: :self_check_recorded,
          payload: %{id: goal_id, reason: :self_check_recorded}
        )

      result =
        Executor.execute(command,
          record?: record_commands?(opts),
          session_id: session_id
        )

      emit_transition(decision.trace_id, :goal_completed, %{count: 1}, %{
        command_id: command.id,
        result: result
      })

      result
    else
      emit_transition(decision.trace_id, :goal_continues, %{count: 1}, %{
        results: command_results
      })

      %{status: :deferred, reason: :self_check_not_executed}
    end
  end

  defp self_check_executed?(command_results) do
    Enum.any?(command_results, fn
      %{type: :run_self_check, status: :executed} -> true
      _ -> false
    end)
  end

  defp seed_decision(trace_id, session_id, uncertainty, metadata) do
    %Decision{
      trace_id: trace_id,
      intent: :internal_salience,
      selected_action: :self_check,
      reasons: [:endogenous_salience],
      self_state: %{
        uncertainty: uncertainty,
        salience: map_get(metadata, :salience, %{})
      },
      confidence: 0.85,
      uncertainty: uncertainty,
      risk: :low,
      permission: %{required?: false, status: :not_required},
      expected_outcome: %{kind: :self_state_reviewed},
      source: :endogenous_drive,
      selector: %{session_id: session_id}
    }
  end

  defp goal(trace_id, uncertainty, metadata) do
    %{
      id: "#{trace_id}|goal|reduce_uncertainty",
      label: "reduce endogenous uncertainty",
      priority: clamp01(0.45 + uncertainty * 0.45),
      tension: uncertainty,
      source: :endogenous_drive,
      reason: map_get(metadata, :reason, :endogenous_salience)
    }
  end

  defp emit_transition(trace_id, step, measurements, metadata) do
    :telemetry.execute(
      @event,
      Map.put(measurements, :count, Map.get(measurements, :count, 1)),
      Map.merge(%{trace_id: trace_id, step: step, v: 1}, metadata)
    )
  end

  defp summarize_candidates(candidates) do
    Enum.map(candidates, fn
      %{} = candidate ->
        %{
          action: map_get(candidate, :action),
          score: map_get(candidate, :score),
          reason: map_get(candidate, :reason)
        }

      other ->
        %{action: other}
    end)
  end

  defp enabled?(cfg) when is_map(cfg) do
    cfg
    |> Map.get(:enabled?, config(:enabled?, true))
    |> enabled_value?()
  end

  defp enabled?(_), do: config(:enabled?, true) |> enabled_value?()

  defp record_commands?(opts) do
    opts
    |> Keyword.get(:record_commands?, config(:record_commands?, true))
    |> enabled_value?()
  end

  defp config(key, default) do
    Application.get_env(:core, __MODULE__, [])
    |> Keyword.get(key, default)
  end

  defp trace_id(metadata) do
    case map_get(metadata, :trace_id) do
      value when is_binary(value) and value != "" -> value
      _ -> "agency|endogenous|#{System.unique_integer([:positive, :monotonic])}"
    end
  end

  defp session_id(metadata) do
    metadata
    |> map_get(:session_id, "global")
    |> to_string()
  end

  defp map_get(map, key, default \\ nil)

  defp map_get(%_struct{} = struct, key, default),
    do: struct |> Map.from_struct() |> map_get(key, default)

  defp map_get(%{} = map, key, default) when is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_map, _key, default), do: default

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0

  defp enabled_value?(value), do: value in [true, "true", "1", 1, :on, "on", :yes, "yes"]
end
