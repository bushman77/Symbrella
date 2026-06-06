defmodule Core.Agency.Executor do
  @moduledoc """
  Conservative executor for agency commands.

  Unknown commands are rejected. Permission-gated commands are deferred unless
  the caller explicitly passes `permission?: true`.
  """

  alias Core.Agency.Command

  @type result :: %{
          required(:command_id) => String.t() | nil,
          required(:type) => atom() | nil,
          required(:status) => atom(),
          optional(:reason) => atom() | String.t(),
          optional(:outcome) => term()
        }

  @spec execute(Command.t() | map(), keyword()) :: result()
  def execute(command, opts \\ []) do
    command = normalize_command(command)

    result =
      cond do
        command == nil ->
          %{command_id: nil, type: nil, status: :rejected, reason: :invalid_command}

        command.requires_permission? and Keyword.get(opts, :permission?, false) != true ->
          result(command, :deferred, :permission_required)

        true ->
          execute_allowed(command, opts)
      end

    maybe_record(command, result, opts)
    result
  end

  @spec execute_all([Command.t() | map()], keyword()) :: [result()]
  def execute_all(commands, opts \\ [])

  def execute_all(commands, opts) when is_list(commands) do
    Enum.map(commands, &execute(&1, opts))
  end

  def execute_all(_commands, _opts), do: []

  defp execute_allowed(%Command{type: :run_self_check} = command, _opts) do
    result(command, :executed, :self_check_recorded, %{
      self_state: Map.get(command.payload, :self_state, %{})
    })
  end

  defp execute_allowed(%Command{type: :set_goal} = command, _opts) do
    goal = Map.get(command.payload, :goal, command.payload)

    if goal_stack_available?() do
      case Brain.GoalStack.push(goal) do
        {:ok, stored_goal} -> result(command, :executed, :goal_set, stored_goal)
        {:error, reason} -> result(command, :failed, reason)
      end
    else
      result(command, :deferred, :goal_stack_unavailable)
    end
  end

  defp execute_allowed(%Command{type: :complete_goal} = command, _opts) do
    id = Map.get(command.payload, :id) || Map.get(command.payload, "id")
    reason = Map.get(command.payload, :reason, :completed)

    cond do
      not is_binary(id) ->
        result(command, :rejected, :missing_goal_id)

      goal_stack_available?() ->
        case Brain.GoalStack.complete(id, reason) do
          :ok -> result(command, :executed, :goal_completed, %{id: id, reason: reason})
          {:error, reason} -> result(command, :failed, reason)
        end

      true ->
        result(command, :deferred, :goal_stack_unavailable)
    end
  end

  defp execute_allowed(%Command{type: :observe_environment} = command, _opts) do
    result(command, :deferred, :observer_not_bound)
  end

  defp execute_allowed(%Command{type: :write_memory} = command, _opts) do
    result(command, :deferred, :memory_policy_required)
  end

  defp execute_allowed(%Command{} = command, _opts) do
    result(command, :rejected, :unknown_command)
  end

  defp maybe_record(nil, _result, _opts), do: :ok

  defp maybe_record(command, result, opts) do
    if Keyword.get(opts, :record?, true) do
      Core.Response.AgencyLedger.record_command(command, result, opts)
    else
      :ok
    end
  end

  defp normalize_command(%Command{} = command), do: command

  defp normalize_command(%{} = command) do
    %Command{
      id: map_get(command, :id),
      type: map_get(command, :type),
      source: map_get(command, :source, :runtime),
      reason: map_get(command, :reason),
      risk: map_get(command, :risk, :low),
      requires_permission?: map_get(command, :requires_permission?, false),
      payload: map_get(command, :payload, %{}),
      decision_trace_id: map_get(command, :decision_trace_id),
      status: map_get(command, :status, :proposed)
    }
  end

  defp normalize_command(_), do: nil

  defp result(command, status, reason, outcome \\ nil) do
    %{
      command_id: command.id,
      type: command.type,
      status: status,
      reason: reason,
      outcome: outcome
    }
  end

  defp goal_stack_available? do
    Code.ensure_loaded?(Brain.GoalStack) and Process.whereis(Brain.GoalStack) != nil
  end

  defp map_get(map, key, default \\ nil)

  defp map_get(%{} = map, key, default) when is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_map, _key, default), do: default
end
