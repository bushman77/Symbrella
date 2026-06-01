defmodule Brain.GoalStack do
  @moduledoc """
  Bounded runtime goal stack for self-regulation.

  Goals are intentionally small maps. The stack is inspectable, priority ordered,
  and safe to restore from persisted self-continuity snapshots.
  """

  use GenServer

  @name __MODULE__
  @default_keep 12

  @type goal :: %{
          id: binary(),
          label: binary(),
          priority: float(),
          tension: float(),
          source: atom() | binary(),
          reason: atom() | binary() | nil,
          status: :active,
          inserted_at_ms: integer(),
          updated_at_ms: integer()
        }

  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: @name)
  end

  @spec push(map() | keyword()) :: {:ok, goal()} | {:error, term()}
  def push(goal) when is_map(goal) or is_list(goal) do
    call_if_running({:push, Map.new(goal)})
  end

  @spec complete(binary(), atom() | binary()) :: :ok | {:error, term()}
  def complete(id, reason \\ :completed) when is_binary(id) do
    call_if_running({:complete, id, reason})
  end

  @spec reduce_uncertainty(map() | struct()) :: :ok | {:error, term()}
  def reduce_uncertainty(self_model) when is_map(self_model) do
    uncertainty = number(map_get(self_model, :uncertainty, 0.0))

    if uncertainty >= 0.65 do
      goal = %{
        id: "self|goal|reduce_uncertainty",
        label: "reduce uncertainty",
        priority: clamp01(0.35 + uncertainty * 0.65),
        tension: uncertainty,
        source: :self_model,
        reason: :high_uncertainty
      }

      case push(goal) do
        {:ok, _goal} -> :ok
        other -> other
      end
    else
      complete("self|goal|reduce_uncertainty", :uncertainty_reduced)
    end
  end

  @spec restore_active([map()]) :: :ok | {:error, term()}
  def restore_active(goals) when is_list(goals) do
    call_if_running({:restore_active, goals})
  end

  @spec active_goals(pos_integer()) :: [goal()]
  def active_goals(limit \\ 5) when is_integer(limit) and limit > 0 do
    case call_if_running({:active_goals, limit}) do
      {:ok, goals} -> goals
      _ -> []
    end
  end

  @spec status() :: {:ok, map()} | {:error, term()}
  def status, do: call_if_running(:status)

  @spec reset() :: :ok | {:error, term()}
  def reset, do: call_if_running(:reset)

  @impl true
  def init(opts) do
    keep =
      opts
      |> Keyword.get(:keep, @default_keep)
      |> normalize_keep()

    {:ok, %{keep: keep, goals: []}}
  end

  @impl true
  def handle_call({:push, raw_goal}, _from, state) do
    goal = normalize_goal(raw_goal)
    goals = upsert_goal(state.goals, goal, state.keep)

    emit(:updated, %{active_count: length(goals)}, %{reason: goal.reason, top_goal: goal.id})

    {:reply, {:ok, goal}, %{state | goals: goals}}
  end

  def handle_call({:complete, id, reason}, _from, state) do
    {removed, goals} = remove_goal(state.goals, id)

    if removed do
      emit(:updated, %{active_count: length(goals)}, %{reason: reason, completed_goal: id})
    end

    {:reply, :ok, %{state | goals: goals}}
  end

  def handle_call({:restore_active, raw_goals}, _from, state) do
    goals =
      raw_goals
      |> Enum.filter(&is_map/1)
      |> Enum.map(&normalize_goal/1)
      |> order_goals()
      |> Enum.take(state.keep)

    emit(:restored, %{active_count: length(goals)}, %{count: length(goals)})

    {:reply, :ok, %{state | goals: goals}}
  end

  def handle_call({:active_goals, limit}, _from, state) do
    {:reply, {:ok, Enum.take(state.goals, limit)}, state}
  end

  def handle_call(:status, _from, state) do
    {:reply, {:ok, %{active_count: length(state.goals), keep: state.keep, goals: state.goals}},
     state}
  end

  def handle_call(:reset, _from, state) do
    {:reply, :ok, %{state | goals: []}}
  end

  defp call_if_running(message) do
    case Process.whereis(@name) do
      nil -> {:error, :not_running}
      pid -> GenServer.call(pid, message)
    end
  end

  defp normalize_goal(raw_goal) do
    now = System.system_time(:millisecond)

    id =
      raw_goal |> map_get(:id, "self|goal|#{System.unique_integer([:positive])}") |> to_string()

    label = raw_goal |> map_get(:label, id) |> to_string()

    %{
      id: id,
      label: label,
      priority: raw_goal |> map_get(:priority, 0.5) |> number() |> clamp01(),
      tension: raw_goal |> map_get(:tension, 0.5) |> number() |> clamp01(),
      source: map_get(raw_goal, :source, :runtime),
      reason: map_get(raw_goal, :reason, nil),
      status: :active,
      inserted_at_ms: map_get(raw_goal, :inserted_at_ms, now),
      updated_at_ms: now
    }
  end

  defp upsert_goal(goals, goal, keep) do
    goals
    |> Enum.reject(&(&1.id == goal.id))
    |> then(&[goal | &1])
    |> order_goals()
    |> Enum.take(keep)
  end

  defp remove_goal(goals, id) do
    goals2 = Enum.reject(goals, &(&1.id == id))
    {length(goals2) != length(goals), goals2}
  end

  defp order_goals(goals) do
    Enum.sort_by(goals, &{&1.priority, &1.tension, &1.updated_at_ms}, :desc)
  end

  defp normalize_keep(keep) when is_integer(keep) and keep > 0, do: keep
  defp normalize_keep(_), do: @default_keep

  defp emit(decision, measurements, metadata) do
    :telemetry.execute(
      [:brain, :goal_stack, :update],
      Map.put(measurements, :count, 1),
      Map.put(metadata, :decision, decision)
    )
  end

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0
end
