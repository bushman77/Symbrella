defmodule SymbrellaWeb.BrainRuntime do
  @moduledoc """
  Web boundary adapter for safe reads from Brain and Db runtime services.

  LiveViews and components should consume plain maps/lists from this module
  instead of reaching directly into OTP processes or database query APIs.
  """

  require Logger

  import Ecto.Query, only: [from: 2]

  alias SymbrellaWeb.Region.Registry, as: RegionRegistry

  @compile {:no_warn_undefined, Brain}
  @compile {:no_warn_undefined, Brain.Blackboard}
  @compile {:no_warn_undefined, Brain.CycleClock}
  @compile {:no_warn_undefined, Brain.Introspection}
  @compile {:no_warn_undefined, Brain.Introspect}
  @compile {:no_warn_undefined, Brain.LIFG}
  @compile {:no_warn_undefined, Brain.Lifg}
  @compile {:no_warn_undefined, Brain.MoodCore}
  @compile {:no_warn_undefined, Brain.SelfPortrait}
  @compile {:no_warn_undefined, Db}
  @compile {:no_warn_undefined, Db.BrainCell}

  @spec ensure_optional(module()) :: :ok
  def ensure_optional(mod) when is_atom(mod) do
    cond do
      Code.ensure_loaded?(mod) and function_exported?(mod, :ensure_started, 0) ->
        case safe_call(fn -> mod.ensure_started() end) do
          {:ok, _} -> :ok
          :error -> :ok
        end

      true ->
        :ok
    end
  end

  @spec cycle_snapshot() :: map()
  def cycle_snapshot do
    case safe_call(fn -> Brain.CycleClock.snapshot() end) do
      {:ok, %{} = snapshot} -> snapshot
      _ -> %{seq: nil, hz: nil, dt_ms: nil, phase: nil}
    end
  end

  @spec mood_snapshot() :: map()
  def mood_snapshot do
    if Code.ensure_loaded?(Brain.MoodCore) and function_exported?(Brain.MoodCore, :snapshot, 0) do
      case safe_call(fn -> Brain.MoodCore.snapshot() end) do
        {:ok, %{} = snapshot} -> snapshot
        _ -> %{}
      end
    else
      %{}
    end
  end

  @spec introspection_snapshot() :: {:ok, map()} | :error
  def introspection_snapshot do
    cond do
      Code.ensure_loaded?(Brain.Introspection) and
          function_exported?(Brain.Introspection, :snapshot, 0) ->
        safe_map_call(fn -> Brain.Introspection.snapshot() end)

      Code.ensure_loaded?(Brain.Introspect) and function_exported?(Brain.Introspect, :snapshot, 0) ->
        safe_map_call(fn -> Brain.Introspect.snapshot() end)

      true ->
        :error
    end
  end

  @spec brain_snapshot() :: {:ok, map()} | :error
  def brain_snapshot do
    if Code.ensure_loaded?(Brain) and function_exported?(Brain, :snapshot, 0) do
      safe_map_call(fn -> Brain.snapshot() end)
    else
      :error
    end
  end

  @spec wm_snapshot() :: {:ok, map()} | :error
  def wm_snapshot do
    if Code.ensure_loaded?(Brain) and function_exported?(Brain, :snapshot_wm, 0) do
      safe_map_call(fn -> Brain.snapshot_wm() end)
    else
      :error
    end
  end

  @spec self_model_snapshot() :: map()
  def self_model_snapshot do
    case introspection_snapshot() do
      {:ok, snapshot} -> snapshot
      :error -> %{}
    end
  end

  @spec self_portrait_snapshot() :: map()
  def self_portrait_snapshot do
    fun =
      cond do
        Code.ensure_loaded?(Brain.SelfPortrait) and
            function_exported?(Brain.SelfPortrait, :snapshot, 0) ->
          fn -> Brain.SelfPortrait.snapshot() end

        Code.ensure_loaded?(Brain.SelfPortrait) and
            function_exported?(Brain.SelfPortrait, :snapshot, 1) ->
          fn -> Brain.SelfPortrait.snapshot(Brain.SelfPortrait) end

        true ->
          nil
      end

    if is_nil(fun) do
      %{}
    else
      case safe_call(fun) do
        {:ok, %{} = portrait} -> portrait
        {:ok, other} -> %{value: other}
        _ -> %{}
      end
    end
  end

  @spec region_snapshot(atom()) :: map()
  def region_snapshot(region_key) when is_atom(region_key) do
    case region_state(region_key) do
      {:ok, snapshot} -> snapshot
      :error -> fallback_region_snapshot(region_key)
    end
  end

  def region_snapshot(_), do: %{}

  @spec region_status(atom()) :: map()
  def region_status(region_key) when is_atom(region_key) do
    region_key
    |> RegionRegistry.process_for()
    |> call_target()
    |> direct_status(region_key)
  end

  def region_status(_), do: %{}

  @spec all_region_status() :: map()
  def all_region_status do
    RegionRegistry.keys()
    |> Enum.map(fn key ->
      status =
        key
        |> region_status()
        |> Map.put_new(:region, key)
        |> Map.put_new(:status, :down)

      {key, status}
    end)
    |> Enum.into(%{})
  end

  @spec blackboard_history(non_neg_integer()) :: list()
  def blackboard_history(limit \\ 50) do
    if Code.ensure_loaded?(Brain.Blackboard) and function_exported?(Brain.Blackboard, :history, 1) do
      case safe_call(fn -> Brain.Blackboard.history(limit) end) do
        {:ok, events} when is_list(events) -> events
        _ -> []
      end
    else
      []
    end
  end

  @spec brain_cells_for_choices([String.t()], [String.t()]) :: list(map())
  def brain_cells_for_choices(ids, norms) when is_list(ids) and is_list(norms) do
    ids =
      ids
      |> Enum.filter(&is_binary/1)
      |> Enum.reject(&String.contains?(&1, "|fallback"))
      |> Enum.uniq()

    norms = norms |> Enum.filter(&is_binary/1) |> Enum.reject(&(&1 == "")) |> Enum.uniq()

    cond do
      ids == [] and norms == [] ->
        []

      not (Code.ensure_loaded?(Db) and Code.ensure_loaded?(Db.BrainCell)) ->
        []

      true ->
        safe_query(fn ->
          from(c in Db.BrainCell,
            where:
              (not is_nil(c.id) and c.id in ^ids) or
                (not is_nil(c.norm) and c.norm in ^norms),
            select: %{
              id: c.id,
              word: c.word,
              norm: c.norm,
              pos: c.pos,
              definition: c.definition,
              example: c.example
            },
            limit: 50
          )
          |> Db.all()
        end)
    end
  end

  def brain_cells_for_choices(_ids, _norms), do: []

  defp region_state(region_key) do
    if Code.ensure_loaded?(Brain.Introspect) and
         function_exported?(Brain.Introspect, :region_state, 1) do
      safe_map_call(fn -> Brain.Introspect.region_state(region_key) end)
    else
      :error
    end
  end

  defp fallback_region_snapshot(region_key) do
    with {:ok, snapshot} <- introspection_snapshot(),
         regions when is_map(regions) <-
           Map.get(snapshot, :regions) || Map.get(snapshot, "regions"),
         region when not is_nil(region) <-
           Map.get(regions, region_key) || Map.get(regions, to_string(region_key)) do
      normalize_map(region)
    else
      _ -> direct_module_snapshot(region_key)
    end
  end

  defp direct_module_snapshot(region_key) do
    mod = region_key |> RegionRegistry.process_for() |> call_target()

    cond do
      is_nil(mod) ->
        %{}

      Code.ensure_loaded?(mod) and function_exported?(mod, :get_state, 0) ->
        case safe_call(fn -> mod.get_state() end) do
          {:ok, state} -> normalize_map(state)
          _ -> %{}
        end

      true ->
        %{}
    end
  end

  defp direct_status(nil, _region_key), do: %{}

  defp direct_status(mod, region_key) do
    base = base_status_from_process(mod, region_key)

    status =
      case safe_call(fn -> GenServer.call(mod, :status, 150) end) do
        {:ok, s} when is_map(s) ->
          s

        _ ->
          if Code.ensure_loaded?(mod) and function_exported?(mod, :status, 0) do
            case safe_call(fn -> mod.status() end) do
              {:ok, s} when is_map(s) -> s
              _ -> %{}
            end
          else
            %{}
          end
      end

    Map.merge(base, status)
  end

  defp base_status_from_process(mod, region_key) do
    {pid, reg_name} = pid_of(mod)

    case pid do
      pid when is_pid(pid) ->
        info = :erlang.process_info(pid, [:message_queue_len, :current_function]) || []

        %{
          region: region_key,
          status: :up,
          pid: pid,
          registered_name: reg_name,
          queue: info[:message_queue_len] || 0,
          current: info[:current_function] || :idle
        }

      _ ->
        %{
          region: region_key,
          status: :down,
          pid: nil,
          queue: 0,
          current: :idle
        }
    end
  end

  defp pid_of(mod) when is_atom(mod) do
    cond do
      function_exported?(mod, :name, 0) and is_pid(Process.whereis(mod.name())) ->
        {Process.whereis(mod.name()), mod.name()}

      is_pid(Process.whereis(mod)) ->
        {Process.whereis(mod), mod}

      true ->
        {nil, nil}
    end
  end

  defp call_target(mod) when is_atom(mod) do
    cond do
      Code.ensure_loaded?(Brain.LIFG) and mod == Brain.Lifg -> Brain.LIFG
      true -> mod
    end
  end

  defp call_target(_), do: nil

  defp safe_map_call(fun) when is_function(fun, 0) do
    case safe_call(fun) do
      {:ok, value} -> {:ok, normalize_map(value)}
      :error -> :error
    end
  end

  defp safe_query(fun) when is_function(fun, 0) do
    case safe_call(fun) do
      {:ok, rows} when is_list(rows) -> rows
      _ -> []
    end
  end

  defp safe_call(fun) when is_function(fun, 0) do
    try do
      {:ok, fun.()}
    rescue
      error ->
        Logger.debug("BrainRuntime safe_call error: #{inspect(error)}")
        :error
    catch
      kind, reason ->
        Logger.debug("BrainRuntime safe_call caught #{inspect(kind)}: #{inspect(reason)}")
        :error
    end
  end

  defp normalize_map(%{} = map), do: map
  defp normalize_map(other), do: %{value: other}
end
