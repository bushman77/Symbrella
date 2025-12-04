defmodule Brain.Cell.Runtime do
  @moduledoc false

  require Logger
  alias Db.BrainCell, as: Row

  @registry Brain.Registry
  @cell_sup Brain.CellSup

  @spec via(binary()) :: {:via, Registry, {module(), binary()}}
  def via(id) when is_binary(id), do: {:via, Registry, {@registry, id}}

  @spec ensure_start_and_cast(Row.t() | binary(), map()) :: :ok
  def ensure_start_and_cast(%Row{id: id} = row, payload) when is_binary(id) and is_map(payload) do
    ensure_started(id, row)
    GenServer.cast(via(id), {:activate, payload})
    :ok
  end

  def ensure_start_and_cast(id, payload) when is_binary(id) and is_map(payload) do
    ensure_started(id, id)
    GenServer.cast(via(id), {:activate, payload})
    :ok
  end

  @spec handle_map_item_activation(map(), map()) :: :ok
  def handle_map_item_activation(map_item, payload) when is_map(map_item) and is_map(payload) do
    id = map_item[:id] || map_item["id"]
    type = map_item[:type] || map_item["type"]

    cond do
      is_binary(id) and (type == "lexicon" or type == :lexicon) ->
        :telemetry.execute([:brain, :activate, :lexicon_autohydrate], %{count: 1}, %{id: id})
        ensure_start_and_cast(id, payload)
        :ok

      is_binary(id) and (type == "seed" or type == :seed) ->
        :telemetry.execute([:brain, :activate, :seed_unknown], %{count: 1}, %{id: id})
        :ok

      is_binary(id) ->
        ensure_start_and_cast(id, payload)
        :ok

      true ->
        :telemetry.execute([:brain, :activate, :unknown_map], %{count: 1}, %{sample: map_item})
        Logger.debug("Brain.Cell.Runtime: unhandled map item #{inspect(map_item)}")
        :ok
    end
  end

  @spec ensure_started(binary(), term()) :: :ok
  def ensure_started(id, arg) when is_binary(id) do
    case Registry.lookup(@registry, id) do
      [] ->
        case DynamicSupervisor.start_child(@cell_sup, {Brain.Cell, arg}) do
          {:ok, _pid} -> :ok
          {:error, {:already_started, _pid}} -> :ok
          other -> Logger.warning("Brain.Cell.Runtime: start_child(#{id}) -> #{inspect(other)}")
        end

      _ ->
        :ok
    end
  end
end
