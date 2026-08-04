defmodule Core.Response.LlmChatHistoryOwner do
  @moduledoc false

  use GenServer
  require Logger

  @table :core_llm_chat_history

  @spec ensure_table!() :: :ok
  def ensure_table! do
    :ok = ensure_started()
    GenServer.call(__MODULE__, :ensure_table)
  end

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts), do: {:ok, %{}}

  @impl true
  def handle_call(:ensure_table, _from, state) do
    case :ets.whereis(@table) do
      :undefined ->
        _tid =
          :ets.new(@table, [
            :named_table,
            :public,
            :set,
            {:read_concurrency, true},
            {:write_concurrency, true}
          ])

        Logger.debug(
          "[LlmChatHistory] ETS created table=#{inspect(@table)} owner=#{inspect(self())}"
        )

        {:reply, :ok, state}

      _tid ->
        {:reply, :ok, state}
    end
  end

  defp ensure_started do
    case Process.whereis(__MODULE__) do
      pid when is_pid(pid) ->
        :ok

      nil ->
        case start_link() do
          {:ok, _pid} -> :ok
          {:error, {:already_started, _pid}} -> :ok
          {:error, reason} -> raise "failed to start #{inspect(__MODULE__)}: #{inspect(reason)}"
        end
    end
  end
end
