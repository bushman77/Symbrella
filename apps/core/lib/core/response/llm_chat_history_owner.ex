defmodule Core.Response.LlmChatHistoryOwner do
  @moduledoc false

  use GenServer
  require Logger

  @table :core_llm_chat_history
  @recall_table :core_conversation_recall
  @retrieval_table :core_recent_retrieval
  @verified_population_table :core_verified_population_context

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
    Enum.each([@table, @recall_table, @retrieval_table, @verified_population_table], fn table ->
      if :ets.whereis(table) == :undefined do
        :ets.new(table, [
          :named_table,
          :public,
          :set,
          {:read_concurrency, true},
          {:write_concurrency, true}
        ])

        Logger.debug(
          "[LlmChatHistory] ETS created table=#{inspect(table)} owner=#{inspect(self())}"
        )
      end
    end)

    {:reply, :ok, state}
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
