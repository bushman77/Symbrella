defmodule Core.Response.LlmChatHistoryOwner do
  @moduledoc false

  use GenServer
  require Logger
  alias Core.Response.LlmHistory

  @table :core_llm_chat_history
  @recall_table :core_conversation_recall
  @retrieval_table :core_recent_retrieval
  @verified_population_table :core_verified_population_context
  @verified_answer_receipt_table :core_verified_answer_receipts

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
    Enum.each([
      @table, @recall_table, @retrieval_table, @verified_population_table,
      @verified_answer_receipt_table
    ], fn table ->
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

  @impl true
  def handle_call(
        {:record_turn, session_id, user_text, assistant_text, turn_pairs, max_item_chars},
        _from,
        state
      ) do
    history =
      LlmHistory.append_turn(
        lookup_messages(@table, session_id),
        user_text,
        assistant_text,
        turn_pairs,
        max_item_chars
      )

    recall =
      LlmHistory.append_turn(
        lookup_messages(@recall_table, session_id),
        user_text,
        assistant_text,
        100,
        max_item_chars
      )

    :ets.insert(@table, {session_id, history})
    :ets.insert(@recall_table, {session_id, recall})
    {:reply, :ok, state}
  rescue
    error ->
      Logger.warning("[LlmChatHistory] atomic append failed: #{Exception.message(error)}")
      {:reply, :ok, state}
  catch
    kind, reason ->
      Logger.warning("[LlmChatHistory] atomic append #{kind}: #{inspect(reason)}")
      {:reply, :ok, state}
  end

  defp lookup_messages(table, session_id) do
    case :ets.lookup(table, session_id) do
      [{^session_id, messages}] when is_list(messages) -> messages
      _ -> []
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
