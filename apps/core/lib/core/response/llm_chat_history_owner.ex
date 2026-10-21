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

  @impl true
  def handle_call({:replace_last_assistant, sid, user, before, after_text, max_chars},
                  _from, state) do
    current = lookup_messages(@table, sid)
    recall = lookup_messages(@recall_table, sid)

    with {:ok, new_current} <- replace_last_pair(current, user, before, after_text, max_chars),
         {:ok, new_recall} <- replace_last_pair(recall, user, before, after_text, max_chars) do
      :ets.insert(@table, {sid, new_current})
      :ets.insert(@recall_table, {sid, new_recall})
      {:reply, :ok, state}
    else
      _ -> {:reply, :stale, state}
    end
  end

  # CAS-style rewrite: only the exact newest user/assistant pair of THIS
  # session is eligible. Earlier turns and other sessions are untouched.
  defp replace_last_pair(messages, user, before, after_text, max_chars) do
    expected_user = LlmHistory.clamp_text(user, max_chars)
    expected_before = LlmHistory.clamp_text(before, max_chars)

    case Enum.reverse(messages) do
      [%{"role" => "assistant", "content" => ^expected_before},
       %{"role" => "user", "content" => ^expected_user} | earlier] ->
        new_assistant = %{"role" => "assistant",
          "content" => LlmHistory.clamp_text(after_text, max_chars)}
        {:ok, Enum.reverse([new_assistant,
          %{"role" => "user", "content" => expected_user} | earlier])}

      _ ->
        :stale
    end
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
