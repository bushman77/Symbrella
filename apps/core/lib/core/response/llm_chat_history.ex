defmodule Core.Response.LlmChatHistory do
  @moduledoc """
  ETS-backed boundary store for bounded LLM chat history.
  """

  require Logger

  alias Core.Response.LlmHistory
  alias Core.Response.LlmChatHistoryOwner

  @table :core_llm_chat_history
  @turn_pairs 6
  @max_item_chars 1_600

  @compile {:no_warn_undefined, Llm}

  @spec record_turn(term(), String.t(), String.t()) :: :ok
  def record_turn(session_id, user_text, assistant_text) do
    ensure_table!()

    previous = lookup_messages(session_id)

    next =
      LlmHistory.append_turn(previous, user_text, assistant_text, @turn_pairs, @max_item_chars)

    :ets.insert(@table, {session_id, next})
    :ok
  rescue
    e ->
      Logger.warning("[LlmChatHistory] record_turn failed: #{Exception.message(e)}")
      :ok
  catch
    :exit, reason ->
      Logger.warning("[LlmChatHistory] record_turn exit: #{inspect(reason)}")
      :ok
  end

  @spec messages(term(), pos_integer()) :: list()
  def messages(session_id, turn_pairs \\ @turn_pairs) do
    ensure_table!()

    session_id
    |> lookup_messages()
    |> LlmHistory.recent(turn_pairs)
  end

  @spec status(term()) :: map()
  def status(session_id) do
    case :ets.whereis(@table) do
      :undefined -> LlmHistory.missing_status()
      _tid -> LlmHistory.status(true, lookup_messages(session_id))
    end
  rescue
    _ -> LlmHistory.missing_status()
  end

  @spec ensure_table!() :: :ok
  def ensure_table! do
    LlmChatHistoryOwner.ensure_table!()
  end

  defp lookup_messages(session_id) do
    case :ets.whereis(@table) do
      :undefined ->
        []

      _tid ->
        case :ets.lookup(@table, session_id) do
          [{^session_id, list}] when is_list(list) -> list
          _ -> []
        end
    end
  end
end
