defmodule Core.Response.LlmChatHistory do
  @moduledoc """
  ETS-backed boundary store for bounded LLM chat history.
  """

  require Logger

  alias Core.Response.LlmHistory
  alias Core.Response.LlmChatHistoryOwner

  @table :core_llm_chat_history
  @default_turn_pairs 3
  @default_max_item_chars 900

  @compile {:no_warn_undefined, Llm}

  @spec record_turn(term(), String.t(), String.t()) :: :ok
  def record_turn(session_id, user_text, assistant_text) do
    ensure_table!()

    previous = lookup_messages(session_id)

    next =
      LlmHistory.append_turn(
        previous,
        user_text,
        assistant_text,
        configured_turn_pairs(),
        configured_max_item_chars()
      )

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
  def messages(session_id, turn_pairs \\ configured_turn_pairs()) do
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

  defp configured_turn_pairs do
    llm_config(:history_turn_pairs, @default_turn_pairs)
  end

  defp configured_max_item_chars do
    llm_config(:max_item_chars, @default_max_item_chars)
  end

  defp llm_config(key, default) do
    :core
    |> Application.get_env(:llm_synthesis, [])
    |> Keyword.get(key, default)
    |> positive_integer(default)
  end

  defp positive_integer(value, _default) when is_integer(value) and value > 0, do: value
  defp positive_integer(_value, default), do: default
end
