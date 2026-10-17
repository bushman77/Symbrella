defmodule Core.Response.LlmChatHistory do
  @moduledoc """
  ETS-backed boundary store for bounded LLM chat history.
  """

  require Logger

  alias Core.Response.LlmHistory
  alias Core.Response.LlmChatHistoryOwner

  @table :core_llm_chat_history
  @recall_table :core_conversation_recall
  @retrieval_table :core_recent_retrieval
  @retrieval_ttl_ms 300_000
  @recall_turn_pairs 100
  @default_turn_pairs 6
  @default_max_item_chars 900

  @compile {:no_warn_undefined, Llm}

  @spec record_turn(term(), String.t(), String.t()) :: :ok
  def record_turn(session_id, user_text, assistant_text) do
    ensure_table!()

    # A single owner serializes each session's read/append/write transaction.
    # Independent :ets.lookup + :ets.insert calls can otherwise lose turns
    # when two request processes complete simultaneously.
    GenServer.call(
      LlmChatHistoryOwner,
      {:record_turn, session_id, user_text, assistant_text, configured_turn_pairs(),
       configured_max_item_chars()}
    )
  rescue
    e ->
      Logger.warning("[LlmChatHistory] record_turn failed: #{Exception.message(e)}")
      :ok
  catch
    :exit, reason ->
      Logger.warning("[LlmChatHistory] record_turn exit: #{inspect(reason)}")
      :ok
  end

  @doc """
  Keep a bounded, same-session record of an actual dated-news attempt.

  Only the factual outcome is stored; retrieved document content, remote
  page text and model-generated claims are not retained here. The record
  expires after five minutes and cannot cross chat-session keys.
  """
  @spec record_retrieval(term(), map() | nil) :: :ok
  def record_retrieval(session_id, %{relation: :current_events} = evidence)
      when is_binary(session_id) and byte_size(session_id) > 0 and
             byte_size(session_id) <= 128 do
    if is_integer(Map.get(evidence, :attempted_count)) and
         Map.get(evidence, :attempted_count) > 0 do
      ensure_table!()

      summary =
        evidence
        |> Map.take([:query, :relation, :status, :resolution, :attempted_count])
        |> Map.put(:source_count, length(Map.get(evidence, :sources, [])))

      :ets.insert(
        @retrieval_table,
        {session_id, System.monotonic_time(:millisecond), summary}
      )
    end

    :ok
  rescue
    _ -> :ok
  catch
    :exit, _ -> :ok
  end

  def record_retrieval(_session_id, _evidence), do: :ok

  @spec recent_retrieval(term()) :: map() | nil
  def recent_retrieval(session_id)
      when is_binary(session_id) and byte_size(session_id) > 0 and
             byte_size(session_id) <= 128 do
    ensure_table!()

    case :ets.lookup(@retrieval_table, session_id) do
      [{^session_id, recorded_at, summary}]
      when is_integer(recorded_at) and is_map(summary) ->
        age_ms = System.monotonic_time(:millisecond) - recorded_at

        if age_ms >= 0 and age_ms <= @retrieval_ttl_ms,
          do: summary,
          else: nil

      _ ->
        nil
    end
  rescue
    _ -> nil
  catch
    :exit, _ -> nil
  end

  def recent_retrieval(_), do: nil

  @spec messages(term(), pos_integer()) :: list()
  def messages(session_id, turn_pairs \\ configured_turn_pairs()) do
    ensure_table!()

    session_id
    |> lookup_messages()
    |> LlmHistory.recent(turn_pairs)
  end

  @doc """
  Return conversation evidence for verified past-turn recall.

  This is deliberately separate from the short LLM prompt window, and is
  retained in memory only. It is not a durable chat transcript.
  """
  @spec recall_messages(term(), pos_integer()) :: list()
  def recall_messages(session_id, turn_pairs \\ @recall_turn_pairs)
      when is_integer(turn_pairs) and turn_pairs > 0 do
    ensure_table!()

    session_id
    |> then(&lookup_messages(@recall_table, &1))
    |> LlmHistory.recent(min(turn_pairs, @recall_turn_pairs))
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

  defp lookup_messages(session_id), do: lookup_messages(@table, session_id)

  defp lookup_messages(table, session_id) do
    case :ets.whereis(table) do
      :undefined ->
        []

      _tid ->
        case :ets.lookup(table, session_id) do
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
