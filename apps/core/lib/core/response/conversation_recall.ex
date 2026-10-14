defmodule Core.Response.ConversationRecall do
  @moduledoc """
  Evidence-based recall for explicit questions about earlier chat turns.

  The model's short prompt context is not authoritative evidence that a
  particular question was asked. Inspect the independently bounded
  conversation-turn buffer instead. Never infer a past turn from topic
  similarity alone.
  """

  alias Core.Response.LlmChatHistory

  @type answer :: {String.t(), map()} | nil

  @doc """
  Answer supported past-turn questions using only the current session's
  recorded user turns. An absent match is a limited-history result, never
  a claim that the user has never asked the question anywhere.
  """
  @spec reply(String.t(), term()) :: answer()
  def reply(text, session_id) when is_binary(text) do
    case intent(text) do
      {:asked_before, subject, match_mode} ->
        answer_asked_before(session_id, subject, match_mode)

      :last_question ->
        answer_last_question(session_id)

      nil ->
        nil
    end
  end

  def reply(_, _), do: nil

  defp intent(text) do
    cond do
      Regex.match?(
        ~r/^\s*(?:what did i ask you (?:last|earlier|before)|what was (?:the )?last (?:question|thing) i asked you)\s*\??\s*$/iu,
        text
      ) ->
        :last_question

      match =
          Regex.run(
            ~r/^\s*(?:have|did)\s+i\s+(?:ever\s+)?ask(?:ed)?\s+you\s+(.+?)\s*\??\s*$/iu,
            text
          ) ->
        [_, raw_subject] = match
        subject = String.trim(raw_subject)

        {match_mode, target} =
          if Regex.match?(~r/^about\s+/iu, subject) do
            {:topic, Regex.replace(~r/^about\s+/iu, subject, "")}
          else
            {:question, subject}
          end

        if String.length(normalize(target)) >= 3 do
          {:asked_before, target, match_mode}
        end

      true ->
        nil
    end
  end

  defp answer_asked_before(session_id, target, mode) do
    questions = prior_user_turns(session_id)

    match =
      Enum.find(Enum.reverse(questions), fn question ->
        matches?(question, target, mode)
      end)

    case match do
      nil ->
        count = length(questions)

        text =
          if count == 0 do
            "I don't have any earlier questions recorded for this conversation."
          else
            "I don't see that question in the last #{count} user turns I have recorded."
          end

        {text, metadata(false, count)}

      question ->
        {"Yes. Earlier you asked: “#{question}”.", metadata(true, length(questions))}
    end
  end

  defp answer_last_question(session_id) do
    questions = prior_user_turns(session_id)

    case List.last(questions) do
      nil ->
        {"I don't have any earlier questions recorded for this conversation.",
         metadata(false, 0)}

      question ->
        {"The last thing you asked me was: “#{question}”.",
         metadata(true, length(questions))}
    end
  end

  defp prior_user_turns(session_id) do
    session_id
    |> LlmChatHistory.recall_messages()
    |> Enum.filter(&(Map.get(&1, "role") == "user"))
    |> Enum.map(&Map.get(&1, "content", ""))
    |> Enum.filter(&(is_binary(&1) and String.trim(&1) != ""))
  end

  defp matches?(question, target, :question) do
    normalize(question) == normalize(target)
  end

  defp matches?(question, target, :topic) do
    needle = normalize(target)
    String.contains?(" " <> normalize(question) <> " ", " " <> needle <> " ")
  end

  defp normalize(text) when is_binary(text) do
    normalized =
      text
      |> String.downcase()
      |> String.replace(~r/[^\p{L}\p{N}\s]/u, " ")
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()

    case Regex.run(~r/^what (.+) is$/u, normalized) do
      [_, subject] -> "what is " <> subject
      _ -> normalized
    end
  end

  defp metadata(found?, count) do
    %{
      action: :conversation_recall,
      intent_inferred: :conversation_history_query,
      response_source: :conversation_history,
      memory_source: :session_turns,
      retrieval_found?: found?,
      neural_ranker_used?: false,
      inspected_user_turns: count
    }
  end
end
