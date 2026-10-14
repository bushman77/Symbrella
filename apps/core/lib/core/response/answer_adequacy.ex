defmodule Core.Response.AnswerAdequacy do
  @moduledoc """
  Minimal answer-adequacy checks for factual questions.

  A syntactic question is evidence of an information request, not its answer.
  Exact question echoes and content-free "I don't know" drafts must not be
  published as successful factual answers. The caller may make one bounded
  model retry; this module never fabricates factual values.

  Same-session follow-ups may refer back to the last user question. Assistant
  messages are not allowed to become evidence for a new factual question.
  """

  @question_start ~r/\A(?:what|where|when|who|whom|whose|which|why|how|is|are|was|were|do|does|did|can|could|should|would|will|has|have|had)\b/iu
  @social_question ~r/\A(?:how\s+are\s+you(?:\s+(?:doing|feeling|today))?|how\s+do\s+you\s+feel|what'?s\s+up|what\s+are\s+you\s+doing)\s*[?!.]*\z/iu
  @followup ~r/\A(?:(?:(?:i\s+)?(?:do\s+not|don't)\s+know)[,!.]?\s*)?(?:you\s+tell\s+me|tell\s+me(?:\s+the\s+answer)?|answer\s+(?:it|the\s+question)|just\s+answer(?:\s+it)?|so\s+what'?s\s+the\s+answer)\s*[.!?]*\z/iu
  @empty_reply ~r/\A(?:i\s+(?:do\s+not|don't)\s+know|not\s+sure|i'?m\s+not\s+sure|i\s+have\s+no\s+idea)[.!?]*\z/iu

  @spec factual_question?(term()) :: boolean()
  def factual_question?(text) when is_binary(text) do
    text = String.trim(text)

    String.length(text) >= 8 and String.length(text) <= 500 and
      Regex.match?(@question_start, text) and
      not Regex.match?(@social_question, text)
  end

  def factual_question?(_), do: false

  @spec answer_target(term(), list()) :: String.t() | nil
  def answer_target(user_text, history) when is_binary(user_text) and is_list(history) do
    cond do
      factual_question?(user_text) -> String.trim(user_text)
      Regex.match?(@followup, String.trim(user_text)) -> recent_user_question(history)
      true -> nil
    end
  end

  def answer_target(_, _), do: nil

  @spec failed_answer?(term(), term()) :: boolean()
  def failed_answer?(target, draft) when is_binary(target) and is_binary(draft) do
    factual_question?(target) and
      (normalize(target) == normalize(draft) or
         Regex.match?(@empty_reply, String.trim(draft)))
  end

  def failed_answer?(_, _), do: false

  defp recent_user_question(history) do
    # Only the immediately preceding conversational turn may supply the
    # antecedent; never reach through an unrelated conversation.
    case Enum.take(history, -2) do
      [%{"role" => "user", "content" => text}, %{"role" => "assistant"}]
      when is_binary(text) -> if factual_question?(text), do: String.trim(text), else: nil

      [%{role: "user", content: text}, %{role: "assistant"}]
      when is_binary(text) -> if factual_question?(text), do: String.trim(text), else: nil

      _ -> nil
    end
  end

  defp normalize(text) do
    text
    |> String.downcase()
    |> String.replace(~r/^[[:space:]]*(?:assistant|symbrella)\s*:\s*/iu, "")
    |> String.replace(~r/[^\p{L}\p{N}]+/u, " ")
    |> String.trim()
  end
end
