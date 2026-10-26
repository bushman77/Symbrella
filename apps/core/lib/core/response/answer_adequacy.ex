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

  defp regex_question_start, do: ~r/\A(?:what|where|when|who|whom|whose|which|why|how|is|are|was|were|do|does|did|can|could|should|would|will|has|have|had)\b/iu
  defp regex_social_question, do: ~r/\A(?:how\s+are\s+you(?:\s+(?:doing|feeling|today))?|how\s+do\s+you\s+feel|what'?s\s+up|what\s+are\s+you\s+doing)\s*[?!.]*\z/iu
  defp regex_followup, do: ~r/\A(?:(?:(?:i\s+)?(?:do\s+not|don't)\s+know)[,!.]?\s*)?(?:you\s+tell\s+me|tell\s+me(?:\s+the\s+answer)?|answer\s+(?:it|the\s+question)|just\s+answer(?:\s+it)?|so\s+what'?s\s+the\s+answer)\s*[.!?]*\z/iu
  defp regex_empty_reply, do: ~r/\A(?:i\s+(?:do\s+not|don't)\s+know|not\s+sure|i'?m\s+not\s+sure|i\s+have\s+no\s+idea)[.!?]*\z/iu

  defp regex_plain_location_question, do: ~r/^\s*where\s+is\s+[^?!.]+[?!.]*\s*$/iu
  defp regex_realtime_access_disclaimer, do: ~r/\b(?:i\s+)?(?:do\s+not|don't|cannot|can't)\s+have\s+(?:direct\s+)?access\s+to\s+(?:real[- ]time|live|current)\s+(?:information|data|updates)\b/iu
  # The qualification must be followed by an affirmative location clause,
  # not a repeated question such as "but I don't know where X is located in Y".
  # Exclude denial/uncertainty verbs inside the short subject phrase.
  defp regex_specific_location_after_qualification, do: ~r/\b(?:but|however|nevertheless)\s+(?:(?!(?:where|don't|cannot|can't|not|access|have|know)\b)[\p{L}\p{M}'’-]+\s+){1,10}(?:is|lies|located|situated)\s+(?:in|near|within|north|south|east|west)\b/iu

  @spec factual_question?(term()) :: boolean()
  def factual_question?(text) when is_binary(text) do
    text = String.trim(text)

    String.length(text) >= 8 and String.length(text) <= 500 and
      Regex.match?(regex_question_start(), text) and
      not Regex.match?(regex_social_question(), text)
  end

  def factual_question?(_), do: false

  @spec answer_target(term(), list()) :: String.t() | nil
  def answer_target(user_text, history) when is_binary(user_text) and is_list(history) do
    cond do
      factual_question?(user_text) -> String.trim(user_text)
      Regex.match?(regex_followup(), String.trim(user_text)) -> recent_user_question(history)
      true -> nil
    end
  end

  def answer_target(_, _), do: nil

  @spec failed_answer?(term(), term()) :: boolean()
  def failed_answer?(target, draft) when is_binary(target) and is_binary(draft) do
    factual_question?(target) and
      (normalize(target) == normalize(draft) or
         Regex.match?(regex_empty_reply(), String.trim(draft)) or
         unsupported_realtime_location_refusal?(target, draft))
  end

  def failed_answer?(_, _), do: false

  # Real-time map/data access is not required merely to identify a public
  # place. Reject a blanket access disclaimer only if it supplies no
  # affirmative location after the qualification. This leaves properly
  # qualified answers and genuinely current/geographically precise requests
  # to the normal grounding rules.
  defp unsupported_realtime_location_refusal?(target, draft) do
    Regex.match?(regex_plain_location_question(), target) and
      not Regex.match?(
        ~r/\b(?:now|today|currently|tonight|right now|nearest|closest|live)\b/iu,
        target
      ) and
      Regex.match?(regex_realtime_access_disclaimer(), draft) and
      not Regex.match?(regex_specific_location_after_qualification(), draft)
  end

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
