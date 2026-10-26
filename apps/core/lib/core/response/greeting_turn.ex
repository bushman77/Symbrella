defmodule Core.Response.GreetingTurn do
  @moduledoc """
  A deliberately narrow, surface-only boundary for standalone greetings.

  A greeting can address the assistant by name, but that name is not the
  user's identity and must not be echoed back as a form of address. Mixed
  messages ("good afternoon, I got a job") are not standalone greetings.
  This is response-style evidence, never a substitute for LIFG/ATL parsing.
  """

  defp regex_standalone, do: ~r/\A\s*(?:hi+|he+y+a?|hiya|hello|yo|sup|good\s+(?:morning|afternoon|eve(?:n+i+|i+)ng))(?:\s+there)?(?:\s*,?\s*symbrella)?\s*[.!?]*\s*\z/iu

  defp regex_service_desk, do: ~r/\b(?:how\s+(?:may|can)\s+i\s+(?:assist|help)\s+you(?:\s+(?:today|with\b[^.!?]*))?|what\s+can\s+i\s+(?:do|help\s+you\s+with)\s+for\s+you|is\s+there\s+anything\s+(?:else\s+)?i\s+can\s+(?:help|assist)\s+(?:you\s+)?with|let\s+me\s+know\s+(?:how|if)\s+i\s+can\s+help)\b[^.!?]*[.!?]*/iu

  defp regex_unnecessary_clarification, do: ~r/\b(?:not\s+(?:entirely\s+)?sure\s+what\s+you\s+mean|could\s+you\s+(?:please\s+)?(?:clarify|rephrase|restate))\b/iu

  # These indicate the model is describing a simple greeting instead of replying.
  # Only apply them after the incoming turn has been proven to be a standalone greeting.
  defp regex_meta_greeting, do: ~r/\b(?:i\s+(?:understand|see|recognize)\s+(?:that\s+)?you(?:'re| are)\s+(?:saying|greeting)|if\s+you(?:'re| are)\s+just\s+saying\s+(?:hello|hi)|you(?:'re| are)\s+(?:just\s+)?greeting\s+me)\b/iu
  defp regex_interview_prompt, do: ~r/\b(?:is\s+there\s+anything\s+(?:specific|in\s+particular)\s+you(?:'d| would)?\s+like\s+to\s+(?:discuss|ask)|i(?:'m| am)\s+here\s+to\s+(?:assist|help)\s+you)\b/iu

  # MythoMax may put the assistant's name in a trailing vocative addressed to
  # the user. Only trim that vocative on otherwise standalone greeting turns.
  defp regex_misaddressed_user, do: ~r/,\s*symbrella\b(?=\s*[.!?]|\s*$)/iu

  defp regex_echoed_name, do: ~r/\A\s*(?:hi+|he+y+a?|hiya|hello|yo|sup|good\s+(?:morning|afternoon|eve(?:n+i+|i+)ng))\s*,?\s*symbrella\b/iu

  @doc "True only for an isolated social greeting, optionally addressing Symbrella."
  @spec standalone?(term()) :: boolean()
  def standalone?(text) when is_binary(text), do: Regex.match?(regex_standalone(), text)
  def standalone?(_), do: false

  @doc "Recognize an assistant-as-help-desk draft or an echoed assistant vocative."
  @spec robotic_draft?(term(), term()) :: boolean()
  def robotic_draft?(user_text, draft)
      when is_binary(user_text) and is_binary(draft) do
    standalone?(user_text) and
      (Regex.match?(regex_service_desk(), draft) or Regex.match?(regex_echoed_name(), draft) or
         Regex.match?(regex_unnecessary_clarification(), draft) or
         Regex.match?(regex_meta_greeting(), draft) or
         Regex.match?(regex_interview_prompt(), draft) or
         Regex.match?(regex_misaddressed_user(), draft))
  end

  def robotic_draft?(_, _), do: false

  @doc """
  Remove only the unsupported help-desk wording and repeated assistant name.
  Preserve any additional natural conversation authored by the model.
  No new personal anecdotes or affective statements are invented.
  """
  @spec repair(String.t(), String.t()) :: String.t()
  def repair(user_text, draft) when is_binary(user_text) and is_binary(draft) do
    cond do
      not standalone?(user_text) ->
        draft

      Regex.match?(regex_unnecessary_clarification(), draft) or
          Regex.match?(regex_meta_greeting(), draft) or
          Regex.match?(regex_interview_prompt(), draft) ->
        safe_acknowledgement(user_text)

      true ->
        cleaned =
          draft
          |> String.replace(regex_service_desk(), "")
          |> String.replace(regex_misaddressed_user(), "")
          |> then(fn text ->
            Regex.replace(regex_echoed_name(), text, fn greeting ->
              String.replace(greeting, ~r/\s*,?\s*symbrella\b/iu, "")
            end)
          end)
          |> String.replace(~r/\s+([.!?])/u, "\\1")
          |> String.replace(~r/[ \t]{2,}/u, " ")
          |> String.trim()

        if cleaned == "", do: safe_acknowledgement(user_text), else: cleaned
    end
  end

  def repair(_user_text, draft), do: draft

  defp safe_acknowledgement(text) do
    case Regex.run(~r/\bgood\s+(morning|afternoon|eve(?:n+i+|i+)ng)\b/iu, text) do
      [_, time] ->
        greeting =
          if Regex.match?(~r/^eve(?:n+i+|i+)ng$/iu, time),
            do: "evening",
            else: String.downcase(time)
        "Good #{greeting}."
      _ -> "Hello."
    end
  end
end
