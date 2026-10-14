defmodule Core.Response.GreetingTurn do
  @moduledoc """
  A deliberately narrow, surface-only boundary for standalone greetings.

  A greeting can address the assistant by name, but that name is not the
  user's identity and must not be echoed back as a form of address. Mixed
  messages ("good afternoon, I got a job") are not standalone greetings.
  This is response-style evidence, never a substitute for LIFG/ATL parsing.
  """

  @standalone ~r/\A\s*(?:hi+|he+y+|hiya|hello|yo|good\s+(?:morning|afternoon|evening))(?:\s+there)?(?:\s*,?\s*symbrella)?\s*[.!?]*\s*\z/iu

  @service_desk ~r/\b(?:how\s+(?:may|can)\s+i\s+(?:assist|help)\s+you(?:\s+(?:today|with\b[^.!?]*))?|what\s+can\s+i\s+(?:do|help\s+you\s+with)\s+for\s+you|is\s+there\s+anything\s+(?:else\s+)?i\s+can\s+(?:help|assist)\s+(?:you\s+)?with|let\s+me\s+know\s+(?:how|if)\s+i\s+can\s+help)\b[^.!?]*[.!?]*/iu

  @echoed_name ~r/\A\s*(?:hi+|he+y+|hiya|hello|yo|good\s+(?:morning|afternoon|evening))\s*,?\s*symbrella\b/iu

  @doc "True only for an isolated social greeting, optionally addressing Symbrella."
  @spec standalone?(term()) :: boolean()
  def standalone?(text) when is_binary(text), do: Regex.match?(@standalone, text)
  def standalone?(_), do: false

  @doc "Recognize an assistant-as-help-desk draft or an echoed assistant vocative."
  @spec robotic_draft?(term(), term()) :: boolean()
  def robotic_draft?(user_text, draft)
      when is_binary(user_text) and is_binary(draft) do
    standalone?(user_text) and
      (Regex.match?(@service_desk, draft) or Regex.match?(@echoed_name, draft))
  end

  def robotic_draft?(_, _), do: false

  @doc """
  Remove only the unsupported help-desk wording and repeated assistant name.
  Preserve any additional natural conversation authored by the model.
  No new personal anecdotes or affective statements are invented.
  """
  @spec repair(String.t(), String.t()) :: String.t()
  def repair(user_text, draft) when is_binary(user_text) and is_binary(draft) do
    if standalone?(user_text) do
      cleaned =
        draft
        |> String.replace(@service_desk, "")
        |> String.replace(@echoed_name, fn greeting ->
          greeting
          |> String.replace(~r/\s*,?\s*symbrella\b/iu, "")
        end)
        |> String.replace(~r/\s+([.!?])/u, "\\1")
        |> String.replace(~r/[ \t]{2,}/u, " ")
        |> String.trim()

      if cleaned == "", do: safe_acknowledgement(user_text), else: cleaned
    else
      draft
    end
  end

  def repair(_user_text, draft), do: draft

  defp safe_acknowledgement(text) do
    case Regex.run(~r/\bgood\s+(morning|afternoon|evening)\b/iu, text) do
      [_, time] -> "Good #{String.downcase(time)}."
      _ -> "Hello."
    end
  end
end
