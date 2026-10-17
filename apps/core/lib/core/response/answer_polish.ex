defmodule Core.Response.AnswerPolish do
  @moduledoc """
  Remove only known, nonessential generic closing prompts after direct factual
  answers. Do not strip genuine clarification or conversational questions.
  """

  @generic_closing ~r/\s*(?:Is there anything else (?:you(?:'d| would)? like to know|I can help you with)\?|Would you like to know more\?|What is the most important detail to resolve next\?)\s*$/iu

  @spec polish(String.t(), String.t(), map()) :: String.t()
  def polish(user_text, draft, context)
      when is_binary(user_text) and is_binary(draft) and is_map(context) do
    selected =
      context
      |> Map.get(:features, %{})
      |> Map.get(:selected_action)

    if direct_factual_question?(user_text) and selected in [:answer_user, nil] do
      draft
      |> String.replace(@generic_closing, "")
      |> String.trim()
    else
      draft
    end
  end

  def polish(_user_text, draft, _context), do: draft

  defp direct_factual_question?(text) do
    Regex.match?(
      ~r/^\s*(?:what|where|when|who|which|how many|how much|is|are|was|were|does|do|did)\b/iu,
      text
    )
  end
end
