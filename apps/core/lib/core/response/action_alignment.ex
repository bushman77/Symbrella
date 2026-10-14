defmodule Core.Response.ActionAlignment do
  @moduledoc """
  Keep response posture consistent with a verified speech action selected by
  Core. A factual answer does not become an option-offering task simply
  because planner uncertainty is elevated.
  """

  @spec align(map(), atom() | nil, String.t(), boolean()) :: map()
  def align(%{action: action} = decision, :answer_user, text, false)
      when action in [:offer_options, :ask_first] and is_binary(text) do
    if Regex.match?(
         ~r/^\s*(?:what|where|when|who|which|how many|how much|is|are|was|were|does|do|did)\b/iu,
         text
       ) do
      Map.put(decision, :action, :answer)
    else
      decision
    end
  end

  def align(decision, _selected, _text, _guardrail?) when is_map(decision), do: decision
end
