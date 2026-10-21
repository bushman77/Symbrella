defmodule Core.Response.ActionAlignment do
  @moduledoc """
  Keep response posture consistent with a verified speech action selected by
  Core. A factual answer or a witnessed, low-risk code request does not turn
  into an option-offering task merely because planner uncertainty is elevated.
  """

  alias Core.Response.CodeRequest

  @spec align(map(), atom() | nil, String.t(), boolean()) :: map()
  def align(decision, selected, text, guardrail?),
    do: align(decision, selected, text, guardrail?, nil)

  @spec align(map(), atom() | nil, String.t(), boolean(), map() | nil) :: map()
  def align(%{action: action} = decision, :answer_user, text, false, structure)
      when action in [:offer_options, :ask_first] and is_binary(text) do
    cond do
      CodeRequest.eligible_structure?(structure, text) ->
        Map.put(decision, :action, :act_first)

      Regex.match?(
        ~r/^\s*(?:what|where|when|who|which|how many|how much|is|are|was|were|does|do|did)\b/iu,
        text
      ) ->
        Map.put(decision, :action, :answer)

      true ->
        decision
    end
  end

  def align(decision, _selected, _text, _guardrail?, _structure)
      when is_map(decision), do: decision
end
