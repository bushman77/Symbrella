defmodule Core.Response.OverrideSkills.SelfState do
  @moduledoc """
  Runtime self-state override skills and query predicates.
  """

  alias Core.Response.OverrideSkills.Decision

  @spec apply(map(), map()) :: {:ok, {map(), map()}} | :pass
  def apply(features, decision) do
    text = Map.get(features, :text, "")

    cond do
      mood_indices_query?(text) ->
        decision =
          decision
          |> Decision.put(tone: :neutral, mode: :explainer, action: :answer)
          |> Decision.add_override(:mood_indices_answer)

        {:ok,
         {decision,
	          %{
	            id: :mood_indices,
	            reason: :mood_indices_query,
	            llm_prompt: :mood_indices
	          }}}

      self_state_feeling_query?(text) ->
        decision =
          decision
          |> Decision.put(tone: :neutral, mode: :explainer, action: :answer)
          |> Decision.add_override(:self_state_feeling_answer)

        {:ok,
         {decision,
	          %{
	            id: :self_state_feeling,
	            reason: :self_state_feeling_query,
	            llm_prompt: :self_state_feeling
	          }}}

      self_portrait_query?(text) ->
        decision =
          decision
          |> Decision.put(tone: :neutral, mode: :explainer, action: :answer)
          |> Decision.add_override(:self_portrait_answer)

        {:ok,
         {decision,
	          %{
	            id: :self_portrait,
	            reason: :self_portrait_query,
	            llm_prompt: :self_portrait
	          }}}

      self_check_query?(text) ->
        decision =
          decision
          |> Decision.put(tone: :neutral, mode: :explainer, action: :answer)
          |> Decision.add_override(:runtime_self_check)

        {:ok,
         {decision,
	          %{
	            id: :runtime_self_check,
	            reason: :self_check_query,
	            llm_prompt: :runtime_self_check
	          }}}

      true ->
        :pass
    end
  end

  @spec read_only_mood_query?(String.t()) :: boolean()
  def read_only_mood_query?(text) do
    mood_indices_query?(text) or self_state_feeling_query?(text) or self_portrait_query?(text) or
      self_check_query?(text)
  end

  @spec self_check_query?(String.t()) :: boolean()
  def self_check_query?(text) when is_binary(text) do
    t = String.downcase(text)

    Regex.match?(
      ~r/\b(self[-\s]?check|check yourself|check your state|runtime self[-\s]?check)\b/u,
      t
    ) or
      (String.contains?(t, "something is wrong") and
         (String.contains?(t, "dangerous") or String.contains?(t, "unstable")))
  end

  def self_check_query?(_), do: false

  defp mood_indices_query?(text) when is_binary(text) do
    t = String.downcase(text)

    Regex.match?(~r/\b(my|your|current|live)?\s*mood\s+(indices|index|state|levels)\b/u, t) or
      Regex.match?(~r/\bhow\s+(is|are)\s+(your\s+)?mood\b/u, t)
  end

  defp mood_indices_query?(_), do: false

  defp self_state_feeling_query?(text) when is_binary(text) do
    t = String.downcase(text)

    Regex.match?(
      ~r/\b(how are you feeling|how do you feel|how are you doing|how are you|are you ok|are you okay)\b/u,
      t
    )
  end

  defp self_state_feeling_query?(_), do: false

  defp self_portrait_query?(text) when is_binary(text) do
    t = String.downcase(text)

    Regex.match?(~r/\b(self[-\s]?portrait|self[-\s]?state|self[-\s]?model)\b/u, t) and
      Regex.match?(~r/\b(how|what|where|get|show|see|view|fetch|read|inspect|snapshot)\b/u, t)
  end

  defp self_portrait_query?(_), do: false
end
