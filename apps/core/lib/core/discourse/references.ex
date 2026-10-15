defmodule Core.Discourse.References do
  @moduledoc """
  Bounded, inspectable antecedent binding for clear singular location follow-ups.

  This is conversation evidence, not dictionary sense selection or geographic
  verification. Ambiguous or absent history produces no binding.
  """

  alias Core.Response.LlmChatHistory

  @lookback_turns 6

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    sentence = Map.get(si, :sentence)
    session_id = Map.get(si, :session_id)

    case resolve(sentence, session_id) do
      nil -> si
      reference -> Map.put(si, :discourse_reference, reference)
    end
  end

  def attach(other), do: other

  @spec resolve(String.t() | nil, term()) :: map() | nil
  def resolve(sentence, session_id)
      when is_binary(sentence) and not is_nil(session_id) do
    with pronoun when is_binary(pronoun) <- followup_pronoun(sentence),
         antecedent when is_binary(antecedent) <- previous_explicit_location(session_id) do
      %{
        pronoun: pronoun,
        antecedent: antecedent,
        source: :recent_user_turn,
        evidence: :explicit_location_question,
        confidence: 0.9
      }
    else
      _ -> nil
    end
  end

  def resolve(_, _), do: nil

  defp followup_pronoun(sentence) do
    cond do
      Regex.match?(~r/^\s*(?:what|how)\s+(?:is|was)\s+its?\b/iu, sentence) ->
        "its"

      Regex.match?(~r/^\s*(?:is|was|does|did|can|could|will|would)\s+it\b/iu, sentence) or
          Regex.match?(~r/^\s*how\s+far\s+is\s+it\s+from\b/iu, sentence) ->
        "it"

      true ->
        nil
    end
  end

  defp previous_explicit_location(session_id) do
    session_id
    |> LlmChatHistory.messages(@lookback_turns)
    |> Enum.reverse()
    |> Enum.find_value(fn
      %{"role" => "user", "content" => text} when is_binary(text) ->
        extract_location(text)

      _ ->
        nil
    end)
  end

  defp extract_location(text) do
    capture =
      Regex.run(
        ~r/^\s*(?:where\s+is|tell\s+me\s+about|what\s+is\s+the\s+population\s+of)\s+(\p{L}[\p{L}\p{M}\s,'.-]{1,78})\s*\??\s*$/iu,
        text
      )

    case capture do
      [_, entity] ->
        entity =
          entity
          |> String.trim()
          |> String.trim_trailing(".")
          |> String.trim_trailing("?")
          |> String.trim()
          |> String.replace(~r/\s+/u, " ")

        if String.length(entity) > 1 and
             not Regex.match?(~r/^(?:it|that|this|there|my|your|our|their)\b/iu, entity) do
          entity
        end

      _ ->
        nil
    end
  end
end
