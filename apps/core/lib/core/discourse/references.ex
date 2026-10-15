defmodule Core.Discourse.References do
  @moduledoc """
  Bounded, inspectable antecedent binding for clear singular location follow-ups.

  This is conversation evidence, not dictionary sense selection or geographic
  verification. Ambiguous or absent history produces no binding.
  """

  alias Core.Response.LlmChatHistory
  alias Core.Semantic.EventFrames

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
    if Regex.match?(~r/^\s*what\s+is\s+it\s*[?!.]*\s*$/iu, sentence) do
      # "What is it?" has no safe literal antecedent. Only bind a bounded,
      # immediately preceding distance exchange, not a distant older topic.
      recent_distance_reference(session_id)
    else
      case named_geographic_reference(sentence, session_id) do
        %{source: :recent_geographic_entity} = named ->
          named

        _ ->
          with pronoun when is_binary(pronoun) <- followup_pronoun(sentence),
               antecedent when is_binary(antecedent) <-
                 geographic_pronoun_antecedent(sentence, session_id) do
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
    end
  end

  def resolve(_, _), do: nil

  defp recent_distance_reference(session_id) do
    case LlmChatHistory.messages(session_id, 1) do
      [
        %{"role" => "user", "content" => previous_question},
        %{"role" => "assistant", "content" => previous_reply}
      ] when is_binary(previous_question) and is_binary(previous_reply) ->
        case EventFrames.location_question(previous_question) do
          %{relation: :proximity, subject: subject, target: target}
          when is_binary(subject) and is_binary(target) and
               byte_size(subject) > 1 and byte_size(target) > 1 ->
            %{
              pronoun: "it",
              antecedent: "the distance between #{subject} and #{target}",
              subject: subject,
              target: target,
              source: :recent_distance_turn,
              evidence: :explicit_distance_question,
              confidence: 0.75
            }

          _ ->
            nil
        end

      _ ->
        nil
    end
  end

  # Match a place named in this turn only against explicit entities from a
  # recent distance question. Do not infer that arbitrary "what is X?" names
  # are geographic just because they resemble proper nouns.
  defp named_geographic_reference(sentence, session_id) do
    named =
      case Regex.run(~r/^\s*what\s+is\s+([\p{L}][\p{L}\p{M}\s,.'-]{1,78})\s*\?\s*$/iu, sentence) do
        [_, place] -> place
        _ ->
          case Regex.run(
                 ~r/^\s*(?:that|this)\s+is\s+not\s+where\s+([\p{L}][\p{L}\p{M}\s,.'-]{1,78}?)\s+is\s*[.!?]*\s*$/iu,
                 sentence
               ) do
            [_, place] -> place
            _ -> nil
          end
      end

    case match_known_geographic_entity(session_id, named) do
      nil -> nil
      place ->
        %{pronoun: nil, antecedent: place, source: :recent_geographic_entity,
          evidence: :previous_explicit_distance_question, confidence: 0.9}
    end
  end

  defp geographic_pronoun_antecedent(sentence, session_id) do
    if Regex.match?(~r/\bhow\s+far\s+is\s+it\s+from\b/iu, sentence) do
      recent_geographic_focus(session_id) || previous_explicit_location(session_id)
    else
      previous_explicit_location(session_id)
    end
  end

  defp recent_geographic_focus(session_id) do
    previous =
      session_id
      |> LlmChatHistory.messages(@lookback_turns)
      |> Enum.reverse()
      |> Enum.find_value(fn
        %{"role" => "user", "content" => text} when is_binary(text) -> text
        _ -> nil
      end)

    if is_binary(previous) and
         (Regex.match?(~r/^\s*what\s+is\b/iu, previous) or
            Regex.match?(~r/^\s*(?:that|this)\s+is\s+not\s+where\b/iu, previous)) do
      case named_geographic_reference(previous, session_id) do
        %{antecedent: place} -> place
        _ -> nil
      end
    end
  end

  defp match_known_geographic_entity(_session_id, nil), do: nil

  defp match_known_geographic_entity(session_id, candidate) when is_binary(candidate) do
    wanted = normalize_place(candidate)

    session_id
    |> LlmChatHistory.messages(@lookback_turns)
    |> Enum.reverse()
    |> Enum.find_value(fn
      %{"role" => "user", "content" => text} when is_binary(text) ->
        case EventFrames.location_question(text) do
          %{relation: :proximity, antecedent_source: :explicit_user_question,
            subject: subject, target: target} ->
            Enum.find([subject, target], fn place ->
              normalize_place(place) == wanted or
                normalize_place(place |> String.split(",", parts: 2) |> hd()) == wanted
            end)

          _ -> nil
        end

      _ -> nil
    end)
  end

  defp normalize_place(text) do
    text
    |> String.trim()
    |> String.downcase()
    |> String.replace(~r/[^\p{L}\p{N}]+/u, " ")
    |> String.trim()
  end

  defp followup_pronoun(sentence) do
    cond do
      Regex.match?(~r/^\s*(?:what|how)\s+(?:is|was)\s+its\b/iu, sentence) ->
        "its"

      Regex.match?(~r/^\s*(?:is|was|does|did|can|could|will|would)\s+it\b/iu, sentence) or
          Regex.match?(~r/^\s*(?:no[,]?\s+it\s+is\s+not[.!]?\s+)?how\s+far\s+is\s+it\s+from\b/iu, sentence) ->
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
