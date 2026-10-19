defmodule Core.Response.SpeakerLocationFidelity do
  @moduledoc """
  Read-only attribution review for *current physical location*.

  A public place identity ("Kamloops is in British Columbia") is not
  evidence that either conversational participant is currently there.
  This guard operates on the user's deictic question and a bounded, same-
  session correction target. It does not infer a location from prior user
  prose, assistant prose, dictionary meaning, or verified city coordinates.

  It deliberately does not read external geolocation or modify LIFG, WM,
  hippocampus, or synapses. A valid place-verification result can therefore
  never be repurposed as a person's current-location measurement.
  """

  @user_location_question ~r/^\s*(?:where\s+am\s+i(?:\s+(?:now|right\s+now|currently|atm|at\s+the\s+moment))?|where\s+am\s+i\s+located(?:\s+(?:now|currently|atm))?|what(?:'s|\s+is)\s+my\s+(?:current\s+)?(?:physical\s+)?location|can\s+you\s+tell\s+me\s+where\s+i\s+am)\s*[?.!]*\s*$/iu
  @assistant_location_question ~r/^\s*(?:where\s+are\s+you(?:\s+(?:located|now|right\s+now|currently|atm|at\s+the\s+moment))?|what(?:'s|\s+is)\s+your\s+(?:current\s+)?(?:physical\s+)?location)\s*[?.!]*\s*$/iu
  @assistant_physical_presence_question ~r/^\s*(?:are\s+you|you're)\s+(?:(?:currently|actually)\s+)?(?:(?:physically|located|based|situated)\s+){1,2}(?:in|at|near)\s+[^?!.]+[?.!]*\s*$/iu

  # Some small-model drafts answer a person's location question with only a
  # city/province fragment rather than an explicit "I am in ..." claim.
  # Constrain this to the start of a reply, with an explicit regional marker.
  # Public place facts remain exempt outside participant-location questions.
  @bare_place_reply ~r/\A\s*(?:(?:in|at|near)\s+)?[A-Z][\p{L}\p{M}'-]*(?:\s+[A-Z][\p{L}\p{M}'-]*){0,5}\s*,\s*(?:[A-Z]{2,3}|British Columbia|Alberta|Ontario|Quebec|Canada)\s*(?:[.,!?]|\z)/u
  @echoed_user_location ~r/\bwhere\s+am\s+i(?:\s+(?:now|right\s+now|currently|atm|at\s+the\s+moment))?\s*\?/iu
  @direct_correction ~r/^\s*(?:no[,!]?\s*)?(?:you\s+are\s+not|you're\s+not|you\s+aren't|that's\s+(?:not\s+true|wrong)|that\s+is\s+(?:not\s+true|wrong))\s*[.!]*\s*$/iu

  # A positive assertion of physical location. The place phrase begins with
  # a proper-name token, so "I'm in no position to say" is not captured.
  # This intentionally covers straightforward claims, not semantic entailment.
  @first_person_location ~r/\b(?:i\s+am|i'm)\s+(?:(?:currently|physically|actually|right\s+now)\s+)?(?:(?:located|based|situated)\s+)?(?:in|at|near)\s+(?:the\s+)?[A-Z][\p{L}\p{M}'-]*(?:[\s,]+[A-Z][\p{L}\p{M}'-]*){0,5}\b/iu
  @second_person_location ~r/\b(?:you\s+are|you're)\s+(?:(?:currently|physically|actually|right\s+now)\s+)?(?:(?:located|based|situated)\s+)?(?:in|at|near)\s+(?:the\s+)?[A-Z][\p{L}\p{M}'-]*(?:[\s,]+[A-Z][\p{L}\p{M}'-]*){0,5}\b/iu
  @location_is ~r/\b(?:your|my)\s+(?:current\s+)?(?:physical\s+)?location\s+is\s+(?:in\s+)?[A-Z][\p{L}\p{M}'-]*(?:[\s,]+[A-Z][\p{L}\p{M}'-]*){0,5}\b/iu

  @type issue ::
          :user_current_location_unverified
          | :assistant_current_location_unverified
          | :correction_repeats_self_location
          | :location_correction_self_claim
          | :user_location_question_echo

  @spec issue(String.t(), String.t(), map()) :: issue() | nil
  def issue(user_text, draft, context)
      when is_binary(user_text) and is_binary(draft) and is_map(context) do
    cond do
      correction_to_unsupported_self_location?(user_text, context) and
          affirmative_location?(draft) ->
        :correction_repeats_self_location

      correction_follows_location_question?(user_text, context) and
          affirmative_location?(draft) ->
        :location_correction_self_claim

      Regex.match?(@user_location_question, user_text) and
          affirmative_location?(draft) ->
        :user_current_location_unverified

      Regex.match?(@user_location_question, user_text) and
          echoed_location_question?(draft) ->
        :user_location_question_echo

      (Regex.match?(@assistant_location_question, user_text) or
         Regex.match?(@assistant_physical_presence_question, user_text)) and
          affirmative_location?(draft) ->
        :assistant_current_location_unverified

      true ->
        nil
    end
  end

  def issue(_, _, _), do: nil

  @spec repair(issue()) :: String.t()
  def repair(:user_location_question_echo), do: repair(:user_current_location_unverified)

  def repair(:user_current_location_unverified) do
    "I can't verify your current physical location from the information " <>
      "available in this conversation. A city's geographic coordinates " <>
      "do not establish where you are."
  end

  def repair(:assistant_current_location_unverified) do
    "I don't have verified evidence of my own physical location. " <>
      "A place mentioned in our conversation isn't proof that I'm there."
  end

  def repair(:correction_repeats_self_location) do
    "You're right to question that. My earlier claim about being physically " <>
      "located in a particular city wasn't verified, and I shouldn't repeat it."
  end

  def repair(:location_correction_self_claim) do
    "I don't have verified evidence of either of our current physical " <>
      "locations, and I shouldn't claim to be in a particular city."
  end

  @doc "Inspect only the immediately preceding completed chat pair."
  @spec correction_to_unsupported_self_location?(String.t(), map()) :: boolean()
  def correction_to_unsupported_self_location?(user_text, context)
      when is_binary(user_text) and is_map(context) do
    if Regex.match?(@direct_correction, user_text) do
      case context |> Map.get(:review_history, []) |> List.wrap() |> Enum.take(-2) do
        [%{"role" => "user", "content" => previous_question},
         %{"role" => "assistant", "content" => previous_reply}]
        when is_binary(previous_question) and is_binary(previous_reply) ->
          # A former assistant's city claim is a correction target, NEVER
          # independent evidence that a city is its real-world location.
          (Regex.match?(@user_location_question, previous_question) or
             Regex.match?(@assistant_location_question, previous_question)) and
            affirmative_first_person_location?(previous_reply)

        _ ->
          false
      end
    else
      false
    end
  end

  def correction_to_unsupported_self_location?(_, _), do: false

  defp correction_follows_location_question?(user_text, context) do
    if Regex.match?(@direct_correction, user_text) do
      case context |> Map.get(:review_history, []) |> List.wrap() |> Enum.take(-2) do
        [%{"role" => "user", "content" => previous_question},
         %{"role" => "assistant", "content" => _previous_reply}]
        when is_binary(previous_question) ->
          Regex.match?(@user_location_question, previous_question) or
            Regex.match?(@assistant_location_question, previous_question)

        _ -> false
      end
    else
      false
    end
  end

  defp affirmative_location?(draft) do
    affirmative_first_person_location?(draft) or
      Regex.match?(@second_person_location, draft) or
      Regex.match?(@location_is, draft) or
      Regex.match?(@bare_place_reply, draft)
  end

  defp echoed_location_question?(draft) do
    Regex.match?(@echoed_user_location, draft) and
      not Regex.match?(~r/\b(?:can't|cannot|don't|do not|unable|unverified)\b/iu, draft)
  end

  defp affirmative_first_person_location?(draft) do
    Regex.match?(@first_person_location, draft) and
      not Regex.match?(~r/\b(?:i\s+am|i'm)\s+not\s+(?:currently\s+)?(?:located\s+)?(?:in|at|near)\b/iu, draft)
  end
end
