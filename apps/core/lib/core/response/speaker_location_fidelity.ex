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
  @direct_correction ~r/^\s*(?:no[,!]?\s*)?(?:you\s+are\s+not|you're\s+not|you\s+aren't|that's\s+(?:not\s+true|wrong)|that\s+is\s+(?:not\s+true|wrong))\s*[.!]*\s*$/iu

  # A positive assertion of physical location. The place phrase begins with
  # a proper-name token, so "I'm in no position to say" is not captured.
  # This intentionally covers straightforward claims, not semantic entailment.
  @first_person_location ~r/\b(?:i\s+am|i'm)\s+(?:(?:currently|physically|actually|right\s+now)\s+)?(?:(?:located|based|situated)\s+)?(?:in|at|near)\s+(?:the\s+)?[A-Z][\p{L}\p{M}'-]*(?:[\s,]+[A-Z][\p{L}\p{M}'-]*){0,5}\b/u
  @second_person_location ~r/\b(?:you\s+are|you're)\s+(?:(?:currently|physically|actually|right\s+now)\s+)?(?:(?:located|based|situated)\s+)?(?:in|at|near)\s+(?:the\s+)?[A-Z][\p{L}\p{M}'-]*(?:[\s,]+[A-Z][\p{L}\p{M}'-]*){0,5}\b/u
  @location_is ~r/\b(?:your|my)\s+(?:current\s+)?(?:physical\s+)?location\s+is\s+(?:in\s+)?[A-Z][\p{L}\p{M}'-]*(?:[\s,]+[A-Z][\p{L}\p{M}'-]*){0,5}\b/u

  @type issue ::
          :user_current_location_unverified
          | :assistant_current_location_unverified
          | :correction_repeats_self_location

  @spec issue(String.t(), String.t(), map()) :: issue() | nil
  def issue(user_text, draft, context)
      when is_binary(user_text) and is_binary(draft) and is_map(context) do
    cond do
      correction_to_unsupported_self_location?(user_text, context) and
          affirmative_first_person_location?(draft) ->
        :correction_repeats_self_location

      Regex.match?(@user_location_question, user_text) and
          affirmative_location?(draft) ->
        :user_current_location_unverified

      Regex.match?(@assistant_location_question, user_text) and
          affirmative_first_person_location?(draft) ->
        :assistant_current_location_unverified

      true ->
        nil
    end
  end

  def issue(_, _, _), do: nil

  @spec repair(issue()) :: String.t()
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

  defp affirmative_location?(draft) do
    affirmative_first_person_location?(draft) or
      Regex.match?(@second_person_location, draft) or
      Regex.match?(@location_is, draft)
  end

  defp affirmative_first_person_location?(draft) do
    Regex.match?(@first_person_location, draft) and
      not Regex.match?(~r/\b(?:i\s+am|i'm)\s+not\s+(?:currently\s+)?(?:located\s+)?(?:in|at|near)\b/iu, draft)
  end
end
