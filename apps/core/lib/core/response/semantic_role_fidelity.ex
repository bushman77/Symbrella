defmodule Core.Response.SemanticRoleFidelity do
  @moduledoc """
  Read-only, deliberately narrow role-fidelity audit of generated answers.

  Source authority is an independently revalidated user SVO frame from
  ProjectedExpression, never the response itself. The response is NOT
  passed through the production LIFG/ATL/hippocampal pipeline.

  A syntactically explicit, standalone generated subject-action-object
  assertion may be compared against the current user report. The
  conservative check calls a high-confidence drift only when the model
  explicitly presents a *different additional action and patient* as
  the reply to the current report, or reverses the original actor and
  patient. Other apparent lexical changes are diagnostic-only; the
  detector cannot adjudicate synonyms or complex sentence meaning.

  No dictionary winner, episodic record, WM entry, or synapse is changed.
  """

  @max_draft_bytes 360

  # A single main finite-present clause, or a clearly stated progressive
  # action. The optional "which ..." relative describes its direct object;
  # arbitrary continuations, quotes, conjunctions and second sentences do
  # not qualify for automatic correction. Named captures are diagnostic
  # surface spans, not a newly validated dictionary sense.
  @noun_phrase "(?:the|a|an)\\s+[\\p{L}][\\p{L}'-]{1,46}"
  @tail "(?:[.!]?|,\\s+which\\b[^.!?]{0,140}[.!]?)"
  defp regex_progressive_pattern, do: Regex.compile!(
    "\\A(?<actor>#{@noun_phrase})\\s+" <>
      "(?:is|are)\\s+(?<also>also\\s+)?" <>
      "(?<predicate>[\\p{L}]{3,36}ing)\\s+" <>
      "(?<patient>#{@noun_phrase})(?<tail>#{@tail})\\z",
    "iu"
  )
  defp regex_finite_pattern, do: Regex.compile!(
    "\\A(?<actor>#{@noun_phrase})\\s+" <>
      "(?<predicate>[\\p{L}]{3,36})\\s+" <>
      "(?<patient>#{@noun_phrase})(?<tail>#{@tail})\\z",
    "iu"
  )

  @auxiliaries ~w(is are was were be been being has have had
                  do does did will would can could may might must
                  should shall)

  @spec audit(String.t(), String.t(), map() | nil) :: map()
  def audit(user_text, draft, %{
        kind: :user_reported_action,
        evidence_level: :structurally_interpreted_not_verified,
        sentence: original,
        actor: actor, predicate_surface: surface,
        predicate_lemma: base, patient: patient,
        event_occurrence_verified?: false,
        exact_verb_sense_verified?: false
      })
      when is_binary(user_text) and is_binary(draft) and
        is_binary(original) and is_binary(actor) and
        is_binary(patient) and is_binary(surface) and is_binary(base) do
    cond do
      normalized(original) != normalized(user_text) ->
        no_check(:source_does_not_match)

      byte_size(draft) > @max_draft_bytes or
          String.contains?(draft, ["\"", "“", "”", "?", "\n", ";", ":"]) ->
        no_check(:complex_or_quoted_draft)

      true ->
        case candidate(draft) do
          {:ok, %{actor: observed_actor, predicate: observed_verb,
                  patient: observed_patient, form: form} = parsed} ->
            mismatches =
              []
              |> mismatch(:actor, observed_actor, actor)
              |> predicate_mismatch(form, observed_verb, surface, base)
              |> mismatch(:patient, observed_patient, patient)
              |> Enum.reverse()

            if role_substitution?(mismatches, parsed, actor, patient) do
              %{
                status: :role_substitution_detected,
                mismatched_roles: mismatches,
                source: :revalidated_sentence_roles,
                draft_scope: :standalone_main_clause,
                candidate_form: form,
                explicitly_claims_additional_action?: parsed.continuity_claim?,
                event_truth_verified?: false,
                exact_sense_verified?: false,
                modified_cognition?: false
              }
            else
              %{
                status: if(length(mismatches) >= 2,
                  do: :possible_role_drift,
                  else: :no_high_confidence_role_drift),
                mismatched_roles: mismatches,
                source: :revalidated_sentence_roles,
                draft_scope: :standalone_main_clause,
                event_truth_verified?: false,
                exact_sense_verified?: false,
                modified_cognition?: false
              }
            end

          :error -> no_check(:draft_outside_supported_syntax)
        end
    end
  end

  def audit(_, _, _), do: no_check(:missing_verified_source_frame)

  # Lexical mismatch is not semantic contradiction: cooking a meal
  # versus preparing dinner is left unresolved. A confident, automatic
  # repair requires an explicit "also" continuation replacing the
  # current predicate and patient for the SAME actor, or an exact role
  # reversal with the original surface noun phrases.
  defp role_substitution?(mismatches, parsed, actor, patient) do
    changed_additional_action? =
      parsed.continuity_claim? and
        :actor not in mismatches and
        :predicate in mismatches and :patient in mismatches

    reversed_roles? =
      :actor in mismatches and :patient in mismatches and
        normalized(parsed.actor) == normalized(patient) and
        normalized(parsed.patient) == normalized(actor)

    changed_additional_action? or reversed_roles?
  end

  defp candidate(draft) do
    text = String.trim(draft)

    case Regex.named_captures(regex_progressive_pattern(), text) do
      %{"actor" => actor, "predicate" => predicate,
        "patient" => patient} = captures ->
        {:ok, %{actor: actor, predicate: predicate, patient: patient,
                form: :progressive,
                continuity_claim?:
                  String.trim(Map.get(captures, "also") || "") != ""}}

      _ ->
        case Regex.named_captures(regex_finite_pattern(), text) do
          %{"actor" => actor, "predicate" => predicate, "patient" => patient} ->
            if String.downcase(predicate) in @auxiliaries do
              :error
            else
              {:ok, %{actor: actor, predicate: predicate,
                      patient: patient, form: :finite,
                      continuity_claim?: false}}
            end

          _ -> :error
        end
    end
  end

  defp mismatch(acc, key, actual, expected) do
    if normalized(actual) == normalized(expected),
      do: acc, else: [key | acc]
  end

  defp predicate_mismatch(acc, :finite, actual, surface, _base) do
    mismatch(acc, :predicate, actual, surface)
  end

  defp predicate_mismatch(acc, :progressive, actual, _surface, base) do
    if String.downcase(actual) in progressive_forms(base),
      do: acc, else: [:predicate | acc]
  end

  defp progressive_forms(base) do
    base = String.downcase(base)

    if Regex.match?(~r/\A[a-z]{3,32}\z/u, base) do
      cond do
        String.ends_with?(base, "ie") ->
          [String.replace_suffix(base, "ie", "ying")]

        String.ends_with?(base, "ee") ->
          [base <> "ing"]

        String.ends_with?(base, "e") ->
          [String.replace_suffix(base, "e", "ing")]

        true ->
          [base <> "ing"]
      end
    else
      []
    end
  end

  defp normalized(value) do
    value
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/[.!?]+\z/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp no_check(reason) do
    %{
      status: :not_adjudicated, reason: reason,
      mismatched_roles: [], source: :revalidated_sentence_roles,
      event_truth_verified?: false, exact_sense_verified?: false,
      modified_cognition?: false
    }
  end
end
