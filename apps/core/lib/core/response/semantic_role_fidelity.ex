defmodule Core.Response.SemanticRoleFidelity do
  @moduledoc """
  Read-only, deliberately narrow role-fidelity audit of generated answers.

  Source authority is an independently revalidated user SVO frame from
  ProjectedExpression, never the response itself. The response is NOT
  passed through the production LIFG/ATL/hippocampal pipeline.

  A syntactically explicit, standalone generated subject-action-object
  assertion may be compared against the current user report. The
  conservative check only calls a drift when the model asserts a *new*
  predicate AND patient for the same actor, or substitutes at least two
  explicitly bound participants. The detector cannot adjudicate synonyms
  or complex sentence meaning; all other forms abstain.

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
  @progressive_pattern Regex.compile!(
    "\\A(?<actor>#{@noun_phrase})\\s+" <>
      "(?:is|are)\\s+(?<also>also\\s+)?" <>
      "(?<predicate>[\\p{L}]{3,36}ing)\\s+" <>
      "(?<patient>#{@noun_phrase})(?<tail>#{@tail})\\z",
    "iu"
  )
  @finite_pattern Regex.compile!(
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
                  patient: observed_patient, form: form}} ->
            mismatches =
              []
              |> mismatch(:actor, observed_actor, actor)
              |> predicate_mismatch(form, observed_verb, surface, base)
              |> mismatch(:patient, observed_patient, patient)
              |> Enum.reverse()

            if role_substitution?(mismatches) do
              %{
                status: :role_substitution_detected,
                mismatched_roles: mismatches,
                source: :revalidated_sentence_roles,
                draft_scope: :standalone_main_clause,
                candidate_form: form,
                event_truth_verified?: false,
                exact_sense_verified?: false,
                modified_cognition?: false
              }
            else
              %{
                status: :no_high_confidence_role_drift,
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

  # Never automatically rewrite a different actor's otherwise coherent
  # account, or a semantic paraphrase changing just one lexical role.
  # At least two independently bound roles must disagree, including a
  # changed action or a full actor/patient reversal.
  defp role_substitution?(mismatches) do
    length(mismatches) >= 2 and
      (:predicate in mismatches or
         (:actor in mismatches and :patient in mismatches))
  end

  defp candidate(draft) do
    text = String.trim(draft)

    case Regex.named_captures(@progressive_pattern, text) do
      %{"actor" => actor, "predicate" => predicate, "patient" => patient} ->
        {:ok, %{actor: actor, predicate: predicate,
                patient: patient, form: :progressive}}

      _ ->
        case Regex.named_captures(@finite_pattern, text) do
          %{"actor" => actor, "predicate" => predicate, "patient" => patient} ->
            if String.downcase(predicate) in @auxiliaries do
              :error
            else
              {:ok, %{actor: actor, predicate: predicate,
                      patient: patient, form: :finite}}
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
