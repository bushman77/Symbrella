defmodule Core.Comprehension.Diagnosis do
  @moduledoc """
  Pure, read-only classification of existing comprehension evidence.

  This observer distinguishes selected lexical candidates, dictionary-verified
  lexical constructions, structural interpretation and consistency auditing.
  It never reranks candidates, retries composition, changes memory, or treats
  an interpretation as fact. Its output contains only bounded counters,
  enumerated statuses and rates; no user text, lexical IDs or definitions.
  """

  @reasons [
    :no_lifg_choices, :weak_decision_rate_high, :fallback_rate_high,
    :unresolved_content_tokens
  ]
  @structural_statuses [
    :preferred, :unresolved, :no_complete_candidate, :ambiguous
  ]
  @meaning_statuses [
    :not_yet_interpreted, :provisional, :structurally_interpreted
  ]
  @audit_statuses [:consistent, :inconsistent, :unassessed]
  @feedback_modes [:bounded_reanalysis, :observe_only]

  @doc "Bounded diagnostic projection from the current SemanticInput."
  @spec assess(map()) :: map()
  def assess(%{comprehension: %{} = summary} = si) do
    reasons =
      summary
      |> Map.get(:reasons, [])
      |> list()
      |> Enum.filter(&(&1 in @reasons))
      |> Enum.uniq()
      |> Enum.take(4)

    stats = Map.get(summary, :stats)
    stats = if is_map(stats), do: stats, else: %{}
    reported_feedback = Map.get(si, :lifg_feedback)
    feedback = if is_map(reported_feedback), do: reported_feedback, else: %{}

    degraded? = Map.get(summary, :degraded?) == true

    %{
      reasons: reasons,
      choice_count: bound_count(Map.get(stats, :total)),
      weak_rate: rate(Map.get(stats, :weak_rate)),
      fallback_rate: rate(Map.get(stats, :fallback_rate)),
      unresolved_content_count: bound_count(Map.get(stats, :unresolved_content)),
      verified_lexical_count: count(Map.get(summary, :lexical_meanings)),
      structural_status:
        observed_status(Map.get(si, :structural_competition), @structural_statuses),
      sentence_meaning_status:
        observed_status(Map.get(si, :sentence_meaning), @meaning_statuses),
      integration_status:
        observed_status(Map.get(si, :integration_audit), @audit_statuses),
      feedback_mode: observed_status(feedback, @feedback_modes, :mode),
      feedback_applied:
        if(is_map(reported_feedback) and Map.has_key?(feedback, :applied?),
          do: Map.get(feedback, :applied?) == true, else: :not_observed),
      repair_hint: repair_hint(degraded?, reasons),
      curiosity_gap_gate: gap_gate(si, summary, degraded?)
    }
  end

  def assess(_), do: %{}

  defp repair_hint(false, _), do: :not_indicated
  defp repair_hint(true, reasons) do
    cond do
      :no_lifg_choices in reasons -> :review_missing_choices
      :unresolved_content_tokens in reasons -> :review_unresolved_content
      Enum.any?(reasons, &(&1 in [:weak_decision_rate_high, :fallback_rate_high])) ->
        :review_lexical_evidence
      true -> :reason_not_observed
    end
  end

  # Mirrors the conservative current-turn gate in
  # Core.Curiosity.ComprehensionGaps.from_completed_turn/2. This is an audit of
  # its prerequisites, NOT a request to generate curiosity or clarification.
  defp gap_gate(si, summary, degraded?) do
    cond do
      not degraded? -> :not_degraded
      count(Map.get(summary, :uncertain)) == 0 -> :needs_uncertainty
      count(Map.get(summary, :unresolved)) == 0 -> :needs_unresolved_content
      is_nil(Map.get(si, :session_id)) -> :needs_session
      not user_turn?(si) -> :needs_user_origin
      not nonempty_sentence?(Map.get(si, :sentence)) -> :needs_input
      true -> :eligible_for_review
    end
  end

  defp user_turn?(%{source: :user}), do: true
  defp user_turn?(%{source: :prod, experience_origin: :user_turn}), do: true
  defp user_turn?(_), do: false

  defp nonempty_sentence?(s) when is_binary(s), do: byte_size(s) > 0
  defp nonempty_sentence?(_), do: false

  defp observed_status(value, allowed), do: observed_status(value, allowed, :status)

  defp observed_status(%{} = value, allowed, key) do
    status = Map.get(value, key)
    if status in allowed, do: status, else: :not_observed
  end

  defp observed_status(_, _, _), do: :not_observed

  defp rate(n) when is_number(n) and n >= 0.0 and n <= 1.0,
    do: Float.round(n * 1.0, 6)
  defp rate(_), do: nil

  defp bound_count(n) when is_integer(n) and n >= 0 and n <= 10_000, do: n
  defp bound_count(_), do: nil

  defp count(values) when is_list(values), do: min(length(values), 10_000)
  defp count(_), do: 0

  defp list(values) when is_list(values), do: values
  defp list(_), do: []
end
