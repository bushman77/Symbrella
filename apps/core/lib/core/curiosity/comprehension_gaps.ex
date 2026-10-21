defmodule Core.Curiosity.ComprehensionGaps do
  @moduledoc """
  Read-only, session-bound projection of an unresolved language-understanding gap.

  This is *not* a CognitiveIssue, a verified fact, or a symbolic investigation.
  It never writes a curiosity audit, emits a question into chat, retrieves
  evidence, or trains investigative strategy biases. A failed comprehension
  must not masquerade as an independently resolved proof.
  """

  alias Core.Brain.WM.Comprehension

  @max_age_ms 20 * 60 * 1_000

  @doc """
  Inspect the current user's session frame. No global-session fallback.
  Returns :none when Brain/WM is unavailable.
  """
  @spec for_session(term()) :: {:ok, map()} | :none
  def for_session(session_id) when not is_nil(session_id) do
    from_snapshot(Brain.snapshot_wm(), session_id, System.system_time(:millisecond))
  rescue
    _ -> :none
  catch
    :exit, _ -> :none
  end

  def for_session(_), do: :none

  @doc """
  Pure snapshot projection for deterministic tests and offline diagnostics.
  No raw sentence, recognized topics, or unresolved terms leave this boundary.
  """
  @spec from_snapshot(map(), term(), integer()) :: {:ok, map()} | :none
  def from_snapshot(%{wm: items}, session_id, now_ms)
      when is_list(items) and not is_nil(session_id) and is_integer(now_ms) do
    id = Comprehension.frame_id(session_id)

    items
    |> Enum.find(&(field(&1, :id) == id))
    |> candidate(now_ms)
  end

  def from_snapshot(_, _, _), do: :none

  defp candidate(nil, _), do: :none

  defp candidate(item, now_ms) do
    payload = field(item, :payload)
    context = field(payload, :context)
    updated_at_ms = field(context, :updated_at_ms)
    bumped_at_ms = field(item, :last_bump) || field(item, :ts)

    with :comprehension <- field(item, :source),
         :comprehension_frame <- field(payload, :kind),
         :conversation_context_only <- field(context, :verification),
         true <- field(context, :degraded?) == true,
         uncertain when is_list(uncertain) and uncertain != [] <- field(context, :uncertain),
         true <- is_integer(updated_at_ms) and is_integer(bumped_at_ms),
         true <- now_ms >= updated_at_ms and now_ms - updated_at_ms <= @max_age_ms,
         true <- now_ms >= bumped_at_ms and now_ms - bumped_at_ms <= @max_age_ms do
      understood = field(context, :understood)
      status = field(context, :interpretation_status)
      intent = field(context, :intent)

      reason =
        if status == :not_yet_interpreted,
          do: :uninterpreted_language,
          else: :unresolved_lexical_meaning

      # Only a typed review action. No unsolicited clarification, lexical
      # promotion, or web research is allowed from a WM frame alone.
      next_action =
        if intent in [:ask, :question],
          do: :inspect_comprehension,
          else: :consider_user_clarification

      {:ok, %{
        kind: :comprehension_gap,
        status: :review_required,
        source: :session_working_memory,
        verification: :conversation_context_only,
        reason: reason,
        next_action: next_action,
        uncertain_count: min(length(uncertain), 6),
        understood_count: length(List.wrap(understood)),
        interpretation_status: status,
        age_ms: now_ms - updated_at_ms,
        factual_authority?: false,
        investigation_authorized?: false
      }}
    else
      _ -> :none
    end
  end

  defp field(%{} = map, key), do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
  defp field(_, _), do: nil
end
