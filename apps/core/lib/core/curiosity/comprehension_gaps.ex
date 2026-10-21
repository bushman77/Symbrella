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
  Resolve a clarification candidate from the just-completed, same-session
  SemanticInput. WM is a transient attention cache, not an authoritative
  prerequisite for feedback on an actual user turn. No database or outside
  information is consulted, and no lexical sense is promoted to fact.

  Only explicitly user-originated SI is eligible. A caller-provided WM
  projection is preserved as diagnostic fallback when no completed-turn
  candidate exists. A session mismatch always fails closed.
  """
  @spec for_observed_turn(term(), map(), term()) :: {:ok, map()} | :none
  def for_observed_turn(session_id, si, wm_gap \\ :none)

  def for_observed_turn(session_id, %{} = si, wm_gap)
      when is_binary(session_id) and byte_size(session_id) > 2 do
    if field(si, :session_id) == session_id do
      case from_completed_turn(si, session_id) do
        {:ok, _} = current -> current
        :none -> if(match?({:ok, %{}}, wm_gap), do: wm_gap, else: :none)
      end
    else
      :none
    end
  end

  def for_observed_turn(_, _, _), do: :none

  @doc """
  A pure, unverified current-turn review projection. The caller must supply
  the actual completed SemanticInput; this does not recover old chat history.
  Its source is never labeled as WM or dictionary/source verification.
  """
  @spec from_completed_turn(term(), term()) :: {:ok, map()} | :none
  def from_completed_turn(%{} = si, session_id) when not is_nil(session_id) do
    summary = field(si, :comprehension)
    sentence = field(si, :sentence)
    unresolved = field(summary, :unresolved)
    uncertain = field(summary, :uncertain)
    source = field(si, :source)
    origin = field(si, :experience_origin)

    with true <- field(si, :session_id) == session_id,
         true <- source == :user or (source == :prod and origin == :user_turn),
         true <- is_binary(sentence) and byte_size(sentence) > 0,
         true <- is_map(summary) and field(summary, :degraded?) == true,
         true <- is_list(unresolved) and
                   Enum.any?(unresolved, &(is_binary(&1) and String.trim(&1) != "")),
         true <- is_list(uncertain) and uncertain != [] do
      interpreted = field(field(si, :sentence_meaning), :status)
      next_action =
        if String.contains?(sentence, "?"),
          do: :inspect_comprehension,
          else: :consider_user_clarification

      {:ok, %{
        kind: :comprehension_gap,
        status: :review_required,
        source: :completed_semantic_input,
        verification: :conversation_context_only,
        reason: if(interpreted == :not_yet_interpreted,
          do: :uninterpreted_language, else: :unresolved_lexical_meaning),
        next_action: next_action,
        uncertain_count: min(length(uncertain), 6),
        unresolved_count: min(length(unresolved), 6),
        understood_count: length(List.wrap(field(summary, :understood))),
        interpretation_status: interpreted,
        age_ms: 0,
        factual_authority?: false,
        investigation_authorized?: false
      }}
    else
      _ -> :none
    end
  end

  def from_completed_turn(_, _), do: :none

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
