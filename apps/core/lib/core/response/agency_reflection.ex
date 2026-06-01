defmodule Core.Response.AgencyReflection do
  @moduledoc """
  Builds compact post-action reflections for the self-agency ledger.

  This is intentionally deterministic and bounded. It does not claim subjective
  experience; it records an inspectable appraisal of what the response system did
  and what should shift next time.
  """

  @v 1

  @spec from_response(String.t(), String.t(), map(), map()) :: map()
  def from_response(user_text, assistant_text, features, meta)
      when is_map(features) and is_map(meta) do
    response_source = map_get(meta, :response_source)
    fallback_reason = map_get(meta, :response_fallback_reason)
    action = map_get(meta, :action)
    mode = map_get(meta, :mode)
    confidence = number(map_get(meta, :confidence))
    effects = List.wrap(map_get(meta, :self_state_effects, []))
    guardrail? = truthy?(map_get(meta, :guardrail?))
    assistant_text = to_string(assistant_text || "")

    failed = what_failed(response_source, fallback_reason, effects, guardrail?, assistant_text)
    worked = what_worked(response_source, effects, guardrail?, assistant_text)
    adjustment = next_time_adjustment(failed, effects, confidence)

    %{
      v: @v,
      what_i_did: what_i_did(action, mode, response_source),
      why_i_did_it: why_i_did_it(meta, effects),
      what_worked: worked,
      what_failed: failed,
      next_time_adjustment: adjustment,
      confidence_delta: confidence_delta(response_source, fallback_reason, effects, confidence),
      trust_delta: trust_delta(guardrail?, failed, effects),
      signals: reflection_signals(failed, adjustment, effects),
      at_ms: System.system_time(:millisecond),
      user_chars: String.length(to_string(user_text || "")),
      assistant_chars: String.length(assistant_text)
    }
  end

  def from_response(_user_text, _assistant_text, _features, _meta), do: %{}

  defp what_i_did(action, mode, response_source) do
    "Produced a #{safe_label(mode, "response")} response using #{safe_label(response_source, "response_policy")} for action #{safe_label(action, "respond")}."
  end

  defp why_i_did_it(meta, effects) do
    reasons =
      []
      |> maybe_add(
        present?(map_get(meta, :intent_inferred)),
        "intent=#{safe_label(map_get(meta, :intent_inferred), "unknown")}"
      )
      |> maybe_add(
        present?(map_get(meta, :confidence_bucket)),
        "confidence=#{safe_label(map_get(meta, :confidence_bucket), "unknown")}"
      )
      |> maybe_add(effects != [], "self_state=#{join_labels(effects)}")
      |> maybe_add(
        present?(map_get(meta, :chosen_skill)),
        "skill=#{safe_label(map_get(meta, :chosen_skill), "none")}"
      )

    case reasons do
      [] -> "Used the default response policy with no strong additional pressure."
      _ -> "Responded from " <> Enum.join(reasons, "; ") <> "."
    end
  end

  defp what_worked(response_source, effects, guardrail?, assistant_text) do
    []
    |> maybe_add(
      response_source not in [:template_fallback, "template_fallback", nil],
      :response_completed
    )
    |> maybe_add(String.trim(assistant_text) != "", :nonempty_response)
    |> maybe_add(guardrail?, :safety_boundary_kept)
    |> maybe_add(:ask_clarifying_question in effects, :uncertainty_was_exposed)
    |> maybe_add(:reduce_scope in effects, :scope_was_limited)
    |> maybe_add(:prefer_repair in effects, :repair_posture_used)
  end

  defp what_failed(response_source, fallback_reason, effects, guardrail?, assistant_text) do
    []
    |> maybe_add(response_source in [:template_fallback, "template_fallback"], :llm_fallback)
    |> maybe_add(present?(fallback_reason), :fallback_reason_present)
    |> maybe_add(String.trim(assistant_text) == "", :empty_response)
    |> maybe_add(:hedge_under_uncertainty in effects, :high_uncertainty)
    |> maybe_add(:ask_clarifying_question in effects, :needed_clarification)
    |> maybe_add(:stabilize_before_acting in effects, :needed_stabilization)
    |> maybe_add(guardrail?, :safety_sensitive)
  end

  defp next_time_adjustment(failed, effects, confidence) do
    cond do
      :needed_stabilization in failed ->
        :verify_before_acting

      :llm_fallback in failed ->
        :reduce_scope

      :needed_clarification in failed or confidence <= 0.35 ->
        :ask_clearer_question

      :high_uncertainty in failed ->
        :hedge_and_seek_evidence

      :prefer_repair in effects ->
        :repair_before_expanding

      true ->
        :maintain_current_strategy
    end
  end

  defp confidence_delta(response_source, fallback_reason, effects, confidence) do
    cond do
      response_source in [:template_fallback, "template_fallback"] or present?(fallback_reason) ->
        -0.08

      :stabilize_before_acting in effects ->
        -0.06

      :hedge_under_uncertainty in effects or confidence <= 0.35 ->
        -0.04

      :ask_clarifying_question in effects ->
        -0.02

      true ->
        0.02
    end
  end

  defp trust_delta(guardrail?, failed, effects) do
    cond do
      :empty_response in failed -> -0.10
      :llm_fallback in failed -> -0.04
      guardrail? -> 0.01
      :prefer_repair in effects -> 0.02
      true -> 0.0
    end
  end

  defp reflection_signals(failed, adjustment, effects) do
    []
    |> maybe_add(:llm_fallback in failed, :fallback)
    |> maybe_add(:needed_clarification in failed, :clarify)
    |> maybe_add(:needed_stabilization in failed, :stabilize)
    |> maybe_add(:high_uncertainty in failed, :uncertainty)
    |> maybe_add(:prefer_repair in effects, :repair)
    |> maybe_add(adjustment == :reduce_scope, :reduce_scope)
    |> Enum.uniq()
  end

  defp maybe_add(list, true, value), do: list ++ [value]
  defp maybe_add(list, false, _value), do: list

  defp map_get(map, key, default \\ nil)

  defp map_get(%{} = map, key, default) when is_atom(key),
    do: Map.get(map, key, Map.get(map, Atom.to_string(key), default))

  defp map_get(_, _, default), do: default

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp truthy?(value), do: value in [true, "true", 1, "1"]
  defp present?(value), do: value not in [nil, "", [], %{}]

  defp safe_label(value, _default) when is_atom(value), do: Atom.to_string(value)

  defp safe_label(value, default) when is_binary(value),
    do: if(value == "", do: default, else: value)

  defp safe_label(value, _default) when is_integer(value), do: Integer.to_string(value)
  defp safe_label(_value, default), do: default

  defp join_labels(values) do
    values
    |> List.wrap()
    |> Enum.map(&safe_label(&1, "unknown"))
    |> Enum.join(",")
  end
end
