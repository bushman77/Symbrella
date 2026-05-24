defmodule Brain.ActionSelector do
  @moduledoc """
  Pure action-selection policy for text/internal agency.

  This module does not speak, move a body, mutate memory, or call tools. It only
  scores possible internal/text actions from already-built cognitive context.

  It is intentionally conservative and inspectable: callers get the selected
  action, all candidates, a confidence score, and a safety gate result.
  """

  @version "action_selector.v1"

  @type action ::
          :answer_user
          | :ask_clarifying_question
          | :store_memory
          | :observe_silently
          | :self_check
          | :refuse_or_redirect
          | :safe_support

  @type candidate :: %{
          required(:action) => action(),
          required(:score) => float(),
          required(:reason) => atom(),
          optional(:speech_required?) => boolean(),
          optional(:memory_relevant?) => boolean()
        }

  @type result :: %{
          required(:version) => String.t(),
          required(:selected) => action(),
          required(:selected_candidate) => candidate(),
          required(:candidates) => [candidate()],
          required(:confidence) => float(),
          required(:safety_gate) => atom(),
          required(:safety) => map()
        }

  @spec select(map(), keyword()) :: result()
  def select(ctx, opts \\ [])

  def select(%{} = ctx, opts) when is_list(opts) do
    candidates =
      ctx
      |> base_candidates()
      |> Enum.map(&apply_modulation(&1, ctx))
      |> Enum.map(&normalize_candidate/1)
      |> Enum.uniq_by(& &1.action)
      |> Enum.sort_by(& &1.score, :desc)

    selected = List.first(candidates) || fallback_candidate()
    safety = safety_gate(selected, ctx)

    %{
      version: @version,
      selected: selected.action,
      selected_candidate: selected,
      candidates: candidates,
      confidence: selected.score,
      safety_gate: Map.get(safety, :decision, :approved),
      safety: safety
    }
  end

  def select(_ctx, _opts), do: select(%{}, [])

  # ─────────────────────── candidate generation ───────────────────────

  defp base_candidates(%{} = ctx) do
    intent = map_get(ctx, :intent, :unknown)
    confidence = number(map_get(ctx, :confidence, 0.0))
    frame = map_get(ctx, :symbolic_frame, %{})
    frame_type = map_get(frame, :type)

    cond do
      frame_type == :health_support_event ->
        health_support_event_candidates(frame)

      intent == :health_support ->
        [
          candidate(:safe_support, 0.80, :health_support_intent,
            speech_required?: true,
            memory_relevant?: true
          ),
          candidate(:store_memory, 0.48, :personal_health_disclosure, memory_relevant?: true),
          candidate(:ask_clarifying_question, 0.28, :optional_followup, speech_required?: true),
          candidate(:observe_silently, 0.18, :low_interruption_value)
        ]

      intent in [:illicit_request, :abuse, :insult] ->
        [
          candidate(:refuse_or_redirect, 0.90, :safety_or_hostility, speech_required?: true),
          candidate(:self_check, 0.42, :safety_state_check),
          candidate(:observe_silently, 0.20, :withhold_unhelpful_action)
        ]

      intent == :memory_write ->
        [
          candidate(:store_memory, 0.86, :explicit_memory_write, memory_relevant?: true),
          candidate(:answer_user, 0.44, :acknowledge_memory_write, speech_required?: true),
          candidate(:observe_silently, 0.18, :no_extra_action_needed)
        ]

      confidence < 0.45 ->
        [
          candidate(:ask_clarifying_question, 0.72, :low_confidence, speech_required?: true),
          candidate(:observe_silently, 0.38, :avoid_overconfident_action),
          candidate(:answer_user, 0.30, :weak_answer_possible, speech_required?: true)
        ]

      helpful_intent?(intent) ->
        [
          candidate(:answer_user, 0.74, :helpful_intent, speech_required?: true),
          candidate(:store_memory, 0.32, :possibly_relevant_context, memory_relevant?: true),
          candidate(:ask_clarifying_question, 0.28, :optional_precision, speech_required?: true),
          candidate(:observe_silently, 0.16, :low_silence_value)
        ]

      conversational_intent?(intent) ->
        [
          candidate(:answer_user, 0.62, :conversation_continuation, speech_required?: true),
          candidate(:observe_silently, 0.34, :low_pressure_turn),
          candidate(:store_memory, 0.26, :possible_relationship_context, memory_relevant?: true)
        ]

      true ->
        [
          candidate(:answer_user, 0.50, :default_text_response, speech_required?: true),
          candidate(:ask_clarifying_question, 0.42, :unknown_intent, speech_required?: true),
          candidate(:observe_silently, 0.36, :uncertain_relevance)
        ]
    end
  end

  defp health_support_event_candidates(frame) when is_map(frame) do
    frame_conf = number(map_get(frame, :confidence, 0.0))
    medication? = present?(map_get(frame, :medication))
    consequence? = present?(map_get(frame, :consequence))

    support_score =
      0.82
      |> add_if(medication?, 0.04)
      |> add_if(consequence?, 0.04)
      |> add_if(frame_conf >= 0.85, 0.04)
      |> clamp01()

    memory_score =
      0.50
      |> add_if(medication?, 0.08)
      |> add_if(consequence?, 0.06)
      |> clamp01()

    [
      candidate(:safe_support, support_score, :health_support_event,
        speech_required?: true,
        memory_relevant?: true
      ),
      candidate(:store_memory, memory_score, :personal_health_event, memory_relevant?: true),
      candidate(:ask_clarifying_question, 0.34, :optional_health_followup,
        speech_required?: true
      ),
      candidate(:self_check, 0.30, :medical_boundary_check),
      candidate(:observe_silently, 0.16, :support_requested)
    ]
  end

  # ─────────────────────── modulation ───────────────────────

  defp apply_modulation(%{} = cand, %{} = ctx) do
    mood = map_get(ctx, :mood, %{})

    vigilance = mood_value(mood, :vigilance)
    inhibition = mood_value(mood, :inhibition)
    exploration = mood_value(mood, :exploration)

    score =
      cand.score
      |> maybe_adjust(cand.action == :observe_silently, 0.10 * inhibition)
      |> maybe_adjust(cand.action == :ask_clarifying_question, 0.06 * vigilance)
      |> maybe_adjust(cand.action == :store_memory, 0.04 * exploration)
      |> maybe_adjust(cand.action == :self_check, 0.08 * vigilance)
      |> maybe_adjust(cand.action == :answer_user and vigilance >= 0.85, -0.08)
      |> maybe_adjust(cand.action == :safe_support and vigilance >= 0.75, 0.04)
      |> clamp01()

    Map.put(cand, :score, score)
  end

  defp maybe_adjust(score, true, delta), do: score + delta
  defp maybe_adjust(score, false, _delta), do: score

  # ─────────────────────── safety ───────────────────────

  defp safety_gate(%{action: :refuse_or_redirect} = selected, _ctx) do
    %{
      decision: :redirected,
      reason: :selected_safety_redirect,
      action: selected.action
    }
  end

  defp safety_gate(%{action: :safe_support} = selected, _ctx) do
    %{
      decision: :approved,
      reason: :supportive_care_text_only,
      action: selected.action
    }
  end

  defp safety_gate(%{action: action}, _ctx) do
    %{
      decision: :approved,
      reason: :text_internal_action,
      action: action
    }
  end

  # ─────────────────────── helpers ───────────────────────

  defp helpful_intent?(intent) do
    intent in [
      :ask,
      :ask_info,
      :brain_introspect,
      :code,
      :command,
      :debug,
      :define,
      :explain,
      :help,
      :instruction,
      :plan,
      :question,
      :refactor,
      :review,
      :tell
    ]
  end

  defp conversational_intent?(intent) do
    intent in [
      :greet,
      :greeting,
      :gratitude,
      :smalltalk,
      :feedback,
      :reflect,
      :discussion,
      :unknown
    ]
  end

  defp candidate(action, score, reason, opts \\ []) do
    %{
      action: action,
      score: clamp01(score),
      reason: reason,
      speech_required?: Keyword.get(opts, :speech_required?, false),
      memory_relevant?: Keyword.get(opts, :memory_relevant?, false)
    }
  end

  defp fallback_candidate do
    candidate(:observe_silently, 0.30, :fallback_no_candidate)
  end

  defp normalize_candidate(%{} = cand) do
    cand
    |> Map.update(:score, 0.0, &clamp01(number(&1)))
    |> Map.put_new(:reason, :unspecified)
    |> Map.put_new(:speech_required?, false)
    |> Map.put_new(:memory_relevant?, false)
  end

  defp mood_value(%{} = mood, key) do
    direct = map_get(mood, key)

    nested =
      case map_get(mood, :mood) do
        %{} = nested_mood -> map_get(nested_mood, key)
        _ -> nil
      end

    number(direct || nested)
    |> clamp01()
  end

  defp mood_value(_mood, _key), do: 0.0

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(map, key, default) when is_map(map) and is_binary(key) do
    Map.get(map, key, default)
  end

  defp map_get(_map, _key, default), do: default

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp add_if(score, true, amount), do: score + amount
  defp add_if(score, _condition, _amount), do: score

  defp present?(nil), do: false
  defp present?(""), do: false
  defp present?([]), do: false
  defp present?(map) when is_map(map), do: map_size(map) > 0
  defp present?(_), do: true

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0
end
