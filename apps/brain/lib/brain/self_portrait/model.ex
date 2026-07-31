defmodule Brain.SelfPortrait.Model do
  @moduledoc """
  SelfPortrait (pure model) — a compact, symbolic self-model.

  This module is intentionally pure/deterministic:
    * `new/1` creates a portrait
    * `observe/2` updates it given an observation event

  The GenServer wrapper (`Brain.SelfPortrait`) can cache/publish snapshots,
  but tests should primarily target this module.

  Portrait fields:
    * :traits      — slow-moving scalars (0..1) updated via small EWMA steps
    * :patterns    — counters for recurrent issues / behaviors
    * :sources     — counts by event namespace head (e.g., :brain, :core)
    * :last_events — bounded rolling buffer for introspection/debugging

  Supported inputs:
    * Blackboard-style payloads: %{kind: :telemetry, event: [...], measurements: %{}, meta: %{}}
    * Any map with at least :kind / :event.
  """

  @type event :: map()

  @type t :: %{
          version: pos_integer(),
          traits: map(),
          patterns: map(),
          sources: map(),
          last_events: [map()],
          max_events: pos_integer(),
          last_update_ms: non_neg_integer()
        }

  @default_max_events 50

  @spec new(keyword() | map()) :: t()
  def new(opts \\ []) do
    opts_map = Brain.Region.opts_to_map(opts)
    max_events = clamp_int(Map.get(opts_map, :max_events, @default_max_events), 1, 500)

    %{
      version: 1,
      traits: %{
        curiosity_bias: 0.50,
        confidence_baseline: 0.50,
        stability: 0.50,
        novelty_seeking: 0.50,
        risk_aversion: 0.50
      },
      patterns: %{
        wm_updates: 0,
        pmtg_consults: 0,
        no_mwe_senses: 0,
        mwe_fallbacks: 0,
        mwe_compat_misses: 0,
        boundary_drops: 0,
        chargram_violations: 0,
        gate_failures: 0,
        fallback_wins: 0,
        lifg_payload_gaps: 0,
        lifg_pos_anomalies: 0
      },
      sources: %{},
      last_events: [],
      max_events: max_events,
      last_update_ms: now_ms()
    }
  end

  @spec observe(t(), event()) :: t()
  def observe(%{} = portrait, %{} = ev) do
    event = Map.get(ev, :event) || Map.get(ev, "event")
    kind = Map.get(ev, :kind) || Map.get(ev, "kind")

    portrait
    |> bump_source(event, ev)
    |> bump_patterns(kind, event, ev)
    |> bump_traits(kind, event, ev)
    |> push_event(ev)
    |> Map.put(:last_update_ms, now_ms())
  end

  def observe(portrait, _), do: portrait

  # ───────────────────────── internals ─────────────────────────

  defp bump_source(%{} = portrait, event, ev) do
    head = source_head(event, ev)

    src =
      portrait.sources
      |> Map.update(head, 1, &(&1 + 1))

    %{portrait | sources: src}
  end

  defp source_head([h | _], _ev), do: h

  defp source_head(_event, ev) when is_map(ev) do
    cond do
      get_any(ev, [:region, "region"]) in [:ml, "ml"] ->
        :brain

      get_any(ev, [:kind, "kind"]) in [:ml_turn, "ml_turn"] ->
        :brain

      source_known?(get_any(ev, [:source, "source"])) ->
        normalize_source(get_any(ev, [:source, "source"]))

      true ->
        :unknown
    end
  end

  defp source_head(_event, _ev), do: :unknown

  defp source_known?(source), do: normalize_source(source) != :unknown

  defp normalize_source(source) when source in [:brain, :core, :ml], do: source

  defp normalize_source(source) when source in ["brain", "core", "ml"],
    do: String.to_existing_atom(source)

  defp normalize_source(_), do: :unknown

  defp bump_patterns(%{} = portrait, :telemetry, event, ev) when is_list(event) do
    p0 = portrait.patterns

    p1 =
      cond do
        event == [:brain, :wm, :update] ->
          Map.update(p0, :wm_updates, 1, &(&1 + 1))

        event == [:brain, :pmtg, :consult] ->
          Map.update(p0, :pmtg_consults, 1, &(&1 + 1))

        event == [:brain, :pmtg, :no_mwe_senses] ->
          p0
          |> Map.update(:no_mwe_senses, 1, &(&1 + 1))
          |> Map.update(:mwe_compat_misses, 1, &(&1 + 1))

        # Stage1 emits this when local fallback had to stand in for a phrase.
        # Keep it separate from pMTG compatibility misses; otherwise
        # SelfPortrait cannot tell weak supply from true "no compatible MWE sense".
        event == [:brain, :pmtg, :mwe_fallback_emitted] ->
          Map.update(p0, :mwe_fallbacks, 1, &(&1 + 1))

        contains?(event, :boundary_drop) ->
          Map.update(p0, :boundary_drops, 1, &(&1 + 1))

        contains?(event, :chargram_violation) ->
          Map.update(p0, :chargram_violations, 1, &(&1 + 1))

        contains?(event, :gate_failure) ->
          Map.update(p0, :gate_failures, 1, &(&1 + 1))

        contains?(event, :fallback) and contains?(event, :winner) ->
          Map.update(p0, :fallback_wins, 1, &(&1 + 1))

        true ->
          p0
      end

    # NEW: also read counters from meta/measurements (covers summary events like
    # [:brain, :pipeline, :lifg_stage1, :stop])
    p2 = apply_meta_counters(p1, ev)

    %{portrait | patterns: p2}
  end

  defp bump_patterns(portrait, _kind, _event, _ev), do: portrait

  defp apply_meta_counters(patterns, ev) when is_map(patterns) and is_map(ev) do
    meas = Map.get(ev, :measurements) || Map.get(ev, "measurements") || %{}
    meta = Map.get(ev, :meta) || Map.get(ev, "meta") || %{}

    bd =
      int_or_0(
        get_any(meta, [
          :boundary_drops,
          :boundary_drop_count,
          "boundary_drops",
          "boundary_drop_count"
        ])
      ) +
        int_or_0(get_any(meas, [:boundary_drops, "boundary_drops"]))

    cg =
      int_or_0(get_any(meta, [:chargram_violation, :chargram, "chargram_violation", "chargram"])) +
        int_or_0(get_any(meas, [:chargram, "chargram"]))

    no_mwe =
      int_or_0(get_any(meta, [:no_mwe_senses, "no_mwe_senses"])) +
        int_or_0(get_any(meas, [:no_mwe_senses, "no_mwe_senses"]))

    mwe_fallbacks =
      int_or_0(get_any(meta, [:mwe_fallbacks, "mwe_fallbacks"])) +
        int_or_0(get_any(meas, [:mwe_fallbacks, "mwe_fallbacks"]))

    fb =
      int_or_0(
        get_any(meta, [:fallback_winners, :fallback_wins, "fallback_winners", "fallback_wins"])
      ) +
        int_or_0(get_any(meas, [:fallback_winners, "fallback_winners"]))

    patterns
    |> bump_by(:boundary_drops, bd)
    |> bump_by(:chargram_violations, cg)
    |> bump_by(:no_mwe_senses, no_mwe)
    |> bump_by(:mwe_compat_misses, no_mwe)
    |> bump_by(:mwe_fallbacks, mwe_fallbacks)
    |> bump_by(:fallback_wins, fb)
    |> bump_by(:lifg_payload_gaps, lifg_payload_gap_count(ev))
    |> bump_by(:lifg_pos_anomalies, lifg_pos_anomaly_count(ev))
  end

  defp lifg_pos_anomaly_count(%{} = ev) do
    event = Map.get(ev, :event) || Map.get(ev, "event")
    meta = Map.get(ev, :meta) || Map.get(ev, "meta") || %{}

    cond do
      event != [:brain, :pipeline, :lifg_stage1, :stop] ->
        0

      get_any(meta, [:intent, "intent"]) not in [:greet, "greet"] ->
        0

      true ->
        tokens_by_index =
          meta
          |> get_any([:tokens, "tokens"])
          |> list_or_empty()
          |> Enum.into(%{}, fn tok ->
            {token_index(tok), normalize_phrase(get_any(tok, [:phrase, "phrase"]))}
          end)

        meta
        |> get_any([:choices, "choices"])
        |> list_or_empty()
        |> Enum.count(fn choice ->
          phrase = Map.get(tokens_by_index, token_index(choice))
          chosen_id = get_any(choice, [:chosen_id, "chosen_id"]) || ""
          pos = choice_pos(choice)

          phrase == "good" and (pos == "verb" or String.contains?(chosen_id, "|verb|"))
        end)
    end
  end

  defp lifg_pos_anomaly_count(_), do: 0

  defp list_or_empty(list) when is_list(list), do: list
  defp list_or_empty(_), do: []

  defp token_index(%{} = item),
    do: int_or_0(get_any(item, [:token_index, :index, "token_index", "index"]))

  defp token_index(_), do: 0

  defp choice_pos(%{} = choice) do
    case get_any(choice, [:chosen_pos, "chosen_pos"]) do
      pos when is_atom(pos) -> Atom.to_string(pos)
      pos when is_binary(pos) -> String.downcase(pos)
      _ -> chosen_id_pos(get_any(choice, [:chosen_id, "chosen_id"]))
    end
  end

  defp choice_pos(_), do: nil

  defp chosen_id_pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_word, pos, _sense] -> String.downcase(pos)
      _ -> nil
    end
  end

  defp chosen_id_pos(_), do: nil

  defp normalize_phrase(phrase) when is_binary(phrase) do
    phrase
    |> String.trim()
    |> String.downcase()
  end

  defp normalize_phrase(_), do: ""

  defp lifg_payload_gap_count(%{} = ev) do
    event = Map.get(ev, :event) || Map.get(ev, "event")
    meta = Map.get(ev, :meta) || Map.get(ev, "meta") || %{}
    meas = Map.get(ev, :measurements) || Map.get(ev, "measurements") || %{}

    kept =
      int_or_0(get_any(meta, [:kept_tokens, "kept_tokens"])) +
        int_or_0(get_any(meas, [:kept, "kept"]))

    cond do
      event != [:brain, :pipeline, :lifg_stage1, :stop] ->
        0

      kept <= 0 ->
        0

      missing_payload_list?(meta, :tokens) or missing_payload_list?(meta, :choices) or
          missing_payload_list?(meta, :finalists) ->
        1

      true ->
        0
    end
  end

  defp lifg_payload_gap_count(_), do: 0

  defp missing_payload_list?(%{} = meta, key) do
    case get_any(meta, [key, to_string(key)]) do
      list when is_list(list) -> list == []
      _ -> true
    end
  end

  defp bump_by(pats, _k, 0), do: pats

  defp bump_by(pats, k, n) when is_integer(n) and n > 0 do
    Map.update(pats, k, n, &(&1 + n))
  end

  defp bump_by(pats, _k, _n), do: pats

  defp bump_traits(%{} = portrait, :telemetry, event, ev) when is_list(event) do
    traits0 = portrait.traits

    meta = Map.get(ev, :meta) || Map.get(ev, "meta") || %{}
    meas = Map.get(ev, :measurements) || Map.get(ev, "measurements") || %{}

    # Confidence: softly track margin if present (best-effort).
    margin =
      get_any(meta, [:margin_mean, :margin, :prob_margin, "margin_mean", "margin", "prob_margin"]) ||
        get_any(meas, [:margin_mean, "margin_mean"])

    traits1 =
      case num_or_nil(margin) do
        m when is_number(m) ->
          conf = clamp_float(m * 1.0, 0.0, 1.0)
          Map.put(traits0, :confidence_baseline, ewma(traits0.confidence_baseline, conf, 0.06))

        _ ->
          traits0
      end

    # Stability: drift down on guardrail-ish activity; drift up on WM updates.
    weak_n =
      int_or_0(get_any(meta, [:weak_decisions, "weak_decisions"])) +
        int_or_0(get_any(meas, [:weak, "weak"]))

    viol_n =
      int_or_0(get_any(meta, [:guardrail_violations, "guardrail_violations"])) +
        int_or_0(
          get_any(meta, [
            :boundary_drops,
            :boundary_drop_count,
            "boundary_drops",
            "boundary_drop_count"
          ])
        ) +
        int_or_0(
          get_any(meta, [:chargram_violation, :chargram, "chargram_violation", "chargram"])
        )

    traits2 =
      cond do
        event == [:brain, :wm, :update] ->
          Map.put(traits1, :stability, ewma(traits1.stability, 0.60, 0.03))

        weak_n > 0 or viol_n > 0 ->
          penalty = clamp_float(0.05 * weak_n + 0.10 * viol_n, 0.0, 0.60)
          target = clamp_float(0.65 - penalty, 0.0, 1.0)
          Map.put(traits1, :stability, ewma(traits1.stability, target, 0.06))

        true ->
          traits1
      end

    # Curiosity: if pMTG consult fires, nudge upward (retrieval drive).
    traits3 =
      if event == [:brain, :pmtg, :consult] do
        Map.put(traits2, :curiosity_bias, ewma(traits2.curiosity_bias, 0.65, 0.04))
      else
        traits2
      end

    %{portrait | traits: clamp_traits(traits3)}
  end

  defp bump_traits(portrait, _kind, _event, _ev), do: portrait

  defp push_event(%{} = portrait, %{} = ev) do
    slim = %{
      kind: Map.get(ev, :kind) || Map.get(ev, "kind"),
      event: Map.get(ev, :event) || Map.get(ev, "event"),
      at_ms: Map.get(ev, :at_ms) || Map.get(ev, "at_ms") || now_ms()
    }

    events = [slim | portrait.last_events] |> Enum.take(portrait.max_events)
    %{portrait | last_events: events}
  end

  defp contains?(list, atom) when is_list(list), do: Enum.any?(list, &(&1 == atom))
  defp contains?(_, _), do: false

  defp get_any(map, keys) when is_map(map) do
    Enum.reduce_while(keys, nil, fn k, _acc ->
      case Map.get(map, k) do
        nil -> {:cont, nil}
        v -> {:halt, v}
      end
    end)
  end

  defp int_or_0(v) when is_integer(v), do: v
  defp int_or_0(v) when is_float(v), do: trunc(v)

  defp int_or_0(v) when is_binary(v) do
    case Integer.parse(v) do
      {i, _} -> i
      _ -> 0
    end
  end

  defp int_or_0(_), do: 0

  defp num_or_nil(v) when is_integer(v), do: v * 1.0
  defp num_or_nil(v) when is_float(v), do: v

  defp num_or_nil(v) when is_binary(v) do
    case Float.parse(String.trim(v)) do
      {f, ""} -> f
      _ -> nil
    end
  end

  defp num_or_nil(_), do: nil

  defp ewma(prev, x, alpha) when is_number(prev) and is_number(x) do
    a = clamp_float(alpha, 0.0, 1.0)
    (1.0 - a) * (prev * 1.0) + a * (x * 1.0)
  end

  defp clamp_traits(traits) do
    traits
    |> Enum.into(%{}, fn {k, v} ->
      vv =
        if is_number(v) do
          clamp_float(v * 1.0, 0.0, 1.0)
        else
          v
        end

      {k, vv}
    end)
  end

  defp clamp_float(x, lo, _hi) when x < lo, do: lo
  defp clamp_float(x, _lo, hi) when x > hi, do: hi
  defp clamp_float(x, _lo, _hi), do: x

  defp clamp_int(x, lo, _hi) when is_integer(x) and x < lo, do: lo
  defp clamp_int(x, _lo, hi) when is_integer(x) and x > hi, do: hi
  defp clamp_int(x, _lo, _hi) when is_integer(x), do: x
  defp clamp_int(_, lo, _hi), do: lo

  defp now_ms, do: System.system_time(:millisecond)
end
