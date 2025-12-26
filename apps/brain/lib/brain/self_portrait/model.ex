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
        boundary_drops: 0,
        chargram_violations: 0,
        gate_failures: 0,
        fallback_wins: 0
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
    |> bump_source(event)
    |> bump_patterns(kind, event, ev)
    |> bump_traits(kind, event, ev)
    |> push_event(ev)
    |> Map.put(:last_update_ms, now_ms())
  end

  def observe(portrait, _), do: portrait

  # ───────────────────────── internals ─────────────────────────

  defp bump_source(%{} = portrait, event) do
    head =
      case event do
        [h | _] -> h
        _ -> :unknown
      end

    src =
      portrait.sources
      |> Map.update(head, 1, &(&1 + 1))

    %{portrait | sources: src}
  end

  defp bump_patterns(%{} = portrait, :telemetry, event, ev) when is_list(event) do
    p0 = portrait.patterns

    p1 =
      cond do
        event == [:brain, :wm, :update] ->
          Map.update(p0, :wm_updates, 1, &(&1 + 1))

        event == [:brain, :pmtg, :consult] ->
          Map.update(p0, :pmtg_consults, 1, &(&1 + 1))

        event == [:brain, :pmtg, :no_mwe_senses] ->
          Map.update(p0, :no_mwe_senses, 1, &(&1 + 1))

        # Stage1 emits this when MWE tokens had no senses (fallback token)
        event == [:brain, :pmtg, :mwe_fallback_emitted] ->
          Map.update(p0, :no_mwe_senses, 1, &(&1 + 1))

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
      int_or_0(get_any(meta, [:no_mwe_senses, :mwe_fallbacks, "no_mwe_senses", "mwe_fallbacks"])) +
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
    |> bump_by(:fallback_wins, fb)
  rescue
    _ -> patterns
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
    case Float.parse(v) do
      {f, _} -> f
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
