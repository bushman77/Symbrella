defmodule Brain.WM.Gate do
  @moduledoc """
  Gating helpers that take LIFG output living on the SI (`:lifg_pairs`, `:lifg_choices`)
  and update the Brain server state’s WM-related fields.

  Entry point: `ingest_from_si/3` — returns a new `state`.
  """

  @wm_gate_event [:brain, :wm, :gate]

  # ── Public API ──────────────────────────────────────────────────────────────

  @spec ingest_from_si(map(), map(), keyword()) :: map()
  def ingest_from_si(state, si, opts \\ [])

  def ingest_from_si(state, si, opts) when is_map(state) and is_map(si) and is_list(opts) do
    allow_fb? =
      Keyword.get(opts, :allow_phrase_fallback?, false) or
        Keyword.get(opts, :allow_fallback?, false)

    min_score =
      opts
      |> Keyword.get(:min_score, 0.0)
      |> as_float()
      |> clamp01()

    gate_event = Keyword.get(opts, :gate_event, @wm_gate_event)

    tuples = lifg_pairs_to_tuples(si)

    state1 =
      cond do
        tuples != [] ->
          gate_pairs(state, tuples, allow_fb?, min_score, gate_event)

        true ->
          gate_winners(state, si, allow_fb?, min_score, gate_event)
      end

    bump_wm_ts(state1)
  end

  def ingest_from_si(state, _si, _opts), do: state

  # ── Pair normalization (SI → tuples) ────────────────────────────────────────

  # Convert SI.lifg_pairs (maps) → canonical tuples [{mwe_id, mwe_span, uni_id, uni_span, score}]
  @spec lifg_pairs_to_tuples(map()) :: list()
  defp lifg_pairs_to_tuples(si) when is_map(si) do
    tokens = (si[:tokens] || si["tokens"] || []) |> List.wrap()

    get_span = fn idx ->
      case Enum.find(tokens, fn t -> (t[:index] || t["index"]) == idx end) do
        %{span: span} -> span
        %{"span" => span} -> span
        _ -> nil
      end
    end

    mwe_span_fallback =
      case Enum.find(tokens, fn t -> t[:mw] == true or t["mw"] == true end) do
        %{span: span} -> span
        %{"span" => span} -> span
        _ -> nil
      end

    (si[:lifg_pairs] || si["lifg_pairs"] || [])
    |> List.wrap()
    |> Enum.flat_map(fn
      # canonical ATL pair shape
      %{type: :mwe_unigram, mwe_id: mwe, unigram_id: uni, token_index: j} = m ->
        ms = m[:mwe_span] || m["mwe_span"] || mwe_span_fallback
        us = get_span.(j)
        sc = m[:weight] || m["weight"] || 1.0
        if ms && us, do: [{to_string(mwe), ms, to_string(uni), us, sc}], else: []

      # already-normalized tuple forms
      tup when is_tuple(tup) and tuple_size(tup) == 5 ->
        [tup]

      tup when is_tuple(tup) and tuple_size(tup) == 2 ->
        [tup]

      _ ->
        []
    end)
  end

  # ── Gating reducers ─────────────────────────────────────────────────────────

  defp gate_pairs(state, tuples, allow_fb?, min_score, gate_event) do
    Enum.reduce(tuples, state, fn
      {mwe, _ms, uni, _us, sc}, st ->
        score = clamp01(as_float(sc))

        st =
          if score >= min_score do
            st |> admit_lifg_id(uni, score, %{lemma: lemma_from_id(uni)}, gate_event)
          else
            st
          end

        cond do
          score < min_score ->
            st

          not allow_fb? and phrase_fallback_id?(mwe) ->
            st

          true ->
            admit_lifg_id(st, mwe, score, %{lemma: lemma_from_id(mwe)}, gate_event)
        end

      {ida, idb}, st ->
        # Legacy form: treat as weak evidence unless overridden by opts
        score = clamp01(as_float(Keyword.get([], :legacy_pair_score, 0.3)))

        st =
          if score >= min_score do
            st
            |> admit_lifg_id(
              to_string(idb),
              score,
              %{lemma: lemma_from_id(to_string(idb))},
              gate_event
            )
          else
            st
          end

        cond do
          score < min_score ->
            st

          not allow_fb? and phrase_fallback_id?(to_string(ida)) ->
            st

          true ->
            admit_lifg_id(
              st,
              to_string(ida),
              score,
              %{lemma: lemma_from_id(to_string(ida))},
              gate_event
            )
        end

      _, st ->
        st
    end)
  end

  defp gate_winners(state, si, allow_fb?, min_score, gate_event) do
    winners = (si[:lifg_choices] || si["lifg_choices"] || []) |> List.wrap()

    Enum.reduce(winners, state, fn w, st ->
      id = w[:chosen_id] || w["chosen_id"] || w[:id] || w["id"]
      score = w[:score] || w["score"] || w[:prob] || w["prob"] || 1.0
      tok_i = w[:token_index] || w["token_index"] || w[:index] || w["index"]

      cond do
        is_nil(id) ->
          st

        phrase_fallback_id?(to_string(id)) and not allow_fb? ->
          st

        clamp01(as_float(score)) < min_score ->
          st

        true ->
          admit_lifg_id(
            st,
            to_string(id),
            clamp01(as_float(score)),
            %{lemma: lemma_from_id(to_string(id)), token_index: tok_i},
            gate_event
          )
      end
    end)
  end

  # ── Admission: update state.active_cells (if map) + state.wm (if list) + telemetry ──

  defp admit_lifg_id(state, id, score, payload_meta, gate_event)
       when is_binary(id) and is_number(score) and is_map(payload_meta) do
    now = System.system_time(:millisecond)

    payload =
      %{
        id: id,
        source: :lifg,
        lemma: Map.get(payload_meta, :lemma, lemma_from_id(id)),
        score: score
      }
      |> maybe_put(:token_index, Map.get(payload_meta, :token_index))

    state =
      state
      |> maybe_put_active_cells(id, score)
      |> maybe_upsert_wm_item(id, score, payload, now)

    emit_gate(gate_event, %{score: score}, %{decision: :allow, source: :lifg, id: id})
    state
  end

  defp maybe_put_active_cells(state, id, score) do
    case Map.get(state, :active_cells) do
      ac when is_map(ac) ->
        key = normalize_cell_id(id)
        ac2 = Map.update(ac || %{}, key, score, &(&1 + score))
        Map.put(state, :active_cells, ac2)

      _ ->
        state
    end
  end

  defp maybe_upsert_wm_item(state, id, score, payload, now_ms) do
    case Map.get(state, :wm) do
      nil ->
        Map.put(state, :wm, [new_wm_item(id, score, payload, now_ms)])

      list when is_list(list) ->
        rest =
          Enum.reject(list, fn it ->
            Map.get(it, :id) == id and Map.get(it, :source) == :lifg
          end)

        item0 =
          Enum.find(list, fn it -> Map.get(it, :id) == id and Map.get(it, :source) == :lifg end)

        item =
          if is_map(item0) do
            item0
            |> Map.put(:payload, payload)
            |> Map.put(:score, max(as_float(Map.get(item0, :score, 0.0)), score))
            |> Map.put(:last_bump, now_ms)
          else
            new_wm_item(id, score, payload, now_ms)
          end

        Map.put(state, :wm, [item | rest])

      _ ->
        state
    end
  end

  defp new_wm_item(id, score, payload, now_ms) do
    %{
      id: id,
      source: :lifg,
      activation: 1.0,
      payload: payload,
      ts: now_ms,
      inserted_at: now_ms,
      score: score,
      last_bump: now_ms
    }
  end

  defp emit_gate(ev, meas, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(ev, meas, meta)
    else
      :ok
    end
  end

  defp maybe_put(m, _k, nil), do: m
  defp maybe_put(m, k, v), do: Map.put(m, k, v)

  # ── Local helpers ───────────────────────────────────────────────────────────

  defp bump_wm_ts(state),
    do: Map.put(state, :wm_last_ms, System.system_time(:millisecond))

  # Avoid “Hello|…” vs “hello|…”
  defp normalize_cell_id(id) when is_binary(id) do
    case String.split(id, "|") do
      [lemma, pos, sense] -> Enum.join([String.downcase(lemma), pos, sense], "|")
      _ -> String.downcase(id)
    end
  end

  defp phrase_fallback_id?(id) when is_binary(id),
    do: String.contains?(id, "|phrase|fallback")

  defp phrase_fallback_id?(_), do: false

  defp lemma_from_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [lemma, _] -> lemma
      _ -> id
    end
  end

  # --- converters ---
  defp as_float(nil), do: 0.0
  defp as_float(v) when is_number(v), do: v * 1.0

  defp as_float(v) when is_binary(v) do
    case Float.parse(v) do
      {f, _} -> f
      _ -> 0.0
    end
  end

  defp as_float(_), do: 0.0

  defp clamp01(x) when is_number(x) and x < 0.0, do: 0.0
  defp clamp01(x) when is_number(x) and x > 1.0, do: 1.0
  defp clamp01(x) when is_number(x), do: x
end
