defmodule Brain.WM.Gate do
  @moduledoc """
  Gating helpers that take LIFG output living on the SI (`:lifg_pairs`, `:lifg_choices`)
  and update the Brain server state’s WM-related fields.

  Entry point: `ingest_from_si/3` — returns a new `state`.
  """

  # ── Public API ──────────────────────────────────────────────────────────────

  @spec ingest_from_si(map(), map(), keyword()) :: map()
  def ingest_from_si(state, si, opts) when is_map(state) and is_map(si) and is_list(opts) do
    if Keyword.get(opts, :gate_into_wm, false) do
      allow_fb? = get_in(state, [:wm_cfg, :allow_fallback_into_wm?]) || false

      tuples = lifg_pairs_to_tuples(si)

      state2 =
        if tuples != [] do
          gate_pairs_into_wm_state(state, tuples, allow_fb?)
        else
          gate_winners_into_wm_state(state, si, allow_fb?)
        end

      bump_wm_ts(state2)
    else
      state
    end
  end

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

      # already-normalized tuple forms (avoid binding unused vars)
      tup when is_tuple(tup) and tuple_size(tup) == 5 ->
        [tup]

      tup when is_tuple(tup) and tuple_size(tup) == 2 ->
        [tup]

      _ ->
        []
    end)
  end

  # ── Gating reducers ─────────────────────────────────────────────────────────

  # Pairs: bump unigram (always), and MWE only if allowed (or not a fallback)
  defp gate_pairs_into_wm_state(state, tuples, allow_fb?) do
    Enum.reduce(tuples, state, fn
      {mwe, _ms, uni, _us, sc}, st ->
        st1 =
          if allow_fb? or not phrase_fallback_id?(mwe) do
            wm_put(st, mwe, clamp01(as_float(sc)))
          else
            st
          end

        wm_put(st1, uni, clamp01(as_float(sc)))

      {ida, idb}, st ->
        st
        |> (fn st0 ->
              if allow_fb? or not phrase_fallback_id?(ida), do: wm_put(st0, ida, 0.3), else: st0
            end).()
        |> wm_put(idb, 0.3)

      _, st ->
        st
    end)
  end

  # Winners-only fallback path
  defp gate_winners_into_wm_state(state, si, allow_fb?) do
    winners = (si[:lifg_choices] || si["lifg_choices"] || []) |> List.wrap()

    Enum.reduce(winners, state, fn w, st ->
      id = w[:id] || w["id"]
      score = w[:score] || w["score"] || 1.0

      cond do
        is_nil(id) -> st
        phrase_fallback_id?(id) and not allow_fb? -> st
        true -> wm_put(st, id, clamp01(as_float(score)))
      end
    end)
  end

  # ── Local, WM-scoped helpers (kept here to avoid coupling with Brain) ───────

  defp wm_put(%{active_cells: ac} = state, id, score)
       when is_binary(id) and is_number(score) do
    key = normalize_cell_id(id)
    ac2 = Map.update(ac || %{}, key, score, &(&1 + score))
    %{state | active_cells: ac2}
  end

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
