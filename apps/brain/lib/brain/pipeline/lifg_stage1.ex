defmodule Brain.Pipeline.LIFGStage1 do
  @moduledoc false

  alias Brain.Episodes.Writer, as: EpWriter
  alias Brain.LIFG
  alias Brain.LIFG.Gate, as: LIFGGate

  @pipeline_stop_event [:brain, :pipeline, :lifg_stage1, :stop]
  @no_candidates_event [:brain, :pipeline, :lifg_stage1, :no_candidates]

  @spec run(map() | [map()], term(), keyword(), map()) :: {{:ok, map()}, map()}
  def run(si_or_cands, _ctx_vec, opts, state) when is_list(opts) and is_map(state) do
    t0 = System.monotonic_time()
    now_ms = System.system_time(:millisecond)

    # Build inputs (tokens + sentence) so PFC can see tokens for intent biasing
    %{tokens: tokens0, sentence: sentence} = build_lifg_inputs(si_or_cands)

    # NEW: self-name detection (independent of whether Stage-1 has candidates)
    self = detect_self_name(tokens0, sentence)

    state0 =
      state
      |> Map.put(:attention, Map.get(state, :attention, %{}) |> Map.put(:self_name, self))

    # Enrich SI for PFC policy (ensures tokens present)
    si_for_policy = enrich_for_policy(si_or_cands, tokens0)

    # Pull dynamic policy from PFC safely (no-op if PFC not started)
    pfc_opts = safe_pfc_policy(si_for_policy)

    lifg_opts =
      [scores: :all, normalize: :softmax, parallel: :auto]
      |> Keyword.merge(pfc_opts)
      |> Keyword.merge(opts)

    # Preserve existing candidate info; overlay normalized :tokens/:sentence for Stage-1.
    base_si =
      case si_or_cands do
        m when is_map(m) -> m
        _ -> %{}
      end

    si1 =
      base_si
      |> Map.put(:tokens, tokens0)
      |> Map.put(:sentence, sentence)
      |> Map.put_new(:trace, [])

    # NEW: don’t crash if SI has no candidates; just no-op Stage-1
    si2 = safe_disambiguate_stage1(si1, lifg_opts)

    {:ok, out0_raw} = lifg_out_from_trace(si2)

    out0 =
      out0_raw
      |> Map.update(:audit, %{}, fn audit -> Map.put(audit || %{}, :self_name, self) end)

    out0 =
      case Brain.LIFG.Hygiene.run(%{}, out0.choices, []) do
        {:ok, %{choices: cleaned}} -> %{out0 | choices: cleaned}
        _ -> out0
      end

    # ACC conflict assessment (no-op if ACC region isn't started)
    {_si_acc, _conflict} =
      maybe_assess_acc(%{tokens: tokens0, choices: out0.choices}, already_needy: true)

    _ = maybe_ingest_atl(out0.choices, tokens0)
    _ = maybe_consult_pmtg(out0.choices, tokens0)
    _ = maybe_store_episode(tokens0, si2, out0)

    {boosts2, inhib2} = maybe_rescale_signals(out0, lifg_opts)
    _ = Brain.__pipeline_apply_control_signals__(boosts2, inhib2, lifg_opts)

    state1 = Brain.apply_decay(state0, now_ms)

    state2 =
      if Keyword.get(lifg_opts, :gate_into_wm, false) do
        min =
          Keyword.get(
            lifg_opts,
            :lifg_min_score,
            Application.get_env(:brain, :lifg_min_score, 0.6)
          )

        lifg_cands = LIFGGate.stage1_wm_candidates(out0.choices, now_ms, min)

        lifg_cands2 =
          case lifg_cands do
            [] -> lifg_wm_candidates_fallback(out0.choices, now_ms, min)
            other -> other
          end

        if lifg_cands2 == [] do
          state1
        else
          {wm_next, added, removed} = Brain.__pipeline_do_focus__(state1, lifg_cands2, %{})

          Brain.__pipeline_emit_wm_update__(
            state1.wm_cfg.capacity,
            length(wm_next),
            added,
            removed,
            :gate_from_lifg
          )

          _ = Brain.__pipeline_safe_mood_update_wm__(wm_next)
          %{state1 | wm: wm_next}
        end
      else
        state1
      end

    state3 = Brain.evict_if_needed(state2)

    :telemetry.execute(
      @pipeline_stop_event,
      %{
        duration_ms: System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)
      },
      %{
        winners: length(out0.choices),
        boosts: length(out0.boosts),
        inhibitions: length(out0.inhibitions)
      }
    )

    {{:ok, out0}, state3}
  end

  # ───────────────────────── Stage-1 safety ─────────────────────────

  defp safe_disambiguate_stage1(si, lifg_opts) do
    try do
      LIFG.disambiguate_stage1(si, lifg_opts)
    rescue
      e in ArgumentError ->
        msg = Exception.message(e)

        if is_binary(msg) and String.contains?(msg, "Cannot extract LIFG candidates") do
          :telemetry.execute(@no_candidates_event, %{count: 1}, %{stage: :lifg_stage1})
          si
        else
          reraise e, __STACKTRACE__
        end
    end
  end

  # ───────────────────────── self name ─────────────────────────

  defp detect_self_name(tokens, sentence) do
    names =
      Application.get_env(:brain, :self_names, [])
      |> List.wrap()
      |> Enum.map(&norm_surface/1)
      |> Enum.reject(&(&1 == ""))
      |> MapSet.new()

    token_hit =
      tokens
      |> List.wrap()
      |> Enum.find_value(fn t ->
        surface =
          (t[:phrase] || t["phrase"] || t[:word] || t["word"] || t[:lemma] || t["lemma"] || "")
          |> norm_surface()

        if surface != "" and MapSet.member?(names, surface) do
          idx = t[:index] || t["index"] || t[:token_index] || t["token_index"]

          %{
            hit?: true,
            match: surface,
            token_index: (is_integer(idx) && idx) || nil,
            source: :tokens
          }
        else
          nil
        end
      end)

    cond do
      is_map(token_hit) ->
        token_hit

      is_binary(sentence) and sentence != "" ->
        sent_hit =
          sentence
          |> words_from_sentence()
          |> Enum.find(&MapSet.member?(names, &1))

        if is_binary(sent_hit) do
          %{hit?: true, match: sent_hit, token_index: nil, source: :sentence}
        else
          %{hit?: false, match: nil, token_index: nil, source: nil}
        end

      true ->
        %{hit?: false, match: nil, token_index: nil, source: nil}
    end
  end

  defp words_from_sentence(sentence) when is_binary(sentence) do
    Regex.scan(~r/[\p{L}\p{N}_']+/u, sentence)
    |> Enum.map(fn [w] -> norm_surface(w) end)
    |> Enum.reject(&(&1 == ""))
  end

  defp words_from_sentence(_), do: []

  defp norm_surface(x) when is_binary(x) do
    x
    |> String.downcase()
    |> String.trim()
    # FIX: don't use ~s() with a ")" inside the delimiter; use a normal string instead
    |> String.trim("'\".,!?;:()[]{}<>")
  end

  defp norm_surface(_), do: ""

  # ───────────────────────── trace → output ─────────────────────────

  defp lifg_out_from_trace(%{trace: [ev | _]}) when is_map(ev) do
    choices = Map.get(ev, :choices) || Map.get(ev, "choices") || []
    boosts = Map.get(ev, :boosts) || Map.get(ev, "boosts") || []
    inhibitions = Map.get(ev, :inhibitions) || Map.get(ev, "inhibitions") || []

    audit =
      ev
      |> Map.drop([:choices, "choices", :boosts, "boosts", :inhibitions, "inhibitions"])

    {:ok, %{choices: choices, boosts: boosts, inhibitions: inhibitions, audit: audit}}
  end

  defp lifg_out_from_trace(_),
    do:
      {:ok, %{choices: [], boosts: [], inhibitions: [], audit: %{stage: :lifg_stage1, groups: 0}}}

  # ───────────────────────── ATL / PMTG / Episodes ─────────────────────────

  defp maybe_ingest_atl(choices, tokens) do
    case Process.whereis(Brain.ATL) do
      nil -> :noop
      _pid -> Brain.ATL.ingest(choices, tokens)
    end
  end

  defp confidence_from_choices(choices) when is_list(choices) do
    choices
    |> Enum.map(&Map.get(&1, :margin, 0.0))
    |> Enum.reject(&is_nil/1)
    |> case do
      [] -> 1.0
      ms -> Enum.min(ms) * 1.0
    end
  end

  defp lifg_conf_threshold, do: Application.get_env(:brain, :lifg_conf_threshold, 0.18) * 1.0

  defp maybe_consult_pmtg(choices, tokens) do
    conf = confidence_from_choices(choices)
    :telemetry.execute([:brain, :lifg, :confidence], %{value: conf}, %{})

    needy_thr = Application.get_env(:brain, :pmtg_margin_threshold, 0.18)

    needy =
      Enum.filter(choices, fn ch ->
        m = Map.get(ch, :margin, 1.0)
        alts = Map.get(ch, :alt_ids, [])
        is_number(m) and m < needy_thr and is_list(alts) and length(alts) > 0
      end)

    should_consult? = conf < lifg_conf_threshold() or needy != []

    if should_consult? and is_pid(Process.whereis(Brain.PMTG)) do
      Brain.PMTG.consult(needy, tokens, already_needy: true, limit: 5, mode: :boost)
    else
      :ok
    end
  end

  defp maybe_store_episode(tokens, si1, out0) do
    mode =
      Application.get_env(:brain, :episodes_mode, :async)
      |> EpWriter.normalize_episode_mode()

    tags = Application.get_env(:brain, :episodes_tags, ["auto", "lifg"])

    si = %{
      tokens: tokens,
      lifg_choices:
        Enum.map(out0.choices, fn ch ->
          %{
            token_index: ch[:token_index],
            lemma: ch[:lemma],
            chosen_id: ch[:chosen_id],
            alt_ids: ch[:alt_ids] || [],
            margin: ch[:margin],
            scores: ch[:scores] || %{}
          }
        end),
      trace: si1.trace
    }

    EpWriter.store(si, tags, mode)
  end

  # ───────────────────────── signals ─────────────────────────

  defp maybe_rescale_signals(%{choices: choices, boosts: b, inhibitions: i}, opts) do
    case Keyword.get(opts, :delta_model, :fixed) do
      :fixed ->
        {b, i}

      :margin_scaled ->
        base_boost = Keyword.get(opts, :base_boost, 0.2)
        base_inhib = Keyword.get(opts, :base_inhib, 0.1)
        clamp = fn x -> x |> min(0.5) |> max(-0.5) end

        boosts2 =
          for ch <- choices do
            m = ch[:margin] || Map.get(ch, :margin, 0.05)
            delta = clamp.(base_boost * max(m, 0.05))
            {ch[:chosen_id] || Map.get(ch, :chosen_id), delta}
          end

        inhib2 =
          choices
          |> Enum.flat_map(fn ch ->
            alts = ch[:alt_ids] || Map.get(ch, :alt_ids, [])
            m = ch[:margin] || Map.get(ch, :margin, 0.0)

            for aid <- alts do
              delta = clamp.(-base_inhib * max(0.2, 1.0 - m))
              {aid, delta}
            end
          end)

        {boosts2, inhib2}
    end
  end

  # ───────────────────────── LIFG → WM fallback helper ─────────────────────────

  defp lifg_wm_candidates_fallback(choices, now_ms, min)
       when is_list(choices) and is_number(min) do
    choices
    |> Enum.filter(fn ch ->
      choice_gate_score(ch) >= min
    end)
    |> Enum.map(fn ch ->
      id =
        ch[:id] ||
          ch["id"] ||
          ch[:chosen_id] ||
          ch["chosen_id"] ||
          "lifg|unknown|0"

      lemma =
        ch[:lemma] ||
          ch["lemma"] ||
          guess_lemma_from_id(id) ||
          ""

      %{
        token_index: ch[:token_index] || ch["token_index"] || 0,
        id: to_string(id),
        lemma: to_string(lemma),
        score: choice_gate_score(ch),
        source: :lifg,
        reason: :lifg_stage1,
        ts: now_ms,
        payload: ch
      }
    end)
  end

  defp lifg_wm_candidates_fallback(_choices, _now_ms, _min), do: []

  defp choice_gate_score(ch) when is_map(ch) do
    s =
      ch[:score] ||
        ch["score"] ||
        ch[:prob] ||
        ch["prob"] ||
        ch[:margin] ||
        ch["margin"] ||
        0.0

    case s do
      v when is_number(v) ->
        v * 1.0

      v when is_binary(v) ->
        case Float.parse(v) do
          {f, _} -> f
          _ -> 0.0
        end

      _ ->
        0.0
    end
  end

  defp choice_gate_score(_), do: 0.0

  defp guess_lemma_from_id(nil), do: nil

  defp guess_lemma_from_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [w | _] -> w
      _ -> nil
    end
  end

  defp guess_lemma_from_id(_), do: nil

  # ───────────────────────── ACC hook (safe) ─────────────────────────

  defp maybe_assess_acc(si, opts) do
    unless is_map(si) do
      {si, 0.0}
    else
      case safe_acc_assess(si, opts) do
        {:ok, %{si: si2, conflict: c}} ->
          c1 = to_float_01(c)
          {Map.put(si2, :acc_conflict, c1), c1}

        _ ->
          c0 = Map.get(si, :acc_conflict, 0.0) |> to_float_01()
          {si, c0}
      end
    end
  end

  defp safe_acc_assess(si, opts) do
    opts_kw = if is_list(opts) and Keyword.keyword?(opts), do: opts, else: []

    if Code.ensure_loaded?(Brain.ACC) and function_exported?(Brain.ACC, :assess, 2) do
      try do
        apply(Brain.ACC, :assess, [si, opts_kw])
      rescue
        _ -> {:ok, %{si: si, conflict: Map.get(si, :acc_conflict, 0.0)}}
      catch
        _, _ -> {:ok, %{si: si, conflict: Map.get(si, :acc_conflict, 0.0)}}
      end
    else
      {:ok, %{si: si, conflict: Map.get(si, :acc_conflict, 0.0)}}
    end
  end

  defp as_float(x) when is_number(x), do: x * 1.0

  defp as_float(x) when is_binary(x) do
    case Float.parse(x) do
      {f, _} -> f
      _ -> 0.0
    end
  end

  defp as_float(_), do: 0.0

  defp to_float_01(x), do: as_float(x) |> clamp01()
  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x))
  defp clamp01(_), do: 0.0

  # ───────────────────────── PFC integration ─────────────────────────

  defp safe_pfc_policy(si) do
    try do
      Brain.PFC.policy(si)
    rescue
      _ -> []
    catch
      _, _ -> []
    end
  end

  defp enrich_for_policy(si_or_cands, tokens0) do
    if is_map(si_or_cands), do: Map.put(si_or_cands, :tokens, tokens0), else: %{tokens: tokens0}
  end

  # ───────────────────────── Inputs ─────────────────────────

  defp build_lifg_inputs(si_or_cands) do
    cond do
      is_map(si_or_cands) ->
        sentence =
          Map.get(si_or_cands, :sentence) ||
            Map.get(si_or_cands, "sentence")

        tokens0 =
          Map.get(si_or_cands, :tokens) ||
            Map.get(si_or_cands, "tokens") ||
            []

        tokens =
          if is_list(tokens0) do
            tokens0
          else
            []
          end

        %{tokens: tokens, sentence: sentence}

      is_list(si_or_cands) ->
        %{tokens: [], sentence: nil}

      true ->
        %{tokens: [], sentence: nil}
    end
  end
end
