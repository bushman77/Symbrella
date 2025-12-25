defmodule Brain.Pipeline.LIFGStage1 do
  @moduledoc false

  alias Brain.Episodes.Writer, as: EpWriter
  alias Brain.LIFG
  alias Brain.WorkingMemory

  @pipeline_stop_event [:brain, :pipeline, :lifg_stage1, :stop]
  @no_candidates_event [:brain, :pipeline, :lifg_stage1, :no_candidates]

  @spec run(map() | [map()], term(), keyword(), map()) :: {{:ok, map()}, map()}
  def run(si_or_cands, _ctx_vec, opts, state) when is_list(opts) and is_map(state) do
    t0 = System.monotonic_time()
    now_ms = System.system_time(:millisecond)

    # ── Inputs ───────────────────────────────────────────────────────

    %{tokens: tokens0, sentence: sentence} = build_lifg_inputs(si_or_cands)

    self_name = detect_self_name(tokens0, sentence)

    state0 =
      state
      |> Map.put(:attention, Map.get(state, :attention, %{}) |> Map.put(:self_name, self_name))

    si_for_policy = enrich_for_policy(si_or_cands, tokens0)

    pfc_opts = safe_pfc_policy(si_for_policy)

    lifg_opts =
      [scores: :all, normalize: :softmax, parallel: :auto]
      |> Keyword.merge(pfc_opts)
      |> Keyword.merge(opts)

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

    # ── Stage-1 disambiguation (safe) ─────────────────────────────────

    si2 = safe_disambiguate_stage1(si1, lifg_opts)
    {:ok, out0_raw} = lifg_out_from_trace(si2)

    out0 =
      out0_raw
      |> Map.update(:audit, %{}, fn audit ->
        Map.put(audit || %{}, :self_name, self_name)
      end)

    out0 =
      case Brain.LIFG.Hygiene.run(%{}, out0.choices, []) do
        {:ok, %{choices: cleaned}} -> %{out0 | choices: cleaned}
        _ -> out0
      end

    # ── Downstream side-effects (read-only) ──────────────────────────

    _ = maybe_assess_acc(%{tokens: tokens0, choices: out0.choices}, already_needy: true)
    _ = maybe_ingest_atl(out0.choices, tokens0)
    _ = maybe_consult_pmtg(out0.choices, tokens0)
    _ = maybe_store_episode(tokens0, si2, out0)

    {boosts2, inhib2} = maybe_rescale_signals(out0, lifg_opts)
    _ = Brain.__pipeline_apply_control_signals__(boosts2, inhib2, lifg_opts)

    # ── Brain-owned WM maintenance (decay + optional gating + eviction) ─────────

    stateA =
      state0
      |> Brain.apply_decay(now_ms)

    stateB =
      maybe_gate_into_wm(stateA, si2, lifg_opts, now_ms)

    state1 =
      stateB
      |> Brain.evict_if_needed()

    :telemetry.execute(
      @pipeline_stop_event,
      %{
        duration_ms:
          System.convert_time_unit(
            System.monotonic_time() - t0,
            :native,
            :millisecond
          )
      },
      %{
        winners: length(out0.choices),
        boosts: length(out0.boosts),
        inhibitions: length(out0.inhibitions)
      }
    )

    {{:ok, out0}, state1}
  end

  # ───────────────────────── Safety wrappers ─────────────────────────

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

  # ───────────────────────── Trace → output ──────────────────────────

  defp lifg_out_from_trace(%{trace: [ev | _]}) when is_map(ev) do
    choices = ev[:choices] || ev["choices"] || []
    boosts = ev[:boosts] || ev["boosts"] || []
    inhibitions = ev[:inhibitions] || ev["inhibitions"] || []

    audit =
      ev
      |> Map.drop([:choices, "choices", :boosts, "boosts", :inhibitions, "inhibitions"])

    {:ok, %{choices: choices, boosts: boosts, inhibitions: inhibitions, audit: audit}}
  end

  defp lifg_out_from_trace(_),
    do: {:ok, %{choices: [], boosts: [], inhibitions: [], audit: %{stage: :lifg_stage1}}}

  # ───────────────────────── Integration hooks ───────────────────────

  defp maybe_ingest_atl(choices, tokens) do
    if Process.whereis(Brain.ATL), do: Brain.ATL.ingest(choices, tokens), else: :noop
  end

  defp maybe_consult_pmtg(choices, tokens) do
    conf =
      choices
      |> Enum.map(&Map.get(&1, :margin, 1.0))
      |> Enum.min(fn -> 1.0 end)

    :telemetry.execute([:brain, :lifg, :confidence], %{value: conf}, %{})

    needy =
      Enum.filter(choices, fn ch ->
        m = Map.get(ch, :margin, 1.0)
        alts = Map.get(ch, :alt_ids, [])
        is_number(m) and m < 0.18 and alts != []
      end)

    if (conf < 0.18 or needy != []) and Process.whereis(Brain.PMTG) do
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

  # ───────────────────────── Signals ─────────────────────────────────

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
            m = ch[:margin] || 0.05
            {ch[:chosen_id], clamp.(base_boost * max(m, 0.05))}
          end

        inhib2 =
          for ch <- choices,
              aid <- ch[:alt_ids] || [] do
            m = ch[:margin] || 0.0
            {aid, clamp.(-base_inhib * max(0.2, 1.0 - m))}
          end

        {boosts2, inhib2}
    end
  end

  # ───────────────────────── WM gating (tests depend on this) ─────────

  defp maybe_gate_into_wm(state, si, opts, now_ms) when is_map(state) and is_map(si) do
    gate? = Keyword.get(opts, :gate_into_wm, false)

    if gate? do
      ev1 =
        case Map.get(si, :trace) do
          [ev | _] when is_map(ev) -> ev
          _ -> %{}
        end

      decisions = stage2_decisions(ev1, si, opts)

      wm0 = Map.get(state, :wm, [])

      cfg =
        Map.get(state, :wm_cfg) ||
          %{
            capacity: Application.get_env(:brain, :wm_capacity, 64),
            decay_ms: Application.get_env(:brain, :wm_decay_ms, 20_000),
            gate_threshold: Keyword.get(opts, :lifg_min_score, 0.6),
            merge_duplicates?: true
          }

      wm1 = WorkingMemory.ingest_stage2(wm0, decisions, now_ms, cfg)
      added = max(length(wm1) - length(wm0), 0)

      emit_gate_decisions(decisions)

      :telemetry.execute(
        [:brain, :wm, :update],
        %{added: added, size: length(wm1)},
        %{reason: :gate_from_lifg}
      )

      Map.put(state, :wm, wm1)
    else
      state
    end
  end

  defp maybe_gate_into_wm(state, _si, _opts, _now_ms), do: state

defp stage2_decisions(ev1, si, opts) do
  # Fallback: commit any choice with score >= lifg_min_score
  min_score = (Keyword.get(opts, :lifg_min_score, 0.6) || 0.6) * 1.0

  choices =
    (ev1[:choices] || ev1["choices"] || [])
    |> List.wrap()

  fallback =
    for ch <- choices,
        score = gate_score(ch),
        score >= min_score do
      ch
      |> Map.merge(%{decision: :allow, source: :lifg, score: score})
    end

  st2 = Brain.LIFG.Stage2

  if Code.ensure_loaded?(st2) and function_exported?(st2, :run, 2) do
    try do
      case apply(st2, :run, [si, opts]) do
        {:ok, %{event: ev2}} ->
          decisions = Map.get(ev2, :decisions, []) |> List.wrap()

          if decisions != [] do
            Enum.map(decisions, &ensure_decision_score!/1)
          else
            fallback
          end

        {:skip, _} ->
          fallback

        _ ->
          fallback
      end
    rescue
      _ -> fallback
    catch
      :exit, _ -> fallback
    end
  else
    fallback
  end
end

# Prefer probability-like confidence for gating (p_top1 / probs / scores), not raw prior :score.
defp gate_score(%{} = ch) do
  score0 = ch[:score] || ch["score"]

  score01 =
    cond do
      is_number(score0) and score0 >= 0.0 and score0 <= 1.0 ->
        score0 * 1.0

      is_binary(score0) ->
        case Float.parse(score0) do
          {f, ""} when f >= 0.0 and f <= 1.0 -> f
          _ -> 0.0
        end

      true ->
        0.0
    end

  p_top1 =
    case ch[:p_top1] || ch["p_top1"] do
      p when is_number(p) -> p * 1.0
      p when is_binary(p) ->
        case Float.parse(p) do
          {f, ""} -> f
          _ -> nil
        end
      _ ->
        nil
    end

  p1 =
    cond do
      is_number(p_top1) ->
        p_top1

      Code.ensure_loaded?(LIFG) and function_exported?(LIFG, :top1_prob, 1) ->
        LIFG.top1_prob(ch)

      true ->
        score01
    end

  max(score01, p1)
  |> max(0.0)
  |> min(1.0)
end

defp gate_score(_), do: 0.0

defp ensure_decision_score!({tag, %{} = ch}) when tag in [:commit, :allow, :boost],
  do: {tag, Map.put(ch, :score, gate_score(ch))}

defp ensure_decision_score!(%{} = m),
  do: Map.put(m, :score, gate_score(m))

defp ensure_decision_score!(other), do: other

  defp emit_gate_decisions(decisions) when is_list(decisions) do
    Enum.each(decisions, fn dec ->
      {choice, decision} =
        case dec do
          {:commit, %{} = ch} -> {ch, :allow}
          {:allow, %{} = ch} -> {ch, :allow}
          {:boost, %{} = ch} -> {ch, :boost}
          %{} = m ->
            d = m[:decision] || m["decision"] || :allow
            d = if is_binary(d), do: String.to_atom(d), else: d
            {m, d}
          _ ->
            {nil, nil}
        end

      if is_map(choice) and decision in [:allow, :boost] do
score = gate_score(choice)
        id = choice[:chosen_id] || choice["chosen_id"] || choice[:id] || choice["id"]
        ti = choice[:token_index] || choice["token_index"]

        :telemetry.execute(
          [:brain, :gate, :decision],
          %{score: score},
          %{decision: decision, source: :lifg, id: id, token_index: ti}
        )
      end
    end)
  end

  defp emit_gate_decisions(_), do: :ok

  # ───────────────────────── ACC / PFC / Inputs ──────────────────────

  defp maybe_assess_acc(si, opts) do
    if Code.ensure_loaded?(Brain.ACC) and function_exported?(Brain.ACC, :assess, 2) do
      try do
        Brain.ACC.assess(si, opts)
      rescue
        _ -> :noop
      end
    else
      :noop
    end
  end

  defp safe_pfc_policy(si) do
    # In tests, policy must be best-effort and must never block the Brain call path.
    if test_env?() and not Application.get_env(:brain, :pfc_policy_in_tests, false) do
      []
    else
      cond do
        not (Code.ensure_loaded?(Brain.PFC) and function_exported?(Brain.PFC, :policy, 1)) ->
          []

        not is_pid(Process.whereis(Brain.PFC)) ->
          []

        true ->
          try do
            Brain.PFC.policy(si)
          rescue
            _ -> []
          catch
            :exit, _ -> []
          end
      end
    end
  end

  defp enrich_for_policy(si_or_cands, tokens),
    do: if(is_map(si_or_cands), do: Map.put(si_or_cands, :tokens, tokens), else: %{tokens: tokens})

  defp build_lifg_inputs(%{} = si) do
    %{
      tokens: Map.get(si, :tokens) || Map.get(si, "tokens") || [],
      sentence: Map.get(si, :sentence) || Map.get(si, "sentence")
    }
  end

  defp build_lifg_inputs(_), do: %{tokens: [], sentence: nil}

  # ───────────────────────── Self-name detection ─────────────────────

  defp detect_self_name(tokens, sentence) do
    names =
      Application.get_env(:brain, :self_names, [])
      |> Enum.map(&norm_surface/1)
      |> MapSet.new()

    token_hit =
      tokens
      |> List.wrap()
      |> Enum.find_value(fn t0 ->
        t = if is_map(t0), do: t0, else: %{}
        surface = t |> token_surface() |> norm_surface()

        if surface != "" and MapSet.member?(names, surface) do
          %{hit?: true, match: surface, token_index: token_index(t), source: :tokens}
        else
          nil
        end
      end)

    cond do
      is_map(token_hit) ->
        token_hit

      is_binary(sentence) ->
        case Enum.find(words_from_sentence(sentence), &MapSet.member?(names, &1)) do
          nil -> %{hit?: false}
          w -> %{hit?: true, match: w, token_index: nil, source: :sentence}
        end

      true ->
        %{hit?: false}
    end
  end

  defp token_surface(t) when is_map(t) do
    Map.get(t, :phrase) ||
      Map.get(t, "phrase") ||
      Map.get(t, :word) ||
      Map.get(t, "word") ||
      Map.get(t, :lemma) ||
      Map.get(t, "lemma") ||
      Map.get(t, :text) ||
      Map.get(t, "text") ||
      Map.get(t, :norm) ||
      Map.get(t, "norm") ||
      ""
  end

  defp token_surface(_), do: ""

  defp token_index(t) when is_map(t) do
    raw =
      Map.get(t, :token_index) ||
        Map.get(t, "token_index") ||
        Map.get(t, :index) ||
        Map.get(t, "index")

    case raw do
      i when is_integer(i) and i >= 0 ->
        i

      i when is_binary(i) ->
        case Integer.parse(String.trim(i)) do
          {n, _} when n >= 0 -> n
          _ -> nil
        end

      _ ->
        nil
    end
  end

  defp token_index(_), do: nil

  defp words_from_sentence(s),
    do: Regex.scan(~r/[\p{L}\p{N}_']+/u, s) |> Enum.map(fn [w] -> norm_surface(w) end)

  defp norm_surface(nil), do: ""

  defp norm_surface(s) when is_binary(s),
    do: s |> String.downcase() |> String.trim("'\".,!?;:()[]{}<>")

  defp norm_surface(s), do: s |> to_string() |> norm_surface()

  defp test_env? do
    Code.ensure_loaded?(Mix) and function_exported?(Mix, :env, 0) and Mix.env() == :test
  end
end
