defmodule Brain.PMTG do
  @moduledoc """
  Posterior Middle Temporal Gyrus (pMTG) — controlled semantic retrieval.

  Role (modeled): when LIFG returns low-confidence choices (`needy`), pMTG plans
  retrieval queries to fetch clarifying evidence (e.g., DB rows, episodic context),
  then optionally boosts the current winners or re-runs LIFG with enriched evidence.

  Modes:
    • `:boost` (default) — bump `chosen_id` if any evidence found; softly inhibit alts
    • `:rerun`          — re-run `LIFG.Stage1` with a slate built from evidence
    • `:none`           — plan/record only (no side effects)
  """

  use Brain, region: :pmtg
  @name __MODULE__

  @type si :: map()
  @type choice :: map()

  @typedoc "Retrieval query planned per needy choice"
  @type query :: %{
          required(:stage) => :pmtg,
          required(:token_index) => non_neg_integer(),
          required(:lemma) => String.t(),
          required(:chosen_id) => String.t() | nil,
          required(:alt_ids) => [String.t()],
          required(:limit) => pos_integer()
        }

  @typedoc "Evidence bundle returned by Episodes/Lexicon backends"
  @type evidence_item :: %{
          required(:token_index) => non_neg_integer(),
          required(:lemma) => String.t(),
          required(:chosen_id) => String.t() | nil,
          required(:alt_ids) => [String.t()],
          required(:episodes) => list(),
          required(:lexicon) => list()
        }

  # ───────────────────────────── Public API ─────────────────────────────

  @spec consult([choice()], [map()], keyword()) :: :ok
  def consult(list, tokens, opts \\ [])
      when is_list(list) and is_list(tokens) and is_list(opts) do
    :gen_server.cast(@name, {:consult, list, tokens, opts})
  end

  @spec consult_sync([choice()], [map()], keyword()) ::
          {:ok,
           %{
             si: si(),
             queries: [query()],
             evidence: [evidence_item()],
             choices: [choice()],
             mode: atom(),
             rerun?: boolean()
           }}
  def consult_sync(list, tokens, opts \\ [])
      when is_list(list) and is_list(tokens) and is_list(opts) do
    GenServer.call(@name, {:consult_sync, list, tokens, opts})
  end

  @spec status(pid() | module()) :: map()
  def status(server \\ @name), do: GenServer.call(server, :status)

  @spec configure(keyword()) :: :ok
  def configure(opts) when is_list(opts), do: :gen_server.cast(@name, {:configure, opts})

  @spec reset() :: :ok
  def reset, do: GenServer.call(@name, :reset)

  # ─────────────────────── GenServer overrides/state ───────────────────────

  @impl true
  def init(opts) do
    # Region start_link/1 may pass a map (common under supervisors). Do not use Keyword.get/3 here.
    opts_map = Brain.Region.opts_to_map(opts)

    keep =
      Map.get(opts_map, :window_keep) ||
        Map.get(opts_map, "window_keep") ||
        Application.get_env(:brain, :pmtg_window_keep, 50)

    mode =
      Map.get(opts_map, :mode) ||
        Map.get(opts_map, "mode") ||
        Application.get_env(:brain, :pmtg_mode, :boost)

    {:ok,
     %{
       region: :pmtg,
       opts: %{mode: mode, window_keep: keep} |> Map.merge(opts_map),
       window_keep: keep,
       window: [],
       last: nil
     }}
  end

  @impl true
  def handle_cast({:configure, opts}, state) do
    opts_map = Brain.Region.opts_to_map(opts)

    keep =
      Map.get(opts_map, :window_keep) ||
        Map.get(opts_map, "window_keep") ||
        state.window_keep

    mode =
      Map.get(opts_map, :mode) ||
        Map.get(opts_map, "mode") ||
        Map.get(state.opts, :mode)

    {:noreply,
     state
     |> Map.put(:window_keep, keep)
     |> Map.put(:opts, state.opts |> Map.merge(%{mode: mode, window_keep: keep}) |> Map.merge(opts_map))}
  end

  @impl true
  def handle_cast({:consult, list, tokens, opts}, state) do
    t0 = System.monotonic_time()

    %{need: need, si: si1, queries: queries, evidence: evidence, thr: thr, pmin: pmin, mode: mode} =
      plan_and_collect(list, tokens, opts, state)

    _ = apply_mode(si1, need, list, evidence, mode, opts, emit_mode: :async)

    timing_ms = System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)
    {last, window} = build_last_and_window(si1, queries, evidence, need, state, timing_ms)

    :telemetry.execute(
      [:brain, :pmtg, :consult],
      %{needy: length(need)},
      %{thr: thr, p_min: pmin, mode: mode}
    )

    {:noreply, %{state | last: last, window: window}}
  end

  @impl true
  def handle_call({:consult_sync, list, tokens, opts}, _from, state) do
    t0 = System.monotonic_time()

    %{need: need, si: si1, queries: queries, evidence: evidence, thr: thr, pmin: pmin, mode: mode} =
      plan_and_collect(list, tokens, opts, state)

    {choices, rerun?, si_final} =
      apply_mode(si1, need, list, evidence, mode, opts, emit_mode: :sync)

    timing_ms = System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)
    {last, window} = build_last_and_window(si1, queries, evidence, need, state, timing_ms)

    :telemetry.execute(
      [:brain, :pmtg, :consult],
      %{needy: length(need)},
      %{thr: thr, p_min: pmin, mode: mode}
    )

    {:reply,
     {:ok,
      %{
        si: si_final,
        queries: queries,
        evidence: evidence,
        choices: choices,
        mode: mode,
        rerun?: rerun?
      }}, %{state | last: last, window: window}}
  end

  @impl true
  def handle_call(:reset, _from, state), do: {:reply, :ok, %{state | last: nil, window: []}}

  @impl true
  def handle_call(:status, _from, state), do: {:reply, state, state}

  @impl true
  def handle_call(other, from, state), do: super(other, from, state)

  # ─────────────────────────── Pure planning stage ───────────────────────────

  @spec resolve(si(), [choice()], keyword()) ::
          {:ok, %{si: si(), queries: [query()], audit: map()}}
  def resolve(si, needy, opts \\ []) when is_map(si) and is_list(needy) do
    t0 = System.monotonic_time()

    queries =
      Enum.map(needy, fn ch ->
        %{
          stage: :pmtg,
          token_index: ch[:token_index],
          lemma: lemma_from_choice(ch, si),
          chosen_id: ch[:chosen_id],
          alt_ids: List.wrap(ch[:alt_ids] || []),
          limit: Keyword.get(opts, :limit, 5)
        }
      end)

    ev = %{stage: :pmtg, needy_count: length(needy), planned_queries: length(queries)}
    si2 = Map.update(si, :trace, [ev], fn tr -> [ev | tr] end)

    audit = %{stage: :pmtg, needy_count: ev.needy_count}
    timing_ms = System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)
    {:ok, %{si: si2, queries: queries, audit: Map.put(audit, :timing_ms, timing_ms)}}
  end

  # ───────────────────── Evidence gathering (best-effort) ───────────────────

  @spec fetch_evidence([query()], [map()], keyword()) :: [evidence_item()]
  def fetch_evidence(queries, tokens, opts) when is_list(queries) and is_list(tokens) do
    tmap =
      Enum.reduce(tokens, %{}, fn
        %{} = t, acc ->
          idx = Map.get(t, :index) || Map.get(t, "index")
          if is_integer(idx), do: Map.put(acc, idx, t), else: acc

        _, acc ->
          acc
      end)

    use_syns? = Keyword.get(opts, :use_synonyms?, true)

    syn_top_k =
      Keyword.get(
        opts,
        :synonyms_top_k,
        Application.get_env(:brain, Brain.Recall.Synonyms, [])[:top_k] || 8
      )

    dbg? =
      Keyword.get(
        opts,
        :emit_variant_debug?,
        Application.get_env(:brain, :pmtg_variant_debug, false)
      )

    Enum.map(queries, fn q ->
      tok = Map.get(tmap, q.token_index, %{})

      variants_core = lemma_variants(q.lemma, tok)
      variants_syns = if use_syns?, do: synonyms_for(q.lemma, tok, syn_top_k), else: []

      variants =
        (variants_core ++ variants_syns)
        |> Enum.map(&String.downcase/1)
        |> Enum.uniq()

      eps_hits =
        variants
        |> Enum.flat_map(&episodes_hits(&1, q.limit))
        |> uniq_by_id()

      lex_hits =
        variants
        |> Enum.flat_map(&lexicon_hits(&1, q.limit, opts))
        |> uniq_by_id()

      _ =
        if dbg? do
          :telemetry.execute(
            [:brain, :pmtg, :variants],
            %{tried: length(variants), eps: length(eps_hits), lex: length(lex_hits)},
            %{lemma: q.lemma, token_index: q.token_index}
          )
        end

      base = %{
        token_index: q.token_index,
        lemma: q.lemma,
        chosen_id: q.chosen_id,
        alt_ids: q.alt_ids,
        episodes: eps_hits,
        lexicon: lex_hits
      }

      if dbg? do
        Map.merge(base, %{
          variants_tried: variants,
          variants_hit: %{
            episodes:
              Enum.map(eps_hits, &(&1[:lemma] || &1["lemma"] || &1[:id] || &1["id"])),
            lexicon: Enum.map(lex_hits, &(&1[:lemma] || &1["lemma"] || &1[:id] || &1["id"]))
          }
        })
      else
        base
      end
    end)
  end

  @spec fetch_evidence([query()], keyword()) :: [evidence_item()]
  def fetch_evidence(queries, opts) when is_list(queries) and is_list(opts) do
    fetch_evidence(queries, [], opts)
  end

  defp episodes_hits(nil, _limit), do: []
  defp episodes_hits("", _limit), do: []

  defp episodes_hits(lemma, limit) do
    mod = Module.concat([Db, Episodes])

    cond do
      Code.ensure_loaded?(mod) and function_exported?(mod, :find_by_lemma, 2) ->
        safe_call(fn -> mod.find_by_lemma(lemma, limit) end)

      Code.ensure_loaded?(mod) and function_exported?(mod, :search, 1) ->
        safe_call(fn -> mod.search(lemma) end) |> Enum.take(limit)

      true ->
        []
    end
  end

  defp lexicon_hits(nil, _limit, _opts), do: []
  defp lexicon_hits("", _limit, _opts), do: []

  defp lexicon_hits(lemma, limit, opts) do
    injected = Keyword.get(opts, :lexicon_mod)

    cond do
      is_atom(injected) and Code.ensure_loaded?(injected) and function_exported?(injected, :lookup, 2) ->
        safe_call(fn -> injected.lookup(lemma, limit) end)

      true ->
        mods = [Module.concat([Db, Lexicon]), Lexicon]

        case Enum.find(mods, &Code.ensure_loaded?/1) do
          nil ->
            []

          mod ->
            cond do
              function_exported?(mod, :lookup, 2) ->
                safe_call(fn -> mod.lookup(lemma, limit) end)

              function_exported?(mod, :definitions_for, 2) ->
                safe_call(fn -> mod.definitions_for(lemma, limit) end)

              true ->
                []
            end
        end
    end
  end

  defp safe_call(fun) do
    try do
      case fun.() do
        list when is_list(list) -> list
        other -> List.wrap(other)
      end
    rescue
      _ -> []
    catch
      _, _ -> []
    end
  end

  # ─────────────── Sense compatibility (MWE vs unigram) ───────────────

  @spec enforce_sense_compatibility([evidence_item()], [map()]) :: [evidence_item()]
  def enforce_sense_compatibility(evidence, tokens)
      when is_list(evidence) and is_list(tokens) do
    tmap =
      Enum.reduce(tokens, %{}, fn
        %{} = t, acc ->
          case token_index_of(t) do
            i when is_integer(i) -> Map.put(acc, i, t)
            _ -> acc
          end

        _, acc ->
          acc
      end)

    Enum.map(evidence, fn
      %{} = ev ->
        idx = ev[:token_index] || ev["token_index"]
        token = if is_integer(idx), do: Map.get(tmap, idx, %{}), else: %{}

        orig_lex = ev[:lexicon] || ev["lexicon"] || []
        lex = if is_list(orig_lex), do: orig_lex, else: List.wrap(orig_lex)

        {lex_out, kept_compatible, fallback?} = filter_lexicon_for_token(lex, token)

        ev
        |> Map.put(:lexicon, lex_out)
        |> maybe_emit_no_mwe_senses(token, lex, kept_compatible, fallback?)

      other ->
        other
    end)
  end

  defp filter_lexicon_for_token(lexicon, token) when is_list(lexicon) and is_map(token) do
    mwe? = token_mwe?(token)
    unigram? = token_unigram?(token)

    cond do
      mwe? -> filter_by_space_compat(lexicon, true)
      unigram? -> filter_by_space_compat(lexicon, false)
      true -> {lexicon, length(lexicon), false}
    end
  end

  defp filter_lexicon_for_token(lexicon, _token) when is_list(lexicon),
    do: {lexicon, length(lexicon), false}

  defp filter_by_space_compat([], _want_space?), do: {[], 0, false}

  defp filter_by_space_compat(lexicon, want_space?) when is_list(lexicon) do
    compatible =
      Enum.filter(lexicon, fn sense ->
        lemma = (sense[:lemma] || sense["lemma"] || "") |> to_string()
        has_space = String.contains?(lemma, " ")
        if want_space?, do: has_space, else: not has_space
      end)

    cond do
      compatible != [] ->
        {compatible, length(compatible), false}

      true ->
        {lexicon, 0, true}
    end
  end

  defp maybe_emit_no_mwe_senses(ev, token, orig_lex, kept_compatible, _fallback?)
       when is_map(ev) and is_map(token) and is_list(orig_lex) and is_integer(kept_compatible) do
    if token_mwe?(token) and orig_lex != [] and kept_compatible == 0 do
      safe_exec_telemetry(
        [:brain, :pmtg, :no_mwe_senses],
        %{
          orig: length(orig_lex),
          kept: 0,
          token_index: token_index_of(token) || ev[:token_index] || ev["token_index"] || 0,
          phrase: token_phrase_of(token) || ev[:lemma] || ev["lemma"] || ""
        }
      )
    end

    ev
  end

  defp maybe_emit_no_mwe_senses(ev, _token, _orig_lex, _kept_compatible, _fallback?), do: ev

  # ───────────────────────── Actions on evidence ─────────────────────────────

  defp do_boost(evidence, opts) do
    boost = Keyword.get(opts, :boost, 0.15) * 1.0
    inhib = Keyword.get(opts, :inhib, -0.05) * 1.0

    Enum.each(evidence, fn ev ->
      has_hits = ev.episodes != [] or ev.lexicon != []

      if has_hits and is_binary(ev.chosen_id) do
        Brain.cell_cast(ev.chosen_id, {:activate, %{delta: boost}})

        ev.alt_ids
        |> List.wrap()
        |> Enum.uniq()
        |> Enum.each(fn aid ->
          Brain.cell_cast(aid, {:activate, %{delta: inhib}})
        end)
      end
    end)

    :ok
  end

  @spec do_rerun(si(), [evidence_item()], keyword()) ::
          {:ok, %{si: si(), choices: [choice()]}}
  defp do_rerun(si, evidence, opts) do
    sense_candidates =
      evidence
      |> Enum.group_by(& &1.token_index)
      |> Enum.into(%{}, fn {tidx, evs} ->
        senses =
          evs
          |> Enum.flat_map(fn ev -> List.wrap(ev.lexicon) end)
          |> Enum.uniq_by(fn s ->
            Map.get(s, :id) || Map.get(s, "id") || sense_key_fallback(s)
          end)
          |> Enum.with_index()
          |> Enum.map(fn {s, i} ->
            id = Map.get(s, :id) || Map.get(s, "id") || sense_key_fallback(s)
            lemma = Map.get(s, :lemma) || Map.get(s, "lemma") || ""
            pos = Map.get(s, :pos) || Map.get(s, "pos") || ""

            defn =
              Map.get(s, :gloss) ||
                Map.get(s, "gloss") ||
                Map.get(s, :definition) ||
                Map.get(s, "definition") ||
                ""

            syns = Map.get(s, :synonyms) || Map.get(s, "synonyms") || []
            feats_from_sense = Map.get(s, :features) || %{}

            eps = 1.0e-6 * (1000 - i)

            base_feats = %{
              lex_fit: 0.60,
              rel_prior: 0.50,
              activation: 0.0,
              intent_bias: 0.0,
              pos: pos,
              lemma: lemma,
              definition: defn,
              synonyms: syns
            }

            feats =
              base_feats
              |> Map.merge(feats_from_sense, fn _k, _base, from_sense -> from_sense end)
              |> Map.update(:rel_prior, 0.50 + eps, &(&1 + eps))
              |> maybe_salutation_nudge(si, tidx)

            %{id: id, features: feats}
          end)

        {tidx, senses}
      end)

    si2 = Map.put(si, :sense_candidates, sense_candidates)

    env =
      Application.get_env(:brain, :lifg_stage1_weights, %{
        lex_fit: 0.4,
        rel_prior: 0.3,
        activation: 0.2,
        intent_bias: 0.1
      })

    base = Map.merge(env, Map.new(Keyword.get(opts, :weights, [])))
    bump = Map.get(Map.new(opts), :rerun_weights_bump, %{})
    weights = Map.merge(base, bump, fn _k, a, b -> a + b end)

    {:ok, %{choices: choices}} =
      Brain.LIFG.Stage1.run(
        si2,
        weights: weights,
        chargram_event: [:brain, :pmtg, :chargram_violation],
        boundary_event: [:brain, :pmtg, :boundary_drop]
      )

    slate = Map.get(si2, :sense_candidates, %{})
    choices = break_rerun_ties(choices, slate, weights, opts)

    {:ok, %{si: si2, choices: choices}}
  end

  defp break_rerun_ties(choices, slate_by_tidx, weights, opts)
       when is_list(choices) and is_map(slate_by_tidx) and is_map(weights) do
    eps = Keyword.get(opts, :rerun_tie_epsilon, 1.0e-9) * 1.0

    Enum.map(choices, fn ch ->
      tidx = choice_token_index(ch)
      cands = Map.get(slate_by_tidx, tidx, [])

      cond do
        cands == [] -> ch
        not tied_choice?(ch, eps) -> ch
        true ->
          {best_id, best, second, score_map} = best_candidate(cands, weights)

          alt_ids =
            score_map
            |> Map.keys()
            |> Enum.reject(&(&1 == best_id))

          ch
          |> Map.put(:chosen_id, best_id)
          |> Map.put(:id, best_id)
          |> Map.put(:scores, score_map)
          |> Map.put(:alt_ids, alt_ids)
          |> Map.put(:margin, best - second)
      end
    end)
  end

  defp tied_choice?(ch, eps) do
    m = num(Map.get(ch, :margin) || Map.get(ch, :prob_margin) || 0.0)
    sm = score_margin_from_choice(ch)
    abs(m) <= eps and abs(sm) <= eps
  end

  defp score_margin_from_choice(ch) do
    scores = Map.get(ch, :scores) || Map.get(ch, :probs) || %{}

    if is_map(scores) and map_size(scores) > 1 do
      vals =
        scores
        |> Map.values()
        |> Enum.map(&num/1)
        |> Enum.sort(:desc)

      Enum.at(vals, 0, 0.0) - Enum.at(vals, 1, 0.0)
    else
      0.0
    end
  end

  defp best_candidate(cands, weights) do
    scored =
      Enum.map(cands, fn c ->
        id = Map.get(c, :id) || Map.get(c, "id") || "?"
        feats = Map.get(c, :features) || Map.get(c, "features") || %{}
        {to_string(id), candidate_score(feats, weights)}
      end)

    score_map = Map.new(scored)

    {best_id, best_score} =
      scored
      |> Enum.sort_by(fn {id, s} -> {-s, id} end)
      |> hd()

    second_score =
      scored
      |> Enum.map(fn {_id, s} -> s end)
      |> Enum.sort(:desc)
      |> Enum.at(1, 0.0)

    {best_id, best_score, second_score, score_map}
  end

  defp candidate_score(feats, weights) when is_map(feats) and is_map(weights) do
    w_lex = Map.get(weights, :lex_fit, 0.0) |> num()
    w_rel = Map.get(weights, :rel_prior, 0.0) |> num()
    w_act = Map.get(weights, :activation, 0.0) |> num()
    w_int = Map.get(weights, :intent_bias, 0.0) |> num()

    lex = Map.get(feats, :lex_fit) || Map.get(feats, "lex_fit") || 0.0
    rel = Map.get(feats, :rel_prior) || Map.get(feats, "rel_prior") || 0.0
    act = Map.get(feats, :activation) || Map.get(feats, "activation") || 0.0
    ib = Map.get(feats, :intent_bias) || Map.get(feats, "intent_bias") || 0.0

    w_lex * num(lex) + w_rel * num(rel) + w_act * num(act) + w_int * num(ib)
  end

  defp num(v) when is_integer(v), do: v * 1.0
  defp num(v) when is_float(v), do: v
  defp num(_), do: 0.0

  defp maybe_salutation_nudge(feats, si, tidx) when is_map(feats) do
    phrase =
      si
      |> Map.get(:tokens, [])
      |> Enum.at(tidx, %{})
      |> Map.get(:phrase, "")
      |> to_string()

    pos = (feats[:pos] || feats["pos"] || "") |> to_string() |> String.downcase()

    if Regex.match?(~r/^(hello|hi|hey)\b/i, phrase) and String.contains?(pos, "interjection") do
      Map.update(feats, :lex_fit, 0.63, &(&1 + 0.03))
    else
      feats
    end
  end

  # ─────────────────────────────── Helpers ───────────────────────────────

  defp plan_and_collect(list, tokens, opts, state) do
    thr = margin_threshold(opts)
    pmin = p_min(opts)

    need =
      if Keyword.get(opts, :already_needy, false),
        do: list,
        else: needy_from_choices(list, thr, pmin)

    si0 = %{tokens: tokens, trace: []}
    {:ok, %{si: si1, queries: queries}} = resolve(si0, need, limit: Keyword.get(opts, :limit, 5))

    evidence =
      fetch_evidence(queries, si1.tokens, opts)
      |> enforce_sense_compatibility(si1.tokens)

    mode =
      opts[:mode] || Map.get(state.opts, :mode) || Application.get_env(:brain, :pmtg_mode, :boost)

    %{need: need, si: si1, queries: queries, evidence: evidence, thr: thr, pmin: pmin, mode: mode}
  end

  defp apply_mode(si1, _need, list, evidence, mode, opts, emit_mode: emit_mode) do
    case mode do
      :boost ->
        do_boost(evidence, opts)
        {list, false, si1}

      :rerun ->
        case do_rerun(si1, evidence, opts) do
          {:ok, %{si: si2, choices: rerun_choices}} ->
            emit_rerun_event(rerun_choices, emit_mode)
            {merge_choices(list, rerun_choices), true, si2}

          _ ->
            {list, false, si1}
        end

      _ ->
        {list, false, si1}
    end
  end

  defp build_last_and_window(si1, queries, evidence, need, state, timing_ms) do
    last = %{
      si: si1,
      queries: queries,
      evidence: evidence,
      audit: %{stage: :pmtg, timing_ms: timing_ms, needy_count: length(need)}
    }

    window =
      [
        {System.system_time(:millisecond), %{needy_count: length(need), queries: queries}}
        | state.window
      ]
      |> Enum.take(state.window_keep)

    {last, window}
  end

  defp needy_from_choices(choices, thr, pmin) do
    Enum.filter(choices, fn ch ->
      m = Map.get(ch, :margin, 1.0) * 1.0
      alts = Map.get(ch, :alt_ids, [])
      p1 = p_top1(ch)

      low_margin? = is_number(m) and m < thr * 1.0
      low_prob? = is_number(p1) and p1 < pmin * 1.0
      has_alts? = is_list(alts) and length(alts) > 0

      (low_margin? or low_prob?) and has_alts?
    end)
  end

  defp p_top1(ch) do
    chosen_id = Map.get(ch, :chosen_id) || Map.get(ch, :id)
    probs = Map.get(ch, :probs, %{})

    cond do
      is_binary(chosen_id) and is_map(probs) and is_number(Map.get(probs, chosen_id)) ->
        Map.get(probs, chosen_id) * 1.0

      is_number(Map.get(ch, :p_top1)) ->
        Map.get(ch, :p_top1) * 1.0

      is_map(Map.get(ch, :scores)) ->
        ch
        |> Map.get(:scores)
        |> Map.values()
        |> Enum.max(fn -> 0.0 end)
        |> Kernel.*(1.0)

      true ->
        1.0
    end
  end

  defp margin_threshold(opts) do
    Keyword.get(
      opts,
      :margin_threshold,
      Application.get_env(:brain, :pmtg_margin_threshold, 0.15)
    )
    |> Kernel.*(1.0)
  end

  defp p_min(opts) do
    Keyword.get(opts, :p_min, Application.get_env(:brain, :acc_p_min, 0.65))
    |> Kernel.*(1.0)
  end

  defp lemma_from_choice(ch, si) do
    idx = ch[:token_index]

    with true <- is_integer(idx),
         tokens when is_list(tokens) <- Map.get(si, :tokens),
         %{} = tok <- Enum.at(tokens, idx),
         phrase when is_binary(phrase) <- Map.get(tok, :phrase) || Map.get(tok, "phrase") do
      phrase
    else
      _ -> ch[:lemma] || "?"
    end
  end

  defp merge_choices(baseline, rerun) do
    by_tok =
      Enum.reduce(baseline, %{}, fn ch, acc ->
        Map.put(acc, choice_token_index(ch), ch)
      end)

    by_tok =
      Enum.reduce(rerun, by_tok, fn ch, acc ->
        Map.put(acc, choice_token_index(ch), ch)
      end)

    by_tok
    |> Map.values()
    |> Enum.sort_by(&choice_token_index/1)
  end

  defp choice_token_index(ch) when is_map(ch) do
    case {Map.get(ch, :token_index), Map.get(ch, :index)} do
      {i, _} when is_integer(i) -> i
      {_, i} when is_integer(i) -> i
      _ -> 0
    end
  end

  defp emit_rerun_event(choices, mode) do
    groups =
      choices
      |> Enum.map(& &1.token_index)
      |> MapSet.new()
      |> MapSet.size()

    :telemetry.execute(
      [:brain, :pmtg, :rerun],
      %{groups: groups, choices: length(choices)},
      %{mode: mode}
    )
  end

  defp sense_key_fallback(s) do
    "#{Map.get(s, :lemma) || Map.get(s, "lemma") || "?"}|#{Map.get(s, :pos) || Map.get(s, "pos") || "?"}"
  end

  # ── Variant helpers & synonyms (Brain-side) ────────────────────────────

  defp lemma_variants(nil, _tok), do: []
  defp lemma_variants("", _tok), do: []

  defp lemma_variants(lemma, tok) do
    base = to_string(lemma) |> String.trim()
    down = String.downcase(base)

    expanded =
      down
      |> String.replace(~r/\bim\b/, "i'm")
      |> String.replace(~r/\bi am\b/, "i'm")

    head =
      case {Map.get(tok, :n) || Map.get(tok, "n"), String.contains?(down, " ")} do
        {n, true} when is_integer(n) and n > 1 -> down |> String.split() |> List.last()
        _ -> nil
      end

    [down, expanded, head]
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
  end

  defp synonyms_for(nil, _tok, _top_k), do: []
  defp synonyms_for("", _tok, _top_k), do: []

  defp synonyms_for(lemma, tok, top_k) do
    pos = Map.get(tok, :pos) || Map.get(tok, "pos")

    try do
      if pos do
        Brain.Recall.Synonyms.lookup_by_pos(lemma, pos, top_k) |> List.wrap()
      else
        Brain.Recall.Synonyms.lookup(lemma, top_k) |> List.wrap()
      end
    rescue
      _ -> []
    catch
      _, _ -> []
    end
  end

  defp uniq_by_id(list) do
    Enum.uniq_by(list, fn s ->
      Map.get(s, :id) || Map.get(s, "id") ||
        {Map.get(s, :lemma) || Map.get(s, "lemma"), Map.get(s, :pos) || Map.get(s, "pos")}
    end)
  end

  # ── Local token helpers ───────────────────────────────────────────────────

  defp token_index_of(%{} = t),
    do: t[:index] || t["index"] || t[:token_index] || t["token_index"]

  defp token_phrase_of(%{} = t),
    do: t[:phrase] || t["phrase"] || t[:lemma] || t["lemma"] || t[:word] || t["word"] || ""

  defp token_mwe?(%{} = t) do
    n = t[:n] || t["n"]
    mw = t[:mw] || t["mw"] || false
    (is_integer(n) and n > 1) or mw == true
  end

  defp token_unigram?(%{} = t) do
    n = t[:n] || t["n"]
    is_integer(n) and n == 1
  end

  # --- Telemetry helpers ---

  defp safe_exec_telemetry(event, measurements),
    do: safe_exec_telemetry(event, measurements, %{})

  defp safe_exec_telemetry(event, measurements, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(event, measurements, meta)
    else
      :ok
    end
  end
end

