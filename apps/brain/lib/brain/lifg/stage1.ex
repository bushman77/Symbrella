# apps/brain/lib/brain/lifg/stage1.ex
defmodule Brain.LIFG.Stage1 do
  @moduledoc """
  LIFG.Stage1 — first-pass disambiguation scoring.

  Responsibilities
  ----------------
  • Score sense candidates per token using a weighted feature mix:
      - :lex_fit     — lexical compatibility with the token (esp. for MWEs)
      - :rel_prior   — relational/heuristic prior from candidate/id
      - :activation  — candidate activation/score (DB, slate, or synthesized)
      - :intent_bias — bias from `si.intent_bias[token_index]`
  • Apply mood nudging if the Stage1 server is running (best-effort, never blocks).
  • Apply Cerebellum calibration (best-effort) and learn after the decision.
  • Normalize to probabilities (softmax), compute margins, return winners.
  • Emit a Stage1 summary event for Blackboard/SelfPortrait.

  Notes
  -----
  Stage1 is intentionally side-effect-light. It must not depend on other GenServers
  during `handle_call/3`. Any semantic enrichment should occur upstream.

  Telemetry
  ---------
  • [:brain, :lifg, :stage1, :score]
  • [:brain, :lifg, :stage1, :boundary_drop]
  • [:brain, :lifg, :stage1, :chargram_violation]
  • [:brain, :pmtg, :mwe_fallback_emitted]
  • [:brain, :pipeline, :lifg_stage1, :stop]
  """

  use Brain, region: :lifg_stage1

  alias Brain.Utils.Safe
  alias Brain.Cerebellum
  alias Brain.MoodWeights
  alias Brain.LIFG.Guard
  alias Brain.LIFG.Reanalysis

  @default_weights %{lex_fit: 0.40, rel_prior: 0.30, activation: 0.20, intent_bias: 0.10}
  @default_scores_mode :all
  @default_margin_thr 0.15
  @default_min_margin 0.05

  # Mood weights/cap (match MoodWeights.bias/3)
  @default_mw %{expl: 0.02, inhib: -0.03, vigil: 0.02, plast: 0.00}
  @default_cap 0.05
  @mood_handler_prefix "brain-lifg-stage1-mood-"

  @function_pos ~w(determiner preposition conjunction auxiliary modal pronoun adverb particle)

  @default_rel_prior 0.93

  @pos_prior %{
    "phrase" => 0.98,
    "verb" => 0.96,
    "adj" => 0.95,
    "adv" => 0.94,
    "noun" => 0.93,
    "other" => 0.93
  }

  @greeting_lemmas MapSet.new([
                     "hey how is",
                     "how are you",
                     "how is life treating you",
                     "how is life treating"
                   ])

  # Default event for MWE fallback telemetry (tests attach to this)
  @mwe_fallback_event [:brain, :pmtg, :mwe_fallback_emitted]

  # Summary event (Blackboard typically bridges this)
  @stage1_stop_event [:brain, :pipeline, :lifg_stage1, :stop]

  # When true, Stage1 synthesizes a deterministic phrase fallback candidate for MWEs
  # that have no candidates (prevents “missing_candidates” drops).
  @default_stage1_local_mwe_fallback true

  # Function-word set used to reject junk MWE fallbacks like "to go", "going to", etc.
  # (kept local so Stage1 compiles standalone).
  @function_words MapSet.new(
                    ~w(
                      of to in on at by for from with about into over after between through during before under without within along across behind beyond up down off near among
                      the a an this that these those some any each every no neither either
                      and or but nor so yet for
                      be am is are was were being been do does did have has had having
                      can could may might must shall should will would
                      i you he she it we they me him her us them my your his her our their mine yours hers ours theirs myself yourself himself herself itself ourselves yourselves themselves
                      not never
                    )
                  )

  # ---------- Public server API (mood) ----------

  def start_link(opts), do: GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  def score(server \\ __MODULE__, ctx), do: GenServer.call(server, {:score, ctx})

  # ---------- Public Stage1 API (pure scoring) ----------

  @spec run(map()) :: {:ok, %{si: map(), choices: list(), audit: map()}} | {:error, term()}
  def run(si), do: run(si, [])

  @spec run(map(), keyword()) :: {:ok, %{si: map(), choices: list(), audit: map()}} | {:error, term()}
  def run(si, opts) when is_map(si) and is_list(opts) do
    try do
      sent0 = Safe.get(si, :sentence) || Safe.get(si, "sentence")

      sent =
        if is_binary(sent0) and String.trim(sent0) != "" do
          sent0
        else
          nil
        end

      # Stamp this run for downstream/UI separation.
      frame = build_frame(si, opts)

      si_base =
        si
        |> put_frame(frame)
        |> maybe_reset_sense_candidates(opts)
        |> stamp_slates(frame)

      # 1) Tokens + Guard preprocessing (may drop tokens)
      {tokens, guard_rejected, guard_drops} = prepare_tokens(si_base, sent)

      # Ensure SI carries sanitized tokens (critical for span logic)
      si0 = Map.put(si_base, :tokens, tokens)

      # Wire candidate slate from active_cells + MWE fallback/backfill
      lifg_opts = merged_lifg_opts(si0, opts)

si1 =
  si0
  |> Brain.LIFG.MWE.ensure_mwe_candidates(lifg_opts)
  |> Brain.LIFG.MWE.backfill_unigrams_from_active_cells(lifg_opts)
  |> Brain.LIFG.MWE.absorb_unigrams_into_mwe(lifg_opts)

si1 = stamp_slates(si1, frame)
      buckets = buckets_from_si(si1, tokens)


      # 2) Effective knobs
      weights =
        Application.get_env(:brain, :lifg_stage1_weights, @default_weights)
        |> Map.merge(Map.new(Keyword.get(opts, :weights, [])))

      scores_mode =
        Keyword.get(
          opts,
          :scores,
          Application.get_env(:brain, :lifg_stage1_scores_mode, @default_scores_mode)
        )

      margin_thr =
        Keyword.get(
          opts,
          :margin_threshold,
          lifg_default(:margin_threshold, Application.get_env(:brain, :lifg_margin_threshold, @default_margin_thr))
        )

      min_margin =
        Keyword.get(
          opts,
          :min_margin,
          lifg_default(:min_margin, Application.get_env(:brain, :lifg_min_margin, @default_min_margin))
        )

      bias_map = Keyword.get(opts, :intent_bias, Safe.get(si1, :intent_bias, %{})) || %{}

      # Event names (allow override)
      chargram_event =
        Keyword.get(opts, :chargram_event, [:brain, :lifg, :stage1, :chargram_violation])

      boundary_event =
        Keyword.get(opts, :boundary_event, [:brain, :lifg, :stage1, :boundary_drop])

      mwe_event = Keyword.get(opts, :mwe_event, @mwe_fallback_event)

      mwe_fallback? =
        Keyword.get(opts, :mwe_fallback, Application.get_env(:brain, :lifg_stage1_mwe_fallback, true))

      stage1_local_mwe_fallback? =
        Keyword.get(
          opts,
          :stage1_local_mwe_fallback,
          Application.get_env(:brain, :lifg_stage1_local_mwe_fallback, @default_stage1_local_mwe_fallback)
        )

      stop_event = Keyword.get(opts, :stage1_stop_event, @stage1_stop_event)

      # Reanalysis flag (ensures runner-up is available when vetoed)
      reanalysis? = Keyword.get(opts, :reanalysis, false) or Keyword.get(opts, :reanalysis?, false)

      # 3) Main scoring loop
      {choices, acc} =
        score_tokens(tokens, %{
          si: si1,
          sent: sent,
          buckets: buckets,
          weights: weights,
          scores_mode: scores_mode,
          margin_thr: margin_thr,
          min_margin: min_margin,
          bias_map: bias_map,
          chargram_event: chargram_event,
          boundary_event: boundary_event,
          mwe_event: mwe_event,
          mwe_fallback?: mwe_fallback?,
          stage1_local_mwe_fallback?: stage1_local_mwe_fallback?,
          stage1_stop_event: stop_event,
          reanalysis?: reanalysis?,
          # frame stamping for all telemetry events
          frame_seq: Safe.get(si1, :frame_seq) || Safe.get(si1, "frame_seq"),
          frame_ts_ms: Safe.get(si1, :frame_ts_ms) || Safe.get(si1, "frame_ts_ms"),
          frame_run_id: Safe.get(si1, :frame_run_id) || Safe.get(si1, "frame_run_id")
        })

      # 4) Audit + reanalysis hook
      dropped_total = acc.boundary_drops + acc.chargram + acc.no_cand + guard_drops

      rejected_all =
        guard_rejected
        |> Enum.concat(Enum.reverse(acc.rejected))
        |> Enum.uniq()
        |> Enum.sort()

      missing_tokens =
        acc.no_cand_tokens
        |> Enum.uniq()
        |> Enum.sort()

      guardrail_violations = acc.chargram + acc.boundary_drops + guard_drops

      audit =
        build_audit(acc.kept, dropped_total, rejected_all, guardrail_violations, acc.weak, acc.no_cand, missing_tokens)
        |> Map.put(:guard_drops, guard_drops)
        |> Map.put(:boundary_drop_count, acc.boundary_drops)
        |> Map.put(:mwe_fallbacks, acc.mwe_fallbacks)

      out = %{si: si1, choices: Enum.reverse(choices), audit: audit}

      emit_stage1_stop(
        ctx_from_run(si1, sent, opts),
        out.choices,
        audit,
        acc,
        stop_event,
        weights,
        scores_mode,
        margin_thr,
        min_margin
      )

      {:ok, maybe_reanalyse(out, si1, opts)}
    rescue
      e -> {:error, e}
    end
  end

  @spec run(map(), map() | keyword(), keyword()) ::
          {:ok, %{si: map(), choices: list(), audit: map()}} | {:error, term()}
  def run(si, weights_or_kw, opts) when is_list(opts) do
    # Tolerate legacy ctx passed as arg2; only treat arg2 as weights when it is a map or keyword list.
    weights_map =
      cond do
        is_map(weights_or_kw) -> weights_or_kw
        is_list(weights_or_kw) and Keyword.keyword?(weights_or_kw) -> Map.new(weights_or_kw)
        true -> %{}
      end

    run(si, Keyword.merge(opts, weights: weights_map))
  end

  # ---------- Internal helpers for run/2 ----------

  defp merged_lifg_opts(si0, opts) do
    base =
      case Safe.get(si0, :lifg_opts) do
        kw when is_list(kw) -> kw
        %{} = m -> Map.to_list(m)
        _ -> []
      end

    base
    |> Keyword.merge(opts)
    |> Enum.reject(fn {_k, v} -> is_nil(v) end)
  end

  defp lifg_default(key, fallback) do
    case Application.get_env(:brain, :lifg_defaults) do
      kw when is_list(kw) -> Keyword.get(kw, key, fallback)
      %{} = m -> Map.get(m, key, fallback)
      _ -> fallback
    end
  end

  defp ctx_from_run(si1, sent, opts) do
    %{
      sentence: sent,
      intent: Safe.get(si1, :intent) || Safe.get(si1, "intent"),
      confidence:
        Safe.get(si1, :confidence) ||
          Safe.get(si1, "confidence") ||
          Safe.get(si1, :intent_confidence) ||
          Safe.get(si1, "intent_confidence"),
      source: Keyword.get(opts, :source, :run),
      frame_seq: Safe.get(si1, :frame_seq) || Safe.get(si1, "frame_seq"),
      frame_ts_ms: Safe.get(si1, :frame_ts_ms) || Safe.get(si1, "frame_ts_ms"),
      frame_run_id: Safe.get(si1, :frame_run_id) || Safe.get(si1, "frame_run_id")
    }
  end

defp maybe_reset_sense_candidates(%{} = si, opts) do
  if Keyword.get(opts, :preserve_sense_candidates, false) do
    si
  else
    # Clear *only* if the slate is stamped for a different frame run.
    # If there is no stamp, treat it as caller-owned (tests / PMTG) and keep it.
    run_id =
      Safe.get(si, :frame_run_id) ||
        Safe.get(si, "frame_run_id") ||
        (Safe.get(si, :frame) && Safe.get(Safe.get(si, :frame), :run_id)) ||
        (Safe.get(si, "frame") && Safe.get(Safe.get(si, "frame"), "run_id"))

    si
    |> maybe_reset_one_slate(:sense_candidates, :sense_candidates_frame_run_id, run_id)
    |> maybe_reset_one_slate(:candidates_by_token, :candidates_by_token_frame_run_id, run_id)
  end
end

defp maybe_reset_sense_candidates(si, _opts), do: si

defp maybe_reset_one_slate(%{} = si, field, stamp_field, run_id) do
  stamp = Safe.get(si, stamp_field) || Safe.get(si, to_string(stamp_field))

  cond do
    is_nil(run_id) ->
      # No run id => permissive
      si

    is_nil(stamp) ->
      # No stamp => caller-owned input, keep it
      si

    stamp == run_id ->
      si

    true ->
      si
      |> Map.put(field, %{})
      |> Map.put(stamp_field, run_id)
  end
end

  defp prepare_tokens(si0, sent) do
    {raw_tokens, raw_indices} =
      si0
      |> Safe.get(:tokens, [])
      |> Enum.map(&Safe.to_plain/1)
      |> Enum.with_index()
      |> Enum.map(fn {tok, fallback_idx} -> {tok, token_index(tok, fallback_idx)} end)
      |> Enum.unzip()

    sanitized =
      %{tokens: raw_tokens, sentence: sent}
      |> Guard.sanitize()

    tokens =
      sanitized
      |> Map.get(:tokens, [])
      |> Enum.map(&Safe.to_plain/1)

    kept_indices =
      tokens
      |> Enum.with_index()
      |> Enum.map(fn {tok, fallback_idx} ->
        # Do NOT collapse missing indexes to 0.
        token_index(tok, fallback_idx)
      end)

    guard_rejected =
      raw_indices
      |> Enum.uniq()
      |> Enum.reject(&(&1 in kept_indices))
      |> Enum.sort()

    guard_drops = length(guard_rejected)

    {tokens, guard_rejected, guard_drops}
  end

  defp token_index(tok, fallback_idx) do
    raw =
      Safe.get(tok, :token_index) ||
        Safe.get(tok, "token_index") ||
        Safe.get(tok, :index) ||
        Safe.get(tok, "index")

    idx =
      cond do
        is_integer(raw) -> raw
        is_float(raw) -> trunc(raw)
        is_binary(raw) ->
          case Integer.parse(String.trim(raw)) do
            {n, _} -> n
            :error -> fallback_idx
          end

        true ->
          fallback_idx
      end

    if is_integer(idx) and idx >= 0, do: idx, else: fallback_idx
  end

  defp token_raw_phrase(tok) do
    Safe.get(tok, :phrase) ||
      Safe.get(tok, "phrase") ||
      Safe.get(tok, :lemma) ||
      Safe.get(tok, "lemma") ||
      Safe.get(tok, :word) ||
      Safe.get(tok, "word") ||
      phrase_from_id(Safe.get(tok, :id) || Safe.get(tok, "id")) ||
      ""
  end

  defp phrase_from_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [ph, _rest] -> ph
      _ -> nil
    end
  end

  defp phrase_from_id(_), do: nil

  defp token_mwe?(tok) do
    n_val = Safe.get(tok, :n) || Safe.get(tok, "n") || 1
    has_mw_flag = Safe.get(tok, :mw, false) || Safe.get(tok, "mw", false)
    id = to_string(Safe.get(tok, :id) || Safe.get(tok, "id") || "")
    has_mw_flag || (is_integer(n_val) and n_val > 1) || String.contains?(id, "|phrase|")
  end

  defp telemetry_phrase(tok, token_phrase) do
    Safe.get(tok, :lemma) ||
      Safe.get(tok, "lemma") ||
      Safe.get(tok, :word) ||
      Safe.get(tok, "word") ||
      token_phrase
  end

  # ---------- Boundary / chargram guard ----------

  @spec boundary_check(binary() | nil, map(), binary(), boolean()) ::
          :ok | {:error, :chargram | :nonword_edges | :span_mismatch}
  defp boundary_check(sentence, tok, phrase_norm, token_mwe?) do
    source = Safe.get(tok, :source) || Safe.get(tok, "source")
    kind = Safe.get(tok, :kind) || Safe.get(tok, "kind")
    flag = Safe.get(tok, :chargram?, false) || Safe.get(tok, "chargram?", false)

    cond do
      source in [:chargram, :char_ngram, "chargram", "char_ngram"] or
          kind in [:chargram, :char_ngram, "chargram", "char_ngram"] or flag ->
        {:error, :chargram}

      phrase_nil_or_empty?(phrase_norm) ->
        {:error, :chargram}

      token_mwe? ->
        boundary_check_mwe(sentence, tok, phrase_norm)

      true ->
        boundary_check_unigram(sentence, tok, phrase_norm)
    end
  end

  # ---------- Scoring loop ----------

  defp score_tokens(tokens, ctx) do
    acc0 = %{
      choices: [],
      weak: 0,
      kept: 0,
      no_cand: 0,
      no_cand_tokens: [],
      rejected: [],
      chargram: 0,
      boundary_drops: 0,
      mwe_fallbacks: 0
    }

    final =
      Enum.reduce(tokens, acc0, fn tok, acc ->
        score_token(tok, acc, ctx)
      end)

    {final.choices, final}
  end

  defp score_token(tok, acc, ctx) do
    raw_phrase = token_raw_phrase(tok)
    token_phrase = norm(raw_phrase)
    token_mwe? = token_mwe?(tok)
    tok_index = token_index(tok, 0)

    case boundary_check(ctx.sent, tok, token_phrase, token_mwe?) do
      :ok ->
        score_token_ok(tok, tok_index, raw_phrase, token_phrase, token_mwe?, acc, ctx)

      {:error, :chargram} ->
        handle_chargram_drop(tok, tok_index, token_phrase, token_mwe?, acc, ctx)

      {:error, reason} ->
        handle_boundary_drop(tok, tok_index, token_phrase, token_mwe?, reason, acc, ctx)
    end
  end

  defp score_token_ok(tok, tok_index, raw_phrase, token_phrase, token_mwe?, acc, ctx) do
    orig_cands =
      Map.get(ctx.buckets, tok_index, [])
      |> Enum.map(&Safe.to_plain/1)

    cand_list0 =
      orig_cands
      |> Enum.filter(&is_map/1)
      |> restrict_to_phrase_if_mwe(token_mwe?)
      |> restrict_phrase_match_if_mwe(token_mwe?, token_phrase)

    {cand_list, synth_mwe_fallback?} =
      cond do
        cand_list0 != [] ->
          {cand_list0, false}

        ctx.mwe_fallback? and ctx.stage1_local_mwe_fallback? and token_mwe? and
            allow_mwe_fallback_phrase?(token_phrase) ->
          emit_mwe_fallback(ctx.mwe_event, ctx, tok_index, raw_phrase, 1.0)

          fallback_id = "#{token_phrase}|phrase|fallback"

          {[
             %{
               id: fallback_id,
               lemma: token_phrase,
               norm: token_phrase,
               pos: "phrase",
               activation: 0.30,
               score: 0.30
             }
           ], true}

        true ->
          {cand_list0, false}
      end

    # Count synthesized fallbacks as fallbacks (keeps audit + stop telemetry coherent)
    acc =
      if synth_mwe_fallback? do
        Map.update!(acc, :mwe_fallbacks, &(&1 + 1))
      else
        acc
      end

    if cand_list == [] do
      acc2 =
        acc
        |> Map.update!(:no_cand, &(&1 + 1))
        |> Map.update!(:no_cand_tokens, &[tok_index | &1])

      # Preserve old counter behavior if still empty and MWE fallback path was enabled.
      if ctx.mwe_fallback? and token_mwe? do
        Map.update!(acc2, :mwe_fallbacks, &(&1 + 1))
      else
        acc2
      end
    else
      bias_val = get_float(ctx.bias_map, tok_index, 0.0)
      {syn_hits, ant_hits} = relations_count_overlaps(ctx.si)

      scored_trip =
        Enum.map(cand_list, fn c ->
          id = sense_id_for(c, token_phrase)
          pos = pos_of(c)

          cnrm =
            norm(
              Safe.get(c, :norm) ||
                Safe.get(c, :lemma) ||
                Safe.get(c, :word) ||
                token_phrase
            )

          lex0 = lex_fit(cnrm, token_phrase, token_mwe?)

          rel0 =
            guess_rel_prior(c, id, token_phrase)
            |> clamp01()

          act0 =
            clamp01(Safe.get(c, :activation, Safe.get(c, :score, guess_activation(c))))

          lex_ctx = clamp01(lex0 + 0.6 * syn_hits)
          rel_ctx = clamp01(rel0 - 0.4 * ant_hits)

          intent0 =
            intent_alignment_feature(bias_val, token_mwe?, cnrm, token_phrase, pos)

          feat_override =
            Safe.get(c, :features) ||
              Safe.get(c, "features") ||
              %{}

          lex = feat_override |> get_num(:lex_fit, lex_ctx) |> clamp01()
          rel = feat_override |> get_num(:rel_prior, rel_ctx) |> clamp01()
          act = feat_override |> get_num(:activation, act0) |> clamp01()
          intent_feat = feat_override |> get_num(:intent_bias, intent0) |> clamp01()

          base0 =
            (ctx.weights[:lex_fit] * lex +
               ctx.weights[:rel_prior] * rel +
               ctx.weights[:activation] * act +
               ctx.weights[:intent_bias] * intent_feat)
            |> clamp01()

          hom_bump = if relations_homonym_bonus?(ctx.si, c), do: 0.5, else: 0.0
          base = apply_mood_if_up(clamp01(base0 + hom_bump), tok, id)

          feat = %{
            id: id,
            lex_fit: lex,
            rel_prior: rel,
            activation: act,
            intent_bias: intent_feat
          }

          {id, base, feat}
        end)

      ids = Enum.map(scored_trip, fn {id, _b, _f} -> id end)
      base_scores = Map.new(scored_trip, fn {id, b, _} -> {id, b} end)
      feats = Enum.map(scored_trip, fn {_id, _b, f} -> f end)

      ctx_key =
        Cerebellum.context_key({:lifg_stage1, intent: intent_key(ctx.si), mwe: token_mwe?})

      cereb_opts =
        kw_to_map(scope: "lifg_stage1", context_key: ctx_key, margin_tau: ctx.margin_thr)

      cal_scores = cereb_calibrate(ctx.si, base_scores, feats, cereb_opts)

      logits = Enum.map(ids, &Map.get(cal_scores, &1, 0.0))
      probs = softmax(logits)

      ranked =
        Enum.zip(ids, probs)
        |> Enum.map(fn {id, p} -> {id, Float.round(p, 6)} end)
        |> Enum.sort_by(fn {_id, p} -> -p end)

      {chosen_id, top_p} =
        case ranked do
          [{id1, p1} | _] -> {id1, p1}
          _ -> {nil, 0.0}
        end

      second_p =
        case ranked do
          [_first, {_id2, p2} | _] -> p2
          _ -> 0.0
        end

      margin0 = top_p - second_p
      margin = Float.round(max(margin0, ctx.min_margin), 6)

      _ = cereb_learn(ctx.si, chosen_id, feats, base_scores, cereb_opts)

      scores_out =
        case ctx.scores_mode do
          :all -> Map.new(ranked)
          :top2 -> ranked |> Enum.take(2) |> Map.new()
          _ -> %{}
        end

      runner_up_id =
        case ranked do
          [_first, {id2, _p2} | _] -> id2
          _ -> nil
        end

      chosen_veto? =
        if is_binary(chosen_id) do
          candidate_veto?(cand_list, token_phrase, chosen_id)
        else
          false
        end

      alt_ids =
        cond do
          is_binary(runner_up_id) and chosen_veto? and ctx.reanalysis? -> [runner_up_id]
          is_binary(runner_up_id) and margin < ctx.margin_thr -> [runner_up_id]
          true -> []
        end

      choice = %{
        token_index: tok_index,
        index: tok_index,
        id: chosen_id,
        chosen_id: chosen_id,
        veto?: chosen_veto?,
        score: top_p,
        prob: top_p,
        scores: scores_out,
        alt_ids: alt_ids,
        margin: margin,
        prob_margin: margin
      }

      weak_next = if margin < ctx.margin_thr, do: acc.weak + 1, else: acc.weak

      acc
      |> Map.update!(:choices, &[choice | &1])
      |> Map.put(:weak, weak_next)
      |> Map.update!(:kept, &(&1 + 1))
    end
  end

  defp candidate_veto?(cand_list, token_phrase, chosen_id)
       when is_list(cand_list) and is_binary(chosen_id) do
    Enum.any?(cand_list, fn c ->
      id = sense_id_for(c, token_phrase)

      if id == chosen_id do
        v =
          Safe.get(c, :veto?) ||
            Safe.get(c, "veto?") ||
            Safe.get(c, :veto) ||
            Safe.get(c, "veto")

        truthy?(v)
      else
        false
      end
    end)
  end

  defp candidate_veto?(_cand_list, _token_phrase, _chosen_id), do: false

  defp truthy?(true), do: true
  defp truthy?("true"), do: true
  defp truthy?("TRUE"), do: true
  defp truthy?(1), do: true
  defp truthy?("1"), do: true
  defp truthy?(_), do: false

  defp emit_mwe_fallback(event, ctx, tok_index, raw_phrase, score0) do
    :telemetry.execute(
      event,
      %{count: 1},
      %{
        token_index: tok_index,
        phrase: to_string(raw_phrase || ""),
        score: score0 * 1.0,
        frame_seq: ctx.frame_seq,
        frame_ts_ms: ctx.frame_ts_ms,
        frame_run_id: ctx.frame_run_id,
        v: 2
      }
    )

    :ok
  rescue
    _ -> :ok
  end

  defp handle_chargram_drop(tok, tok_index, token_phrase, token_mwe?, acc, ctx) do
    tele_phrase = telemetry_phrase(tok, token_phrase)

    :telemetry.execute(
      ctx.chargram_event,
      %{},
      %{
        token_index: tok_index,
        phrase: tele_phrase,
        mw: token_mwe?,
        reason: :chargram,
        count: 1,
        frame_seq: ctx.frame_seq,
        frame_ts_ms: ctx.frame_ts_ms,
        frame_run_id: ctx.frame_run_id,
        v: 2
      }
    )

    acc
    |> Map.update!(:rejected, &[tok_index | &1])
    |> Map.update!(:chargram, &(&1 + 1))
  end

  defp handle_boundary_drop(_tok, tok_index, token_phrase, token_mwe?, reason, acc, ctx) do
    :telemetry.execute(
      ctx.boundary_event,
      %{},
      %{
        token_index: tok_index,
        phrase: token_phrase,
        mw: token_mwe?,
        reason: reason,
        count: 1,
        frame_seq: ctx.frame_seq,
        frame_ts_ms: ctx.frame_ts_ms,
        frame_run_id: ctx.frame_run_id,
        v: 2
      }
    )

    acc
    |> Map.update!(:rejected, &[tok_index | &1])
    |> Map.update!(:boundary_drops, &(&1 + 1))
  end

  # ---------- Stage1 summary telemetry ----------

defp emit_stage1_stop(
       run_ctx,
       choices,
       audit,
       acc,
       event,
       weights,
       scores_mode,
       margin_thr,
       min_margin
     )
     when is_list(choices) and is_map(audit) and is_map(acc) and is_list(event) do
  ts = System.system_time(:millisecond)

  frame_seq = Map.get(run_ctx, :frame_seq)
  frame_ts_ms = Map.get(run_ctx, :frame_ts_ms) || ts
  frame_run_id = Map.get(run_ctx, :frame_run_id)

  # Prefer audit (post-processed) but fall back to acc (raw loop counters).
  kept_tokens = audit_get(audit, :kept_tokens, acc.kept)
  dropped_tokens = audit_get(audit, :dropped_tokens, 0)
  weak_decisions = audit_get(audit, :weak_decisions, acc.weak)
  missing_candidates = audit_get(audit, :missing_candidates, acc.no_cand)
  missing_candidate_tokens = audit_get(audit, :missing_candidate_tokens, [])
  guard_drops = audit_get(audit, :guard_drops, 0)
  guardrail_violations = audit_get(audit, :chargram_violation, 0)

  boundary_drops = acc.boundary_drops
  chargram = acc.chargram
  mwe_fallbacks = acc.mwe_fallbacks

  {fallback_winners, margins, probs} = summarize_choices(choices)

  {margin_min, margin_mean} = {min_or_zero(margins), mean_or_zero(margins)}
  {p1_min, p1_mean} = {min_or_zero(probs), mean_or_zero(probs)}

  total = max(kept_tokens + dropped_tokens, 0)
  kept_rate = safe_div(kept_tokens, total)
  weak_rate = safe_div(weak_decisions, max(kept_tokens, 0))
  fallback_rate = safe_div(fallback_winners, max(kept_tokens, 0))
  missing_rate = safe_div(missing_candidates, max(total, 0))

  meta = %{
    at_ms: ts,
    ts_ms: ts,
    frame_seq: frame_seq,
    frame_ts_ms: frame_ts_ms,
    frame_run_id: frame_run_id,

    sentence: Map.get(run_ctx, :sentence),
    intent: Map.get(run_ctx, :intent),
    confidence: Map.get(run_ctx, :confidence),
    source: Map.get(run_ctx, :source, :run),

    kept_tokens: kept_tokens,
    dropped_tokens: dropped_tokens,
    weak_decisions: weak_decisions,

    missing_candidates: missing_candidates,
    missing_candidate_tokens: missing_candidate_tokens,

    boundary_drops: boundary_drops,
    chargram_violation: chargram,
    guard_drops: guard_drops,
    guardrail_violations: guardrail_violations,

    mwe_fallbacks: mwe_fallbacks,
    fallback_winners: fallback_winners,

    margin_min: Float.round(margin_min, 6),
    margin_mean: Float.round(margin_mean, 6),
    p1_min: Float.round(p1_min, 6),
    p1_mean: Float.round(p1_mean, 6),

    # Handy derived scalars for gating and UI.
    rates: %{
      kept: Float.round(kept_rate, 6),
      weak: Float.round(weak_rate, 6),
      fallback_win: Float.round(fallback_rate, 6),
      missing: Float.round(missing_rate, 6)
    },

    scores_mode: scores_mode,
    margin_threshold: margin_thr * 1.0,
    min_margin: min_margin * 1.0,
    weights: weights,
    v: 3
  }

  meas = %{
    kept: kept_tokens,
    dropped: dropped_tokens,
    weak: weak_decisions,
    missing: missing_candidates,
    boundary_drops: boundary_drops,
    chargram: chargram,
    guard_drops: guard_drops,
    mwe_fallbacks: mwe_fallbacks,
    fallback_winners: fallback_winners
  }

  :telemetry.execute(event, meas, meta)
rescue
  _ -> :ok
end

defp summarize_choices(choices) do
  fallback_winners =
    Enum.count(choices, fn ch ->
      id =
        Safe.get(ch, :chosen_id) || Safe.get(ch, :id) ||
          Safe.get(ch, "chosen_id") || Safe.get(ch, "id") || ""

      String.contains?(to_string(id), "|phrase|fallback")
    end)

  margins =
    choices
    |> Enum.map(fn ch -> get_num(ch, :margin, get_num(ch, :prob_margin, 0.0)) end)
    |> Enum.map(&clamp01/1)

  probs =
    choices
    |> Enum.map(fn ch -> get_num(ch, :prob, get_num(ch, :score, 0.0)) end)
    |> Enum.map(&clamp01/1)

  {fallback_winners, margins, probs}
end

defp audit_get(audit, k, default) when is_map(audit) do
  case {Map.get(audit, k), Map.get(audit, to_string(k))} do
    {nil, nil} -> default
    {v, nil} -> v
    {nil, v} -> v
    {v, _} -> v
  end
end

defp safe_div(_a, b) when not is_number(b) or b <= 0, do: 0.0
defp safe_div(a, b) when is_number(a) and is_number(b), do: (a * 1.0) / (b * 1.0)
defp safe_div(_a, _b), do: 0.0

  defp min_or_zero([]), do: 0.0
  defp min_or_zero(xs), do: Enum.min(xs, fn -> 0.0 end) * 1.0

  defp mean_or_zero([]), do: 0.0

  defp mean_or_zero(xs) do
    n = length(xs)
    if n == 0, do: 0.0, else: Enum.sum(xs) / (n * 1.0)
  end

  # ---------- GenServer lifecycle (mood) ----------

  @impl true
  def init(opts) do
    state = %{
      region: :lifg_stage1,
      opts: normalize_opts(opts),
      mood: nil,
      mood_last_ms: nil
    }

    mood_id = unique(@mood_handler_prefix)

    :ok =
      :telemetry.attach(
        mood_id,
        [:brain, :mood, :update],
        &__MODULE__.on_mood_update/4,
        %{pid: self()}
      )

    {:ok, Map.put(state, :mood_handler, mood_id)}
  end

  @impl true
  def terminate(_reason, %{mood_handler: mood_id}) when is_binary(mood_id) do
    :telemetry.detach(mood_id)
    :ok
  end

  @impl true
  def terminate(_reason, _state), do: :ok

  def status do
    case Process.whereis(__MODULE__) do
      nil -> %{}
      _ -> GenServer.call(__MODULE__, :status, 150)
    end
  end

  @impl true
  def handle_call(:status, _from, state) do
    reply = %{
      region: :lifg_stage1,
      status: :ok,
      pid: self(),
      opts: state.opts,
      mood: state.mood,
      mood_last_ms: state.mood_last_ms
    }

    {:reply, reply, state}
  end

  @impl true
  def handle_call(:get_state, _from, state), do: {:reply, state, state}

  @impl true
  def handle_call({:score, ctx}, _from, state) do
    base = get_num(ctx, :base_score, 0.0) |> clamp01()
    {factor, bias, mood_snapshot, mw, cap} = mood_factor(state.mood, state.opts)
    final = clamp01(base * factor)

    :telemetry.execute(
      [:brain, :lifg, :stage1, :score],
      %{score: final},
      %{
        token: Map.get(ctx, :token) || Map.get(ctx, "token"),
        sense_id: Map.get(ctx, :sense_id) || Map.get(ctx, "sense_id"),
        base_score: base,
        mood_bias: bias,
        mood_snapshot: mood_snapshot,
        mood_weights: mw,
        mood_cap: cap,
        v: 2
      }
    )

    {:reply, final, state}
  end

  @doc false
  def on_mood_update(_event, measurements, _meta, %{pid: pid}) when is_pid(pid) do
    send(pid, {:mood_update, measurements})
    :ok
  catch
    _, _ -> :ok
  end

  def on_mood_update(_e, _m, _meta, _cfg), do: :ok

  @impl true
  def handle_info({:mood_update, meas}, state) do
    mood = %{
      exploration: get_num(meas, :exploration, 0.5) |> clamp01(),
      inhibition: get_num(meas, :inhibition, 0.5) |> clamp01(),
      vigilance: get_num(meas, :vigilance, 0.5) |> clamp01(),
      plasticity: get_num(meas, :plasticity, 0.5) |> clamp01()
    }

    {:noreply, %{state | mood: mood, mood_last_ms: System.system_time(:millisecond)}}
  end

  @impl true
  def handle_info(_msg, state), do: {:noreply, state}

  # ---------- Feature engineering ----------

  defp lex_fit(cnrm, token_phrase, token_mwe?) do
    cond do
      cnrm == token_phrase -> 1.0
      token_mwe? and String.contains?(cnrm, " ") -> 0.80
      not token_mwe? and not String.contains?(cnrm, " ") -> 0.60
      true -> 0.40
    end
  end

  defp intent_alignment_feature(bias_val, token_mwe?, cand_norm, token_phrase, pos_str) do
    b = clamp(bias_val, -0.5, 0.5)

    cond do
      token_mwe? and cand_norm == token_phrase -> max(0.0, b)
      function_pos?(pos_str) -> min(0.0, b)
      true -> 0.0
    end
  end

  defp restrict_to_phrase_if_mwe(candidates, false), do: candidates

  defp restrict_to_phrase_if_mwe(candidates, true) when is_list(candidates) do
    phrase_like =
      Enum.filter(candidates, fn c ->
        id = Safe.get(c, :id) || Safe.get(c, "id")
        norm0 = Safe.get(c, :norm) || Safe.get(c, :lemma) || Safe.get(c, :word)

        id_s = to_string(id || "")
        norm_s = to_string(norm0 || "")

        String.contains?(id_s, "|phrase|") or String.contains?(norm_s, " ")
      end)

    case phrase_like do
      [] -> candidates
      list -> list
    end
  end

  defp function_pos?(p) when is_binary(p), do: String.downcase(p) in @function_pos
  defp function_pos?(p) when is_atom(p), do: function_pos?(Atom.to_string(p))
  defp function_pos?(_), do: false

  # ---------- Candidate bucket extraction ----------

defp buckets_from_si(si0, tokens) do
  sc =
    case Safe.get(si0, :sense_candidates, %{}) do
      %{} = m -> m
      _ -> %{}
    end

  cbt =
    case Safe.get(si0, :candidates_by_token, %{}) do
      %{} = m -> m
      _ -> %{}
    end

  ac_map =
    case Safe.get(si0, :active_cells) || Safe.get(si0, "active_cells") || [] do
      list when is_list(list) and list != [] ->
        token_phrase_by_idx = token_phrase_by_idx(tokens)

        list
        |> Enum.map(&Safe.to_plain/1)
        |> active_cells_to_buckets(token_phrase_by_idx)

      _ ->
        %{}
    end

  ac_map
  |> Map.merge(cbt) # candidates_by_token overrides active_cells
  |> Map.merge(sc)  # sense_candidates overrides both
end

  defp token_phrase_by_idx(tokens) when is_list(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {tok, fallback_idx}, acc ->
      idx = token_index(tok, fallback_idx)
      phrase = tok |> token_raw_phrase() |> norm()
      Map.put(acc, idx, phrase)
    end)
  end

  defp token_phrase_by_idx(_), do: %{}

  defp active_cells_to_buckets(cells, token_phrase_by_idx)
       when is_list(cells) and is_map(token_phrase_by_idx) do
    cells
    |> Enum.map(&Safe.to_plain/1)
    |> Enum.reduce(%{}, fn cell, acc ->
      idx =
        case {Safe.get(cell, :token_index), Safe.get(cell, "token_index")} do
          {i, _} when is_integer(i) and i >= 0 -> i
          {_, i} when is_integer(i) and i >= 0 -> i
          _ -> :skip
        end

      case idx do
        :skip ->
          acc

        i ->
          cand = active_cell_to_candidate(cell)
          id = Safe.get(cand, :id) || Safe.get(cand, "id")

          # Reject stale/misaligned active-cell candidates whose lemma doesn't match this token.
          tok_phrase = Map.get(token_phrase_by_idx, i, "")
          cand_lemma = Safe.get(cand, :lemma) || Safe.get(cand, "lemma") || guess_cell_lemma(id)

          aligned? = tok_phrase == "" or norm(to_string(cand_lemma || "")) == tok_phrase

          cond do
            not aligned? -> acc
            is_nil(id) -> acc
            true -> Map.update(acc, i, [cand], fn lst -> [cand | lst] end)
          end
      end
    end)
    |> Enum.into(%{}, fn {idx, list} -> {idx, Enum.reverse(list)} end)
  end

  defp active_cell_to_candidate(cell) do
    id =
      Safe.get(cell, :id) ||
        Safe.get(cell, "id") ||
        Safe.get(cell, :chosen_id) ||
        Safe.get(cell, "chosen_id") ||
        guess_cell_id(cell)

    lemma =
      Safe.get(cell, :lemma) ||
        Safe.get(cell, "lemma") ||
        guess_cell_lemma(id)

    features =
      Safe.get(cell, :features) ||
        Safe.get(cell, "features") ||
        %{}

    %{
      id: id,
      lemma: lemma,
      pos: Safe.get(cell, :pos) || Safe.get(cell, "pos") || "other",
      score: Safe.get(cell, :score, cell_score_from_map(cell)),
      features: features
    }
  end

  defp cell_score_from_map(cell) do
    scores =
      Safe.get(cell, :scores) ||
        Safe.get(cell, "scores") ||
        %{}

    if is_map(scores) and map_size(scores) > 0 do
      chosen = Safe.get(cell, :chosen_id) || Safe.get(cell, "chosen_id")

      cond do
        chosen && Map.has_key?(scores, chosen) -> Map.get(scores, chosen)
        chosen && Map.has_key?(scores, to_string(chosen)) -> Map.get(scores, to_string(chosen))
        true -> scores |> Map.values() |> Enum.max(fn -> 0.0 end)
      end
    else
      0.0
    end
  end

  defp guess_cell_id(cell) do
    lemma = Safe.get(cell, :lemma) || Safe.get(cell, "lemma") || "cell"
    pos = Safe.get(cell, :pos) || Safe.get(cell, "pos") || "other"
    "#{lemma}|#{pos}|0"
  end

  defp guess_cell_lemma(id) when is_binary(id) do
    case String.split(id, "|") do
      [lemma | _] -> lemma
      _ -> id
    end
  end

  defp guess_cell_lemma(id), do: to_string(id)

  # ---------- Boundary helpers ----------

  defp boundary_check_mwe(sentence, tok, phrase_norm) do
    span = tok_span(tok)

    case {sentence, span} do
      {s, {start, b}} when is_binary(s) and is_integer(start) and is_integer(b) ->
        {s0, stop} = normalize_span(s, {start, b}, phrase_norm)
        len = stop - s0

        if len > 0 and span_sub_norm(s, s0, len) == phrase_norm do
          :ok
        else
          {:error, :chargram}
        end

      _ ->
        if phrase_valid_mwe?(phrase_norm), do: :ok, else: {:error, :chargram}
    end
  end

  # Normalize ambiguous {start, b} into {start, end_exclusive}.
  defp normalize_span(sentence, {start, b}, phrase_norm)
       when is_binary(sentence) and is_integer(start) and is_integer(b) do
    size = byte_size(sentence)

    candidates =
      [
        {start, b},
        {start, start + b},
        {start, b + 1},
        {start, start + byte_size(to_string(phrase_norm || ""))}
      ]
      |> Enum.uniq()
      |> Enum.filter(fn {s, e} -> s >= 0 and e > s and e <= size end)

    match =
      Enum.find_value(candidates, fn {s, e} ->
        len = e - s
        if span_sub_norm(sentence, s, len) == phrase_norm, do: {s, e}, else: nil
      end)

    match ||
      Enum.find_value(candidates, fn {s, e} ->
        if s >= 0 and e > s and e <= size, do: {s, e}, else: nil
      end) ||
      {start, start}
  end

  defp normalize_span(_sentence, _span, _phrase_norm), do: {0, 0}

  defp tok_span(tok) do
    case {Safe.get(tok, :span), Safe.get(tok, "span")} do
      {{a, b}, _} when is_integer(a) and is_integer(b) -> {a, b}
      {_, {a, b}} when is_integer(a) and is_integer(b) -> {a, b}
      _ -> nil
    end
  end

  defp span_sub_norm(sentence, start, len) do
    try do
      sentence |> binary_part(start, len) |> norm()
    rescue
      _ -> ""
    end
  end

  defp byte_at(bin, idx) when is_binary(bin) and is_integer(idx) do
    if idx >= 0 and idx < byte_size(bin), do: :binary.at(bin, idx), else: nil
  end

  defp word_byte?(nil), do: false

  defp word_byte?(c) when is_integer(c) do
    (c >= ?0 and c <= ?9) or (c >= ?a and c <= ?z) or (c >= ?A and c <= ?Z) or c == ?_
  end

  defp boundary_check_unigram(sentence, tok, phrase_norm) do
    span = tok_span(tok)

    case {sentence, span} do
      {s, {start, b}} when is_binary(s) and is_integer(start) and is_integer(b) ->
        {s0, stop} = normalize_span(s, {start, b}, phrase_norm)
        size = byte_size(s)
        len = stop - s0
        sub_norm = if(len > 0 and stop <= size, do: span_sub_norm(s, s0, len), else: "")

        cond do
          len <= 0 or stop > size ->
            if phrase_valid_unigram?(phrase_norm) or phrase_valid_mwe?(phrase_norm),
              do: :ok,
              else: {:error, :chargram}

          sub_norm != phrase_norm ->
            cond do
              String.contains?(phrase_norm, " ") ->
                {:error, :chargram}

              phrase_valid_unigram?(phrase_norm) or phrase_valid_mwe?(phrase_norm) ->
                {:error, :span_mismatch}

              true ->
                {:error, :chargram}
            end

          true ->
            left_ok = s0 == 0 or not word_byte?(byte_at(s, s0 - 1))
            right_ok = stop == size or not word_byte?(byte_at(s, stop))
            if left_ok and right_ok, do: :ok, else: {:error, :nonword_edges}
        end

      _ ->
        if phrase_valid_unigram?(phrase_norm) or phrase_valid_mwe?(phrase_norm) do
          :ok
        else
          {:error, :chargram}
        end
    end
  end

  # ---------- Phrase shape helpers ----------

  defp phrase_nil_or_empty?(nil), do: true
  defp phrase_nil_or_empty?(""), do: true
  defp phrase_nil_or_empty?(p) when is_binary(p), do: String.trim(p) == ""
  defp phrase_nil_or_empty?(_), do: false

  defp phrase_valid_unigram?(phrase) when is_binary(phrase) do
    clean =
      phrase
      |> String.downcase()
      |> String.replace(~r/[^[:alnum:]]/u, "")
      |> String.trim()

    clean != "" and not String.contains?(clean, " ")
  end

  defp phrase_valid_unigram?(_), do: false

  defp phrase_valid_mwe?(phrase) when is_binary(phrase) do
    tokens =
      phrase
      |> String.downcase()
      |> String.trim()
      |> String.split(~r/\s+/, trim: true)

    length(tokens) >= 2 and Enum.all?(tokens, &phrase_valid_unigram?/1)
  end

  defp phrase_valid_mwe?(_), do: false

  # ---------- Mood / Cerebellum helpers ----------

  defp apply_mood_if_up(base, token, sense_id) do
    case GenServer.whereis(__MODULE__) do
      pid when is_pid(pid) ->
        try do
          score(__MODULE__, %{base_score: base, token: token, sense_id: sense_id})
        catch
          _, _ -> base
        end

      _ ->
        base
    end
  end

  defp cereb_calibrate(si0, base_scores, feats, opts) do
    try do
      Cerebellum.calibrate_scores(si0, base_scores, feats, opts)
    rescue
      _ -> base_scores
    catch
      _, _ -> base_scores
    end
  end

  defp cereb_learn(si0, chosen_id, feats, base_scores, opts) do
    try do
      Cerebellum.learn_lifg(si0, chosen_id, feats, base_scores, opts)
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end

  defp mood_factor(nil, opts), do: {1.0, 0.0, nil, cfg_mw(opts), cfg_cap(opts)}

  defp mood_factor(mood, opts) do
    w = cfg_mw(opts)
    cap = cfg_cap(opts)

    bias =
      try do
        MoodWeights.bias(mood, w, cap)
      rescue
        _ ->
          dx = %{
            expl: mood.exploration - 0.5,
            inhib: mood.inhibition - 0.5,
            vigil: mood.vigilance - 0.5,
            plast: mood.plasticity - 0.5
          }

          raw =
            dx.expl * (w.expl || 0.0) +
              dx.inhib * (w.inhib || 0.0) +
              dx.vigil * (w.vigil || 0.0) +
              dx.plast * (w.plast || 0.0)

          clamp(raw, -cap, cap)
      end

    factor = 1.0 + bias
    {factor, bias, mood, w, cap}
  end

  defp cfg_mw(opts) do
    val = get_opt(opts, :mood_weights, Application.get_env(:brain, :lifg_mood_weights, @default_mw))

    %{
      expl: to_small(val[:expl] || val["expl"] || @default_mw.expl),
      inhib: to_small(val[:inhib] || val["inhib"] || @default_mw.inhib),
      vigil: to_small(val[:vigil] || val["vigil"] || @default_mw.vigil),
      plast: to_small(val[:plast] || val["plast"] || @default_mw.plast)
    }
  end

  defp cfg_cap(opts),
    do: to_cap(get_opt(opts, :mood_cap, Application.get_env(:brain, :lifg_mood_cap, @default_cap)))

  # ---------- Frame stamping helpers ----------

  defp build_frame(si, opts) when is_map(si) and is_list(opts) do
    ts_opt = Keyword.get(opts, :frame_ts_ms)
    seq_opt = Keyword.get(opts, :frame_seq)

    ts =
      cond do
        is_integer(ts_opt) and ts_opt > 0 ->
          ts_opt

        true ->
          si_ts = Safe.get(si, :frame_ts_ms) || Safe.get(si, "frame_ts_ms")
          if is_integer(si_ts) and si_ts > 0, do: si_ts, else: System.system_time(:millisecond)
      end

    seq =
      cond do
        is_integer(seq_opt) ->
          seq_opt

        true ->
          si_seq = Safe.get(si, :frame_seq) || Safe.get(si, "frame_seq")

          cond do
            is_integer(si_seq) ->
              si_seq

            true ->
              clock = Safe.get(si, :clock) || Safe.get(si, "clock") || %{}
              clock_seq = Safe.get(clock, :seq) || Safe.get(clock, "seq")

              if is_integer(clock_seq) do
                clock_seq
              else
                :erlang.unique_integer([:positive, :monotonic])
              end
          end
      end

    run_id = :erlang.unique_integer([:positive, :monotonic])
    %{seq: seq, ts_ms: ts, run_id: run_id}
  end

  defp build_frame(_si, _opts) do
    ts = System.system_time(:millisecond)

    %{
      seq: :erlang.unique_integer([:positive, :monotonic]),
      ts_ms: ts,
      run_id: :erlang.unique_integer([:positive, :monotonic])
    }
  end

  defp put_frame(%{} = si, %{seq: seq, ts_ms: ts, run_id: run_id} = frame) do
    si
    |> Map.put(:frame, frame)
    |> Map.put(:frame_seq, seq)
    |> Map.put(:frame_ts_ms, ts)
    |> Map.put(:frame_run_id, run_id)
  end

  defp put_frame(si, _frame), do: si

  # ---------- Small utils ----------

  defp sense_id_for(c, token_phrase) do
    id = Safe.get(c, :id) || Safe.get(c, "id")

    if is_nil(id) do
      lemma = Safe.get(c, :lemma) || Safe.get(c, :word) || token_phrase
      pos = pos_of(c)
      "#{lemma}|#{pos}|0"
    else
      to_string(id)
    end
  end

  defp pos_of(c) do
    p = Safe.get(c, :pos) || Safe.get(c, "pos") || "other"
    p |> to_string() |> String.downcase()
  end

  defp guess_rel_prior(c, id, token_phrase) do
    norm0 = Safe.get(c, :norm) || Safe.get(c, :lemma) || Safe.get(c, :word) || ""
    id_s = to_string(id || "")

    {lemma, pos, tag} = parse_sense_id(id_s)
    lemma_norm = norm(lemma)

    phrase_like? =
      String.contains?(to_string(token_phrase || ""), " ") or
        String.contains?(id_s, "|phrase|") or
        String.contains?(to_string(norm0), " ")

    base_pos =
      @pos_prior
      |> Map.get(pos, @default_rel_prior)
      |> apply_phrase_fallback_adjustment(pos, tag)
      |> maybe_boost_greeting_phrase(lemma_norm, pos, tag)

    adj = if phrase_like?, do: 0.02, else: -0.02
    clamp01(base_pos + adj)
  end

  defp parse_sense_id(id_str) when is_binary(id_str) do
    case String.split(id_str, "|") do
      [lemma, pos, tag] -> {lemma, String.downcase(pos || "other"), String.downcase(tag || "")}
      [lemma, pos] -> {lemma, String.downcase(pos || "other"), ""}
      [lemma] -> {lemma, "other", ""}
      _ -> {id_str, "other", ""}
    end
  end

  defp parse_sense_id(other), do: parse_sense_id(to_string(other))

  defp apply_phrase_fallback_adjustment(base, "phrase", "fallback"), do: clamp(base + 0.02, 0.0, 1.0)
  defp apply_phrase_fallback_adjustment(base, _pos, _tag), do: base

  defp maybe_boost_greeting_phrase(base, lemma_norm, "phrase", "fallback") do
    if MapSet.member?(@greeting_lemmas, lemma_norm), do: clamp(base + 0.03, 0.0, 1.0), else: base
  end

  defp maybe_boost_greeting_phrase(base, _lemma_norm, _pos, _tag), do: base

  defp guess_activation(c) do
    norm0 = Safe.get(c, :norm) || Safe.get(c, :lemma) || ""
    if String.contains?(to_string(norm0), " "), do: 0.30, else: 0.25
  end

  defp softmax([]), do: []

  defp softmax(xs) do
    xs = Enum.map(xs, &to_float/1)
    m = Enum.max(xs)
    exps = Enum.map(xs, fn x -> :math.exp(x - m) end)
    denom = Enum.sum(exps)

    cond do
      denom == 0.0 ->
        n = length(xs)
        u = 1.0 / n
        Enum.map(xs, fn _ -> u end)

      true ->
        Enum.map(exps, &(&1 / denom))
    end
  end

  defp to_float(x) when is_float(x), do: x
  defp to_float(x) when is_integer(x), do: x * 1.0
  defp to_float(_), do: 0.0

  defp norm(nil), do: ""

  defp norm(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp norm(v), do: v |> to_string() |> norm()

  defp clamp(x, lo, hi) when is_number(x), do: min(max(x, lo), hi)
  defp clamp(_, lo, _hi), do: lo
  defp clamp01(x) when is_number(x), do: clamp(x * 1.0, 0.0, 1.0)
  defp clamp01(_), do: 0.0

  defp get_float(map, k, dflt) when is_map(map) do
    case {Map.get(map, k), Map.get(map, to_string(k))} do
      {v, _} when is_number(v) -> v * 1.0
      {_, v} when is_number(v) -> v * 1.0
      _ -> dflt * 1.0
    end
  end

  defp get_float(_, _, d), do: d * 1.0

  defp get_opt(opts, key, default) when is_list(opts), do: Keyword.get(opts, key, default)
  defp get_opt(%{} = opts, key, default), do: Map.get(opts, key, default)
  defp get_opt(_, _, default), do: default

  defp to_small(x) when is_number(x), do: x * 1.0
  defp to_small(_), do: 0.0

  defp to_cap(x) when is_number(x), do: max(0.0, min(1.0, x * 1.0))
  defp to_cap(_), do: @default_cap

  defp get_num(map, key, default) do
    case {Map.get(map, key), Map.get(map, to_string(key))} do
      {v, _} when is_integer(v) -> v * 1.0
      {v, _} when is_float(v) -> v
      {_, v} when is_integer(v) -> v * 1.0
      {_, v} when is_float(v) -> v
      _ -> default * 1.0
    end
  end

  defp unique(prefix),
    do:
      prefix <>
        Integer.to_string(:erlang.unique_integer([:positive])) <>
        "-" <> Integer.to_string(System.system_time(:microsecond))

  defp normalize_opts(opts) when is_list(opts), do: if(Keyword.keyword?(opts), do: opts, else: [])
  defp normalize_opts(%{} = opts), do: Map.to_list(opts)
  defp normalize_opts(_), do: []

  defp intent_key(si0) do
    i = Safe.get(si0, :intent)

    cond do
      is_binary(i) -> i
      is_map(i) -> to_string(Safe.get(i, :intent) || Safe.get(i, :name) || Safe.get(i, :type) || "none")
      true -> "none"
    end
  end

  defp kw_to_map(v) do
    cond do
      is_list(v) and Keyword.keyword?(v) -> Map.new(v)
      is_map(v) -> v
      true -> %{}
    end
  end

  # ---------- Relation helpers ----------

  defp relations_count_overlaps(si0) do
    try do
      if Code.ensure_loaded?(Brain.LIFG.RelationSignals) and
           function_exported?(Brain.LIFG.RelationSignals, :count_overlaps, 1) do
        apply(Brain.LIFG.RelationSignals, :count_overlaps, [si0])
      else
        {0, 0}
      end
    rescue
      _ -> {0, 0}
    catch
      _, _ -> {0, 0}
    end
  end

  defp relations_homonym_bonus?(si0, candidate) do
    try do
      if Code.ensure_loaded?(Brain.LIFG.RelationSignals) and
           function_exported?(Brain.LIFG.RelationSignals, :homonym_bonus?, 2) do
        apply(Brain.LIFG.RelationSignals, :homonym_bonus?, [si0, candidate])
      else
        false
      end
    rescue
      _ -> false
    catch
      _, _ -> false
    end
  end

  defp build_audit(kept, dropped, rejected_by_boundary, chargram_drops, weak, missing_cand, missing_cand_tokens) do
    %{
      feature_mix: :lifg_stage1,
      kept_tokens: kept,
      dropped_tokens: dropped,
      boundary_drops: length(rejected_by_boundary),
      rejected_by_boundary: rejected_by_boundary,
      chargram_violation: chargram_drops,
      missing_candidates: missing_cand,
      missing_candidate_tokens: missing_cand_tokens,
      weak_decisions: weak
    }
  end

  # ---------- Reanalysis hook ----------

  defp maybe_reanalyse(%{choices: _choices} = out, _si0, opts) do
    reanalysis? = Keyword.get(opts, :reanalysis, false) or Keyword.get(opts, :reanalysis?, false)

    if not reanalysis? do
      out
    else
      fail_fun = Keyword.get(opts, :fail_fun) || build_veto_fail_fun(out.si || %{})
      %{choices: flipped, flips: n} = Reanalysis.fallback(out.choices, fail_fun, opts)

      audit0 = out.audit || %{}
      audit = Map.update(audit0, :flips, n, &(&1 + n))

      %{out | choices: flipped, audit: audit}
    end
  end

  defp build_veto_fail_fun(si0) do
    f = Safe.get(si0, :fail_fun) || Safe.get(si0, "fail_fun")

    cond do
      is_function(f, 1) ->
        f

      true ->
        fn choice ->
          v =
            Safe.get(choice, :veto?) ||
              Safe.get(choice, "veto?") ||
              Safe.get(choice, :veto) ||
              Safe.get(choice, "veto")

          truthy?(v)
        end
    end
  end

  # ---------- Candidate filters (MWE hygiene) ----------

  defp restrict_phrase_match_if_mwe(cands, false, _token_phrase), do: cands

  defp restrict_phrase_match_if_mwe(cands, true, token_phrase) when is_list(cands) do
    tp = norm(token_phrase)

    if tp == "" do
      []
    else
      Enum.filter(cands, fn c ->
        id = to_string(Safe.get(c, :id) || Safe.get(c, "id") || "")

        cn =
          norm(
            Safe.get(c, :norm) ||
              Safe.get(c, "norm") ||
              Safe.get(c, :lemma) ||
              Safe.get(c, "lemma") ||
              Safe.get(c, :word) ||
              Safe.get(c, "word") ||
              ""
          )

        cn == tp or String.starts_with?(id, tp <> "|phrase|")
      end)
    end
  end

  defp restrict_phrase_match_if_mwe(cands, _mwe, _tp), do: cands

# Reject local fallback MWEs that are mostly scaffolding:
  # - Reject if head OR tail is a function word
  # - For 3+ grams, also reject if 2+ tokens are function words
  #   (kills "what do you", "do you think", "we should do", etc.)
  defp allow_mwe_fallback_phrase?(phrase) when is_binary(phrase) do
    parts =
      phrase
      |> norm()
      |> String.split(" ", trim: true)

    case parts do
      [] ->
        false

      [_] ->
        false

      [a, b] ->
        (not Brain.LIFG.MWE.function_word?(a)) and (not Brain.LIFG.MWE.function_word?(b))

      xs when length(xs) >= 3 ->
        head = hd(xs)
        tail = List.last(xs)
        fn_count = Enum.count(xs, &Brain.LIFG.MWE.function_word?/1)

        not (Brain.LIFG.MWE.function_word?(head) or Brain.LIFG.MWE.function_word?(tail) or fn_count >= 2)
    end
  end

  defp allow_mwe_fallback_phrase?(_), do: false


# ---------- Slate/frame stamping helpers ----------

defp stamp_slates(%{} = si, %{seq: seq, ts_ms: ts, run_id: run_id}) do
  si
  |> Map.update(:sense_candidates, %{}, &stamp_bucket_map(&1, seq, ts, run_id))
  |> Map.update(:candidates_by_token, %{}, &stamp_bucket_map(&1, seq, ts, run_id))
  |> Map.update(:active_cells, Safe.get(si, :active_cells, []), &stamp_list(&1, seq, ts, run_id))
end

defp stamp_slates(si, _frame), do: si

defp stamp_bucket_map(%{} = m, seq, ts, run_id) do
  Enum.into(m, %{}, fn {k, v} ->
    {k, stamp_list(v, seq, ts, run_id)}
  end)
end

defp stamp_bucket_map(_other, _seq, _ts, _run_id), do: %{}

defp stamp_list(list, seq, ts, run_id) when is_list(list) do
  Enum.map(list, fn item ->
    item
    |> Safe.to_plain()
    |> maybe_stamp(seq, ts, run_id)
  end)
end

defp stamp_list(_other, _seq, _ts, _run_id), do: []

defp maybe_stamp(%{} = m, seq, ts, run_id) do
  m
  |> Map.put_new(:frame_seq, seq)
  |> Map.put_new(:frame_ts_ms, ts)
  |> Map.put_new(:frame_run_id, run_id)
end

defp maybe_stamp(other, _seq, _ts, _run_id), do: other
end

