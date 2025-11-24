defmodule Brain.LIFG.Stage1 do
  @moduledoc """
  LIFG.Stage1 — first-pass disambiguation scoring.

  Responsibilities
  ----------------
  • Score sense candidates per token using a weighted feature mix:
      - :lex_fit     — lexical compatibility with the token (esp. for MWEs)
      - :rel_prior   — relational prior / heuristic prior from candidate
      - :activation  — candidate prior activation / score (DB or slate)
      - :intent_bias — bias from `si.intent_bias[token_index]` (e.g., MWE signatures)
  • Apply mood nudging (multiplicative factor) if the Stage1 server is running.
  • Apply Cerebellum calibration (tiny delta-scores) when margin is thin.
  • Normalize to probabilities (softmax), compute margins, return winners.
  • Learn online in Cerebellum after the decision.

  Integrations
  ------------
  • Optional mood updates via telemetry: [:brain, :mood, :update]
  • Cerebellum forward model via Brain.Cerebellum.{calibrate_scores, learn_lifg}
  • NOTE: To avoid circular GenServer calls, semantic adjustments must be applied upstream
    (e.g., enrich `si.intent_bias` before Stage1). The Stage1 server process
    does not call out to other GenServers during `handle_call/3`.

  Telemetry
  ---------
  • [:brain, :lifg, :stage1, :score]       — per-candidate mood-applied score
  • [:brain, :lifg, :chargram_violation]   — char-gram drops (measurements = %{})
  • [:brain, :lifg, :boundary_drop]        — boundary guard drops (measurements = %{})
  • [:brain, :pmtg, :mwe_fallback_emitted] — MWE tokens with no senses (measurements = %{count: 1})
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
    "verb"   => 0.96,
    "adj"    => 0.95,
    "adv"    => 0.94,
    "noun"   => 0.93,
    "other"  => 0.93
  }

  @greeting_lemmas MapSet.new([
                     "hey how is",
                     "how are you",
                     "how is life treating you",
                     "how is life treating"
                   ])

  # Default event for MWE fallback telemetry (tests attach to this)
  @mwe_fallback_event [:brain, :pmtg, :mwe_fallback_emitted]

  # ---------- Public server API (mood) ----------

  def start_link(opts), do: GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  def score(server \\ __MODULE__, ctx), do: GenServer.call(server, {:score, ctx})

  # ---------- Public Stage1 API (pure scoring) ----------

  @spec run(map()) ::
          {:ok, %{si: map(), choices: list(), audit: map()}} | {:error, term()}
  def run(si), do: run(si, [])

  @spec run(map(), keyword()) ::
          {:ok, %{si: map(), choices: list(), audit: map()}} | {:error, term()}
  def run(si, opts) when is_map(si) and is_list(opts) do
    try do
      si0  = Safe.to_plain(si)
      sent = Safe.get(si0, :sentence, "") |> to_string()

      # 1) Tokens + Guard preprocessing (may drop tokens)
      {tokens, guard_rejected, guard_drops} = prepare_tokens(si0, sent)
      buckets = buckets_from_si(si0)

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
          Application.get_env(:brain, :lifg_min_margin, @default_margin_thr)
        )

      min_margin =
        Keyword.get(
          opts,
          :min_margin,
          Application.get_env(:brain, :lifg_min_margin, @default_min_margin)
        )

      bias_map = Keyword.get(opts, :intent_bias, Safe.get(si0, :intent_bias, %{})) || %{}

      # 3) Telemetry events (overridable by tests / PMTG)
      chargram_event = Keyword.get(opts, :chargram_event, [:brain, :lifg, :chargram_violation])
      boundary_event = Keyword.get(opts, :boundary_event, [:brain, :lifg, :boundary_drop])
      mwe_event      = Keyword.get(opts, :mwe_event, @mwe_fallback_event)
      mwe_fallback?  = Keyword.get(opts, :mwe_fallback, true)

      # 4) Main scoring loop
      {choices, acc} =
        score_tokens(tokens, %{
          si: si0,
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
          mwe_fallback?: mwe_fallback?
        })

      # 5) Audit + reanalysis hook
      dropped_total = acc.boundary_drops + acc.chargram + acc.no_cand + guard_drops

      rejected_all =
        guard_rejected
        |> Enum.concat(Enum.reverse(acc.rejected))
        |> Enum.uniq()
        |> Enum.sort()

      audit =
        build_audit(
          acc.kept,
          dropped_total,
          rejected_all,
          acc.chargram + acc.boundary_drops + guard_drops,
          acc.weak
        )

      out = %{si: si0, choices: Enum.reverse(choices), audit: audit}
      {:ok, maybe_reanalyse(out, si0, opts)}
    rescue
      e -> {:error, e}
    end
  end

  @spec run(map(), map() | keyword(), keyword()) ::
          {:ok, %{si: map(), choices: list(), audit: map()}} | {:error, term()}
  def run(si, weights_or_kw, opts) when is_list(opts) do
    weights_map =
      case weights_or_kw do
        m when is_map(m) -> m
        kw when is_list(kw) -> Map.new(kw)
        _ -> %{}
      end

    run(si, Keyword.merge(opts, weights: weights_map))
  end

  # ---------- Internal helpers for run/2 ------------------------------------

  # Guard + basic token prep:
  #   • converts structs → plain maps
  #   • applies Brain.LIFG.Guard.sanitize/1 (char-grams + boundary)
  #   • returns sanitized tokens + which original indices Guard dropped
  defp prepare_tokens(si0, sent) do
    {raw_tokens, raw_indices} =
      si0
      |> Safe.get(:tokens, [])
      |> Enum.map(&Safe.to_plain/1)
      |> Enum.with_index()
      |> Enum.map(fn {tok, fallback_idx} ->
        {tok, token_index(tok, fallback_idx)}
      end)
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
      |> Enum.map(&token_index(&1, 0))

    guard_rejected =
      raw_indices
      |> Enum.uniq()
      |> Enum.reject(&(&1 in kept_indices))
      |> Enum.sort()

    guard_drops = length(guard_rejected)

    {tokens, guard_rejected, guard_drops}
  end

  defp token_index(tok, fallback_idx) do
    case {Safe.get(tok, :index), Safe.get(tok, "index")} do
      {i, _} when is_integer(i) -> i
      {_, i} when is_integer(i) -> i
      _ -> fallback_idx
    end
  end

  defp token_raw_phrase(tok) do
    Safe.get(tok, :phrase) ||
      Safe.get(tok, :lemma) ||
      Safe.get(tok, :word) ||
      ""
  end

  defp token_mwe?(tok) do
    n_val       = Safe.get(tok, :n) || Safe.get(tok, "n") || 1
    has_mw_flag = Safe.get(tok, :mw, false) || Safe.get(tok, "mw", false)
    has_mw_flag || (is_integer(n_val) and n_val > 1)
  end

  defp telemetry_phrase(tok, token_phrase) do
    Safe.get(tok, :lemma) ||
      Safe.get(tok, "lemma") ||
      Safe.get(tok, :word) ||
      Safe.get(tok, "word") ||
      token_phrase
  end

  # Main scoring loop: walks sanitized tokens and accumulates stats
  defp score_tokens(tokens, ctx) do
    acc0 = %{
      choices: [],
      weak: 0,
      kept: 0,
      no_cand: 0,
      rejected: [],
      chargram: 0,
      boundary_drops: 0
    }

    final =
      Enum.reduce(tokens, acc0, fn tok, acc ->
        score_token(tok, acc, ctx)
      end)

    {final.choices, final}
  end

  defp score_token(tok, acc, ctx) do
    raw_phrase   = token_raw_phrase(tok)
    token_phrase = norm(raw_phrase)
    token_mwe?   = token_mwe?(tok)
    tok_index    = token_index(tok, 0)

    case boundary_check(ctx.sent, tok, token_phrase, token_mwe?) do
      :ok ->
        score_token_ok(tok, tok_index, raw_phrase, token_phrase, token_mwe?, acc, ctx)

      {:error, :chargram} ->
        handle_chargram_drop(tok, tok_index, token_phrase, token_mwe?, acc, ctx)

      {:error, reason} ->
        handle_boundary_drop(tok, tok_index, token_phrase, token_mwe?, reason, acc, ctx)
    end
  end

  # Token passes boundary/char-gram guard → look at candidates and score
  defp score_token_ok(tok, tok_index, raw_phrase, token_phrase, token_mwe?, acc, ctx) do
    orig_cands =
      Map.get(ctx.buckets, tok_index, [])
      |> Enum.map(&Safe.to_plain/1)

    cand_list =
      orig_cands
      |> Enum.filter(&is_map/1)
      |> restrict_to_phrase_if_mwe(token_mwe?)

    if cand_list == [] do
      acc2 = Map.update!(acc, :no_cand, &(&1 + 1))

      # Emit MWE fallback telemetry when an MWE token has no senses at all.
      if ctx.mwe_fallback? and token_mwe? and orig_cands == [] do
        emit_mwe_fallback(ctx.mwe_event, tok_index, raw_phrase, 0.0)
      end

      acc2
    else
      bias_val = get_float(ctx.bias_map, tok_index, 0.0)
      {syn_hits, ant_hits} = relations_count_overlaps(ctx.si)

      scored_trip =
        Enum.map(cand_list, fn c ->
          id  = sense_id_for(c, token_phrase)
          pos = pos_of(c)

          cnrm =
            norm(
              Safe.get(c, :norm) ||
                Safe.get(c, :lemma) ||
                Safe.get(c, :word) ||
                token_phrase
            )

          # Heuristic base features
          lex0 = lex_fit(cnrm, token_phrase, token_mwe?)

          rel0 =
            guess_rel_prior(c, id, token_phrase)
            |> clamp01()

          act0 =
            clamp01(
              Safe.get(c, :activation, Safe.get(c, :score, guess_activation(c)))
            )

          # relation context
          lex_ctx = clamp01(lex0 + 0.6 * syn_hits)
          rel_ctx = clamp01(rel0 - 0.4 * ant_hits)

          intent0 =
            intent_alignment_feature(bias_val, token_mwe?, cnrm, token_phrase, pos)

          # Optional override from nested :features (for tests/synthetic inputs)
          feat_override =
            Safe.get(c, :features) ||
              Safe.get(c, "features") ||
              %{}

          lex =
            feat_override
            |> get_num(:lex_fit, lex_ctx)
            |> clamp01()

          rel =
            feat_override
            |> get_num(:rel_prior, rel_ctx)
            |> clamp01()

          act =
            feat_override
            |> get_num(:activation, act0)
            |> clamp01()

          intent_feat =
            feat_override
            |> get_num(:intent_bias, intent0)
            |> clamp01()

          base0 =
            (ctx.weights[:lex_fit]     * lex   +
               ctx.weights[:rel_prior]   * rel   +
               ctx.weights[:activation]  * act   +
               ctx.weights[:intent_bias] * intent_feat)
            |> clamp01()

          hom_bump = if relations_homonym_bonus?(ctx.si, c), do: 0.5, else: 0.0
          base     = apply_mood_if_up(clamp01(base0 + hom_bump), tok, id)

          feat = %{
            id: id,
            lex_fit: lex,
            rel_prior: rel,
            activation: act,
            intent_bias: intent_feat
          }

          {id, base, feat}
        end)

      ids         = Enum.map(scored_trip, fn {id, _b, _f} -> id end)
      base_scores = Map.new(scored_trip, fn {id, b, _} -> {id, b} end)
      feats       = Enum.map(scored_trip, fn {_id, _b, f} -> f end)

      ctx_key =
        Cerebellum.context_key({:lifg_stage1, intent: intent_key(ctx.si), mwe: token_mwe?})

      cereb_opts =
        kw_to_map(scope: "lifg_stage1", context_key: ctx_key, margin_tau: ctx.margin_thr)

      cal_scores = cereb_calibrate(ctx.si, base_scores, feats, cereb_opts)

      logits = Enum.map(ids, &Map.get(cal_scores, &1, 0.0))
      probs  = softmax(logits)

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
      margin  = Float.round(max(margin0, ctx.min_margin), 6)

      _ = cereb_learn(ctx.si, chosen_id, feats, base_scores, cereb_opts)

      scores_out =
        case ctx.scores_mode do
          :all  -> Map.new(ranked)
          :top2 -> ranked |> Enum.take(2) |> Map.new()
          _     -> %{}
        end

      alt_ids =
        ranked
        |> Enum.map(&elem(&1, 0))
        |> Enum.reject(&(&1 == chosen_id))

      # Backwards-compatible choice shape
      choice = %{
        token_index: tok_index,
        index: tok_index,
        id: chosen_id,
        chosen_id: chosen_id,
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

  # Char-gram drop: emit telemetry with the *underlying lexeme* when possible.
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
        v: 2
      }
    )

    acc
    |> Map.update!(:rejected, &[tok_index | &1])
    |> Map.update!(:chargram, &(&1 + 1))
  end

  # Boundary/shape drop (non-char-gram reasons)
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
        v: 2
      }
    )

    acc
    |> Map.update!(:rejected, &[tok_index | &1])
    |> Map.update!(:boundary_drops, &(&1 + 1))
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

  # Convenience: optional public status/0 for dashboards
  def status do
    case Process.whereis(__MODULE__) do
      nil -> %{}
      _ -> GenServer.call(__MODULE__, :status, 150)
    end
  end

  # Group ALL handle_call/3 clauses together
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

  # Telemetry bridge
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
      inhibition:  get_num(meas, :inhibition,  0.5) |> clamp01(),
      vigilance:   get_num(meas, :vigilance,   0.5) |> clamp01(),
      plasticity:  get_num(meas, :plasticity,  0.5) |> clamp01()
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

  # If the token is an MWE, prefer phrase-like senses.
  defp restrict_to_phrase_if_mwe(candidates, false), do: candidates

  defp restrict_to_phrase_if_mwe(candidates, true) when is_list(candidates) do
    phrase_like =
      Enum.filter(candidates, fn c ->
        id   = Safe.get(c, :id)    || Safe.get(c, "id")
        norm = Safe.get(c, :norm)  || Safe.get(c, :lemma) || Safe.get(c, :word)

        id_s   = to_string(id || "")
        norm_s = to_string(norm || "")

        String.contains?(id_s, "|phrase|") or String.contains?(norm_s, " ")
      end)

    case phrase_like do
      []   -> candidates
      list -> list
    end
  end

  defp function_pos?(p) when is_binary(p), do: String.downcase(p) in @function_pos
  defp function_pos?(p) when is_atom(p),   do: function_pos?(Atom.to_string(p))
  defp function_pos?(_),                  do: false

  # ---------- Candidate bucket extraction ----------

  # Prefer explicit sense maps; fall back to candidates_by_token; otherwise
  # synthesize buckets from :active_cells (used in WM gating tests).
  defp buckets_from_si(si0) do
    sense = Safe.get(si0, :sense_candidates, nil)

    cond do
      good_bucket_map?(sense) ->
        sense

      true ->
        cbt = Safe.get(si0, :candidates_by_token, nil)

        cond do
          good_bucket_map?(cbt) ->
            cbt

          true ->
            ac =
              Safe.get(si0, :active_cells) ||
                Safe.get(si0, "active_cells") ||
                []

            if is_list(ac) and ac != [] do
              ac
              |> Enum.map(&Safe.to_plain/1)
              |> active_cells_to_buckets()
            else
              %{}
            end
        end
    end
  end

  defp good_bucket_map?(m) when is_map(m), do: map_size(m) > 0
  defp good_bucket_map?(_), do: false

  # Group active cells by token_index and turn each into a Stage-1 candidate.
  #
  # This covers the WM tests where we only have :active_cells with:
  #   • token_index
  #   • id + features (first test)
  #   • chosen_id + scores (second test)
  defp active_cells_to_buckets(cells) do
    cells
    |> Enum.group_by(fn cell ->
      case {Safe.get(cell, :token_index), Safe.get(cell, "token_index")} do
        {i, _} when is_integer(i) -> i
        {_, i} when is_integer(i) -> i
        _ -> 0
      end
    end)
    |> Enum.into(%{}, fn {idx, group} ->
      {idx, Enum.map(group, &active_cell_to_candidate/1)}
    end)
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

  # For the WM test with `scores: %{"THIS/strong" => 0.6}`,
  # pick a sensible scalar score to seed activation.
  defp cell_score_from_map(cell) do
    scores =
      Safe.get(cell, :scores) ||
        Safe.get(cell, "scores") ||
        %{}

    if is_map(scores) and map_size(scores) > 0 do
      chosen = Safe.get(cell, :chosen_id) || Safe.get(cell, "chosen_id")

      cond do
        chosen && Map.has_key?(scores, chosen) ->
          Map.get(scores, chosen)

        chosen && Map.has_key?(scores, to_string(chosen)) ->
          Map.get(scores, to_string(chosen))

        true ->
          scores
          |> Map.values()
          |> Enum.max(fn -> 0.0 end)
      end
    else
      0.0
    end
  end

  defp guess_cell_id(cell) do
    lemma = Safe.get(cell, :lemma) || Safe.get(cell, "lemma") || "cell"
    pos   = Safe.get(cell, :pos)   || Safe.get(cell, "pos")   || "other"
    "#{lemma}|#{pos}|0"
  end

  defp guess_cell_lemma(id) when is_binary(id) do
    case String.split(id, "|") do
      [lemma | _] -> lemma
      _ -> id
    end
  end

  defp guess_cell_lemma(id), do: to_string(id)



  # ---------- Boundary / char-gram guard with reasons ----------

  @spec boundary_check(binary() | nil, map(), binary(), boolean()) ::
          :ok | {:error, :chargram | :nonword_edges | :span_mismatch}
  @spec boundary_check(binary() | nil, map(), binary(), boolean()) ::
          :ok | {:error, :chargram | :nonword_edges | :span_mismatch}
  defp boundary_check(sentence, tok, phrase_norm, token_mwe?) do
    source =
      Brain.Utils.Safe.get(tok, :source) ||
        Brain.Utils.Safe.get(tok, "source")

    kind =
      Brain.Utils.Safe.get(tok, :kind) ||
        Brain.Utils.Safe.get(tok, "kind")

    flag =
      Brain.Utils.Safe.get(tok, :chargram?, false) ||
        Brain.Utils.Safe.get(tok, "chargram?", false)

    cond do
      # Any explicit char-gram hint → treat as char-gram
      source in [:chargram, :char_ngram, "chargram", "char_ngram"] or
          kind in [:chargram, :char_ngram, "chargram", "char_ngram"] or
          flag ->
        {:error, :chargram}

      true ->
        phrase_raw = Brain.Utils.Safe.get(tok, :phrase)

        # Empty phrases are always treated as char-grams.
        if phrase_nil_or_empty?(phrase_norm) or phrase_nil_or_empty?(phrase_raw) do
          {:error, :chargram}
        else
          if token_mwe? do
            boundary_check_mwe(sentence, tok, phrase_norm)
          else
            boundary_check_unigram(sentence, tok, phrase_norm)
          end
        end
    end
  end

  # --- MWE path: must still match the span; otherwise it's junk ---
  defp boundary_check_mwe(sentence, tok, phrase_norm) do
    case {sentence, Safe.get(tok, :span)} do
      {s, {start, stop}}
      when is_binary(s) and is_integer(start) and is_integer(stop) and
             start >= 0 and stop > start and stop <= byte_size(s) ->
        len      = stop - start
        sub_norm = s |> binary_part(start, len) |> norm()

        if sub_norm == phrase_norm do
          # Proper aligned MWE: bypass strict boundary checks.
          :ok
        else
          # Span text doesn't match the phrase → treat as char-gram.
          {:error, :chargram}
        end

      _ ->
        # No usable span: accept only "shapely" MWEs, otherwise char-gram.
        if phrase_valid_mwe?(phrase_norm), do: :ok, else: {:error, :chargram}
    end
  end

  # --- Unigram path: full span + boundary invariants ---
  defp boundary_check_unigram(sentence, tok, phrase_norm) do
    case {sentence, Safe.get(tok, :span)} do
      {s, {start, stop}}
      when is_binary(s) and is_integer(start) and is_integer(stop) and
             start >= 0 and stop > start and stop <= byte_size(s) ->
        len      = stop - start
        sub_norm = s |> binary_part(start, len) |> norm()

        cond do
          # The span text does not match the token phrase at all.
          sub_norm != phrase_norm ->
            cond do
              # For a unigram token whose "phrase" contains a space
              # (like "ck t"), treat this mismatch as a char-gram.
              String.contains?(phrase_norm, " ") ->
                {:error, :chargram}

              # Otherwise, clean-looking words/phrases are span mismatches.
              phrase_valid_unigram?(phrase_norm) or phrase_valid_mwe?(phrase_norm) ->
                {:error, :span_mismatch}

              # Fallback: weird junk ⇒ char-gram.
              true ->
                {:error, :chargram}
            end

          true ->
            # Enforce word boundaries (no letter/digit immediately outside the span).
            left_ok =
              start == 0 or
                not word_char?(String.at(s, start - 1))

            right_ok =
              stop == byte_size(s) or
                not word_char?(String.at(s, stop))

            if left_ok and right_ok do
              :ok
            else
              {:error, :nonword_edges}
            end
        end

      _ ->
        # No span at all (typical synthetic / SI-only tests): allow clean
        # words/phrases, treat everything else as char-grams.
        if phrase_valid_unigram?(phrase_norm) or phrase_valid_mwe?(phrase_norm) do
          :ok
        else
          {:error, :chargram}
        end
    end
  end

  # ---------- Phrase shape helpers ----------

  defp phrase_nil_or_empty?(nil), do: true
  defp phrase_nil_or_empty?(""),  do: true

  defp phrase_nil_or_empty?(p) when is_binary(p) do
    String.trim(p) == ""
  end

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

  defp word_char?(nil), do: false

  defp word_char?(<<cp::utf8>>) do
    (cp >= ?0 and cp <= ?9) or
      (cp >= ?a and cp <= ?z) or
      (cp >= ?A and cp <= ?Z)
  end

  defp word_char?(_), do: false

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

  # Use Brain.MoodWeights.bias/3 for the scalar; multiply: final = base * (1 + bias).
  defp mood_factor(nil, opts), do: {1.0, 0.0, nil, cfg_mw(opts), cfg_cap(opts)}

  defp mood_factor(mood, opts) do
    w   = cfg_mw(opts)
    cap = cfg_cap(opts)

    bias =
      try do
        MoodWeights.bias(mood, w, cap)
      rescue
        _ ->
          dx = %{
            expl:  mood.exploration - 0.5,
            inhib: mood.inhibition  - 0.5,
            vigil: mood.vigilance   - 0.5,
            plast: mood.plasticity  - 0.5
          }

          raw =
            dx.expl * (w.expl  || 0.0) +
              dx.inhib * (w.inhib || 0.0) +
              dx.vigil * (w.vigil || 0.0) +
              dx.plast * (w.plast || 0.0)

          clamp(raw, -cap, cap)
      end

    factor = 1.0 + bias
    {factor, bias, mood, w, cap}
  end

  defp cfg_mw(opts) do
    val =
      get_opt(opts, :mood_weights, Application.get_env(:brain, :lifg_mood_weights, @default_mw))

    %{
      expl:  to_small(val[:expl]  || val["expl"]  || @default_mw.expl),
      inhib: to_small(val[:inhib] || val["inhib"] || @default_mw.inhib),
      vigil: to_small(val[:vigil] || val["vigil"] || @default_mw.vigil),
      plast: to_small(val[:plast] || val["plast"] || @default_mw.plast)
    }
  end

  defp cfg_cap(opts),
    do:
      to_cap(get_opt(opts, :mood_cap, Application.get_env(:brain, :lifg_mood_cap, @default_cap)))

  # ---------- Small utils ----------

  defp sense_id_for(c, token_phrase) do
    id = Safe.get(c, :id) || Safe.get(c, "id")

    if is_nil(id) do
      lemma = Safe.get(c, :lemma) || Safe.get(c, :word) || token_phrase
      pos   = pos_of(c)
      "#{lemma}|#{pos}|0"
    else
      to_string(id)
    end
  end

  defp pos_of(c) do
    p = Safe.get(c, :pos) || Safe.get(c, "pos") || "other"
    to_string(p) |> String.downcase()
  end

  defp guess_rel_prior(c, id, token_phrase) do
    norm0 = Safe.get(c, :norm) || Safe.get(c, :lemma) || Safe.get(c, :word) || ""
    id_s  = to_string(id || "")

    {lemma, pos, tag} = parse_sense_id(id_s)
    lemma_norm        = norm(lemma)

    phrase_like? =
      String.contains?(to_string(token_phrase || ""), " ") or
        String.contains?(id_s, "|phrase|") or
        String.contains?(to_string(norm0), " ")

    base_pos =
      @pos_prior
      |> Map.get(pos, @default_rel_prior)
      |> apply_phrase_fallback_adjustment(pos, tag)
      |> maybe_boost_greeting_phrase(lemma_norm, pos, tag)

    adj =
      if phrase_like? do
        0.02
      else
        -0.02
      end

    clamp01(base_pos + adj)
  end

  defp parse_sense_id(id_str) when is_binary(id_str) do
    case String.split(id_str, "|") do
      [lemma, pos, tag] ->
        {lemma, String.downcase(pos || "other"), String.downcase(tag || "")}

      [lemma, pos] ->
        {lemma, String.downcase(pos || "other"), ""}

      [lemma] ->
        {lemma, "other", ""}

      _ ->
        {id_str, "other", ""}
    end
  end

  defp parse_sense_id(other),
    do: parse_sense_id(to_string(other))

  defp apply_phrase_fallback_adjustment(base, "phrase", "fallback"),
    do: clamp(base + 0.02, 0.0, 1.0)

  defp apply_phrase_fallback_adjustment(base, _pos, _tag), do: base

  defp maybe_boost_greeting_phrase(base, lemma_norm, "phrase", "fallback") do
    if MapSet.member?(@greeting_lemmas, lemma_norm) do
      clamp(base + 0.03, 0.0, 1.0)
    else
      base
    end
  end

  defp maybe_boost_greeting_phrase(base, _lemma_norm, _pos, _tag), do: base

  defp guess_activation(c) do
    norm = Safe.get(c, :norm) || Safe.get(c, :lemma) || ""
    if String.contains?(to_string(norm), " "), do: 0.30, else: 0.25
  end

  defp softmax([]), do: []

  defp softmax(xs) when is_list(xs) do
    m   = Enum.max(xs, fn -> 0.0 end)
    exs = Enum.map(xs, fn x -> :math.exp(x * 1.0 - m) end)
    z   = Enum.sum(exs)

    cond do
      z <= 0.0 ->
        n = length(xs)
        if n == 0, do: [], else: List.duplicate(1.0 / n, n)

      true ->
        probs0 = Enum.map(exs, &(&1 / z))
        sum0   = Enum.sum(probs0)

        if sum0 > 0.0 do
          Enum.map(probs0, &(&1 / sum0))
        else
          probs0
        end
    end
  end

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
  defp clamp(_, lo, _hi),                    do: lo
  defp clamp01(x) when is_number(x),         do: clamp(x * 1.0, 0.0, 1.0)
  defp clamp01(_),                           do: 0.0

  defp get_float(map, k, dflt) when is_map(map) do
    case {Map.get(map, k), Map.get(map, to_string(k))} do
      {v, _} when is_number(v) -> v * 1.0
      {_, v} when is_number(v) -> v * 1.0
      _ -> dflt * 1.0
    end
  end

  defp get_float(_, _, d), do: d * 1.0

  defp get_opt(opts, key, default) when is_list(opts), do: Keyword.get(opts, key, default)
  defp get_opt(%{} = opts, key, default),             do: Map.get(opts, key, default)
  defp get_opt(_, _, default),                        do: default

  defp to_small(x) when is_number(x), do: x * 1.0
  defp to_small(_),                   do: 0.0

  defp to_cap(x) when is_number(x), do: max(0.0, min(1.0, x * 1.0))
  defp to_cap(_),                   do: @default_cap

  defp get_num(map, key, default) do
    case {Map.get(map, key), Map.get(map, to_string(key))} do
      {v, _} when is_integer(v) -> v * 1.0
      {v, _} when is_float(v)   -> v
      {_, v} when is_integer(v) -> v * 1.0
      {_, v} when is_float(v)   -> v
      _ -> default * 1.0
    end
  end

  defp unique(prefix),
    do:
      prefix <>
        Integer.to_string(:erlang.unique_integer([:positive])) <>
        "-" <> Integer.to_string(System.system_time(:microsecond))

  defp normalize_opts(opts) when is_list(opts), do: if(Keyword.keyword?(opts), do: opts, else: [])
  defp normalize_opts(%{} = opts),             do: Map.to_list(opts)
  defp normalize_opts(_),                     do: []

  defp intent_key(si0) do
    i = Safe.get(si0, :intent)

    cond do
      is_binary(i) -> i
      is_map(i) ->
        to_string(
          Safe.get(i, :intent) ||
            Safe.get(i, :name) ||
            Safe.get(i, :type) ||
            "none"
        )

      true ->
        "none"
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

  defp build_audit(kept, dropped, rejected_by_boundary, chargram_drops, weak) do
    %{
      feature_mix: :lifg_stage1,
      kept_tokens: kept,
      dropped_tokens: dropped,
      boundary_drops: length(rejected_by_boundary),
      rejected_by_boundary: rejected_by_boundary,
      chargram_violation: chargram_drops,
      weak_decisions: weak
    }
  end

  # ───────────────────────── Reanalysis hook ─────────────────────────

  # Only applies when opts include :reanalysis or :reanalysis?
  #   • Uses a fail_fun from opts if present
  #   • Otherwise builds a fail_fun that respects per-sense `veto?: true`
  defp maybe_reanalyse(%{choices: choices} = out, si0, opts) do
    reanalysis? =
      Keyword.get(opts, :reanalysis, false) or
        Keyword.get(opts, :reanalysis?, false)

    if not reanalysis? do
      out
    else
      fail_fun =
        Keyword.get(opts, :fail_fun) ||
          build_veto_fail_fun(si0)

      %{choices: flipped, flips: n} = Reanalysis.fallback(choices, fail_fun, opts)

      audit0 = out.audit || %{}
      audit = Map.update(audit0, :flips, n, &(&1 + n))

      %{out | choices: flipped, audit: audit}
    end
  end

  defp maybe_reanalyse(out, _si0, _opts), do: out

  # Build a fail_fun/1 that returns true when the chosen sense comes
  # from a candidate flagged as `veto?: true` in `si.sense_candidates`.
  defp build_veto_fail_fun(%{sense_candidates: sc}) when is_map(sc) do
    veto_lookup =
      sc
      |> Enum.flat_map(fn {idx, senses} ->
        Enum.map(senses, fn s ->
          id = s[:id] || s["id"]
          veto? = s[:veto?] || s["veto?"] || false
          {{idx, id}, veto?}
        end)
      end)
      |> Enum.into(%{})

    fn
      %{token_index: idx, chosen_id: id} ->
        Map.get(veto_lookup, {idx, id}, false)

      _ ->
        false
    end
  end

  defp build_veto_fail_fun(_), do: fn _ -> false end

  # ---------- MWE fallback telemetry ----------

  defp emit_mwe_fallback(event, idx, phrase, score) do
    meta = %{
      token_index: idx,
      phrase: to_string(phrase || ""),
      score: score
    }

    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(event, %{count: 1}, meta)
    else
      :ok
    end
  end
end

