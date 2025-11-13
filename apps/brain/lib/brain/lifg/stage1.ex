defmodule Brain.LIFG.Stage1 do
  @moduledoc """
  LIFG.Stage1 — first-pass disambiguation scoring.

  Responsibilities
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
  • Optional mood updates via telemetry: [:brain, :mood, :update]
  • Cerebellum forward model via Brain.Cerebellum.{calibrate_scores, learn_lifg}
  • NOTE: To avoid circular GenServer calls, semantic adjustments must be applied upstream
    (e.g., enrich `si.intent_bias` before Stage1). The Stage1 server process
    does not call out to other GenServers during `handle_call/3`.

  Public API
  • start_link/1 — starts the mood-aware Stage1 server
  • run/2        — pure Stage1 scoring over `si` + opts → {:ok, %{si, choices, audit}}
  • run/3        — legacy shim (weights, opts)
  • score/2      — server call to apply mood factor to a single base score (used internally)

  Telemetry
  • [:brain, :lifg, :stage1, :score] — per-candidate mood-applied score
  • (configurable) char-gram event (default [:brain, :lifg, :chargram_violation])
  • (configurable) boundary-drop event (default [:brain, :lifg, :boundary_drop])
  • (Cerebellum emits:)
    - [:brain, :cerebellum, :lifg, :predict] with %{margin0, margin1}
    - [:brain, :cerebellum, :lifg, :learn] with %{update_norm, loss}
  """

  use Brain, region: :lifg_stage1
  alias Brain.Utils.Safe
  alias Brain.Cerebellum
  alias Brain.MoodWeights

  @default_weights %{lex_fit: 0.40, rel_prior: 0.30, activation: 0.20, intent_bias: 0.10}
  @default_scores_mode :all
  @default_margin_thr 0.15
  @default_min_margin 0.05

  # Mood weights/cap (match MoodWeights.bias/3)
  @default_mw %{expl: 0.02, inhib: -0.03, vigil: 0.02, plast: 0.00}
  @default_cap 0.05
  @mood_handler_prefix "brain-lifg-stage1-mood-"

  @function_pos ~w(determiner preposition conjunction auxiliary modal pronoun adverb particle)

  # ---------- Public server API (mood) ----------
  def start_link(opts), do: GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  def score(server \\ __MODULE__, ctx), do: GenServer.call(server, {:score, ctx})

@spec run(map()) :: {:ok, %{si: map(), choices: list(), audit: map()}} | {:error, term()}
def run(si), do: run(si, [])

@spec run(map(), keyword()) ::
        {:ok, %{si: map(), choices: list(), audit: map()}} | {:error, term()}
def run(si, opts) when is_map(si) and is_list(opts) do
  try do
    si0   = Safe.to_plain(si)
    sent  = Safe.get(si0, :sentence, "") |> to_string()

    tokens =
      Safe.get(si0, :tokens, [])
      |> Enum.map(&Safe.to_plain/1)

    buckets =
      Safe.get(si0, :sense_candidates, %{}) ||
        Safe.get(si0, :candidates_by_token, %{}) || %{}

    # Effective knobs
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

    # Telemetry names (overridable by PMTG)
    chargram_event = Keyword.get(opts, :chargram_event, [:brain, :lifg, :chargram_violation])
    boundary_event = Keyword.get(opts, :boundary_event, [:brain, :lifg, :boundary_drop])

    acc =
      tokens
      |> Enum.with_index()
      |> Enum.reduce(
        %{
          choices: [],
          weak: 0,
          kept: 0,
          no_cand: 0,
          rejected: [],
          chargram: 0,
          boundary_drops: 0
        },
        fn {tok, idx}, acc ->
          cand_list = Map.get(buckets, idx, []) |> Enum.map(&Safe.to_plain/1)
          token_phrase = norm(Safe.get(tok, :phrase, ""))
          token_mwe?   = Safe.get(tok, :mw, false) or Safe.get(tok, :n, 1) > 1

          case boundary_check(sent, tok, token_phrase, token_mwe?) do
            :ok ->
              if cand_list == [] do
                Map.update!(acc, :no_cand, &(&1 + 1))
              else
                bias_val = get_float(bias_map, idx, 0.0)
                {syn_hits, ant_hits} = relations_count_overlaps(si0)

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

                    lex0 = lex_fit(cnrm, token_phrase, token_mwe?)
                    rel0 = clamp01(Safe.get(c, :rel_prior, guess_rel_prior(c)))
                    act  = clamp01(Safe.get(c, :activation, Safe.get(c, :score, guess_activation(c))))

                    # relation context
                    lex = clamp01(lex0 + 0.6 * syn_hits)
                    rel = clamp01(rel0 - 0.4 * ant_hits)

                    intent_feat =
                      intent_alignment_feature(bias_val, token_mwe?, cnrm, token_phrase, pos)

                    base0 =
                      (weights[:lex_fit]     * lex   +
                       weights[:rel_prior]   * rel   +
                       weights[:activation]  * act   +
                       weights[:intent_bias] * intent_feat)
                      |> clamp01()

                    hom_bump = if relations_homonym_bonus?(si0, c), do: 0.5, else: 0.0
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

                ctx_key    = Cerebellum.context_key({:lifg_stage1, intent: intent_key(si0), mwe: token_mwe?})
                cereb_opts = kw_to_map(scope: "lifg_stage1", context_key: ctx_key, margin_tau: margin_thr)

                cal_scores = cereb_calibrate(si0, base_scores, feats, cereb_opts)

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
                margin  = Float.round(max(margin0, min_margin), 6)

                _ = cereb_learn(si0, chosen_id, feats, base_scores, cereb_opts)

                scores_out =
                  case scores_mode do
                    :all  -> Map.new(ranked)
                    :top2 -> ranked |> Enum.take(2) |> Map.new()
                    _     -> %{}
                  end

                alt_ids =
                  ranked
                  |> Enum.map(&elem(&1, 0))
                  |> Enum.reject(&(&1 == chosen_id))

                choice = %{
                  token_index: idx,
                  chosen_id: chosen_id,
                  scores: scores_out,
                  alt_ids: alt_ids,
                  margin: margin,
                  prob_margin: margin
                }

                weak_next = if margin < margin_thr, do: acc.weak + 1, else: acc.weak

                acc
                |> Map.update!(:choices, &[choice | &1])
                |> Map.put(:weak, weak_next)
                |> Map.update!(:kept, &(&1 + 1))
              end

            {:error, :chargram} ->
              :telemetry.execute(
                chargram_event,
                %{count: 1},
                %{token_index: idx, phrase: token_phrase, mw: token_mwe?, reason: :chargram, v: 2}
              )

              acc
              |> Map.update!(:rejected, &[idx | &1])
              |> Map.update!(:chargram, &(&1 + 1))

            {:error, reason} ->
              :telemetry.execute(
                boundary_event,
                %{count: 1},
                %{token_index: idx, phrase: token_phrase, mw: token_mwe?, reason: reason, v: 2}
              )

              acc
              |> Map.update!(:rejected, &[idx | &1])
              |> Map.update!(:boundary_drops, &(&1 + 1))
          end
        end
      )

    dropped_total = acc.boundary_drops + acc.chargram + acc.no_cand

    audit =
      build_audit(
        acc.kept,
        dropped_total,
        Enum.reverse(acc.rejected),
        acc.chargram,
        acc.weak
      )

    {:ok, %{si: si0, choices: Enum.reverse(acc.choices), audit: audit}}
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

  # Group terminate/2 clauses together
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

  # (Telemetry bridge helpers)
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

  defp function_pos?(p) when is_binary(p), do: String.downcase(p) in @function_pos
  defp function_pos?(p) when is_atom(p), do: function_pos?(Atom.to_string(p))
  defp function_pos?(_), do: false

  # ---------- Boundary / char-gram guard with reasons ----------
  # Returns :ok or {:error, reason} where reason ∈
  #   :span_mismatch | :nonword_edges | :chargram
# apps/brain/lib/brain/lifg/stage1.ex

# replace boundary_check/4 with this reasoned version (or update yours accordingly)
# Returns :ok or {:error, reason} where reason ∈ :chargram | :span_mismatch | :nonword_edges
defp boundary_check(sentence, tok, phrase, token_mwe?) do
  # hard tripwire
  case Brain.Utils.Safe.get(tok, :source) do
    :chargram -> {:error, :chargram}
    _ ->
      case {sentence, Brain.Utils.Safe.get(tok, :span)} do
        {s, {i, j}}
          when is_binary(s) and is_integer(i) and is_integer(j) and j > i and
               i >= 0 and j <= byte_size(s) ->
          sub = :binary.part(s, i, j - i)

          if norm(sub) != norm(phrase) do
            {:error, :span_mismatch}
          else
            left_ok  = i == 0 or not word_char?(prev_char(s, i))
            right_ok = j == byte_size(s) or not word_char?(next_char(s, j))
            if token_mwe?, do: :ok, else: if(left_ok and right_ok, do: :ok, else: {:error, :nonword_edges})
          end

        _ ->
          # shape-only fallback
          if token_mwe? do
            if phrase_valid_mwe?(phrase), do: :ok, else: {:error, :chargram}
          else
            if phrase_valid_unigram?(phrase), do: :ok, else: {:error, :chargram}
          end
      end
  end
end

  defp phrase_valid_unigram?(p) when is_binary(p),
    do: String.match?(p, ~r/^\p{L}[\p{L}'-]*$/u)
  defp phrase_valid_unigram?(_), do: false

  defp phrase_valid_mwe?(p) when is_binary(p) do
    parts = String.split(p, ~r/\s+/u, trim: true)
    length(parts) >= 2 and Enum.all?(parts, &phrase_valid_unigram?/1)
  end
  defp phrase_valid_mwe?(_), do: false

  defp word_char?(nil), do: false
  defp word_char?(c) when is_binary(c),
    do: String.match?(c, ~r/^[\p{L}\p{N}'-]$/u)

  defp prev_char(_s, 0), do: nil
  defp prev_char(s, i) when is_binary(s) and is_integer(i) and i > 0 do
    left = :binary.part(s, 0, i)
    String.last(left)
  end

  defp next_char(s, j) when is_binary(s) and is_integer(j) and j < byte_size(s) do
    right = :binary.part(s, j, byte_size(s) - j)
    String.first(right)
  end
  defp next_char(_s, _j), do: nil

  # ---------- Mood helpers ----------
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
# ----- add these helpers (anywhere in the module’s private section) -----
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
    val =
      get_opt(opts, :mood_weights, Application.get_env(:brain, :lifg_mood_weights, @default_mw))

    %{
      expl: to_small(val[:expl] || val["expl"] || @default_mw.expl),
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
      pos = pos_of(c)
      "#{lemma}|#{pos}|0"
    else
      to_string(id)
    end
  end

  defp pos_of(c) do
    p = Safe.get(c, :pos) || Safe.get(c, "pos") || "other"
    to_string(p) |> String.downcase()
  end

  defp guess_rel_prior(c) do
    norm = Safe.get(c, :norm) || Safe.get(c, :lemma) || Safe.get(c, :word) || ""
    if String.contains?(to_string(norm), " "), do: 0.30, else: 0.20
  end

  defp guess_activation(c) do
    norm = Safe.get(c, :norm) || Safe.get(c, :lemma) || ""
    if String.contains?(to_string(norm), " "), do: 0.30, else: 0.25
  end

  defp softmax([]), do: []
  defp softmax(xs) when is_list(xs) do
    m = Enum.max(xs, fn -> 0.0 end)
    exs = Enum.map(xs, fn x -> :math.exp(x * 1.0 - m) end)
    z = Enum.sum(exs)

    if z <= 0.0 do
      n = length(xs)
      if n == 0, do: [], else: List.duplicate(1.0 / n, n)
    else
      Enum.map(exs, &(&1 / z))
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

  # Stable, coarse context label for Cerebellum keying
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
      true -> "none"
    end
  end

  # ——— tolerant map helper (no guard calling Keyword.keyword?/1) ———
  # lib/brain/lifg/stage1.ex
  defp kw_to_map(v) do
    cond do
      is_list(v) and Keyword.keyword?(v) -> Map.new(v)
      is_map(v) -> v
      true -> %{}
    end
  end

  # ---------- Relation helpers (safe dynamic calls) ----------
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
      rejected_by_boundary: rejected_by_boundary,  # <- back-compat for tests
      chargram_violation: chargram_drops,
      weak_decisions: weak
    }
  end

end

