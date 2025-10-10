defmodule Brain.LIFG do
  @moduledoc """
  Left Inferior Frontal Gyrus (LIFG) — Competitive Sense Selection.

  Entry points:
    • Legacy-compatible API (pure): `disambiguate_stage1/1,2`
      - Calls the new `Brain.LIFG.Stage1.run/2` engine and preserves the legacy event shape.
    • Full pipeline: `run/2` (ATL finalize → Stage-1 → optional pMTG, optional ACC gate).

  Optional ACC hook:
    If `Brain.ACC` is available, `run/2` will call it to compute a conflict score
    and only apply pMTG if `conflict >= acc_conflict_tau`. When ACC is absent,
    behavior is unchanged (pMTG applies according to opts).

  Central config (`config/config.exs`):

      config :brain,
        lifg_stage1_weights: %{lex_fit: 0.40, rel_prior: 0.30, activation: 0.20, intent_bias: 0.10},
        lifg_stage1_scores_mode: :all,  # or :top2 | :none
        lifg_min_margin: 0.05,
        lifg_stage1_mwe_fallback: true,
        pmtg_mode: :boost,
        pmtg_margin_threshold: 0.15,
        pmtg_window_keep: 50,
        acc_conflict_tau: 0.50
  """

  require Logger
  use Brain, region: :lifg

  alias Brain.Utils.Safe
  alias Brain.LIFG.Input

  @default_weights %{lex_fit: 0.40, rel_prior: 0.30, activation: 0.20, intent_bias: 0.10}

  # ── Server bootstrap & runtime config ────────────────────────────────

  @impl true
  def init(opts) do
    eff = opts |> Map.new() |> effective_opts()
    {:ok, %{region: :lifg, opts: eff}}
  end

  # ── Optional server API (convenience) ────────────────────────────────

  # Returns effective options (env + overrides), even if the server hasn't started.
  def status(server \\ __MODULE__) do
    case GenServer.whereis(server) do
      nil ->
        effective_opts(%{})

      _pid ->
        try do
          GenServer.call(server, :status)
        catch
          :exit, _ -> effective_opts(%{})
        end
    end
  end

  def reload_config(new_opts, server \\ __MODULE__) when is_list(new_opts) or is_map(new_opts) do
    GenServer.cast(server, {:reload_config, Map.new(new_opts)})
  end

  @impl true
  def handle_call(:status, _from, state) do
    eff = effective_opts(state.opts || %{})
    {:reply, eff, %{state | opts: eff}}
  end

  @impl true
  def handle_cast({:reload_config, new_opts}, state) do
    merged = Map.merge(state.opts || %{}, Map.new(new_opts))
    {:noreply, %{state | opts: effective_opts(merged)}}
  end

  # ── Stage-1 (legacy-compatible event shape) ──────────────────────────

  @doc """
  Pure, stateless Stage-1 wrapper over `Brain.LIFG.Stage1.run/2`.

  Preserves legacy event shape with `:choices/:boosts/:inhibitions`.
  - Boost amount = chosen margin (>= 0)
  - Inhibition amount = max(margin_threshold - score_gap, 0.0)
  - Supports `{id, amount}` tuples when `:emit_pairs` (or WM gating flags) are present.
  """
  @spec disambiguate_stage1(map(), keyword()) :: map()
  def disambiguate_stage1(%{} = si, opts) do
    si_plain = Safe.to_plain(si)
    slate = Input.slate_for(si_plain)

    si_for_stage =
      si_plain
      |> Map.put(:sense_candidates, slate)
      |> Map.delete(:candidates_by_token)
      |> Map.put_new(:trace, [])
      # ← inject fallback MWE if needed
      |> ensure_mwe_candidates(opts)

    si_opts =
      case Map.get(si_for_stage, :lifg_opts) do
        kw when is_list(kw) -> kw
        m when is_map(m) -> Map.to_list(m)
        _ -> []
      end

    eff_opts = Keyword.merge(si_opts, opts)
    margin_thr = Keyword.get(eff_opts, :margin_threshold, 0.15)

    scores_mode =
      Keyword.get(eff_opts, :scores, Application.get_env(:brain, :lifg_stage1_scores_mode, :all))

    min_margin =
      Keyword.get(eff_opts, :min_margin, Application.get_env(:brain, :lifg_min_margin, 0.05))

    stage_weights =
      lifg_weights()
      |> Map.merge(Map.new(Keyword.get(eff_opts, :weights, [])))

    result =
      Brain.LIFG.Stage1.run(
        si_for_stage,
        weights: stage_weights,
        scores: scores_mode,
        margin_threshold: margin_thr,
        chargram_event: [:brain, :lifg, :chargram_violation],
        boundary_event: [:brain, :lifg, :boundary_drop]
      )

    case result do
      {:ok, %{si: si_after0, choices: raw_choices, audit: _audit}} ->
        choices =
          raw_choices
          |> Enum.map(&Safe.to_plain/1)
          |> augment_choices(si_after0, min_margin)

        {boosts_out, inhibitions_out} =
          legacy_boosts_inhibitions(choices, margin_thr, scores_mode, eff_opts)

        evt = %{
          stage: :lifg_stage1,
          choices: choices,
          boosts: boosts_out,
          inhibitions: inhibitions_out,
          opts: Enum.into(eff_opts, %{})
        }

        trace = [evt | Map.get(si_after0, :trace) || []]
        Map.put(si_after0, :trace, trace)

      {:error, reason} ->
        Logger.error("LIFG Stage1 run failed: #{inspect(reason)}")

        evt = %{
          stage: :lifg_stage1,
          choices: [],
          boosts: [],
          inhibitions: [],
          opts: Enum.into(eff_opts, %{error: inspect(reason)})
        }

        trace = [evt | Map.get(si_for_stage, :trace) || []]

        si_for_stage
        |> Map.put(:lifg_error, inspect(reason))
        |> Map.put(:trace, trace)
    end
  end

  @doc "Pure, stateless Stage-1 with default opts."
  @spec disambiguate_stage1(map()) :: map()
  def disambiguate_stage1(%{} = si), do: disambiguate_stage1(si, [])

  # ── Math helpers kept public because other components use them ───────

  @doc "Stable softmax. Uniform if inputs are all equal. Sums to 1.0."
  @spec normalize_scores([number()]) :: [float()]
  def normalize_scores([]), do: []

  def normalize_scores(xs) when is_list(xs) do
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

  @doc "Cosine similarity (nil/zero-safe). Returns 0.0 if either vector has ~0 norm."
  @spec cosine([number()] | nil, [number()] | nil) :: float()
  def cosine(a, b) when is_list(a) and is_list(b) do
    {sa, sb} =
      {Enum.reduce(a, 0.0, fn x, acc -> acc + x * x end),
       Enum.reduce(b, 0.0, fn x, acc -> acc + x * x end)}

    na = :math.sqrt(max(sa, 0.0))
    nb = :math.sqrt(max(sb, 0.0))

    if na <= 1.0e-15 or nb <= 1.0e-15,
      do: 0.0,
      else: (Enum.zip(a, b) |> Enum.reduce(0.0, fn {x, y}, acc -> acc + x * y end)) / (na * nb)
  end

  def cosine(_, _), do: 0.0

  # --- Confidence helpers (pure) ---
  @doc "Return p(top1) from a LIFG choice (max over normalized scores)."
  def top1_prob(choice) when is_map(choice) do
    choice |> Safe.get(:scores, %{}) |> Map.values() |> Enum.max(fn -> 0.0 end)
  end

  def top1_prob(_), do: 0.0

  @doc """
  Low-confidence predicate used by pMTG gate.
  Defaults: tau_confident=0.20, p_min=0.65. Returns true when pMTG should fire.
  """
  def low_confidence?(choice, opts) when is_map(choice) do
    tau = Keyword.get(opts, :tau_confident, 0.20)
    p_min = Keyword.get(opts, :p_min, 0.65)
    m = Safe.get(choice, :margin, 0.0) || 0.0
    p1 = top1_prob(choice)
    alts? = (Safe.get(choice, :alt_ids, []) || []) != []
    m < tau or p1 < p_min or alts?
  end

  def low_confidence?(_choice, _opts), do: true

  # ── Full pipeline (optional ATL + ACC + pMTG) ────────────────────────

  @doc """
  Full LIFG pipeline:
    1) ATL finalize → slate (if available)
    2) Attach si.sense_candidates
    3) Stage-1 disambiguation (probabilities & margins)
    4) ACC assess (optional; computes :conflict and appends trace)
    5) pMTG consult (sync rerun or async boost/none), gated by ACC when available

  Returns {:ok, %{si, choices, slate}} or {:error, term()}.
  """
  @spec run(map(), keyword()) ::
          {:ok, %{si: map(), choices: list(), slate: map()}} | {:error, term()}
  def run(si, opts \\ []) when is_map(si) and is_list(opts) do
    try do
      # 1) ATL finalize (optional)
      {si1, slate} =
        if Code.ensure_loaded?(Brain.ATL) and function_exported?(Brain.ATL, :finalize, 2) do
          case Brain.ATL.finalize(si, opts) do
            {s, sl} when is_map(s) and is_map(sl) -> {s, sl}
            _ -> {si, %{}}
          end
        else
          {si, %{}}
        end

      # 2) Attach slate → sense_candidates (optional)
      si2 =
        if Code.ensure_loaded?(Brain.ATL) and
             function_exported?(Brain.ATL, :attach_sense_candidates, 3) do
          case Brain.ATL.attach_sense_candidates(
                 si1,
                 slate,
                 top_k: Keyword.get(opts, :top_k, 3),
                 margin_window: Keyword.get(opts, :margin_window, 0.05)
               ) do
            %{} = s -> s
            _ -> si1
          end
        else
          si1
        end

      # 2b) Ensure MWE candidates exist for MWE tokens that lack them
      si2a = ensure_mwe_candidates(si2, opts)

      scores_mode =
        Keyword.get(opts, :scores, Application.get_env(:brain, :lifg_stage1_scores_mode, :all))

      margin_thr = Keyword.get(opts, :margin_threshold, 0.15)

      min_margin =
        Keyword.get(opts, :min_margin, Application.get_env(:brain, :lifg_min_margin, 0.05))

      # 3) Stage-1 disambiguation
      case Brain.LIFG.Stage1.run(
             si2a,
             weights:
               Map.get(opts, :weights, %{
                 lex_fit: 0.5,
                 rel_prior: 0.25,
                 activation: 0.15,
                 intent_bias: 0.10
               }),
             scores: scores_mode,
             margin_threshold: margin_thr,
             chargram_event: [:brain, :lifg, :chargram_violation],
             boundary_event: [:brain, :lifg, :boundary_drop]
           ) do
        {:ok, %{si: si3, choices: raw_choices, audit: _a}} ->
          choices =
            raw_choices
            |> Enum.map(&Safe.to_plain/1)
            |> augment_choices(si3, min_margin)

          # 4) ACC (optional). If present, record conflict & gate pMTG by threshold.
          {si3a, acc_conflict, acc_present?} = maybe_acc_gate(si3, choices, opts)

          acc_conf_tau =
            Keyword.get(
              opts,
              :acc_conflict_tau,
              Application.get_env(:brain, :acc_conflict_tau, 0.50)
            )

          # 5) pMTG consult (respect ACC gate when ACC is present)
          pmtg_mode =
            Keyword.get(opts, :pmtg_mode, Application.get_env(:brain, :pmtg_mode, :boost))

          pmtg_apply? =
            Keyword.get(opts, :pmtg_apply?, true) and
              (not acc_present? or acc_conflict >= acc_conf_tau)

          {final_si, final_choices} =
            if Code.ensure_loaded?(Brain.PMTG) and pmtg_apply? do
              case pmtg_mode do
                :rerun ->
                  case Brain.PMTG.consult_sync(
                         choices,
                         Safe.get(si3a, :tokens, []),
                         already_needy: false,
                         margin_threshold: margin_thr,
                         limit: Keyword.get(opts, :limit, 5),
                         mode: :rerun,
                         rerun_only_if_hits: Keyword.get(opts, :rerun_only_if_hits, true),
                         rerun_weights_bump:
                           Keyword.get(opts, :rerun_weights_bump, %{
                             lex_fit: 0.05,
                             rel_prior: 0.05
                           })
                       ) do
                    {:ok, %{si: si_after, choices: merged}} -> {si_after, merged}
                    _ -> {si3a, choices}
                  end

                _other ->
                  _ =
                    Brain.PMTG.consult(
                      choices,
                      Safe.get(si3a, :tokens, []),
                      margin_threshold: margin_thr,
                      limit: Keyword.get(opts, :limit, 5),
                      mode: pmtg_mode
                    )

                  {si3a, choices}
              end
            else
              {si3a, choices}
            end

          {:ok, %{si: final_si, choices: final_choices, slate: slate}}

        {:error, reason} ->
          {:error, {:stage1, reason}}
      end
    rescue
      e ->
        Logger.error("LIFG full run failed: #{inspect(e)}")
        {:error, e}
    end
  end

  # ── weights (outer) ─────────────────────────────────────────────────

  @type lifg_outer_weights ::
          %{
            required(:lex) => float(),
            required(:sim) => float(),
            required(:rel) => float(),
            required(:prag) => float(),
            required(:act) => float(),
            optional(:prime) => float()
          }

  @doc "Default outer LIFG weights (lex/sim/rel/prag/act + optional :prime)."
  @spec default_weights() :: lifg_outer_weights()
  def default_weights, do: %{lex: 0.25, sim: 0.40, rel: 0.15, prag: 0.10, act: 0.10, prime: 0.0}

  # merge order: defaults <- env (lifg_stage1_weights) <- opts[:weights]
  defp lifg_weights do
    env = Application.get_env(:brain, :lifg_stage1_weights, %{})
    Map.merge(@default_weights, env || %{})
  end

  # Build the effective option set we report via `status/0`
  defp effective_opts(overrides) when is_map(overrides) do
    %{
      weights:
        lifg_weights()
        |> Map.merge(Map.new(Map.get(overrides, :weights, %{}))),
      scores:
        Map.get(overrides, :scores, Application.get_env(:brain, :lifg_stage1_scores_mode, :all)),
      margin_threshold: Map.get(overrides, :margin_threshold, 0.15),
      min_margin: Application.get_env(:brain, :lifg_min_margin, 0.05),
      pmtg_mode: Map.get(overrides, :pmtg_mode, Application.get_env(:brain, :pmtg_mode, :boost)),
      pmtg_window_keep: Application.get_env(:brain, :pmtg_window_keep, 50),
      acc_conflict_tau: Application.get_env(:brain, :acc_conflict_tau, 0.50),
      mwe_fallback: Application.get_env(:brain, :lifg_stage1_mwe_fallback, true)
    }
  end

  # ── private: legacy conversion helpers ───────────────────────────────

  defp legacy_boosts_inhibitions(choices, margin_thr, _scores_mode, eff_opts) do
    {boosts_maps, inhibitions_maps} =
      Enum.reduce(choices, {[], []}, fn ch, {boos, inhs} ->
        token_index = Safe.get(ch, :token_index, 0)
        chosen_id = Safe.get(ch, :chosen_id, nil)
        margin = (Safe.get(ch, :margin, 0.0) || 0.0) * 1.0
        scores = Safe.get(ch, :scores, %{})

        top_s =
          if is_map(scores) and map_size(scores) > 0 do
            Map.get(scores, chosen_id, 0.0) * 1.0
          else
            0.0
          end

        loser_ids =
          if is_map(scores) and map_size(scores) > 0 do
            scores |> Map.keys() |> Enum.reject(&(&1 == chosen_id))
          else
            Safe.get(ch, :alt_ids, [])
          end

        boost_here = %{
          token_index: token_index,
          id: chosen_id,
          amount: Float.round(max(margin, 0.0), 6)
        }

        inhibitions_here =
          Enum.map(loser_ids, fn lid ->
            gap = max(top_s - Map.get(scores, lid, 0.0), 0.0)
            amt = Float.round(max(margin_thr - gap, 0.0), 6)
            %{token_index: token_index, id: lid, amount: amt}
          end)

        {[boost_here | boos], inhibitions_here ++ inhs}
      end)

    pairs_mode? =
      Keyword.has_key?(eff_opts, :gate_into_wm) or
        Keyword.has_key?(eff_opts, :lifg_min_score) or
        Keyword.get(eff_opts, :emit_pairs, false)

    to_pair = fn
      %{id: id, amount: amt} -> {to_string(id), (amt || 0.0) * 1.0}
      {id, amt} -> {to_string(id), (amt || 0.0) * 1.0}
      _ -> nil
    end

    if pairs_mode? do
      {
        boosts_maps |> Enum.map(&to_pair.(&1)) |> Enum.reject(&is_nil/1) |> Enum.reverse(),
        inhibitions_maps |> Enum.map(&to_pair.(&1)) |> Enum.reject(&is_nil/1) |> Enum.reverse()
      }
    else
      {Enum.reverse(boosts_maps), Enum.reverse(inhibitions_maps)}
    end
  end

  # ── private: choice augmentation ─────────────────────────────────────

  defp augment_choices(raw_choices, si_after, min_margin) when is_list(raw_choices) do
    slate =
      Safe.get(si_after, :sense_candidates, %{}) || Safe.get(si_after, :candidates_by_token, %{}) ||
        %{}

    Enum.map(raw_choices, fn ch0 ->
      ch = Safe.to_plain(ch0)
      idx = Safe.get(ch, :token_index, 0)
      scores = Safe.get(ch, :scores, %{}) || %{}

      chosen_id =
        case Safe.get(ch, :chosen_id) do
          nil ->
            case scores |> Enum.max_by(fn {_id, s} -> s end, fn -> nil end) do
              {id, _} -> id
              _ -> nil
            end

          x ->
            x
        end

      alt_from_scores = if map_size(scores) > 0, do: Map.keys(scores), else: []

      slate_alts =
        case Map.get(slate, idx) do
          list when is_list(list) ->
            list
            |> Enum.map(fn c ->
              c = Safe.to_plain(c)
              Safe.get(c, :id) || Safe.get(c, :lemma) || Safe.get(c, :word)
            end)
            |> Enum.reject(&is_nil/1)

          _ ->
            []
        end

      chosen_id_s = if is_nil(chosen_id), do: nil, else: to_string(chosen_id)

      alt_ids =
        (Safe.get(ch, :alt_ids, []) ++ alt_from_scores ++ slate_alts)
        |> Enum.map(&to_string/1)
        |> Enum.reject(&(&1 in [nil, ""]))
        |> Enum.uniq()
        |> then(fn ids -> if chosen_id_s, do: ids -- [chosen_id_s], else: ids end)

      margin0 =
        case Safe.get(ch, :margin) do
          m when is_number(m) and m > 0.0 ->
            m

          _ ->
            vals = scores |> Map.values() |> Enum.sort(:desc)

            case vals do
              [a, b | _] -> a - b
              [_a] -> min_margin / 2
              _ -> 0.0
            end
        end

      margin = if margin0 <= 1.0e-9, do: min_margin, else: margin0

      ch
      |> Map.put(:chosen_id, chosen_id_s)
      |> Map.put(:alt_ids, alt_ids)
      |> Map.put(:margin, Float.round(margin * 1.0, 6))
    end)
  end

  # ── private: MWE fallback injection (no Core deps) ───────────────────

  # If a multiword token (n>1 or mw: true) has no compatible MWE senses,
  # synthesize a lightweight phrase candidate so Stage-1 can score it.
  defp ensure_mwe_candidates(%{tokens: tokens} = si, opts) when is_list(tokens) do
    enable? =
      Keyword.get(
        opts,
        :mwe_fallback,
        Application.get_env(:brain, :lifg_stage1_mwe_fallback, true)
      )

    unless enable?, do: si

    sc0 = Map.get(si, :sense_candidates, %{})

    {sc, emitted} =
      Enum.reduce(Enum.with_index(tokens), {sc0, 0}, fn {tok, idx}, {acc, n} ->
        token_n = Map.get(tok, :n, if(Map.get(tok, :mw, false), do: 2, else: 1))
        phrase = Map.get(tok, :phrase) || Map.get(tok, :lemma)
        mw? = Map.get(tok, :mw, token_n > 1)

        cond do
          not mw? or is_nil(phrase) ->
            {acc, n}

          has_compatible_mwe?(acc, idx) ->
            {acc, n}

          true ->
            # Heuristic score: modest, and bounded so real phrase cells win
            score_guess =
              acc
              |> unigram_neighbor_idxs(idx)
              |> Enum.map(fn j ->
                acc
                |> Map.get(j, [])
                |> Enum.map(&Map.get(&1, :score, 0.0))
                |> Enum.max(fn -> 0.0 end)
              end)
              |> case do
                [] -> 0.25
                xs -> Enum.sum(xs) / max(length(xs), 1)
              end
              |> min(0.45)

            candidate = %{
              id: "#{phrase}|phrase|fallback",
              lemma: phrase,
              norm: phrase,
              mw: true,
              pos: :phrase,
              rel_prior: 0.30,
              score: Float.round(score_guess, 4),
              source: :mwe_fallback
            }

            updated = Map.update(acc, idx, [candidate], fn lst -> [candidate | lst] end)

            safe_exec_telemetry([:brain, :pmtg, :mwe_fallback_emitted], %{count: 1}, %{
              token_index: idx,
              phrase: phrase,
              score: candidate.score
            })

            {updated, n + 1}
        end
      end)

    if emitted > 0, do: Map.put(si, :sense_candidates, sc), else: si
  end

  defp ensure_mwe_candidates(si, _opts), do: si

  defp has_compatible_mwe?(sc, idx) do
    sc
    |> Map.get(idx, [])
    |> Enum.any?(fn c ->
      norm = c[:norm] || c["norm"] || c[:lemma] || c["lemma"] || ""
      String.contains?(norm, " ")
    end)
  end

  # Neighbor heuristic: immediate neighbors as unigram companions
  defp unigram_neighbor_idxs(_sc, idx), do: [idx - 1, idx + 1] |> Enum.filter(&(&1 >= 0))

  # ── private: optional ACC hook ───────────────────────────────────────

  # Returns {si_with_trace, conflict_score, acc_present?}
  defp maybe_acc_gate(si, choices, opts) when is_map(si) and is_list(choices) do
    acc_present? =
      Code.ensure_loaded?(Brain.ACC) and
        (function_exported?(Brain.ACC, :assess, 3) or
           function_exported?(Brain.ACC, :assess, 2) or
           function_exported?(Brain.ACC, :score, 2))

    if acc_present? do
      now_ms = System.system_time(:millisecond)

      # IMPORTANT: use apply/3 so the compiler doesn't warn about undefined/private ACC functions.
      result =
        cond do
          function_exported?(Brain.ACC, :assess, 3) -> apply(Brain.ACC, :assess, [si, choices, opts])
          function_exported?(Brain.ACC, :assess, 2) -> apply(Brain.ACC, :assess, [si, choices])
          function_exported?(Brain.ACC, :score, 2)  -> apply(Brain.ACC, :score,  [si, choices])
          true -> :skip
        end

      case result do
        {:ok, %{si: si2, conflict: c} = payload} when is_number(c) ->
          ev = payload |> Map.drop([:si]) |> Map.merge(%{stage: :acc, ts_ms: now_ms})
          si3 = Map.update(si2, :trace, [ev], fn tr -> [ev | tr] end)
          safe_exec_telemetry([:brain, :acc, :assess], %{conflict: c}, %{})
          {si3, clamp01(c), true}

        {:ok, c} when is_number(c) ->
          ev = %{stage: :acc, conflict: c, ts_ms: now_ms}
          si2 = Map.update(si, :trace, [ev], fn tr -> [ev | tr] end)
          safe_exec_telemetry([:brain, :acc, :assess], %{conflict: c}, %{})
          {si2, clamp01(c), true}

        c when is_number(c) ->
          ev = %{stage: :acc, conflict: c, ts_ms: now_ms}
          si2 = Map.update(si, :trace, [ev], fn tr -> [ev | tr] end)
          safe_exec_telemetry([:brain, :acc, :assess], %{conflict: c}, %{})
          {si2, clamp01(c), true}

        _ ->
          {si, 1.0, false}
      end
    else
      {si, 1.0, false}
    end
  end

  defp clamp01(x) when is_number(x) do
    x = x * 1.0

    cond do
      x < 0.0 -> 0.0
      x > 1.0 -> 1.0
      true -> x
    end
  end

  defp clamp01(_), do: 0.0

  # Small local helper, mirroring other regions
  defp safe_exec_telemetry(event, measurements, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(event, measurements, meta)
    else
      :ok
    end
  end
end

