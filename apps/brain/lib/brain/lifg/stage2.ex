defmodule Brain.LIFG.Stage2 do
  @moduledoc """
  LIFG.Stage2 — sentence-level coherence and reanalysis.

  This module is the second stage in the LIFG pipeline. While Stage1 performs
  **per-token competitive sense selection**, Stage2 is responsible (long-term)
  for evaluating **sentence-level interpretations** and, when appropriate,
  reanalysing ambiguous tokens to prefer a globally coherent reading.

  Current status (Phase 1 — observational only)
  ---------------------------------------------
  Stage2 is currently *read-only* with respect to sense choices:

    * `run/2` never raises.
    * It does **not** flip any Stage1 winners or mutate tokens.
    * When a Stage1 event is present, it:
        - computes sentence-level conflict / margin metrics,
        - builds a Stage2 event,
        - pushes that event onto `si.trace` (head-first),
        - attaches a lightweight `:lifg_stage2` summary on `si`,
        - and returns `{:ok, %{si: si_after, event: stage2_event}}`.
    * When no Stage1 event is found, it returns
        - `{:skip, %{si: si, reason: :no_stage1_event}}`.

  This makes Stage2 safe to call from the pipeline: it surfaces useful metrics
  for BrainLive / ACC / PMTG without changing the underlying disambiguation.

  Forward-looking contract
  ------------------------
  In later phases, `run/2` may:

    1. Construct a bounded set of sentence-level interpretations using a beam
       search over per-token candidates.
    2. Score each interpretation using a global scoring mix:
         * lexical fit (Stage1 probabilities),
         * schema/coherence (roles, POS, pattern roles),
         * episode support (Hippocampus),
         * intent alignment.
    3. Select the best interpretation and optionally flip low-margin tokens
       where the global gain is sufficient.
    4. Attach:
         * final Stage2 choices (per token),
         * Stage2 meta (scores, flips, conflict metrics),
         * a sentence-level structure (e.g. `:sentence_graph`),
         * and a Stage2 trace event onto `si.trace`.

  Return shape
  ------------
  `run/2` can return one of:

    * `{:ok, %{si: si_after, event: stage2_event}}`
    * `{:skip, %{si: si, reason: reason_atom}}`
    * `{:error, reason}`

  Callers should treat Stage2 as optional and **fail-open**:
  if it returns `{:skip, ...}` or `{:error, ...}`, they can
  continue with the Stage1 result unchanged.

  This module is intentionally pure (no GenServer) and lives entirely under
  the `Brain.LIFG` namespace to keep region responsibilities local and avoid
  cross-app coupling.
  """

  require Logger

  @type si :: map()
  @type choice :: map()

  @type opts :: [
          max_interpretations: pos_integer(),
          max_alternatives_per_token: pos_integer(),
          reanalysis_margin_threshold: float(),
          global_weight_lex: float(),
          global_weight_schema: float(),
          global_weight_episode: float(),
          global_weight_intent: float()
        ]

  @type run_ok :: {:ok, %{si: si(), event: map()}}
  @type run_skip :: {:skip, %{si: si(), reason: atom()}}
  @type run_error :: {:error, term()}
  @type run_result :: run_ok() | run_skip() | run_error()

  @default_opts [
    max_interpretations: 8,
    max_alternatives_per_token: 2,
    reanalysis_margin_threshold: 0.10,
    global_weight_lex: 0.6,
    global_weight_schema: 0.3,
    global_weight_episode: 0.1,
    global_weight_intent: 0.0
  ]

  @doc """
  Run LIFG Stage2 on the given semantic input `si`.

  Phase 1 behavior (observational only):

    * Merges `opts` with a default option set.
    * Looks for the most recent Stage1 event (`%{stage: :lifg_stage1, ...}`) in
      `si.trace`.
    * If no Stage1 event is found:
        - logs a debug message,
        - returns `{:skip, %{si: si, reason: :no_stage1_event}}`.
    * If a Stage1 event is found:
        - computes conflict / margin metrics over Stage1 choices,
        - constructs a Stage2 event (no flips, no alterations to choices),
        - attaches that event to `si.trace` (head-first),
        - attaches a small `:lifg_stage2` summary map on `si`,
        - returns `{:ok, %{si: si_after, event: stage2_event}}`.

  This function does **not** alter token winners, sense ids, or margins. It is
  safe to treat Stage1 as the sole decider of per-token sense selection for now.
  """
  @spec run(si(), opts()) :: run_result()
  def run(%{} = si, opts \\ []) do
    eff_opts = merge_opts(opts)

    case fetch_stage1_event(si) do
      {:ok, stage1_ev} ->
        conflict = compute_conflict_metrics(stage1_ev, eff_opts)

        event = %{
          stage: :lifg_stage2,
          decisions: [],
          flips: [],
          interpretations: [],
          conflict: conflict,
          stage1_ref: %{
            margin_threshold: conflict.stage1_margin_threshold,
            scores_mode: conflict.stage1_scores_mode,
            token_count: conflict.token_count
          },
          meta: %{
            reanalysis_triggered?: false,
            mode: :observational
          }
        }

        si_after =
          si
          |> put_stage2_trace(event)
          |> Map.put(:lifg_stage2, %{conflict: conflict, opts: eff_opts})

        {:ok, %{si: si_after, event: event}}

      :error ->
        Logger.debug(fn -> "[LIFG.Stage2] skipped (no Stage1 event)" end)
        {:skip, %{si: si, reason: :no_stage1_event}}
    end
  rescue
    exception ->
      Logger.error(fn ->
        "[LIFG.Stage2] error: " <>
          Exception.format(:error, exception, __STACKTRACE__)
      end)

      {:error, exception}
  end

  # ────────────────────── internal helpers ──────────────────────

  @spec merge_opts(opts()) :: opts()
  defp merge_opts(opts) when is_list(opts) do
    Keyword.merge(@default_opts, opts)
  end

  @spec fetch_stage1_event(si()) :: {:ok, map()} | :error
  defp fetch_stage1_event(%{} = si) do
    trace =
      case Map.fetch(si, :trace) do
        {:ok, t} when is_list(t) -> t
        _ -> []
      end

    case Enum.find(trace, fn
           %{stage: :lifg_stage1} -> true
           _ -> false
         end) do
      nil -> :error
      ev -> {:ok, ev}
    end
  end

  @spec compute_conflict_metrics(map(), opts()) :: map()
  defp compute_conflict_metrics(stage1_ev, eff_opts) do
    choices = Map.get(stage1_ev, :choices, [])

    margins =
      Enum.map(choices, fn ch ->
        Map.get(ch, :margin) ||
          Map.get(ch, "margin") ||
          0.0
      end)

    {avg_margin, min_margin, max_margin} =
      case margins do
        [] ->
          {0.0, 0.0, 0.0}

        _ ->
          sum = Enum.sum(margins)
          count = length(margins)
          avg = sum / count
          {avg, Enum.min(margins), Enum.max(margins)}
      end

    token_count = length(choices)

    reanalysis_thr =
      Keyword.get(eff_opts, :reanalysis_margin_threshold, 0.10)

    needy_count =
      Enum.count(margins, fn m -> m < reanalysis_thr end)

    stage1_opts = Map.get(stage1_ev, :opts, %{})

    stage1_margin_thr =
      Map.get(stage1_opts, :margin_threshold) ||
        Map.get(stage1_opts, "margin_threshold") ||
        nil

    stage1_scores_mode =
      Map.get(stage1_opts, :scores) ||
        Map.get(stage1_opts, "scores") ||
        nil

    guards = Map.get(stage1_ev, :guards, %{})
    audit = Map.get(stage1_ev, :audit, %{})

    weak_decisions =
      Map.get(audit, :weak_decisions) ||
        Map.get(audit, "weak_decisions") ||
        0

    boundary_drops =
      Map.get(audit, :boundary_drops) ||
        Map.get(audit, "boundary_drops") ||
        0

    missing_candidates =
      Map.get(guards, :missing_candidates) ||
        Map.get(guards, "missing_candidates") ||
        Map.get(audit, :missing_candidates) ||
        Map.get(audit, "missing_candidates") ||
        0

    conflict_score =
      margins
      |> case do
        [] -> 0.0
        _ -> 1.0 - avg_margin
      end
      |> clamp(0.0, 1.0)

    %{
      avg_margin: avg_margin,
      min_margin: min_margin,
      max_margin: max_margin,
      token_count: token_count,
      needy_count: needy_count,
      needy_threshold: reanalysis_thr,
      stage1_margin_threshold: stage1_margin_thr,
      stage1_scores_mode: stage1_scores_mode,
      weak_decisions: weak_decisions,
      boundary_drops: boundary_drops,
      missing_candidates: missing_candidates,
      conflict_score: conflict_score
    }
  end

  @spec put_stage2_trace(si(), map()) :: si()
  defp put_stage2_trace(%{} = si, event) do
    Map.update(si, :trace, [event], fn
      list when is_list(list) -> [event | list]
      _ -> [event]
    end)
  end

  @spec clamp(number(), number(), number()) :: number()
  defp clamp(v, min, max) do
    v
    |> max(min)
    |> min(max)
  end
end

