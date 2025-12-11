defmodule Brain.LIFG.Stage2 do
  @moduledoc """
  LIFG.Stage2 — sentence-level coherence and reanalysis (scaffold).

  This module is the second stage in the LIFG pipeline. While Stage1 performs
  **per-token competitive sense selection**, Stage2 will be responsible for
  evaluating **sentence-level interpretations** and, when appropriate,
  reanalysing ambiguous tokens to prefer a globally coherent reading.

  Current status (scaffold)
  -------------------------
  This is an inert, non-invasive stub:

    * `run/2` never raises and never mutates the input `si`.
    * If there is **no Stage1 event** in `si.trace`, it returns
      `{:skip, %{si: si, reason: :no_stage1_event}}`.
    * If there **is** a Stage1 event, it returns
      `{:skip, %{si: si, reason: :not_enabled}}`.
    * No telemetry is emitted yet.
    * No wiring has been added from `Brain.LIFG` into Stage2.

  This allows the rest of the system (and tests) to compile and call into
  Stage2 safely, while we iteratively implement:

    * beam search over per-token candidates,
    * global scoring (lexical fit, schema roles, episodes, intent),
    * low-margin reanalysis and flips,
    * a Stage2 trace event and sentence-level structure attached to `si`.

  Intended contract (forward-looking)
  -----------------------------------
  Once implemented, the `run/2` function is expected to:

    1. Read the last Stage1 event from `si.trace` (where `stage == :lifg_stage1`).
    2. Construct a small set of sentence-level interpretations using a bounded
       beam search over per-token candidates.
    3. Score each interpretation using a global scoring mix:
         * lexical fit (Stage1 probabilities),
         * schema/coherence (roles, POS, pattern roles),
         * episode support (Hippocampus),
         * intent alignment.
    4. Select the best interpretation and optionally flip low-margin tokens
       where the global gain is sufficient.
    5. Attach:
         * final Stage2 choices (per token),
         * Stage2 meta (scores, flips, conflict metrics),
         * a sentence-level structure (e.g. `:sentence_graph`),
         * and a Stage2 trace event onto `si.trace`.

  Return shape
  ------------
  When fully implemented, `run/2` will return one of:

    * `{:ok, %{si: si_after, event: stage2_event}}`
    * `{:skip, %{si: si, reason: reason_atom}}`
    * `{:error, reason}`

  In the scaffold:

    * If there is **no Stage1 event** in `si.trace` (including missing/empty trace),
      we return `{:skip, %{si: si, reason: :no_stage1_event}}`.
    * If a Stage1 event is present, we return
      `{:skip, %{si: si, reason: :not_enabled}}`.
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

  Scaffold behavior:

    * Merges `opts` with a default option set.
    * Peeks at the latest Stage1 event in `si.trace` (if any).
    * If no Stage1 event is found, returns:
        `{:skip, %{si: si, reason: :no_stage1_event}}`.
    * If a Stage1 event is found, returns:
        `{:skip, %{si: si, reason: :not_enabled}}`.

  Once Stage2 is implemented, this function will:

    * inspect the last Stage1 event from `si.trace`,
    * construct a bounded set of sentence-level interpretations,
    * select the best interpretation via a global scoring mix,
    * optionally flip low-margin token senses,
    * and return `{:ok, %{si: si_after, event: stage2_event}}`.
  """
  @spec run(si(), opts()) :: run_result()
  def run(%{} = si, opts \\ []) do
    _eff_opts = merge_opts(opts)

    case stage1_snapshot(si) do
      {:ok, %{si: si1, event: ev}} ->
        choices_len =
          ev
          |> Map.get(:choices, [])
          |> case do
            list when is_list(list) -> length(list)
            _ -> 0
          end

        Logger.debug(fn ->
          "[LIFG.Stage2] scaffold not_enabled (stage1_choices=#{choices_len})"
        end)

        # Stage1 has run; Stage2 exists but is not active yet.
        {:skip, %{si: si1, reason: :not_enabled}}

      {:skip, %{si: si1, reason: :no_stage1_event}} = skip ->
        # Stage1 has not run (no event in trace) — surface this explicitly.
        Logger.debug(fn ->
          "[LIFG.Stage2] skip (reason=:no_stage1_event, sentence=#{inspect(si1[:sentence])})"
        end)

        skip

      {:error, reason} ->
        Logger.error("LIFG.Stage2 run/2 error in scaffold: #{inspect(reason)}")
        {:error, reason}
    end
  end

  # ────────────────────── internal helpers (scaffold) ──────────────────────

  @spec merge_opts(opts()) :: opts()
  defp merge_opts(opts) when is_list(opts) do
    Keyword.merge(@default_opts, opts)
  end

  @doc false
  @spec stage1_snapshot(si()) ::
          {:ok, %{si: si(), event: map()}}
          | {:skip, %{si: si(), reason: :no_stage1_event}}
          | {:error, term()}
  defp stage1_snapshot(%{} = si) do
    trace = Map.get(si, :trace, nil)

    cond do
      is_list(trace) ->
        # Find the most recent Stage1 event (we assume head-first trace).
        case Enum.find(trace, fn ev ->
               is_map(ev) and Map.get(ev, :stage) == :lifg_stage1
             end) do
          nil ->
            {:skip, %{si: si, reason: :no_stage1_event}}

          ev ->
            {:ok, %{si: si, event: ev}}
        end

      trace == nil ->
        # No trace at all — no Stage1 event.
        {:skip, %{si: si, reason: :no_stage1_event}}

      true ->
        # Unexpected shape for trace; treat as “no Stage1 event” but be defensive.
        {:skip, %{si: Map.put(si, :trace, List.wrap(trace)), reason: :no_stage1_event}}
    end
  rescue
    e ->
      {:error, e}
  end
end

