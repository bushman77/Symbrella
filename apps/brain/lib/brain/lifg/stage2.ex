defmodule Brain.LIFG.Stage2 do
  @moduledoc """
  LIFG.Stage2 — sentence-level coherence and reanalysis (scaffold).

  Stage2 is the second stage in the LIFG pipeline. Stage1 performs **per-token**
  competitive sense selection; Stage2 is intended to evaluate **sentence-level**
  interpretations and (optionally) reanalyse low-margin tokens for global coherence.

  Current status (scaffold)
  -------------------------
  This module remains inert by default:

    * `run/2` never raises and (by default) does not mutate the input `si`.
    * If there is **no Stage1 event** in `si.trace`, it returns
      `{:skip, %{si: si, reason: :no_stage1_event}}`.
    * If there **is** a Stage1 event and `enabled?: false` (default), it returns
      `{:skip, %{si: si, reason: :not_enabled, ...}}`.

  Integration seam (P-118)
  ------------------------
  To make Stage2 observable and easy to wire-in while staying safe:

    * Emits best-effort telemetry on `[:brain, :lifg, :stage2]` with `phase: :start|:stop`.
    * Always builds a `:stage2_event` map in the meta for logging/recording.
    * Optionally can attach its event to `si.trace` when explicitly opted-in via `attach_trace?: true`.
      (Default is `false` to preserve inert behavior.)

  “Enabled” behavior (P-119 warning cleanup)
  ------------------------------------------
  When `enabled?: true`, Stage2 returns `{:ok, %{si: si_after, event: stage2_event, baseline: baseline}}`.
  In scaffold mode, this still does not perform reanalysis; it simply makes the `:ok` return path real
  (and therefore keeps upstream matching branches type-correct).

  Forward-looking contract
  ------------------------
  Once implemented, Stage2 will:
    1) read last Stage1 event from `si.trace`,
    2) generate bounded interpretations (beam search),
    3) score globally (lex/schema/episode/intent),
    4) optionally flip low-margin tokens,
    5) attach Stage2 artifacts to `si` and `si.trace`.
  """

  require Logger

  @type si :: map()
  @type choice :: map()

  @type opts :: [
          enabled?: boolean(),
          attach_trace?: boolean(),
          telemetry?: boolean(),
          max_interpretations: pos_integer(),
          max_alternatives_per_token: pos_integer(),
          reanalysis_margin_threshold: float(),
          global_weight_lex: float(),
          global_weight_schema: float(),
          global_weight_episode: float(),
          global_weight_intent: float()
        ]

  @type run_ok :: {:ok, %{required(:si) => si(), required(:event) => map(), optional(:baseline) => map()}}
  @type run_skip :: {:skip, map()}
  @type run_error :: {:error, term()}
  @type run_result :: run_ok() | run_skip() | run_error()

  @telemetry_event [:brain, :lifg, :stage2]

  @default_opts [
    enabled?: false,
    attach_trace?: false,
    telemetry?: true,
    max_interpretations: 8,
    max_alternatives_per_token: 2,
    reanalysis_margin_threshold: 0.10,
    global_weight_lex: 0.6,
    global_weight_schema: 0.3,
    global_weight_episode: 0.1,
    global_weight_intent: 0.0
  ]

  @type slot :: %{
          token_index: non_neg_integer(),
          token: map() | nil,
          winner: choice(),
          margin: float(),
          p_top1: float(),
          alt_ids: [term()]
        }

  @doc """
  Run LIFG Stage2 on semantic input `si`.

  Behavior (scaffold):
    * If no Stage1 event exists in `si.trace` → `{:skip, %{si: si, reason: :no_stage1_event}}`
    * If Stage1 event exists and `enabled?: false` → `{:skip, %{... reason: :not_enabled, stage2_event: ..., baseline: ...}}`
    * If Stage1 event exists and `enabled?: true` → `{:ok, %{si: si_after, event: stage2_event, baseline: baseline}}`

  Options:
    * `enabled?:` (default false) — when true, returns `{:ok, ...}` but still does no reanalysis in scaffold mode.
    * `attach_trace?:` (default false) — if true, Stage2 event is prepended to `si.trace`.
    * `telemetry?:` (default true) — emit `[:brain, :lifg, :stage2]` start/stop events.
  """
  @spec run(si(), opts()) :: run_result()
  def run(%{} = si, opts \\ []) do
    eff_opts = merge_opts(opts)

    stage2_id = unique_stage2_id()
    t0_ms = System.monotonic_time(:millisecond)

    safe_exec_telemetry(
      eff_opts,
      @telemetry_event,
      %{phase: :start, v: 2, stage2_id: stage2_id},
      %{enabled?: !!eff_opts[:enabled?]}
    )

    res =
      try do
        case stage1_snapshot(si) do
          {:skip, %{si: si1, reason: :no_stage1_event}} ->
            Logger.debug(fn ->
              "[LIFG.Stage2] skip (reason=:no_stage1_event, sentence=#{inspect(si1[:sentence])})"
            end)

            {:skip, %{si: si1, reason: :no_stage1_event, stage: :lifg_stage2, stage2_id: stage2_id}}

          {:ok, %{si: si1, event: ev1}} ->
            slots = slots_from_stage1(si1, ev1)
            baseline = baseline_from_slots(slots, eff_opts)

            stage2_event =
              build_stage2_event(stage2_id, si1, eff_opts, %{
                mode: :baseline,
                reason: :not_enabled,
                slots: length(slots),
                score_lex: baseline.baseline.score_lex,
                score_total: baseline.baseline.score_total
              })

            Logger.debug(fn ->
              "[LIFG.Stage2] scaffold not_enabled " <>
                "mode=:baseline slots=#{length(slots)} " <>
                "score_lex=#{Float.round(baseline.baseline.score_lex, 4)}"
            end)

            si_out =
              if eff_opts[:attach_trace?] do
                Map.update(si1, :trace, [stage2_event], fn tr -> [stage2_event | List.wrap(tr)] end)
              else
                si1
              end

            if eff_opts[:enabled?] do
              {:ok, %{si: si_out, event: stage2_event, baseline: baseline}}
            else
              {:skip,
               %{
                 si: si_out,
                 reason: :not_enabled,
                 stage: :lifg_stage2,
                 stage2_id: stage2_id,
                 stage2_event: stage2_event,
                 baseline: baseline
               }}
            end

          {:error, reason} ->
            Logger.error("LIFG.Stage2 run/2 error in scaffold: #{inspect(reason)}")
            {:error, reason}
        end
      rescue
        e ->
          Logger.error("LIFG.Stage2 exception in scaffold: #{inspect(e)}")
          {:error, {:exception, e}}
      catch
        kind, reason ->
          Logger.error("LIFG.Stage2 throw/exit in scaffold: #{inspect({kind, reason})}")
          {:error, {kind, reason}}
      end

    dur_ms = max(System.monotonic_time(:millisecond) - t0_ms, 0)

    safe_exec_telemetry(
      eff_opts,
      @telemetry_event,
      %{phase: :stop, v: 2, stage2_id: stage2_id, duration_ms: dur_ms},
      %{result: result_tag(res)}
    )

    res
  end

  # ────────────────────── opts ──────────────────────

  @spec merge_opts(opts()) :: opts()
  defp merge_opts(opts) when is_list(opts), do: Keyword.merge(@default_opts, opts)
  defp merge_opts(_), do: @default_opts

  # ────────────────────── Stage1 snapshot ──────────────────────

  @doc false
  @spec stage1_snapshot(si()) ::
          {:ok, %{si: si(), event: map()}}
          | {:skip, %{si: si(), reason: :no_stage1_event}}
          | {:error, term()}
  defp stage1_snapshot(%{} = si) do
    trace = Map.get(si, :trace, nil)

    cond do
      is_list(trace) ->
        case Enum.find(trace, fn ev ->
               is_map(ev) and Map.get(ev, :stage) == :lifg_stage1
             end) do
          nil -> {:skip, %{si: si, reason: :no_stage1_event}}
          ev -> {:ok, %{si: si, event: ev}}
        end

      trace == nil ->
        {:skip, %{si: si, reason: :no_stage1_event}}

      true ->
        # Unexpected trace shape: treat as no Stage1 event, but DO NOT mutate si.
        {:skip, %{si: si, reason: :no_stage1_event}}
    end
  rescue
    e -> {:error, e}
  end

  # ────────────────────── baseline builders ──────────────────────

  @doc false
  @spec slots_from_stage1(si(), map()) :: [slot()]
  defp slots_from_stage1(%{} = si, %{} = stage1_event) do
    tokens =
      si
      |> Map.get(:tokens, [])
      |> List.wrap()

    choices =
      stage1_event
      |> Map.get(:choices, [])
      |> List.wrap()

    choices
    |> Enum.reduce(%{}, fn ch, acc ->
      case extract_choice_index(ch) do
        idx when is_integer(idx) and idx >= 0 ->
          token = token_at_index(tokens, idx)

          scores =
            ch
            |> Map.get(:scores, %{})
            |> case do
              %{} = m -> m
              _ -> %{}
            end

          p_top1 =
            scores
            |> Map.values()
            |> case do
              [] -> 0.0
              vals -> Enum.max(vals) * 1.0
            end

          margin =
            (Map.get(ch, :margin) || Map.get(ch, "margin") || Map.get(ch, :prob_margin) ||
               Map.get(ch, "prob_margin") || 0.0)
            |> Kernel.*(1.0)

          alt_ids =
            ch
            |> Map.get(:alt_ids, Map.get(ch, "alt_ids", []))
            |> List.wrap()

          slot = %{
            token_index: idx,
            token: token,
            winner: ch,
            margin: margin,
            p_top1: p_top1,
            alt_ids: alt_ids
          }

          Map.put(acc, idx, slot)

        _ ->
          acc
      end
    end)
    |> Enum.map(fn {_idx, slot} -> slot end)
    |> Enum.sort_by(& &1.token_index)
  end

  @doc false
  @spec baseline_from_slots([slot()], opts()) :: map()
  defp baseline_from_slots(slots, eff_opts) when is_list(slots) and is_list(eff_opts) do
    choices = Enum.map(slots, & &1.winner)

    score_lex =
      slots
      |> Enum.map(& &1.p_top1)
      |> avg()

    %{
      mode: :baseline,
      opts: Map.new(eff_opts),
      baseline: %{
        choices: choices,
        score_lex: score_lex,
        score_total: score_lex,
        flips: []
      },
      reason: :not_enabled,
      stage: :lifg_stage2
    }
  end

  defp build_stage2_event(stage2_id, si, eff_opts, stats) when is_map(stats) do
    %{
      stage: :lifg_stage2,
      ts_ms: System.system_time(:millisecond),
      v: 2,
      stage2_id: stage2_id,
      enabled?: !!eff_opts[:enabled?],
      mode: stats.mode,
      reason: stats.reason,
      slots: stats.slots,
      score_lex: stats.score_lex * 1.0,
      score_total: stats.score_total * 1.0,
      sentence: si[:sentence],
      opts: Map.new(eff_opts)
    }
  end

  # ────────────────────── micro helpers ──────────────────────

  defp extract_choice_index(ch) when is_map(ch) do
    normalize_idx(
      Map.get(ch, :token_index) ||
        Map.get(ch, "token_index") ||
        Map.get(ch, :index) ||
        Map.get(ch, "index")
    )
  end

  defp extract_choice_index(_), do: nil

  defp token_at_index(tokens, idx) when is_integer(idx) and idx >= 0 do
    Enum.find(tokens, fn tok ->
      v = Map.get(tok, :index) || Map.get(tok, "index")
      normalize_idx(v) == idx
    end)
  end

  defp token_at_index(_tokens, _idx), do: nil

  defp avg([]), do: 0.0

  defp avg(xs) when is_list(xs) do
    n = length(xs)
    if n == 0, do: 0.0, else: Enum.sum(xs) * 1.0 / n
  end

  defp normalize_idx(i) when is_integer(i) and i >= 0, do: i

  defp normalize_idx(b) when is_binary(b) do
    case Integer.parse(b) do
      {n, _} when n >= 0 -> n
      _ -> nil
    end
  end

  defp normalize_idx(_), do: nil

  defp unique_stage2_id do
    Integer.to_string(:erlang.unique_integer([:positive])) <>
      "-" <> Integer.to_string(System.system_time(:microsecond))
  end

  defp result_tag({:ok, _}), do: :ok
  defp result_tag({:skip, _}), do: :skip
  defp result_tag({:error, _}), do: :error
  defp result_tag(_), do: :unknown

  defp safe_exec_telemetry(eff_opts, event, measurements, meta) do
    if Keyword.get(eff_opts, :telemetry?, true) and Code.ensure_loaded?(:telemetry) and
         function_exported?(:telemetry, :execute, 3) do
      try do
        :telemetry.execute(event, measurements, meta)
      rescue
        _ -> :ok
      catch
        _, _ -> :ok
      end
    else
      :ok
    end
  end
end

