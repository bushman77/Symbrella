# apps/brain/lib/brain/lifg/fallback_rerun.ex
defmodule Brain.LIFG.FallbackRerun do
  @moduledoc """
  P-201 fallback rerun for cases where a token's winner is a `|phrase|fallback`
  and WM disallows fallbacks.

  Responsibilities
  • Detect fallback winners.
  • Absorb unigram children from `active_cells` into affected MWE buckets.
  • Rerun Stage-1 with unchanged knobs.
  • Emit trace + telemetry.
  • Return the updated `si` and **augmented** choices (margin/alt_ids normalized).

  Option A semantics (via `Brain.LIFG.Choices`):
  • `:alt_ids` are *scored competitors only* (keys from `:scores`, excluding `:chosen_id`)
  • `:slate_alt_ids` are slate-only bucket candidates not present in `:scores`
  """

  alias Brain.Utils.Safe
  alias Brain.LIFG.Choices

  @rerun_event [:brain, :lifg, :rerun]

  @doc """
  Maybe rerun Stage-1, returning `{si_out, choices_out}`.

  Keeps the original contract used by `Brain.LIFG`: if a rerun is not needed,
  returns `{si, choices}` as-is; otherwise re-scores and returns updated,
  **augmented** choices.

  Options (subset):
    * `:rerun_on_fallback?` (default: true)
    * `:allow_fallback_into_wm?` (default: false; overridden by `si.wm_cfg`)
    * `:min_margin` (default: 0.05)
    * `:chargram_event` (default: [:brain, :lifg, :chargram_violation])
    * `:boundary_event` (default: [:brain, :lifg, :boundary_drop])
  """
  @spec maybe_rerun(map(), list(), map(), atom(), number(), keyword()) :: {map(), list()}
  def maybe_rerun(si, choices0, weights_for_stage1, scores_mode, margin_thr, opts) do
    rerun_on_fallback? = Keyword.get(opts, :rerun_on_fallback?, true)

    {allow_fallback_into_wm?, wm_present?} =
      case si do
        %{wm_cfg: %{allow_fallback_into_wm?: v}} -> {!!v, true}
        _ -> {Keyword.get(opts, :allow_fallback_into_wm?, false), false}
      end

    idxs = fallback_winner_indices(choices0)

    needs_rerun? =
      rerun_on_fallback? and idxs != [] and not allow_fallback_into_wm?

    if not needs_rerun? do
      {si, choices0}
    else
      rerun_id = unique_rerun_id()

      # Heads are debug-only; keep best-effort and never crash rerun logic.
      heads_by_idx = heads_for_indices(si, idxs)

      # absorb unigrams from active_cells → affected MWE buckets only
      si_absorb =
        Brain.LIFG.MWE.absorb_unigrams_into_mwe(
          si,
          Keyword.put(opts, :absorb_unigrams_into_mwe?, true)
        )

      # trace + telemetry (start)
      ts_ms = System.system_time(:millisecond)
      t0_ms = System.monotonic_time(:millisecond)

      ev = %{
        stage: :lifg_rerun,
        reason: :mwe_fallback,
        ts_ms: ts_ms,
        rerun_id: rerun_id,
        token_indices: idxs,
        heads_by_idx: heads_by_idx,
        wm_cfg_seen?: wm_present?,
        allow_fallback_into_wm?: allow_fallback_into_wm?,
        scores_mode: scores_mode,
        margin_threshold: margin_thr * 1.0
      }

      si_trace = Map.update(si_absorb, :trace, [ev], fn tr -> [ev | tr] end)

      safe_exec_telemetry(
        @rerun_event,
        %{count: length(idxs)},
        %{
          phase: :start,
          v: 2,
          rerun_id: rerun_id,
          reason: :mwe_fallback,
          token_indices: idxs,
          heads_by_idx: heads_by_idx,
          wm_cfg_seen?: wm_present?,
          allow_fallback_into_wm?: allow_fallback_into_wm?,
          scores_mode: scores_mode,
          margin_threshold: margin_thr * 1.0
        }
      )

      # re-run Stage-1 with same knobs
      res =
        try do
          Brain.LIFG.Stage1.run(
            si_trace,
            weights: weights_for_stage1,
            scores: scores_mode,
            margin_threshold: margin_thr,
            chargram_event:
              Keyword.get(opts, :chargram_event, [:brain, :lifg, :chargram_violation]),
            boundary_event: Keyword.get(opts, :boundary_event, [:brain, :lifg, :boundary_drop])
          )
        rescue
          e -> {:error, {:exception, e}}
        catch
          kind, reason -> {:error, {kind, reason}}
        end

      t1_ms = System.monotonic_time(:millisecond)
      dur_ms = max(t1_ms - t0_ms, 0)

      case res do
        {:ok, %{si: si_rerun, choices: raw2}} ->
          min_margin = Keyword.get(opts, :min_margin, 0.05)

          # IMPORTANT: use the canonical augmenter so Option A separation holds:
          # - alt_ids = scored competitors only
          # - slate_alt_ids = slate-only bucket candidates
          choices2 = Choices.augment(raw2, si_rerun, min_margin)

          safe_exec_telemetry(
            @rerun_event,
            %{count: length(idxs), duration_ms: dur_ms},
            %{
              phase: :stop,
              v: 2,
              ok?: true,
              rerun_id: rerun_id,
              reason: :mwe_fallback,
              token_indices: idxs,
              scores_mode: scores_mode,
              margin_threshold: margin_thr * 1.0,
              min_margin: min_margin * 1.0
            }
          )

          {si_rerun, choices2}

        other ->
          # If rerun fails for any reason, fall back to original (but keep trace/telemetry we already emitted).
          safe_exec_telemetry(
            @rerun_event,
            %{count: length(idxs), duration_ms: dur_ms},
            %{
              phase: :stop,
              v: 2,
              ok?: false,
              rerun_id: rerun_id,
              reason: :mwe_fallback,
              token_indices: idxs,
              error: Safe.to_plain(other)
            }
          )

          {si_trace, choices0}
      end
    end
  end

  # ---------- detection ----------

  @spec fallback_winner_indices(list()) :: [non_neg_integer()]
  def fallback_winner_indices(choices) do
    choices
    |> Enum.reduce([], fn ch, acc ->
      id = to_string(Safe.get(ch, :chosen_id, Safe.get(ch, :id, "")))
      idx = Safe.get(ch, :token_index, Safe.get(ch, :index, 0))

      if is_binary(id) and String.ends_with?(id, "|phrase|fallback"),
        do: [idx | acc],
        else: acc
    end)
    |> Enum.reverse()
    |> Enum.uniq()
    |> Enum.map(&normalize_idx/1)
    |> Enum.uniq()
    |> Enum.sort()
  end

  # ---------- debug helpers ----------

  # Build a debug map of heads per token index using child unigrams inside each MWE span.
  # Best-effort only (instrumentation); never crashes rerun.
  defp heads_for_indices(si, idxs) do
    toks = Safe.get(si, :tokens, [])

    Enum.reduce(idxs, %{}, fn idx0, acc ->
      idx = normalize_idx(idx0)

      heads =
        case token_by_index(toks, idx) do
          nil ->
            []

          tok ->
            span = Safe.get(tok, :span)

            toks
            |> Enum.reject(fn t ->
              normalize_idx(Safe.get(t, :index, Safe.get(t, :token_index, 0))) == idx
            end)
            |> Enum.filter(fn t ->
              Safe.get(t, :n, 1) == 1 and inside?(Safe.get(t, :span), span)
            end)
            |> Enum.map(fn t ->
              Safe.get(t, :phrase) || Safe.get(t, :word) || Safe.get(t, :lemma) || ""
            end)
            |> Enum.map(&down/1)
            |> Enum.reject(&(&1 in ["", "a", "an", "the"]))
        end

      Map.put(acc, idx, heads)
    end)
  rescue
    _ -> %{}
  end

  defp token_by_index(tokens, idx) when is_list(tokens) and is_integer(idx) do
    Enum.find(tokens, fn tok ->
      normalize_idx(Safe.get(tok, :index, Safe.get(tok, :token_index, -1))) == idx
    end)
  end

  defp token_by_index(_, _), do: nil

  # ---------- tiny utils duplicated locally to avoid cross-deps ----------

  defp inside?({s, e}, {ps, pe})
       when is_integer(s) and is_integer(e) and is_integer(ps) and is_integer(pe) do
    # closed-open containment: [s, e) inside [ps, pe)
    s >= ps and e <= pe
  end

  defp inside?(_, _), do: false

  defp down(s) when is_binary(s),
    do: s |> String.downcase() |> String.trim() |> String.replace(~r/\s+/u, " ")

  defp down(_), do: ""

  defp normalize_idx(i) when is_integer(i) and i >= 0, do: i

  defp normalize_idx(b) when is_binary(b) do
    case Integer.parse(b) do
      {n, _} when n >= 0 -> n
      _ -> 0
    end
  end

  defp normalize_idx(_), do: 0

  defp unique_rerun_id do
    Integer.to_string(:erlang.unique_integer([:positive])) <>
      "-" <> Integer.to_string(System.system_time(:microsecond))
  end

  defp safe_exec_telemetry(event, measurements, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
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
