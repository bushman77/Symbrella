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
      heads_by_idx = heads_for_indices(si, idxs)

      # absorb unigrams from active_cells → affected MWE buckets only
      si_absorb =
        Brain.LIFG.MWE.absorb_unigrams_into_mwe(
          si,
          Keyword.put(opts, :absorb_unigrams_into_mwe?, true)
        )

      # trace + telemetry
      now_ms = System.system_time(:millisecond)

      ev = %{
        stage: :lifg_rerun,
        reason: :mwe_fallback,
        ts_ms: now_ms,
        token_indices: idxs,
        heads_by_idx: heads_by_idx,
        wm_cfg_seen?: wm_present?,
        allow_fallback_into_wm?: allow_fallback_into_wm?
      }

      si_trace = Map.update(si_absorb, :trace, [ev], fn tr -> [ev | tr] end)

      safe_exec_telemetry(
        [:brain, :lifg, :rerun],
        %{count: length(idxs)},
        %{reason: :mwe_fallback, token_indices: idxs, heads_by_idx: heads_by_idx}
      )

      # re-run Stage-1 with same knobs
      case Brain.LIFG.Stage1.run(
             si_trace,
             weights: weights_for_stage1,
             scores: scores_mode,
             margin_threshold: margin_thr,
             chargram_event:
               Keyword.get(opts, :chargram_event, [:brain, :lifg, :chargram_violation]),
             boundary_event: Keyword.get(opts, :boundary_event, [:brain, :lifg, :boundary_drop])
           ) do
        {:ok, %{si: si_rerun, choices: raw2}} ->
          min_margin = Keyword.get(opts, :min_margin, 0.05)

          # IMPORTANT: use the canonical augmenter so Option A separation holds:
          # - alt_ids = scored competitors only
          # - slate_alt_ids = slate-only bucket candidates
          choices2 = Choices.augment(raw2, si_rerun, min_margin)

          {si_rerun, choices2}

        _ ->
          # If rerun fails for any reason, fall back to original
          {si_trace, choices0}
      end
    end
  end

  # ---------- detection ----------

  @spec fallback_winner_indices(list()) :: [non_neg_integer()]
  def fallback_winner_indices(choices) do
    choices
    |> Enum.reduce([], fn ch, acc ->
      id = to_string(Safe.get(ch, :chosen_id, ""))

      if String.ends_with?(id, "|phrase|fallback"),
        do: [Safe.get(ch, :token_index, 0) | acc],
        else: acc
    end)
    |> Enum.reverse()
    |> Enum.uniq()
  end

  # ---------- debug helpers ----------

  # Build a debug map of heads per token index using child unigrams inside each MWE span
  defp heads_for_indices(si, idxs) do
    toks = Safe.get(si, :tokens, [])

    Enum.reduce(idxs, %{}, fn idx, acc ->
      heads =
        case Enum.at(toks, idx) do
          nil ->
            []

          tok ->
            span = Safe.get(tok, :span)

            toks
            |> Enum.with_index()
            |> Enum.filter(fn {t, j} ->
              j != idx and Safe.get(t, :n, 1) == 1 and inside?(Safe.get(t, :span), span)
            end)
            |> Enum.map(fn {t, _} ->
              Safe.get(t, :phrase) || Safe.get(t, :word) || Safe.get(t, :lemma) || ""
            end)
            |> Enum.map(&down/1)
            |> Enum.reject(&(&1 in ["", "a", "an", "the"]))
        end

      Map.put(acc, idx, heads)
    end)
  end

  # ---------- tiny utils duplicated locally to avoid cross-deps ----------

defp inside?({s, e}, {ps, pe})
     when is_integer(s) and is_integer(e) and is_integer(ps) and is_integer(pe) do
  # closed-open containment: [s, e) inside [ps, pe)
  s >= ps and e <= pe
end

defp inside?(_, _), do: false

  defp down(s) when is_binary(s),
    do: s |> String.downcase() |> String.trim() |> String.replace(~r/\s+/, " ")

  defp down(_), do: ""

  defp safe_exec_telemetry(event, measurements, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(event, measurements, meta)
    else
      :ok
    end
  end
end

