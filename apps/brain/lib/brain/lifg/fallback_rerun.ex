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
  """

  alias Brain.Utils.Safe

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
          choices2 = augment_choices(raw2, si_rerun, min_margin)
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

  # ---------- choice augmentation (kept local to preserve old contract) ----------

  defp augment_choices(raw_choices, si_after, min_margin) when is_list(raw_choices) do
    slate =
      Safe.get(si_after, :sense_candidates, %{}) ||
        Safe.get(si_after, :candidates_by_token, %{}) || %{}

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

      singleton? = length(alt_ids) == 0

      margin0 =
        case Safe.get(ch, :margin) do
          m when is_number(m) and m > 0.0 and not singleton? ->
            m

          _ ->
            vals = scores |> Map.values() |> Enum.sort(:desc)

            case vals do
              [a, b | _] -> a - b
              [_a] -> 0.0
              _ -> 0.0
            end
        end

      margin = Float.round(Kernel.max(margin0, min_margin), 6)

      ch
      |> Map.put(:chosen_id, chosen_id_s)
      |> Map.put(:alt_ids, alt_ids)
      |> Map.put(:margin, margin)
    end)
  end

  # ---------- tiny utils duplicated locally to avoid cross-deps ----------

  defp inside?({s, l}, {ps, pl})
       when is_integer(s) and is_integer(l) and is_integer(ps) and is_integer(pl) do
    e = s + l
    pe = ps + pl
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
