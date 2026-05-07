# apps/brain/lib/brain/lifg/fallback_rerun.ex
defmodule Brain.LIFG.FallbackRerun do
  @moduledoc """
  P-201 fallback rerun for cases where a token's winner is a `|phrase|fallback`
  and WM disallows fallbacks.

  Responsibilities
  • Detect fallback winners.
  • Absorb unigram children from `active_cells` into affected MWE buckets.
  • Backfill real phrase cells into affected MWE buckets.
  • Rerun Stage-1 with unchanged knobs.
  • Emit trace + telemetry.
  • Return the updated `si` and **augmented** choices (margin/alt_ids normalized).

  Option A semantics (via `Brain.LIFG.Choices`):
  • `:alt_ids` are *scored competitors only* (keys from `:scores`, excluding `:chosen_id`)
  • `:slate_alt_ids` are slate-only bucket candidates not present in `:scores`
  """

  import Ecto.Query, only: [from: 2]

  alias Brain.LIFG.Choices
  alias Brain.Utils.Safe
  alias Db.BrainCell

  @rerun_event [:brain, :lifg, :rerun]

  @type stage1_result ::
          {:ok, %{si: map(), choices: list()}}
          | {:error, term()}

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
    * `:debug_rerun?` (default: false)
  """
  @spec maybe_rerun(map(), list(), map(), atom(), number(), keyword()) :: {map(), list()}
  def maybe_rerun(si, choices0, weights_for_stage1, scores_mode, margin_thr, opts) do
    rerun_on_fallback? = Keyword.get(opts, :rerun_on_fallback?, true)
    debug_rerun? = Keyword.get(opts, :debug_rerun?, false)

    {allow_fallback_into_wm?, wm_present?} = wm_fallback_cfg(si, opts)
    idxs = fallback_winner_indices(choices0)

    needs_rerun? =
      rerun_on_fallback? and idxs != [] and not allow_fallback_into_wm?

    if debug_rerun? do
      IO.inspect(
        %{
          rerun_on_fallback?: rerun_on_fallback?,
          debug_rerun?: debug_rerun?,
          allow_fallback_into_wm?: allow_fallback_into_wm?,
          wm_present?: wm_present?,
          idxs: idxs,
          needs_rerun?: needs_rerun?
        },
        label: "[LIFG maybe_rerun gate]",
        charlists: :as_lists
      )
    end

    if not needs_rerun? do
      {si, choices0}
    else
      rerun_id = unique_rerun_id()
      heads_by_idx = heads_for_indices(si, idxs)

      maybe_debug_winners(debug_rerun?, "before-rerun", rerun_id, si, choices0, idxs)

      si_prepared =
        si
        |> absorb_unigrams_into_mwe(opts)
        |> backfill_real_mwe(opts)

      maybe_debug_buckets(debug_rerun?, "after-backfill", rerun_id, si_prepared, idxs)

      ts_ms = System.system_time(:millisecond)
      start_ms = System.monotonic_time(:millisecond)

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

      si_trace = Map.update(si_prepared, :trace, [ev], fn tr -> [ev | tr] end)

      emit_rerun_start(
        rerun_id,
        idxs,
        heads_by_idx,
        wm_present?,
        allow_fallback_into_wm?,
        scores_mode,
        margin_thr
      )

      {res, dur_ms} =
        rerun_stage1(
          si_trace,
          weights_for_stage1,
          scores_mode,
          margin_thr,
          opts,
          start_ms
        )

      handle_rerun_result(
        res,
        dur_ms,
        rerun_id,
        idxs,
        scores_mode,
        margin_thr,
        si_trace,
        choices0,
        debug_rerun?,
        opts
      )
    end
  end

  # ---------- public detection ----------

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
    |> Enum.map(&normalize_idx/1)
    |> Enum.uniq()
    |> Enum.sort()
  end

  # ---------- rerun orchestration ----------

  defp wm_fallback_cfg(si, opts) do
    case si do
      %{wm_cfg: %{allow_fallback_into_wm?: v}} -> {!!v, true}
      _ -> {Keyword.get(opts, :allow_fallback_into_wm?, false), false}
    end
  end

  defp absorb_unigrams_into_mwe(si, opts) do
    Brain.LIFG.MWE.absorb_unigrams_into_mwe(
      si,
      Keyword.put(opts, :absorb_unigrams_into_mwe?, true)
    )
  end

  defp backfill_real_mwe(si, opts) do
    Brain.LIFG.MWE.backfill_real_mwe_from_active_cells(si, opts)
  end

  defp emit_rerun_start(
         rerun_id,
         idxs,
         heads_by_idx,
         wm_present?,
         allow_fallback_into_wm?,
         scores_mode,
         margin_thr
       ) do
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
  end

  @spec rerun_stage1(map(), map(), atom(), number(), keyword(), integer()) ::
          {stage1_result(), non_neg_integer()}
  defp rerun_stage1(si_trace, weights_for_stage1, scores_mode, margin_thr, opts, start_ms) do
    res =
      try do
        Brain.LIFG.Stage1.run(
          si_trace,
          weights: weights_for_stage1,
          scores: scores_mode,
          margin_threshold: margin_thr,
          chargram_event:
            Keyword.get(opts, :chargram_event, [:brain, :lifg, :chargram_violation]),
          boundary_event: Keyword.get(opts, :boundary_event, [:brain, :lifg, :boundary_drop]),
          stage1_stop_event:
            Keyword.get(opts, :rerun_stage1_stop_event, [
              :brain,
              :pipeline,
              :lifg_stage1,
              :rerun_stop
            ])
        )
      rescue
        e -> {:error, {:exception, e}}
      catch
        kind, reason -> {:error, {kind, reason}}
      end

    dur_ms =
      System.monotonic_time(:millisecond)
      |> Kernel.-(start_ms)
      |> max(0)

    {res, dur_ms}
  end

  defp handle_rerun_result(
         {:ok, %{si: si_rerun, choices: raw2}},
         dur_ms,
         rerun_id,
         idxs,
         scores_mode,
         margin_thr,
         _si_trace,
         _choices0,
         debug_rerun?,
         opts
       ) do
    min_margin = Keyword.get(opts, :min_margin, 0.05)
    choices2 = Choices.augment(raw2, si_rerun, min_margin)

    maybe_debug_winners(debug_rerun?, "after-rerun", rerun_id, si_rerun, choices2, idxs)
    maybe_debug_buckets(debug_rerun?, "after-rerun", rerun_id, si_rerun, idxs)

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
  end

  defp handle_rerun_result(
         other,
         dur_ms,
         rerun_id,
         idxs,
         _scores_mode,
         _margin_thr,
         si_trace,
         choices0,
         debug_rerun?,
         _opts
       ) do
    maybe_debug_rerun_error(debug_rerun?, rerun_id, other)

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

  # ---------- debug helpers ----------

  defp maybe_debug_winners(false, _stage, _rerun_id, _si, _choices, _idxs), do: :ok

  defp maybe_debug_winners(true, stage, rerun_id, si, choices, idxs) do
    debug_print_winners(stage, rerun_id, si, choices, idxs)
  end

  defp maybe_debug_buckets(false, _stage, _rerun_id, _si, _idxs), do: :ok

  defp maybe_debug_buckets(true, stage, rerun_id, si, idxs) do
    debug_print_buckets(stage, rerun_id, si, idxs)
  end

  defp maybe_debug_rerun_error(false, _rerun_id, _other), do: :ok

  defp maybe_debug_rerun_error(true, rerun_id, other) do
    IO.puts("\n===== LIFG RERUN #{rerun_id} FAILED =====")
    IO.inspect(other, label: "[LIFG rerun error]")
    :ok
  end

  defp debug_print_winners(stage, rerun_id, si, choices, idxs) do
    IO.puts("\n===== LIFG RERUN #{rerun_id} #{stage} WINNERS =====")

    Enum.each(idxs, fn idx ->
      tok = token_by_index(Safe.get(si, :tokens, []), idx)

      phrase =
        Safe.get(tok, :phrase) ||
          Safe.get(tok, :word) ||
          Safe.get(tok, :lemma) ||
          ""

      chosen =
        Enum.find(choices, fn ch ->
          normalize_idx(Safe.get(ch, :token_index, Safe.get(ch, :index, -1))) == idx
        end)

      chosen_id =
        to_string(
          Safe.get(chosen, :chosen_id) ||
            Safe.get(chosen, :id) ||
            ""
        )

      IO.inspect(
        %{
          token_index: idx,
          phrase: phrase,
          chosen_id: chosen_id
        },
        label: "[LIFG winner]"
      )
    end)

    :ok
  rescue
    e ->
      IO.puts("[LIFG rerun #{rerun_id}] debug_print_winners crashed")
      IO.inspect(e, label: "[LIFG debug winners error]")
      :ok
  end

  defp debug_print_buckets(stage, rerun_id, si, idxs) do
    IO.puts("\n===== LIFG RERUN #{rerun_id} #{stage} BUCKETS =====")

    sc = Safe.get(si, :sense_candidates, %{}) || %{}
    defs_by_id = fetch_definitions_for_indices(sc, idxs)

    Enum.each(idxs, fn idx ->
      tok = token_by_index(Safe.get(si, :tokens, []), idx)

      phrase =
        Safe.get(tok, :phrase) ||
          Safe.get(tok, :word) ||
          Safe.get(tok, :lemma) ||
          ""

      bucket = get_sc_bucket(sc, idx)

      IO.puts(
        "\n--- token_index=#{idx} phrase=#{inspect(phrase)} bucket_size=#{length(bucket)} ---"
      )

      Enum.each(bucket, fn cand ->
        id = to_string(Safe.get(cand, :id) || Safe.get(cand, "id") || "")
        source = Safe.get(cand, :source) || Safe.get(cand, "source")
        pos = Safe.get(cand, :pos) || Safe.get(cand, "pos")
        definition = Map.get(defs_by_id, id, "<no db definition>")

        IO.inspect(
          %{
            id: id,
            source: source,
            pos: pos,
            definition: definition
          },
          label: "[LIFG bucket candidate]"
        )
      end)
    end)

    :ok
  rescue
    e ->
      IO.puts("[LIFG rerun #{rerun_id}] debug_print_buckets crashed")
      IO.inspect(e, label: "[LIFG debug buckets error]")
      :ok
  end

  defp fetch_definitions_for_indices(sc, idxs) do
    ids =
      idxs
      |> Enum.flat_map(fn idx ->
        sc
        |> get_sc_bucket(idx)
        |> Enum.map(fn cand ->
          to_string(Safe.get(cand, :id) || Safe.get(cand, "id") || "")
        end)
      end)
      |> Enum.reject(&(&1 == "" or String.ends_with?(&1, "|phrase|fallback")))
      |> Enum.uniq()

    if ids == [] do
      %{}
    else
      try do
        Db.all(
          from(c in BrainCell,
            where: c.id in ^ids,
            select: %{id: c.id, definition: c.definition}
          )
        )
        |> Enum.into(%{}, fn row ->
          {to_string(row.id), row.definition || "<no definition>"}
        end)
      rescue
        _ -> %{}
      end
    end
  end

  defp get_sc_bucket(sc, idx) when is_map(sc) and is_integer(idx) do
    cond do
      Map.has_key?(sc, idx) -> List.wrap(Map.get(sc, idx))
      Map.has_key?(sc, Integer.to_string(idx)) -> List.wrap(Map.get(sc, Integer.to_string(idx)))
      true -> []
    end
  end

  defp get_sc_bucket(_sc, _idx), do: []

  # Build a debug map of heads per token index using child unigrams inside each MWE span.
  # Best-effort only; never crashes rerun.
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

  # ---------- tiny utils ----------

  defp inside?({s, e}, {ps, pe})
       when is_integer(s) and is_integer(e) and is_integer(ps) and is_integer(pe) do
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
