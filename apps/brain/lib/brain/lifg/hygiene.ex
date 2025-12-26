defmodule Brain.LIFG.Hygiene do
  @moduledoc """
  Post-Stage-1 hygiene pass:
    • Sanitizes score maps (drops non-numeric / NaN)
    • Computes softmax probabilities as `:probs` (uniform fallback if degenerate)
    • Adds `:p_top1` and `:prob_margin` (= p1 - p2)
    • Dedups `:alt_ids` and removes chosen_id from it
    • Flags `:needs_rerun` (by margin / p_top1 *when alts exist*)
    • Emits telemetry: [:brain, :hygiene, :wipe]
  """

  require Logger
  @type choice :: map()

  @spec run(map(), [choice()], keyword()) ::
          {:ok, %{si: map(), choices: [choice()], audit: map()}} | {:error, term()}
  def run(si, choices, opts \\ []) when is_map(si) and is_list(choices) do
    try do
      min_margin =
        Keyword.get(opts, :min_margin, Application.get_env(:brain, :lifg_min_margin, 0.12))

      p_min =
        Keyword.get(opts, :p_min, Application.get_env(:brain, :lifg_min_p_top1, 0.65))

      event = Keyword.get(opts, :event, [:brain, :hygiene, :wipe])
      now_ms = System.system_time(:millisecond)

      {cleaned, stats} =
        Enum.map_reduce(choices, %{sanitized: 0, needs_rerun: 0}, fn ch, acc ->
          chosen = ch[:chosen_id] || ch[:id]

          # 1) sanitize & normalize scores -> probs
          scores_in = Map.get(ch, :scores, %{}) || %{}
          {probs0, dropped} = normalize_scores_map(scores_in)

          probs =
            cond do
              map_size(probs0) > 0 ->
                probs0

              is_binary(chosen) ->
                %{chosen => 1.0}

              true ->
                %{}
            end

          # 2) compute top1 & prob margin
          {p1, p2} = top2_probs(probs)
          prob_margin = max(p1 - p2, 0.0)

          # 3) alt_ids: dedup & remove chosen; backfill from probs if missing
          alt_ids_in = (ch[:alt_ids] || []) |> List.wrap()

          alt_ids2 =
            alt_ids_in
            |> Enum.reject(&(&1 == chosen))
            |> Enum.uniq()

          alt_ids2 =
            cond do
              alt_ids2 != [] ->
                alt_ids2

              is_binary(chosen) and map_size(probs) > 1 ->
                probs
                |> Enum.sort_by(fn {_id, p} -> -p end)
                |> Enum.map(&elem(&1, 0))
                |> Enum.reject(&(&1 == chosen))
                |> Enum.take(1)

              true ->
                []
            end

          # 4) needs_rerun: only when confidence is low *and* there is a real alternative
          needs? =
            ((ch[:margin] || 0.0) < min_margin or p1 < p_min) and alt_ids2 != []

          ch2 =
            ch
            |> Map.put(:alt_ids, alt_ids2)
            |> Map.put(:probs, probs)
            |> Map.put(:p_top1, p1)
            |> Map.put(:prob_margin, prob_margin)
            |> Map.put(:needs_rerun, needs?)

          acc2 =
            acc
            |> Map.update!(:sanitized, &(&1 + dropped))
            |> Map.update!(:needs_rerun, &(&1 + if(needs?, do: 1, else: 0)))

          {ch2, acc2}
        end)

      meas = %{
        total: length(choices),
        sanitized: stats.sanitized,
        needs_rerun: stats.needs_rerun,
        ts_ms: now_ms
      }

      emit(event, meas, %{min_margin: min_margin, p_min: p_min})
      {:ok, %{si: si, choices: cleaned, audit: meas}}
    rescue
      e ->
        Logger.error("LIFG Hygiene run failed: #{inspect(e)}")
        {:error, e}
    end
  end

  # --- internals ---------------------------------------------------------------

  # Returns {normalized_probs_map, dropped_count}
  defp normalize_scores_map(map) when is_map(map) do
    pairs =
      map
      # (v==v) filters NaN
      |> Enum.filter(fn {_k, v} -> is_number(v) and v == v end)

    dropped = map_size(map) - length(pairs)
    vals = Enum.map(pairs, fn {_, v} -> v * 1.0 end)

    case vals do
      [] ->
        {%{}, dropped}

      _ ->
        m = Enum.max([0.0 | vals])
        exps = Enum.map(vals, fn s -> :math.exp(s - m) end)
        z = Enum.sum(exps)

        cond do
          z <= 0.0 ->
            # uniform fallback (avoids all-zeros)
            n = length(pairs)
            u = 1.0 / n
            {Map.new(pairs, fn {k, _} -> {k, u} end), dropped}

          true ->
            {pairs |> Enum.zip(exps) |> Map.new(fn {{k, _}, e} -> {k, e / z} end), dropped}
        end
    end
  end

  defp normalize_scores_map(_), do: {%{}, 0}

  defp top2_probs(%{} = probs) do
    case probs |> Map.values() |> Enum.sort(:desc) do
      [a, b | _] -> {a, b}
      [a] -> {a, 0.0}
      _ -> {0.0, 0.0}
    end
  end

  defp emit(ev, meas, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(ev, meas, meta)
    else
      :ok
    end
  end
end
