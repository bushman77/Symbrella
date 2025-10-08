defmodule Brain.LIFG.Hygiene do
  @moduledoc """
  Post-Stage-1 hygiene pass:
    • Sanitizes score maps (drops non-numeric/NaN)
    • Computes softmax probabilities as `:probs`
    • Adds `:p_top1`
    • Dedups `:alt_ids`
    • Flags `:needs_rerun` (by margin/p_top1/alt_ids)
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
          {probs, dropped} = normalize_scores_map(Map.get(ch, :scores, %{}))
          p1 = max_prob(probs)

          alt_ids2 =
            (ch[:alt_ids] || [])
            |> Enum.reject(&(&1 == ch[:chosen_id]))
            |> Enum.uniq()

          needs? =
            (ch[:margin] || 0.0) < min_margin or p1 < p_min or alt_ids2 != []

          ch2 =
            ch
            |> Map.put(:alt_ids, alt_ids2)
            |> Map.put(:probs, probs)
            |> Map.put(:p_top1, p1)
            |> Map.put(:needs_rerun, needs?)

          acc2 =
            acc
            |> Map.update!(:sanitized, &(&1 + dropped))
            |> Map.update!(:needs_rerun, &(&1 + if(needs?, do: 1, else: 0)))

          {ch2, acc2}
        end)

      meas = %{total: length(choices), sanitized: stats.sanitized, needs_rerun: stats.needs_rerun, ts_ms: now_ms}
      emit(event, meas, %{min_margin: min_margin, p_min: p_min})
      {:ok, %{si: si, choices: cleaned, audit: meas}}
    rescue
      e ->
        Logger.error("LIFG Hygiene run failed: #{inspect(e)}")
        {:error, e}
    end
  end

  defp normalize_scores_map(map) when is_map(map) do
    pairs =
      map
      |> Enum.filter(fn {_k, v} -> is_number(v) and v == v end) # (v==v) filters NaN

    dropped = map_size(map) - length(pairs)
    vals = Enum.map(pairs, fn {_, v} -> v end)

    probs =
      case vals do
        [] -> %{}
        _ ->
          m = Enum.max([0.0 | vals])
          exps = Enum.map(vals, fn s -> :math.exp(s - m) end)
          z = Enum.sum(exps)

          if z == 0.0 do
            Map.new(pairs, fn {k, _} -> {k, 0.0} end)
          else
            pairs |> Enum.zip(exps) |> Map.new(fn {{k, _}, e} -> {k, e / z} end)
          end
      end

    {probs, dropped}
  end

  defp max_prob(%{} = probs), do: probs |> Map.values() |> Enum.max(fn -> 0.0 end)
  defp max_prob(_), do: 0.0

  defp emit(ev, meas, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(ev, meas, meta)
    else
      :ok
    end
  end
end

