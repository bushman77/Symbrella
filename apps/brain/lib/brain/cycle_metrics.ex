defmodule Brain.CycleMetrics do
  @moduledoc """
  TINDA-lite metrics derived from recent activation transitions.

  Input state is expected to have `:activation_log`, but this module degrades
  gracefully if formats differ. It tries to infer a sequence of "network states"
  (e.g., regions) and computes:

    • `:strength` in [0,1] — chain/loopiness vs randomness
    • `:hz`        in Hz   — estimated tempo (kept aligned to CycleClock for stability)
    • `:order`     list    — dominant directed order (greedy)

  Call cheaply on each cycle tick for a heads-up display.
  """

  @window_ms 2_000

  @type snapshot :: %{
          strength: float(),
          hz: float(),
          order: [atom()]
        }

  @doc "Compute a metrics snapshot from a Brain state map."
  @spec snapshot(map()) :: snapshot()
  def snapshot(%{activation_log: log}) when is_list(log) do
    now = now_ms()
    recent = Enum.filter(log, &has_time_after?(&1, now - @window_ms))
    seq = extract_sequence(recent)

    tm = transition_matrix(seq)
    s = cycle_strength(tm)
    hz = est_hz(seq)
    order = dominant_order(tm)

    %{strength: s, hz: hz, order: order}
  end

  def snapshot(_), do: %{strength: 0.0, hz: Brain.CycleClock.hz(), order: []}

  # ---------- helpers ----------

  defp now_ms, do: System.monotonic_time(:millisecond)

  defp has_time_after?({t, _}, min), do: is_number(t) and t >= min
  defp has_time_after?(%{at: t}, min), do: is_number(t) and t >= min
  defp has_time_after?(_other, _min), do: false

  # Accepts items like:
  #   {ts, %{region: :lifg}}  |  %{at: ts, region: :pmtg}  |  %{at: ts, meta: %{region: :hippocampus}}
  # Fallback: returns [] if no region can be inferred.
  defp extract_sequence(items) do
    items
    |> Enum.map(fn
      {t, %{region: r}} when is_atom(r) -> {t, r}
      %{at: t, region: r} when is_atom(r) -> {t, r}
      %{at: t, meta: %{region: r}} when is_atom(r) -> {t, r}
      _ -> nil
    end)
    |> Enum.reject(&is_nil/1)
    |> Enum.sort_by(fn {t, _} -> t end)
    |> Enum.map(fn {_t, r} -> r end)
    |> compress_runs()
  end

  # collapse AAAABBBCC -> A,B,C
  defp compress_runs([]), do: []
  defp compress_runs([h | rest]), do: do_compress(rest, h, [h])
  defp do_compress([], _prev, acc), do: Enum.reverse(acc)

  defp do_compress([x | xs], prev, acc) do
    if x == prev, do: do_compress(xs, prev, acc), else: do_compress(xs, x, [x | acc])
  end

  defp transition_matrix(seq) do
    seq
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.reduce(%{}, fn [a, b], acc ->
      acc
      |> Map.update(a, %{b => 1}, fn m -> Map.update(m, b, 1, &(&1 + 1)) end)
    end)
  end

  # Very simple loopiness proxy:
  # sum of strongest outgoing edge per node divided by total transitions.
  defp cycle_strength(tm) when map_size(tm) == 0, do: 0.0

  defp cycle_strength(tm) do
    {best, total} =
      Enum.reduce(tm, {0, 0}, fn {_from, outs}, {b, t} ->
        vals = Map.values(outs)
        b_node = Enum.max(vals)
        {b + b_node, t + Enum.sum(vals)}
      end)

    if total == 0, do: 0.0, else: min(1.0, best / total)
  end

  defp est_hz(seq) do
    # Keep aligned with the global clock; if you later add per-seq timing, replace here.
    case length(seq) do
      l when l < 3 -> Brain.CycleClock.hz()
      _ -> Brain.CycleClock.hz()
    end
  end

  defp dominant_order(tm) do
    if map_size(tm) == 0 do
      []
    else
      start = tm |> Map.keys() |> List.first()
      walk(tm, start, MapSet.new(), [])
    end
  end

  # NOTE: No guards with MapSet.* (not guard-safe). Do checks in-body.
  defp walk(tm, node, seen, acc) do
    if MapSet.member?(seen, node) do
      Enum.reverse(acc)
    else
      outs = Map.get(tm, node, %{})

      next =
        outs
        |> Enum.sort_by(fn {_n, w} -> -w end)
        |> case do
          [] -> nil
          [{n, _w} | _] -> n
        end

      case next do
        nil -> Enum.reverse([node | acc])
        n -> walk(tm, n, MapSet.put(seen, node), [node | acc])
      end
    end
  end
end
