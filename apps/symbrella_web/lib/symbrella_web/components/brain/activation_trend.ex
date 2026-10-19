defmodule SymbrellaWeb.Components.Brain.ActivationTrend do
  @moduledoc """
  Small, dependency-free SVG chart of observed Brain.Cell activation reports.

  Keeps only 60 seconds of samples (at most 120) in the connected LiveView.
  These are snapshots of the Brain coordinator's runtime activation map, NOT
  persisted brain_cells.activation values. The Brain coordinator supplies
  real time-decayed values; this component only plots those observations.
  """

  use SymbrellaWeb, :html

  @window_ms 60_000
  @max_samples 120

  @doc "Append an observed snapshot, keeping only the latest minute."
  @spec capture([map()], map(), integer()) :: [map()]
  def capture(previous, reports, at_ms)
      when is_list(previous) and is_map(reports) and is_integer(at_ms) do
    values =
      reports
      |> Map.values()
      |> Enum.filter(&(is_number(&1) and &1 > 0))
      |> Enum.map(&(&1 * 1.0))

    count = length(values)

    sample = %{
      at_ms: at_ms,
      count: count,
      mean: if(count == 0, do: 0.0, else: Enum.sum(values) / count),
      peak: Enum.max(values, fn -> 0.0 end)
    }

    (previous ++ [sample])
    |> Enum.filter(fn
      %{at_ms: t} when is_integer(t) -> t >= at_ms - @window_ms and t <= at_ms
      _ -> false
    end)
    |> Enum.take(-@max_samples)
  end

  def capture(previous, _reports, _at_ms) when is_list(previous), do: previous

  attr :samples, :list, default: []

  def panel(assigns) do
    samples = assigns.samples
    latest = List.last(samples) || %{at_ms: 0, count: 0, mean: 0.0, peak: 0.0}

    ceiling =
      Enum.reduce(samples, 1.0, fn s, acc -> max(acc, s.peak) end)

    # Fixed 60-second x-axis; y-axis expands if activations exceed 1.0.
    # Mean/peak share one scale so their visual separation has meaning.
    assigns =
      assigns
      |> assign(:latest, latest)
      |> assign(:ceiling, ceiling)
      |> assign(:mean_points, points(samples, :mean, ceiling))
      |> assign(:peak_points, points(samples, :peak, ceiling))
      |> assign(:latest_x, x_pos(latest.at_ms, latest.at_ms))
      |> assign(:mean_y, y_pos(latest.mean, ceiling))
      |> assign(:peak_y, y_pos(latest.peak, ceiling))

    ~H"""
    <section
      id="brain-activation-trend"
      data-brain-report-section="Neuron Activation History"
      class="w-full max-w-xl rounded-2xl border border-zinc-200 bg-white/75 p-4 shadow-sm dark:border-zinc-800 dark:bg-zinc-900/70"
    >
      <div class="flex flex-wrap items-center justify-between gap-2">
        <h3 class="text-sm font-semibold text-zinc-900 dark:text-zinc-100">Neuron activation</h3>
        <span class="text-xs text-zinc-500 dark:text-zinc-400">Live · last 60 seconds</span>
      </div>
      <div class="mt-2 flex flex-wrap gap-x-4 gap-y-1 text-xs text-zinc-600 dark:text-zinc-300">
        <span><span class="text-sky-500">●</span> Mean: {format_activation(@latest.mean)}</span>
        <span><span class="text-amber-500">●</span> Peak: {format_activation(@latest.peak)}</span>
        <span>Reporting neurons: {@latest.count}</span>
      </div>
      <svg
        class="mt-2 h-36 w-full"
        viewBox="0 0 360 136"
        preserveAspectRatio="none"
        role="img"
        aria-label="Mean and peak observed BrainCell activations over the last sixty seconds"
      >
        <title>Observed neuron activation history</title>
        <g stroke="currentColor" stroke-opacity="0.13" stroke-width="1">
          <line x1="32" y1="16" x2="350" y2="16" />
          <line x1="32" y1="63" x2="350" y2="63" />
          <line x1="32" y1="110" x2="350" y2="110" />
        </g>
        <g fill="currentColor" opacity="0.65" font-size="10">
          <text x="0" y="19">{format_activation(@ceiling)}</text>
          <text x="16" y="113">0</text>
          <text x="32" y="130">−60s</text>
          <text x="350" y="130" text-anchor="end">Now</text>
        </g>
        <polyline
          :if={length(@samples) > 1}
          points={@mean_points}
          fill="none"
          stroke="#0ea5e9"
          stroke-width="2"
          stroke-linejoin="round"
          stroke-linecap="round"
        />
        <polyline
          :if={length(@samples) > 1}
          points={@peak_points}
          fill="none"
          stroke="#f59e0b"
          stroke-width="2"
          stroke-linejoin="round"
          stroke-linecap="round"
        />
        <circle :if={@samples != []} cx={@latest_x} cy={@mean_y} r="2.5" fill="#0ea5e9" />
        <circle :if={@samples != []} cx={@latest_x} cy={@peak_y} r="2.5" fill="#f59e0b" />
      </svg>
      <p :if={@latest.count == 0} class="text-xs text-zinc-500 dark:text-zinc-400">
        No neuron activation reports received yet.
      </p>
      <p class="mt-1 text-xs text-zinc-500 dark:text-zinc-400">
        Runtime only · starts when this page opens · neuron half-life:
        {Brain.Cell.Activation.half_life_ms() / 1000}s · resting threshold:
        {Brain.Cell.Activation.rest_threshold()}.
      </p>
    </section>
    """
  end

  defp points([], _key, _ceiling), do: ""

  defp points(samples, key, ceiling) do
    now = List.last(samples).at_ms

    Enum.map_join(samples, " ", fn s ->
      "#{coord(x_pos(s.at_ms, now))},#{coord(y_pos(Map.fetch!(s, key), ceiling))}"
    end)
  end

  defp x_pos(time, now) do
    32.0 + 318.0 * min(max((time - (now - @window_ms)) / @window_ms, 0.0), 1.0)
  end

  defp y_pos(value, ceiling), do: 110.0 - 94.0 * min(max(value / ceiling, 0.0), 1.0)

  defp coord(value), do: :erlang.float_to_binary(value * 1.0, decimals: 2)
  defp format_activation(value), do: :erlang.float_to_binary(value * 1.0, decimals: 2)
end
