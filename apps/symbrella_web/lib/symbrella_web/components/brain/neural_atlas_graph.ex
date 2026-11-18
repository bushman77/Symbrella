defmodule SymbrellaWeb.Components.Brain.NeuralAtlasGraph do
  @moduledoc """
  Graph-first, accessible view of registered Symbrella cognitive regions.

  Positions are stable presentation coordinates, not brain measurements.
  Only registry conceptual links and cross-region BEAM references produce edges.
  Telemetry and Bus receipts illuminate nodes, never fabricate connections.
  """

  use SymbrellaWeb, :html

  @positions %{
    frontal: {100, 210},
    fpc: {140, 92},
    dlpfc: {270, 75},
    dmpfc: {405, 85},
    prefrontal: {350, 198},
    vmpfc: {230, 298},
    ofc: {122, 403},
    lifg: {390, 390},
    acc: {500, 270},
    salience: {625, 265},
    thalamus: {710, 370},
    basal_ganglia: {590, 455},
    hippocampus: {762, 475},
    atl: {645, 120},
    pmtg: {790, 195},
    temporal: {897, 322},
    cerebellum: {897, 527}
  }

  @doc "Stable graph coordinates, with deterministic fallback positions for new regions."
  def layout(nodes) when is_list(nodes) do
    sorted = Enum.sort_by(nodes, &Atom.to_string(&1.key))
    total = length(sorted)

    sorted
    |> Enum.with_index()
    |> Enum.map(fn {node, index} ->
      {x, y} =
        Map.get(@positions, node.key) ||
          fallback(index, max(total, 1))

      node
      |> Map.put(:x, x)
      |> Map.put(:y, y)
      |> Map.put(:left, "#{x / 10}%")
      |> Map.put(:top, "#{y / 6}%")
    end)
  end

  @doc """
  Returns only evidence-supported graph edges.

  Conceptual edges are undirected registry relationships. Compiled dependencies
  are directed references. Runtime receipts have no verified edge endpoints.
  """
  def edges(nodes, :conceptual, _dependencies) do
    keys = MapSet.new(Enum.map(nodes, & &1.key))

    for node <- nodes,
        target <- List.wrap(Map.get(node, :related, [])),
        MapSet.member?(keys, target),
        target != node.key do
      %{from: node.key, to: target, directed: false}
    end
    |> Enum.uniq_by(fn edge -> Enum.sort([edge.from, edge.to]) end)
    |> Enum.sort_by(&{&1.from, &1.to})
  end

  def edges(nodes, :dependencies, dependencies) do
    keys = MapSet.new(Enum.map(nodes, & &1.key))

    dependencies
    |> Enum.filter(fn edge ->
      MapSet.member?(keys, edge.from) and
        MapSet.member?(keys, edge.to) and edge.from != edge.to
    end)
    |> Enum.uniq_by(&{&1.from, &1.to})
    |> Enum.map(&%{from: &1.from, to: &1.to, directed: true})
    |> Enum.sort_by(&{&1.from, &1.to})
  end

  def edges(_nodes, _layer, _dependencies), do: []

  attr :nodes, :list, required: true
  attr :selected, :atom, required: true
  attr :layer, :atom, default: :conceptual
  attr :dependencies, :list, default: []
  attr :events, :list, default: []
  attr :bus_events, :list, default: []

  def graph(assigns) do
    nodes = layout(assigns.nodes)
    index = Map.new(nodes, &{&1.key, &1})
    observed = counts(assigns.layer, assigns.events, assigns.bus_events)

    nodes = Enum.map(nodes, &Map.put(&1, :observed, Map.get(observed, &1.key, 0)))

    wires =
      assigns.nodes
      |> edges(assigns.layer, assigns.dependencies)
      |> Enum.flat_map(fn edge ->
        case {Map.get(index, edge.from), Map.get(index, edge.to)} do
          {nil, _} -> []
          {_, nil} -> []
          {from, to} ->
            [
              Map.merge(edge, %{
                path: curve(from, to),
                selected: assigns.selected in [edge.from, edge.to]
              })
            ]
        end
      end)

    neighbors =
      wires
      |> Enum.flat_map(fn edge ->
        cond do
          edge.from == assigns.selected -> [edge.to]
          edge.to == assigns.selected -> [edge.from]
          true -> []
        end
      end)
      |> MapSet.new()

    assigns =
      assigns
      |> assign(:graph_nodes, nodes)
      |> assign(:wires, wires)
      |> assign(:neighbors, neighbors)
      |> assign(:received, Enum.sum(Map.values(observed)))

    ~H"""
    <div id="atlas-network-graph" class="min-w-0" data-layer={@layer}>
      <div class="overflow-x-auto rounded-xl border border-cyan-900/70 bg-[#050d19]">
        <div class="relative h-[600px] min-w-[940px] overflow-hidden"
          role="group" aria-label="Selectable Symbrella cognitive-region network">
          <svg class="pointer-events-none absolute inset-0 h-full w-full"
            viewBox="0 0 1000 600" preserveAspectRatio="none"
            aria-hidden="true">
            <defs>
              <pattern id="atlas-network-grid-pattern" width="40" height="40"
                patternUnits="userSpaceOnUse">
                <path d="M 40 0 L 0 0 0 40" fill="none"
                  stroke="#17324a" stroke-width="0.65" opacity="0.44" />
              </pattern>
              <radialGradient id="atlas-network-halo">
                <stop offset="0%" stop-color="#0e7490" stop-opacity="0.19" />
                <stop offset="100%" stop-color="#0e7490" stop-opacity="0" />
              </radialGradient>
              <marker id="atlas-network-arrow" markerWidth="7" markerHeight="7"
                refX="6" refY="3.5" orient="auto">
                <path d="M0,0 L7,3.5 L0,7 Z" fill="#67e8f9" />
              </marker>
            </defs>
            <rect width="1000" height="600" fill="#050d19" />
            <rect width="1000" height="600" fill="url(#atlas-network-grid-pattern)" />
            <ellipse cx="350" cy="265" rx="350" ry="255" fill="url(#atlas-network-halo)" />
            <ellipse cx="730" cy="355" rx="270" ry="205" fill="url(#atlas-network-halo)" />
            <text x="42" y="32" fill="#56849b" font-size="12" letter-spacing="3">
              FRONTAL / CONTROL
            </text>
            <text x="740" y="568" fill="#56849b" font-size="12" letter-spacing="3">
              TEMPORAL / MEMORY
            </text>

            <path :for={edge <- @wires} d={edge.path} fill="none"
              stroke={if edge.selected, do: "#67e8f9", else: "#28738c"}
              stroke-width={if edge.selected, do: "3.2", else: "1.7"}
              stroke-opacity={if edge.selected, do: "0.95", else: "0.48"}
              stroke-dasharray={if edge.directed, do: "7 5", else: nil}
              marker-end={if edge.directed, do: "url(#atlas-network-arrow)", else: nil}
              stroke-linecap="round"
            />
          </svg>

          <button :for={node <- @graph_nodes}
            type="button"
            id={"atlas-region-#{node.key}"}
            phx-click="select-region" phx-value-region={node.key}
            aria-pressed={to_string(@selected == node.key)}
            aria-label={"Inspect #{node.label} cognitive region"}
            title={"#{node.label} — #{node.subtitle}"}
            style={"left: #{node.left}; top: #{node.top};"}
            class={[
              "absolute z-10 flex h-[76px] w-[76px] -translate-x-1/2 -translate-y-1/2 flex-col items-center justify-center rounded-full border-2 px-1 text-center shadow-lg transition-colors duration-200 focus:outline-none focus:ring-4 focus:ring-white/80 motion-reduce:transition-none",
              @selected == node.key && "border-emerald-300 bg-emerald-900 text-white shadow-emerald-400/40 ring-4 ring-emerald-500/30",
              @selected != node.key && MapSet.member?(@neighbors, node.key) && "border-cyan-300 bg-cyan-950 text-cyan-100 shadow-cyan-400/30",
              @selected != node.key && node.observed > 0 && @layer in [:telemetry, :bus] && "border-amber-300 bg-amber-950 text-amber-100 shadow-amber-500/30",
              @selected != node.key && not MapSet.member?(@neighbors, node.key) && not (node.observed > 0 && @layer in [:telemetry, :bus]) && "border-slate-600 bg-slate-900 text-slate-200 hover:border-cyan-300"
            ]}>
            <span :if={@layer in [:telemetry, :bus] and node.observed > 0}
              aria-hidden="true"
              class="pointer-events-none absolute inset-[-7px] rounded-full border border-amber-300/70 animate-pulse motion-reduce:animate-none">
            </span>
            <span class="relative max-w-full break-words text-[11px] font-bold leading-tight">
              {node.label}
            </span>
            <span :if={@layer in [:telemetry, :bus] and node.observed > 0}
              class="relative mt-1 text-[10px] font-semibold text-amber-200">
              {node.observed} seen
            </span>
          </button>
        </div>
      </div>
      <div class="mt-3 flex flex-wrap items-center gap-x-5 gap-y-2 text-xs text-zinc-400">
        <span><span class="font-semibold text-emerald-300">●</span> Selected region</span>
        <span :if={@layer in [:conceptual, :dependencies]}>
          <span class="font-semibold text-cyan-300">━</span>
          {length(@wires)} {if @layer == :dependencies, do: "compiled references", else: "documented links"}
        </span>
        <span :if={@layer in [:telemetry, :bus]}>
          <span class="font-semibold text-amber-300">◉</span>
          {@received} event-region observations in this session
        </span>
        <span>Brain-inspired layout; positions are illustrative, not anatomy or measured connections.</span>
      </div>
    </div>
    """
  end

  defp curve(from, to) do
    cx = (from.x + to.x) / 2 - (to.y - from.y) * 0.12
    cy = (from.y + to.y) / 2 + (to.x - from.x) * 0.12

    "M #{from.x} #{from.y} Q #{round(cx)} #{round(cy)} #{to.x} #{to.y}"
  end

  defp fallback(index, total) do
    angle = 2 * :math.pi() * index / total
    {round(500 + 395 * :math.cos(angle)), round(300 + 240 * :math.sin(angle))}
  end

  defp counts(:telemetry, events, _bus_events) do
    Enum.reduce(events, %{}, fn event, totals ->
      Enum.reduce(event.regions, totals, fn region, acc ->
        Map.update(acc, region, 1, &(&1 + 1))
      end)
    end)
  end

  defp counts(:bus, _events, bus_events) do
    Enum.reduce(bus_events, %{}, fn event, totals ->
      if is_nil(event.region) do
        totals
      else
        Map.update(totals, event.region, 1, &(&1 + 1))
      end
    end)
  end

  defp counts(_layer, _events, _bus_events), do: %{}
end
