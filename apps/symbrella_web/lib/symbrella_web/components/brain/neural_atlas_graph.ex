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
  Small satellite nodes describe registered Elixir module membership.

  These spokes are inventory relationships, not telemetry, neural firing,
  module calls or dependencies. Unknown/new regions use stable fallback layout.
  """
  def satellites(nodes) when is_list(nodes) do
    nodes
    |> layout()
    |> Enum.flat_map(fn node ->
      modules = List.wrap(Map.get(node, :modules, []))

      modules
      |> Enum.with_index()
      |> Enum.map(fn {mod, index} ->
        # Explicitly deterministic; one visible satellite per registered module.
        angle = -:math.pi() / 2 + 2 * :math.pi() * index / length(modules)

        %{
          region: node.key,
          module: inspect(mod),
          x: round(node.x + 61 * :math.cos(angle)),
          y: round(node.y + 61 * :math.sin(angle)),
          hub_x: node.x,
          hub_y: node.y
        }
      end)
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

  def edges(nodes, :source, source_relations) do
    keys = MapSet.new(Enum.map(nodes, & &1.key))

    source_relations
    |> Enum.filter(fn edge ->
      MapSet.member?(keys, edge.from) and
        MapSet.member?(keys, edge.to) and edge.from != edge.to
    end)
    |> Enum.uniq_by(&{&1.from, &1.to, &1.kind})
    |> Enum.map(fn edge ->
      %{
        from: edge.from,
        to: edge.to,
        directed: edge.kind == :direct_call,
        kind: edge.kind,
        source: edge.source
      }
    end)
    |> Enum.sort_by(&{&1.from, &1.to, &1.kind})
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
  attr :sources, :list, default: []
  attr :events, :list, default: []
  attr :bus_events, :list, default: []
  attr :trace, :any, default: nil

  def graph(assigns) do
    nodes = layout(assigns.nodes)
    index = Map.new(nodes, &{&1.key, &1})
    observed = counts(assigns.layer, assigns.events, assigns.bus_events, assigns.trace)

    nodes = Enum.map(nodes, &Map.put(&1, :observed, Map.get(observed, &1.key, 0)))
    module_sparks = satellites(assigns.nodes)

    wires =
      assigns.nodes
      |> edges(assigns.layer,
        if(assigns.layer == :source, do: assigns.sources, else: assigns.dependencies))
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

    # The muted network remains *conceptual* context in evidence layers.
    # It is never presented as observed telemetry, Bus traffic or BEAM calls.
    context_wires =
      if assigns.layer == :conceptual do
        []
      else
        assigns.nodes
        |> edges(:conceptual, [])
        |> Enum.flat_map(fn edge ->
          case {Map.get(index, edge.from), Map.get(index, edge.to)} do
            {nil, _} -> []
            {_, nil} -> []
            {from, to} -> [%{path: curve(from, to)}]
          end
        end)
      end

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
      |> assign(:context_wires, context_wires)
      |> assign(:module_sparks, module_sparks)
      |> assign(:total_modules, length(module_sparks))
      |> assign(:neighbors, neighbors)
      |> assign(:received, Enum.sum(Map.values(observed)))

    ~H"""
    <div id="atlas-network-graph" class="min-w-0" data-layer={@layer}>
      <div class="overflow-x-auto rounded-2xl border border-cyan-700/50 bg-[#050d19] shadow-[inset_0_0_70px_rgba(6,182,212,0.06)]">
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
                <stop offset="0%" stop-color="#0e7490" stop-opacity="0.22" />
                <stop offset="100%" stop-color="#0e7490" stop-opacity="0" />
              </radialGradient>
              <radialGradient id="atlas-network-soma">
                <stop offset="0%" stop-color="#67e8f9" stop-opacity="0.16" />
                <stop offset="100%" stop-color="#67e8f9" stop-opacity="0" />
              </radialGradient>
              <filter id="atlas-network-wire-glow" x="-50%" y="-50%" width="200%" height="200%">
                <feGaussianBlur stdDeviation="3" />
              </filter>
              <marker id="atlas-network-arrow" markerWidth="7" markerHeight="7"
                refX="6" refY="3.5" orient="auto">
                <path d="M0,0 L7,3.5 L0,7 Z" fill="#67e8f9" />
              </marker>
            </defs>
            <rect width="1000" height="600" fill="#050d19" />
            <rect width="1000" height="600" fill="url(#atlas-network-grid-pattern)" />
            <ellipse cx="350" cy="265" rx="370" ry="255" fill="url(#atlas-network-halo)" />
            <ellipse cx="730" cy="355" rx="310" ry="230" fill="url(#atlas-network-halo)" />
            <text x="34" y="30" fill="#74a1b4" font-size="13" letter-spacing="2.5">
              COGNITIVE REGION NETWORK
            </text>
            <text x="34" y="51" fill="#56849b" font-size="11">
              {@layer |> Atom.to_string() |> String.upcase()} / REGISTERED STRUCTURE
            </text>

            <path :for={edge <- @context_wires} d={edge.path}
              data-edge-kind="conceptual-context" fill="none" stroke="#427184"
              stroke-width="1.3" stroke-opacity="0.3" stroke-dasharray="2 7"
              stroke-linecap="round" />

            <path :for={edge <- @wires} d={edge.path}
              data-edge-kind={
                cond do
                  assigns.layer == :source -> "source-#{edge.kind}"
                  edge.directed -> "beam"
                  true -> "conceptual"
                end
              }
              fill="none"
              stroke={if @layer == :source, do: "#c4b5fd", else: "#22d3ee"}
              stroke-width="8" stroke-opacity="0.13"
              filter="url(#atlas-network-wire-glow)"
              stroke-linecap="round" />

            <path :for={edge <- @wires} d={edge.path} fill="none"
              stroke={
                cond do
                  @layer == :source and edge.selected -> "#ddd6fe"
                  @layer == :source -> "#a78bfa"
                  edge.selected -> "#a5f3fc"
                  true -> "#389fbd"
                end
              }
              stroke-width={if edge.selected, do: "3.4", else: "1.8"}
              stroke-opacity={if edge.selected, do: "1", else: "0.65"}
              stroke-dasharray={
                if @layer == :source or edge.directed, do: "8 6", else: nil
              }
              marker-end={
                if @layer == :source, do: nil,
                  else: if(edge.directed, do: "url(#atlas-network-arrow)", else: nil)
              }
              stroke-linecap="round" />

            <circle :for={node <- @graph_nodes} cx={node.x} cy={node.y} r="59"
              fill="url(#atlas-network-soma)"
              stroke={if @selected == node.key, do: "#6ee7b7", else: "#3b94a9"}
              stroke-opacity={if @selected == node.key, do: "0.48", else: "0.22"}
              stroke-width="1.4" stroke-dasharray="3 7" />

            <g :for={spark <- @module_sparks} data-role="registered-module"
              data-region={spark.region} data-module={spark.module}>
              <path d={"M #{spark.hub_x} #{spark.hub_y} L #{spark.x} #{spark.y}"}
                stroke={if @selected == spark.region, do: "#c4b5fd", else: "#8b8cc8"}
                stroke-opacity={if @selected == spark.region, do: "0.8", else: "0.44"}
                stroke-width="1.4" />
              <circle cx={spark.x} cy={spark.y} r="10" fill="#171c3d"
                stroke={if @selected == spark.region, do: "#ddd6fe", else: "#a5b4fc"}
                stroke-width={if @selected == spark.region, do: "2.5", else: "1.4"} />
              <circle cx={spark.x} cy={spark.y} r="3.5" fill="#c4b5fd" />
              <title>Registered module: {spark.module}</title>
            </g>
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
              "absolute z-10 flex h-[88px] w-[88px] -translate-x-1/2 -translate-y-1/2 flex-col items-center justify-center rounded-full border-2 px-2 text-center shadow-xl transition-colors duration-200 focus:outline-none focus:ring-4 focus:ring-white/80 motion-reduce:transition-none",
              @selected == node.key && "border-emerald-300 bg-emerald-900 text-white shadow-emerald-400/40 ring-4 ring-emerald-500/30",
              @selected != node.key && MapSet.member?(@neighbors, node.key) && "border-cyan-300 bg-cyan-950 text-cyan-100 shadow-cyan-400/30",
              @selected != node.key && node.observed > 0 && @layer in [:telemetry, :bus, :trace] && "border-amber-300 bg-amber-950 text-amber-100 shadow-amber-500/30",
              @selected != node.key && not MapSet.member?(@neighbors, node.key) && not (node.observed > 0 && @layer in [:telemetry, :bus, :trace]) && "border-slate-600 bg-slate-900 text-slate-200 hover:border-cyan-300"
            ]}>
            <span :if={@layer in [:telemetry, :bus, :trace] and node.observed > 0}
              aria-hidden="true"
              class="pointer-events-none absolute inset-[-7px] rounded-full border-2 border-amber-300/75">
            </span>
            <span class="relative max-w-full break-words text-[12px] font-bold leading-tight">
              {node.label}
            </span>
            <span :if={@layer in [:telemetry, :bus, :trace] and node.observed > 0}
              class="relative mt-1 text-[10px] font-semibold text-amber-200">
              {node.observed} seen
            </span>
            <span :if={@layer in [:conceptual, :dependencies]}
              class="absolute -bottom-2 rounded-full border border-slate-600 bg-slate-950 px-2 py-0.5 text-[10px] font-semibold text-violet-200">
              {length(node.modules)} mod
            </span>
          </button>
        </div>
      </div>
      <div class="mt-3 flex flex-wrap items-center gap-x-5 gap-y-2 text-xs text-zinc-400">
        <span><span class="font-semibold text-emerald-300">●</span> Selected region</span>
        <span><span class="font-semibold text-violet-300">◉</span> {@total_modules} registered module satellites (spokes = membership)</span>
        <span :if={@layer in [:conceptual, :dependencies, :source]}>
          <span class="font-semibold text-cyan-300">━</span>
          {length(@wires)}
          {case @layer do
             :source -> "source-backed relationships"
             :dependencies -> "compiled references"
             _ -> "documented links"
           end}
        </span>
        <span :if={@layer in [:telemetry, :bus, :trace]}>
          <span class="font-semibold text-amber-300">◉</span>
          {@received}
          {if @layer == :trace, do: "instrumented region stages in the selected turn", else: "event-region observations in this session"}
        </span>
        <span :if={@layer != :conceptual}>
          <span class="font-semibold text-cyan-500">┄</span>
          Faint dotted links = conceptual reference only
        </span>
        <span>Positions are illustrative; links and satellites are not measured neural firing.</span>
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

  defp counts(:telemetry, events, _bus_events, _trace) do
    Enum.reduce(events, %{}, fn event, totals ->
      Enum.reduce(event.regions, totals, fn region, acc ->
        Map.update(acc, region, 1, &(&1 + 1))
      end)
    end)
  end

  defp counts(:bus, _events, bus_events, _trace) do
    Enum.reduce(bus_events, %{}, fn event, totals ->
      if is_nil(event.region) do
        totals
      else
        Map.update(totals, event.region, 1, &(&1 + 1))
      end
    end)
  end

  defp counts(:trace, _events, _bus_events, %{events: stages}) when is_list(stages) do
    Enum.reduce(stages, %{}, fn event, totals ->
      if event.status == :ok and is_atom(event.region) and event.region != nil do
        Map.update(totals, event.region, 1, &(&1 + 1))
      else
        totals
      end
    end)
  end

  defp counts(_layer, _events, _bus_events, _trace), do: %{}
end
