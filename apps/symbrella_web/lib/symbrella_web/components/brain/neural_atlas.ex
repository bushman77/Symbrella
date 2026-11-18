defmodule SymbrellaWeb.Components.Brain.NeuralAtlas do
  @moduledoc """
  Accessible, source-registry-backed cognitive map for the Brain dashboard.

  Region association links are conceptual navigation only. Live status,
  evidence and measured neuron activity remain separate dashboard panels.
  """
  use SymbrellaWeb, :html

  alias SymbrellaWeb.Region.Registry
  alias SymbrellaWeb.Components.Brain.NeuralAtlasGraph

  @spec regions() :: list(map())
  def regions do
    Registry.keys()
    |> Enum.sort_by(&Atom.to_string/1)
    |> Enum.map(fn key ->
      defn = Registry.defn(key)
      %{
        key: key,
        label: Registry.label_for(key),
        subtitle: Map.get(defn, :subtitle, ""),
        description: Map.get(defn, :desc, ""),
        modules: List.wrap(Map.get(defn, :modules, [])),
        telemetry: List.wrap(Map.get(defn, :telemetry, [])),
        related:
          defn
          |> Map.get(:related, [])
          |> List.wrap()
          |> Enum.filter(&Registry.available?/1)
      }
    end)
  end

  attr :nodes, :list, required: true
  attr :selected, :atom, required: true
  attr :layer, :atom, default: :conceptual
  attr :display, :atom, default: :graph
  attr :dependencies, :list, default: []
  attr :events, :list, default: []
  attr :bus_events, :list, default: []

  def panel(assigns) do
    current = Enum.find(assigns.nodes, &(&1.key == assigns.selected))

    linked =
      (case assigns.layer do
         :dependencies ->
           for edge <- assigns.dependencies, edge.from == assigns.selected, do: edge.to

         :telemetry ->
           for event <- assigns.events, region <- event.regions, do: region

         :bus ->
           for event <- assigns.bus_events, not is_nil(event.region), do: event.region

         _ ->
           if current, do: current.related, else: []
       end)
      |> Enum.uniq()

    assigns =
      assigns
      |> assign(:current, current)
      |> assign(:linked, linked)
      |> assign(:outgoing, Enum.filter(assigns.dependencies, &(&1.from == assigns.selected)))
      |> assign(:incoming, Enum.filter(assigns.dependencies, &(&1.to == assigns.selected)))
      |> assign(:observations, Enum.filter(assigns.events, &(assigns.selected in &1.regions)))
      |> assign(:bus_observations,
        Enum.filter(assigns.bus_events, &(&1.region == assigns.selected)))
      |> assign(:unattributed_bus_events,
        assigns.bus_events |> Enum.filter(&is_nil(&1.region)) |> Enum.take(5))

    ~H"""
    <section id="brain-neural-atlas"
      data-brain-report-section="Neural Atlas Cognitive Regions"
      class="rounded-2xl border border-cyan-700/40 bg-zinc-950 p-4 text-zinc-100 sm:p-6">
      <div class="mb-5">
        <p class="text-xs font-semibold uppercase tracking-widest text-emerald-300">
          Cognitive software atlas
        </p>
        <h2 class="mt-1 text-2xl font-semibold sm:text-3xl">Symbrella Neural Atlas</h2>
        <p class="mt-2 text-sm leading-relaxed text-zinc-300 sm:text-base">
          Explore registered regions through documented associations, compiled BEAM references
          and observed telemetry. Each layer represents a different level of evidence.
        </p>
      </div>

      <nav id="atlas-evidence-layers" aria-label="Neural Atlas evidence layers"
        class="mb-4 flex flex-wrap gap-2">
        <button :for={{layer, name} <- [
            {:conceptual, "Conceptual links"},
            {:dependencies, "BEAM references"},
            {:telemetry, "Live telemetry"},
            {:bus, "Bus events"}
          ]}
          type="button"
          id={"atlas-layer-#{layer}"}
          phx-click="atlas-layer"
          phx-value-layer={layer}
          aria-pressed={to_string(@layer == layer)}
          class={[
            "min-h-11 rounded-lg border px-3 py-2 text-sm font-medium focus:outline-none focus:ring-2 focus:ring-emerald-300",
            @layer == layer && "border-emerald-400 bg-emerald-950 text-emerald-100",
            @layer != layer && "border-zinc-700 text-zinc-300 hover:border-cyan-500"
          ]}>
          {name}
        </button>
      </nav>

      <p id="atlas-layer-explanation" class="mb-4 text-sm leading-relaxed text-zinc-400">
        <%= case @layer do %>
          <% :dependencies -> %>
            BEAM imports are static references to external functions between registered modules.
            They do not establish execution order, dynamic dispatch, or calls that actually ran.
          <% :telemetry -> %>
            Only registered telemetry topics received by this connected page are shown.
            A received event is an observation, not proof of neural firing or a complete execution trace.
          <% :bus -> %>
            Recent Brain.Bus messages exclude telemetry and contain only receipt time, kind and
            any explicitly attributed registered region. They are not verified function executions.
          <% _ -> %>
            Region associations provide conceptual navigation links, not measured neural firing
            or verified execution dependencies.
        <% end %>
      </p>

      <nav id="atlas-display-modes" aria-label="Neural Atlas display mode"
        class="mb-4 flex flex-wrap items-center gap-2">
        <button :for={{mode, title} <- [{:graph, "Network graph"}, {:list, "Region list"}]}
          type="button" id={"atlas-display-#{mode}"}
          phx-click="atlas-display" phx-value-display={mode}
          aria-pressed={to_string(@display == mode)}
          class={[
            "min-h-11 rounded-lg border px-4 py-2 text-sm font-semibold focus:outline-none focus:ring-2 focus:ring-emerald-300",
            @display == mode && "border-emerald-300 bg-emerald-950 text-white",
            @display != mode && "border-zinc-700 text-zinc-300 hover:border-cyan-500"
          ]}>
          {title}
        </button>
        <span class="text-xs text-zinc-400">
          Choose any node to inspect its registered modules and evidence.
        </span>
      </nav>

      <div class={[
        "grid gap-5",
        @display == :list && "lg:grid-cols-2",
        @display == :graph && "2xl:grid-cols-[minmax(0,3fr)_minmax(340px,1fr)]"
      ]}>
        <NeuralAtlasGraph.graph :if={@display == :graph}
          nodes={@nodes}
          selected={@selected}
          layer={@layer}
          dependencies={@dependencies}
          events={@events}
          bus_events={@bus_events}
        />

        <div :if={@display == :list} id="atlas-region-grid" class="grid gap-3 sm:grid-cols-2">
          <button :for={node <- @nodes} id={"atlas-region-#{node.key}"}
            type="button" phx-click="select-region" phx-value-region={node.key}
            aria-pressed={to_string(@selected == node.key)}
            class={[
              "min-h-28 rounded-xl border p-4 text-left transition-colors focus:outline-none focus:ring-2 focus:ring-emerald-300",
              @selected == node.key && "border-emerald-400 bg-emerald-950/50",
              @selected != node.key && node.key in @linked && "border-cyan-400/70 bg-cyan-950/30",
              @selected != node.key && node.key not in @linked && "border-zinc-700 bg-zinc-900 hover:border-cyan-500"
            ]}>
            <span class="flex items-center justify-between gap-2">
              <span class="inline-flex size-10 items-center justify-center rounded-full bg-cyan-900 text-sm font-semibold text-emerald-200">
                {node.key |> Atom.to_string() |> String.slice(0, 2) |> String.upcase()}
              </span>
              <span class="text-xs text-zinc-400">{length(node.modules)} modules</span>
            </span>
            <strong class="mt-3 block text-base text-white">{node.label}</strong>
            <span class="mt-1 block text-xs leading-relaxed text-zinc-300">
              {node.subtitle}
            </span>
            <span :if={@layer == :telemetry and node.key in @linked}
              class="mt-2 inline-block text-xs font-semibold text-emerald-300">
              Observed this session
            </span>
          </button>
        </div>

        <aside id="atlas-inspector" class="min-w-0 rounded-xl border border-zinc-700 bg-zinc-900/75 p-5">
          <%= if @current do %>
            <h3 class="text-2xl font-semibold text-emerald-200">{@current.label}</h3>
            <p class="mt-2 text-base leading-relaxed text-zinc-200">
              {@current.description}
            </p>

            <h4 class="mt-5 text-base font-semibold text-zinc-100">Registered modules</h4>
            <ul class="mt-2 space-y-2">
              <li :for={mod <- @current.modules}
                class="break-all rounded-lg border border-zinc-700 bg-zinc-950 px-3 py-2 font-mono text-sm text-cyan-200">
                {inspect(mod)}
              </li>
            </ul>
            <p :if={@current.modules == []} class="mt-2 text-xs text-zinc-400">
              No modules registered for this region.
            </p>

            <%= if @layer == :conceptual do %>
              <h4 class="mt-5 text-base font-semibold text-zinc-100">Related regions</h4>
              <div class="mt-2 flex flex-wrap gap-2">
                <button :for={key <- @linked} type="button"
                  phx-click="select-region" phx-value-region={key}
                  class="min-h-11 rounded-lg border border-cyan-700 px-3 py-2 text-xs text-cyan-200 hover:bg-cyan-950">
                  {Registry.label_for(key)}
                </button>
              </div>
              <p :if={@linked == []} class="mt-2 text-xs text-zinc-400">
                No conceptual links documented.
              </p>
            <% end %>

            <%= if @layer == :dependencies do %>
              <h4 class="mt-5 text-base font-semibold text-zinc-100">
                Outgoing static references ({length(@outgoing)})
              </h4>
              <ul id="atlas-outgoing-references" class="mt-2 space-y-2">
                <li :for={edge <- @outgoing}
                  class="break-all rounded-lg border border-zinc-700 p-2 text-xs text-cyan-200">
                  <span class="block font-mono">{edge.source} → {edge.target}.{edge.call}</span>
                  <button type="button" phx-click="select-region" phx-value-region={edge.to}
                    class="mt-1 min-h-11 rounded border border-cyan-700 px-3 py-2 text-emerald-200">
                    Inspect {Registry.label_for(edge.to)}
                  </button>
                </li>
              </ul>
              <p :if={@outgoing == []} class="mt-2 text-xs text-zinc-400">
                No cross-region BEAM references found. Dynamic calls and messages are not covered.
              </p>
              <h4 class="mt-5 text-base font-semibold text-zinc-100">
                Incoming static references ({length(@incoming)})
              </h4>
              <ul class="mt-2 space-y-2">
                <li :for={edge <- @incoming}
                  class="break-all font-mono text-xs text-zinc-300">
                  {edge.source} → {edge.target}.{edge.call}
                </li>
              </ul>
              <p :if={@incoming == []} class="mt-2 text-xs text-zinc-400">
                No incoming cross-region BEAM references found.
              </p>
            <% end %>

            <%= if @layer == :telemetry do %>
              <h4 class="mt-5 text-base font-semibold text-zinc-100">
                Received topic events ({length(@observations)})
              </h4>
              <ul id="atlas-observed-telemetry" class="mt-2 max-h-64 space-y-2 overflow-y-auto">
                <li :for={event <- @observations}
                  class="break-all rounded-lg border border-zinc-700 px-3 py-2 text-xs">
                  <time class="text-emerald-200">{event.seen_at}</time>
                  <span class="block font-mono text-cyan-200">{event.topic}</span>
                </li>
              </ul>
              <p :if={@observations == []} class="mt-2 text-xs text-zinc-400">
                No matching registered telemetry observed since this page connected.
              </p>
            <% end %>

            <%= if @layer == :bus do %>
              <h4 class="mt-5 text-base font-semibold text-zinc-100">
                Region-attributed bus events ({length(@bus_observations)})
              </h4>
              <ul id="atlas-observed-bus" class="mt-2 max-h-64 space-y-2 overflow-y-auto">
                <li :for={event <- @bus_observations}
                  class="rounded-lg border border-zinc-700 p-2 text-xs">
                  <time class="text-emerald-200">{event.seen_at}</time>
                  <span class="ml-2 font-mono text-cyan-200">{event.kind}</span>
                </li>
              </ul>
              <p :if={@bus_observations == []} class="mt-2 text-xs text-zinc-400">
                No bus events explicitly attributed to this region since connecting.
              </p>
              <h4 class="mt-5 text-base font-semibold text-zinc-100">
                Unattributed recent messages ({length(@unattributed_bus_events)} shown)
              </h4>
              <ul class="mt-2 space-y-1">
                <li :for={event <- @unattributed_bus_events}
                  class="font-mono text-xs text-zinc-400">
                  {event.seen_at} · {event.kind} (no region attribution)
                </li>
              </ul>
              <p :if={@unattributed_bus_events == []} class="mt-2 text-xs text-zinc-400">
                No unattributed bus events observed.
              </p>
            <% end %>

            <h4 class="mt-5 text-base font-semibold text-zinc-100">
              Registered telemetry topics
            </h4>
            <ul class="mt-2 space-y-1">
              <li :for={topic <- @current.telemetry}
                class="break-all font-mono text-xs text-zinc-300">{inspect(topic)}</li>
            </ul>
            <p :if={@current.telemetry == []} class="mt-2 text-xs text-zinc-400">
              None registered.
            </p>
          <% else %>
            <p class="text-sm text-zinc-300">Select a registered region to inspect it.</p>
          <% end %>
        </aside>
      </div>
      <p class="mt-5 border-t border-zinc-700 pt-3 text-xs leading-relaxed text-zinc-400">
        The anatomical map, runtime status, memory, blackboard and mood remain available
        elsewhere on this page. Evidence layers are read-only.
      </p>
    </section>
    """
  end
end
