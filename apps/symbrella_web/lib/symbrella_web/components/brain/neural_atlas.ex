defmodule SymbrellaWeb.Components.Brain.NeuralAtlas do
  @moduledoc """
  Accessible, source-registry-backed cognitive map for the Brain dashboard.

  Region association links are conceptual navigation only. Live status,
  evidence and measured neuron activity remain separate dashboard panels.
  """
  use SymbrellaWeb, :html

  alias SymbrellaWeb.Region.Registry

  @related %{
    lifg: [:pmtg, :atl, :acc],
    pmtg: [:lifg, :temporal],
    atl: [:lifg, :temporal, :hippocampus],
    acc: [:lifg, :salience, :prefrontal],
    hippocampus: [:atl, :temporal, :prefrontal],
    prefrontal: [:acc, :salience, :basal_ganglia],
    salience: [:acc, :prefrontal, :thalamus],
    thalamus: [:salience, :basal_ganglia],
    basal_ganglia: [:thalamus, :prefrontal],
    amygdala: [:salience, :hippocampus]
  }

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
        related: Map.get(@related, key, [])
      }
    end)
  end

  attr :nodes, :list, required: true
  attr :selected, :atom, required: true

  def panel(assigns) do
    current = Enum.find(assigns.nodes, &(&1.key == assigns.selected))
    linked = if current, do: current.related, else: []
    assigns =
      assigns
      |> assign(:current, current)
      |> assign(:linked, linked)

    ~H"""
    <section id="brain-neural-atlas"
      data-brain-report-section="Neural Atlas Cognitive Regions"
      class="rounded-2xl border border-cyan-700/40 bg-zinc-950 p-4 text-zinc-100 sm:p-6">
      <div class="mb-5">
        <p class="text-xs font-semibold uppercase tracking-widest text-emerald-300">
          Cognitive software atlas
        </p>
        <h2 class="mt-1 text-2xl font-semibold">Symbrella Neural Atlas</h2>
        <p class="mt-2 text-sm leading-relaxed text-zinc-300">
          Select a cognitive-inspired region to explore its registered Elixir modules
          and related regions. Connections are conceptual navigation, not measured
          neural firing or verified execution dependencies.
        </p>
      </div>

      <div class="grid gap-5 lg:grid-cols-2">
        <div id="atlas-region-grid" class="grid gap-3 sm:grid-cols-2">
          <button :for={node <- @nodes} id={"atlas-region-#{node.key}"}
            type="button" phx-click="select-region" phx-value-region={node.key}
            aria-pressed={@selected == node.key}
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
          </button>
        </div>

        <aside id="atlas-inspector" class="min-w-0 rounded-xl border border-zinc-700 bg-zinc-900/75 p-4">
          <%= if @current do %>
            <h3 class="text-xl font-semibold text-emerald-200">{@current.label}</h3>
            <p class="mt-2 text-sm leading-relaxed text-zinc-200">
              {@current.description}
            </p>
            <h4 class="mt-5 text-sm font-semibold text-zinc-100">
              Registered modules
            </h4>
            <ul class="mt-2 space-y-2">
              <li :for={mod <- @current.modules}
                class="break-all rounded-lg border border-zinc-700 bg-zinc-950 px-3 py-2 font-mono text-xs text-cyan-200">
                {inspect(mod)}
              </li>
            </ul>
            <p :if={@current.modules == []} class="mt-2 text-xs text-zinc-400">
              No modules registered for this region.
            </p>
            <h4 class="mt-5 text-sm font-semibold text-zinc-100">
              Related regions
            </h4>
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
            <h4 class="mt-5 text-sm font-semibold text-zinc-100">
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
        The anatomical map, region runtime status, memory, blackboard and mood
        remain available elsewhere on this page. Region links are for exploration only.
      </p>
    </section>
    """
  end
end
