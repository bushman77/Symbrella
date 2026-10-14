# apps/symbrella_web/lib/symbrella_web/components/brain/brain_panels.ex
defmodule SymbrellaWeb.Components.Brain.Panels do
  @moduledoc """
  Extracted 'brain' presentation functions for the Brain dashboard.

  Contains only:
    • brain/1
    • brain_header/1
    • brain_map/1

  All other helpers (HUD, status, mood, etc.) remain in `SymbrellaWeb.BrainHTML`.
  """

  use SymbrellaWeb, :html

  alias SymbrellaWeb.Region.Registry, as: RegionRegistry
  alias SymbrellaWeb.Region.BaseArt
  alias SymbrellaWeb.Components.Brain.Regions

  def brain(assigns) do
    assigns =
      assigns
      |> Map.put_new(:selected, :lifg)
      |> Map.put_new(:regions, [])
      |> Map.put_new(:clock, %{})
      |> Map.put_new(:intent, %{})
      |> Map.put_new(:mood, %{levels: %{}, derived: %{}, tone: :neutral})
      |> Map.put_new(:auto, false)
      |> Map.put_new(:region, %{})
      |> Map.put_new(:region_state, %{})
      |> Map.put_new(:region_status, %{})
      |> Map.put_new(:region_tweaks, %{})
      |> Map.put_new(:svg_base, nil)
      |> Map.put_new(:bb_filter, "")
      |> Map.put_new(:wm, %{})
      |> assign_viewbox()
      |> Map.put_new(:pan, %{x: 0, y: 0})
      |> Map.put_new(:scale, 1.00)

    ~H"""
    <div class="container mx-auto px-4 py-6 space-y-6">
      <.brain_header selected={@selected} regions={@regions} />

      <div data-brain-report-section="HUD, Clock, Intent, Mood and Self-Model">
      <SymbrellaWeb.BrainHTML.hud_row
        clock={@clock}
        intent={@intent}
        mood={@mood}
        auto={@auto}
        snapshot={@snapshot}
        wm={@wm}
        self_portrait={assigns[:self_portrait]}
        self_model={assigns[:self_model]}
      />
      </div>

    <!-- Brain map + Region summary -->
      <div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <div class="border rounded-xl p-4 shadow-sm">
          <.brain_map
            svg_base={@svg_base}
            selected={@selected}
            region_tweaks={@region_tweaks}
            pan={@pan}
            scale={@scale}
            vb={@vb}
          />
        </div>

        <div class="relative h-full" data-brain-report-section="Selected Region Summary and Snapshot">
          <% copy_id = "region-summary-copy-#{@selected}" %>

          <%= if @region_state && map_size(@region_state) > 0 do %>
            <button
              type="button"
              class="absolute right-2 top-2 z-10 inline-flex items-center gap-1 rounded-md border border-zinc-300 bg-white/80 px-2 py-1 text-xs font-medium text-zinc-700 shadow-sm hover:bg-white focus:outline-none focus:ring-2 focus:ring-indigo-500"
              aria-label="Copy region state"
              onclick={"(function(){var el=document.getElementById('#{copy_id}'); if(!el || !navigator.clipboard) return; navigator.clipboard.writeText(el.innerText || '');})();"}
            >
              <.icon name="hero-clipboard-document" class="w-3 h-3" />
              <span>Copy</span>
            </button>
          <% end %>

          <div id={copy_id}>
            <Regions.region_summary
              selected={@selected}
              region={@region}
              state={
                @snapshot ||
                  @region_status ||
                  (@region_state && @region_state[:snapshot]) ||
                  %{}
              }
            />
          </div>
        </div>
      </div>
      
    <!-- Blackboard + WM -->
      <div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <div data-brain-report-section="Complete Blackboard Feed" data-brain-report-source="#blackboard-full-copy-target, #blackboard-copy-target">
        <SymbrellaWeb.BrainHTML.blackboard_panel
          events={get_in(@region_state, [:workspace]) || []}
          filter={@bb_filter}
          limit={50}
        />
        </div>
        <div data-brain-report-section="Working Memory" data-brain-report-source="#wm-copy-target">
          <SymbrellaWeb.BrainHTML.wm_panel wm={@wm} />
        </div>
      </div>
      
    <!-- Live state + Module Status -->
      <div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <div data-brain-report-section="Live Region State">
        <SymbrellaWeb.BrainHTML.live_state_panel
          state={Map.merge(@region_status || %{}, %{state: @snapshot})}
        />
        </div>
        <div data-brain-report-section="Module Status">
          <SymbrellaWeb.BrainHTML.module_status_panel status={@region_status} selected={@selected} />
        </div>
      </div>
      
    <!-- Mood only -->
      <div class="grid grid-cols-1">
        <div data-brain-report-section="Current Mood and Neuromodulators">
          <SymbrellaWeb.BrainHTML.mood_panel mood={@mood} />
        </div>
      </div>
    </div>
    """
  end

  defp brain_header(assigns) do
    selected = assigns[:selected] || :lifg
    base_opts = Regions.region_options(assigns[:regions] || [])

    opts =
      if Enum.any?(base_opts, fn {k, _} -> k == selected end) do
        base_opts
      else
        [{selected, Regions.label_for(selected)} | base_opts]
      end

    assigns =
      assigns
      |> Map.put(:selected, selected)
      |> Map.put(:options, opts)

    ~H"""
    <div class="flex flex-col gap-3 sm:flex-row sm:items-center sm:justify-between">
      <div>
        <h1 class="text-2xl font-semibold">Brain Regions ↔ Symbrella Modules</h1>
        <p class="text-sm text-zinc-500">Systems overview and live signals</p>
      </div>

      <div class="flex flex-wrap items-center justify-end gap-2">
        <button
          id="brain-report-copy-btn"
          type="button"
          aria-label="Copy complete Brain diagnostics report to clipboard"
          title="Copy all Brain dashboard diagnostics, including unfiltered blackboard events"
          class="inline-flex items-center gap-1.5 rounded-md border border-indigo-300 bg-indigo-50 px-3 py-2 text-xs font-semibold text-indigo-900 hover:bg-indigo-100 focus:outline-none focus:ring-2 focus:ring-indigo-500 dark:border-indigo-700 dark:bg-indigo-950 dark:text-indigo-100"
        >
          <.icon name="hero-clipboard-document-check" class="h-4 w-4" />
          <span data-brain-copy-label>Copy Full Brain Report</span>
        </button>
        <button
          id="brain-report-view-btn"
          type="button"
          aria-label="View, select, or download complete Brain diagnostics"
          title="View, select, or download the complete report even if clipboard access is blocked"
          class="inline-flex items-center gap-1.5 rounded-md border border-zinc-300 px-3 py-2 text-xs font-semibold text-zinc-800 hover:bg-zinc-100 dark:border-zinc-700 dark:text-zinc-200 dark:hover:bg-zinc-800"
        >
          <.icon name="hero-document-text" class="h-4 w-4" />
          <span>View / Save Report</span>
        </button>
        <span class="text-sm text-zinc-500">Region:</span>
        <select
          class="border rounded px-2 py-1 text-sm"
          name="region"
          phx-change="select-region"
          value={Atom.to_string(@selected)}
        >
          <%= for {key, label} <- @options do %>
            <option value={Atom.to_string(key)} selected={key == @selected}>{label}</option>
          <% end %>
        </select>
      </div>
    </div>
    """
  end

  attr :svg_base, :any, required: false
  attr :vb, :map, default: %{minx: 0, miny: 0, w: 516, h: 406}
  attr :selected, :atom, default: :lifg
  attr :region_tweaks, :map, default: %{}
  attr :pan, :map, default: %{x: 0, y: 0}
  attr :scale, :float, default: 1.0

  defp brain_map(assigns) do
    selected = assigns.selected || :lifg
    vb = assigns.vb || %{minx: 0, miny: 0, w: 516, h: 406}
    pan = assigns.pan || %{x: 0, y: 0}
    scale = assigns.scale || 1.0
    draw_order = Regions.draw_regions(selected)

    assigns =
      assigns
      |> assign(:selected, selected)
      |> assign(:vb, vb)
      |> assign(:view_w, vb.w |> round())
      |> assign(:view_h, vb.h |> round())
      |> assign(:pan, pan)
      |> assign(:scale, scale)
      |> assign(:draw_order, draw_order)

    ~H"""
    <div class="p-3">
      <div class="flex justify-between items-start mb-2">
        <h2 class="font-semibold">Brain map</h2>
        <span class="text-xs text-zinc-500">Left-lateral human brain (vector)</span>
      </div>

      <div
        class="relative bg-white rounded border overflow-hidden"
        style={"aspect-ratio: #{@view_w} / #{@view_h}; min-height: 260px;"}
      >
        <svg
          viewBox={"#{@vb.minx} #{@vb.miny} #{@vb.w} #{@vb.h}"}
          class="absolute inset-0 w-full h-full"
          preserveAspectRatio="xMidYMid meet"
        >
          <style>
            .r-label{opacity:0; font:10px ui-monospace, SFMono-Regular, Menlo, monospace; fill:#111;}
            g.region:hover .r-label{opacity:.9}
          </style>

          <g
            transform={"translate(#{@pan.x},#{@pan.y}) scale(#{@scale})"}
            class="pointer-events-auto"
            vector-effect="non-scaling-stroke"
          >
            <BaseArt.group svg={@svg_base} />

            <%= for key <- @draw_order do %>
              <% defn = RegionRegistry.defn(key) %>

              <% t0 =
                case defn.tweak do
                  m when is_map(m) -> m
                  _ -> %{}
                end %>
              <% tovr = Map.get(@region_tweaks, key, %{}) %>
              <% t = Map.merge(%{dx: 0, dy: 0, s: 1.0}, Map.merge(t0, tovr)) %>

              <% {fill, stroke} = defn.colors %>
              <% sel? = @selected == key %>
              <% fw = if sel?, do: 2, else: 1 %>
              <% fop = if sel?, do: "0.35", else: "0.20" %>
              <% style =
                "fill: #{fill}; fill-opacity: #{fop}; stroke: #{stroke}; stroke-width: #{fw};" %>
              <% {lx, ly} = defn.anchor %>

              <g id={"g-#{key}"} class="region" transform={"translate(#{t.dx},#{t.dy}) scale(#{t.s})"}>
                <path
                  id={"path-#{key}"}
                  d={defn.path}
                  vector-effect="non-scaling-stroke"
                  phx-click="select-region"
                  phx-value-region={Atom.to_string(key)}
                  class={
                    if(@selected == key,
                      do: "opacity-100 stroke-2",
                      else: "opacity-70 hover:opacity-90"
                    )
                  }
                  style={style}
                >
                  <title>{String.upcase(Atom.to_string(key))}</title>
                </path>
                <text class="r-label" x={lx} y={ly}>{String.upcase(Atom.to_string(key))}</text>
              </g>
            <% end %>
          </g>
        </svg>
      </div>

      <p class="text-[11px] text-zinc-500 mt-2">
        Single SVG, shared viewBox — zero drift across devices. Use per-region
        tweaks for tiny nudges only; prefer adjusting the base <code>viewBox</code> for
        global framing.
      </p>
    </div>
    """
  end

  defp assign_viewbox(assigns) do
    vb = BaseArt.viewbox(Map.get(assigns, :svg_base))
    Map.put(assigns, :vb, vb)
  end
end
