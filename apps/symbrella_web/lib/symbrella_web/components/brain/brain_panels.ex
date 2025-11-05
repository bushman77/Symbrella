# apps/symbrella_web/lib/symbrella_web/components/brain/panels.ex
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
  alias Symbrella.Components.Regions

  # Public entry point used by BrainLive.render/1
  def brain(assigns) do
    assigns =
      assigns
      |> Map.put_new(:selected, :lifg)
      |> Map.put_new(:regions, []) # optional external region listing
      |> Map.put_new(:clock, %{})
      |> Map.put_new(:intent, %{})
      |> Map.put_new(:mood, %{levels: %{}, derived: %{}, tone: :neutral})
      |> Map.put_new(:auto, false)
      |> Map.put_new(:region, %{})
      |> Map.put_new(:region_state, %{})
      |> Map.put_new(:region_status, %{})   # ← status comes from LiveView
      |> Map.put_new(:region_tweaks, %{})   # optional per-region overrides
      |> Map.put_new(:svg_base, nil)        # optional override SVG string
      |> assign_viewbox()                   # put :vb from svg override or BaseArt
      |> Map.put_new(:pan, %{x: 0, y: 0})   # aligned by default
      |> Map.put_new(:scale, 1.00)

    ~H"""
    <div class="container mx-auto px-4 py-6 space-y-6">
      <.brain_header selected={@selected} regions={@regions} />
      <SymbrellaWeb.BrainHTML.hud_row clock={@clock} intent={@intent} mood={@mood} auto={@auto} />

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
        <Regions.region_summary
          selected={@selected}
          region={@region}
          state={@snapshot || @region_status || (@region_state && @region_state[:snapshot]) || %{}}
        />
      </div>

      <!-- Live state + Module Status -->
      <div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <SymbrellaWeb.BrainHTML.live_state_panel state={@region_state} />
        <SymbrellaWeb.BrainHTML.module_status_panel status={@region_status} selected={@selected} />
      </div>

      <!-- Mood only -->
      <div class="grid grid-cols-1">
        <SymbrellaWeb.BrainHTML.mood_panel mood={@mood} />
      </div>
    </div>
    """
  end

  # --- Header (moved) --------------------------------------------------------
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
    <div class="flex items-center justify-between">
      <div>
        <h1 class="text-2xl font-semibold">Brain Regions ↔ Symbrella Modules</h1>
        <p class="text-sm text-zinc-500">Systems overview and live signals</p>
      </div>

      <div class="flex items-center gap-2">
        <span class="text-sm text-zinc-500">Region:</span>
        <select
          class="border rounded px-2 py-1 text-sm"
          name="region"
          phx-change="select-region"
          value={Atom.to_string(@selected)}
        >
          <%= for {key, label} <- @options do %>
            <option value={Atom.to_string(key)} selected={key == @selected}><%= label %></option>
          <% end %>
        </select>
      </div>
    </div>
    """
  end

  # --- Brain map (moved) -----------------------------------------------------
  attr :svg_base, :any, required: false
  attr :vb, :map, default: %{minx: 0, miny: 0, w: 516, h: 406}
  attr :selected, :atom, default: :lifg
  attr :region_tweaks, :map, default: %{}
  attr :pan, :map, default: %{x: 0, y: 0}
  attr :scale, :float, default: 1.0
  defp brain_map(assigns) do
    selected   = assigns.selected || :lifg
    vb         = assigns.vb || %{minx: 0, miny: 0, w: 516, h: 406}
    pan        = assigns.pan || %{x: 0, y: 0}
    scale      = assigns.scale || 1.0
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

          <!-- Pan/scale applies to base art and overlays together -->
          <g
            transform={"translate(#{@pan.x},#{@pan.y}) scale(#{@scale})"}
            class="pointer-events-auto"
            vector-effect="non-scaling-stroke"
          >
            <!-- Base art (component returns a <g>) -->
            <BaseArt.group svg={@svg_base} />

            <!-- Region overlays (same coordinate system as base) -->
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
              <% fw   = if sel?, do: 2, else: 1 %>
              <% fop  = if sel?, do: "0.35", else: "0.20" %>
              <% style = "fill: #{fill}; fill-opacity: #{fop}; stroke: #{stroke}; stroke-width: #{fw};" %>
              <% {lx, ly} = defn.anchor %>

              <g id={"g-#{key}"} class="region" transform={"translate(#{t.dx},#{t.dy}) scale(#{t.s})"}>
                <path
                  id={"path-#{key}"}
                  d={defn.path}
                  vector-effect="non-scaling-stroke"
                  phx-click="select-region"
                  phx-value-region={Atom.to_string(key)}
                  class={if(@selected == key, do: "opacity-100 stroke-2", else: "opacity-70 hover:opacity-90")}
                  style={style}
                >
                  <title><%= String.upcase(Atom.to_string(key)) %></title>
                </path>
                <text class="r-label" x={lx} y={ly}><%= String.upcase(Atom.to_string(key)) %></text>
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

  # local helper so brain/1 can compute @vb without depending on BrainHTML internals
  defp assign_viewbox(assigns) do
    vb = BaseArt.viewbox(Map.get(assigns, :svg_base))
    Map.put(assigns, :vb, vb)
  end
end

