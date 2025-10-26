defmodule SymbrellaWeb.BrainHTML do
  @moduledoc """
  Presentation for the Brain dashboard. Pure HEEx & helpers.

  Uses per-file region modules via SymbrellaWeb.Region.Registry.
  The **base brain art and region overlays render inside one `<svg>`** that
  shares a single `viewBox` → no cross-device drift.
  """

  use SymbrellaWeb, :html

  alias SymbrellaWeb.Region.Registry, as: RegionRegistry
  alias SymbrellaWeb.Region.BaseArt

  # Prefrontal macro → children; we avoid double-painting by only drawing the
  # macro overlay when :prefrontal is selected.
  @region_groups %{
    prefrontal: ~w(dlpfc vmpfc dmpfc fpc lifg ofc)a
  }

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
      |> Map.put_new(:lifg_last, %{})
      |> Map.put_new(:hippo, %{window: []})
      |> Map.put_new(:hippo_metrics, %{})
      |> Map.put_new(:snapshot, nil)
      |> Map.put_new(:region_tweaks, %{})   # optional per-region overrides
      |> Map.put_new(:svg_base, nil)        # optional override SVG string
      |> assign_viewbox()                   # put :vb from svg override or BaseArt
      |> Map.put_new(:pan, %{x: 0, y: 0})   # aligned by default
      |> Map.put_new(:scale, 1.00)

    ~H"""
    <div class="container mx-auto px-4 py-6 space-y-6">
      <.brain_header selected={@selected} regions={@regions} />
      <.hud_row clock={@clock} intent={@intent} mood={@mood} auto={@auto} />

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
        <.region_summary selected={@selected} region={@region} />
      </div>

      <!-- Live state + LIFG decision -->
      <div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <.live_state_panel state={@region_state} />
        <.lifg_panel lifg_last={@lifg_last} />
      </div>

      <!-- Existing panels -->
      <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
        <.mood_panel mood={@mood} />
        <.snapshot_panel snapshot={@snapshot} />
      </div>

      <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
        <.hippo_panel hippo={@hippo} metrics={@hippo_metrics} />
        <div />
      </div>
    </div>
    """
  end

  # --- Header ---------------------------------------------------------------
  defp brain_header(assigns) do
    assigns =
      assigns
      |> Map.put(:selected, assigns[:selected] || :lifg)
      |> Map.put(:regions, assigns[:regions] || [])

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
          <%= for {key, label} <- region_options(@regions) do %>
            <option value={Atom.to_string(key)} selected={key == @selected}><%= label %></option>
          <% end %>
        </select>
      </div>
    </div>
    """
  end

  # --- HUD row ---------------------------------------------------------------
  defp hud_row(assigns) do
    clock   = assigns[:clock] || %{}
    intent  = assigns[:intent] || %{}
    mood    = assigns[:mood] || %{derived: %{}}
    auto    = assigns[:auto]
    d       = Map.get(mood, :derived, %{}) || %{}

    seq  = Map.get(clock, :seq, "—")
    hz   = Map.get(clock, :hz,  "—") |> fmt
    dtms = Map.get(clock, :dt_ms, "—")
    phi  = Map.get(clock, :phi, "—") |> fmt

    label = Map.get(intent, :label) || Map.get(intent, :intent)
    kw    = Map.get(intent, :keyword)
    src   = Map.get(intent, :source) || Map.get(intent, :src)
    conf  = Map.get(intent, :confidence)

    ~H"""
    <div class="flex flex-wrap items-center gap-2">
      <.chip>
        <span class="font-semibold">Clock</span>
        <span class="opacity-70">seq</span> <%= seq %>
        <span class="opacity-70">Hz</span> <%= hz %>
        <span class="opacity-70">Δt</span> <%= dtms %>ms
        <span class="opacity-70">ϕ</span> <%= phi %>
      </.chip>

      <.chip>
        <span class="font-semibold">Intent</span>
        <%= if is_binary(label) do %>
          <code class="px-1"><%= label %></code>
        <% else %>
          <span>—</span>
        <% end %>
        <%= if is_binary(kw) do %><span>· <%= kw %></span><% end %>
        <%= if is_number(conf) do %>· <%= fmt_pct(conf) %><% else %>· --<% end %>
        <%= if is_binary(src) do %>
          <span class="opacity-60">( <%= src %> )</span>
        <% end %>
      </.chip>

      <.chip>
        <span class="font-semibold">Mood</span>
        Expl: <%= fmt(d[:exploration]) %>
        · Inhib: <%= fmt(d[:inhibition]) %>
        · Vigil: <%= fmt(d[:vigilance]) %>
        · Plast: <%= fmt(d[:plasticity]) %>
      </.chip>

      <.chip>
        <button class="text-xs border px-2 py-1 rounded" phx-click="refresh">Refresh</button>
        <span class="opacity-60">Auto:</span> <%= (auto in [true, "on", "ON"]) && "ON" || "OFF" %>
      </.chip>
    </div>
    """
  end

  # HUD chip component
  slot :inner_block, required: true
  defp chip(assigns) do
    ~H"""
    <span class="text-xs px-2 py-1 rounded-md border bg-white/70 text-zinc-700 flex items-center gap-1">
      <%= render_slot(@inner_block) %>
    </span>
    """
  end

  # --- Draw order helper (prefrontal hierarchy) ------------------------------
  defp draw_regions(selected) do
    base  = [:frontal, :temporal, :cerebellum]
    deep  = [:thalamus, :basal_ganglia, :hippocampus]
    other = [:pmtg, :atl, :acc]
    pfc_children = Map.get(@region_groups, :prefrontal, [])

    order =
      if selected == :prefrontal,
        do: base ++ [:prefrontal] ++ pfc_children ++ deep ++ other,
        else: base ++ pfc_children ++ deep ++ other

    Enum.filter(order, &RegionRegistry.available?/1)
  end

  # --- Brain map (single SVG: base + overlays share one viewBox) -------------
  attr :svg_base, :any, required: false
  attr :vb, :map, default: %{minx: 0, miny: 0, w: 516, h: 406}
  attr :selected, :atom, default: :lifg
  attr :region_tweaks, :map, default: %{}
  attr :pan, :map, default: %{x: 0, y: 0}
  attr :scale, :float, default: 1.0
  defp brain_map(assigns) do
    selected = assigns.selected || :lifg
    vb       = assigns.vb || %{minx: 0, miny: 0, w: 516, h: 406}
    pan      = assigns.pan || %{x: 0, y: 0}
    scale    = assigns.scale || 1.0
    draw_order = draw_regions(selected)

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

      <div class="relative bg-white rounded border overflow-hidden" style={"aspect-ratio: #{@view_w} / #{@view_h}; min-height: 260px;"}>
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
          <g transform={"translate(#{@pan.x},#{@pan.y}) scale(#{@scale})"} class="pointer-events-auto" vector-effect="non-scaling-stroke">
            <!-- Base art (component returns a <g>) -->
            <BaseArt.group svg={@svg_base} />

            <!-- Region overlays (same coordinate system as base) -->
            <%= for key <- @draw_order do %>
              <% defn = RegionRegistry.defn(key) %>

              <% # Merge optional per-render overrides (%{key => %{dx/dy/s}}) safely %>
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

              <g class="region" transform={"translate(#{t.dx},#{t.dy}) scale(#{t.s})"}>
                <path
                  d={defn.path}
                  vector-effect="non-scaling-stroke"
                  phx-click="select-region"
                  phx-value-region={Atom.to_string(key)}
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
        tweaks for tiny nudges only; prefer adjusting the base `viewBox` for
        global framing.
      </p>
    </div>
    """
  end

  # --- Region summary --------------------------------------------------------
  defp region_summary(assigns) do
    region   = assigns[:region] || %{}
    selected = assigns[:selected] || :lifg

    title    = Map.get(region, :title)    || Atom.to_string(selected) |> String.upcase()
    subtitle = Map.get(region, :subtitle) || default_subtitle(selected)
    desc     = Map.get(region, :desc)     || default_region_desc(selected)

    mods     = Map.get(region, :modules, [])
    telem    = Map.get(region, :telemetry, [])
    confs    = Map.get(region, :config_examples, [])

    ~H"""
    <div class="border rounded-xl p-4 shadow-sm">
      <h2 class="font-semibold"> <%= title %> — <%= subtitle %> </h2>
      <p class="text-sm text-zinc-600 mb-3"><%= desc %></p>

      <div class="grid grid-cols-1 md:grid-cols-3 gap-3">
        <div>
          <h3 class="text-sm text-zinc-500 mb-1">Modules</h3>
          <%= if mods == [] do %>
            <div class="text-sm text-zinc-400">None</div>
          <% else %>
            <ul class="text-xs space-y-1">
              <%= for m <- mods do %>
                <li><code><%= m %></code></li>
              <% end %>
            </ul>
          <% end %>
        </div>

        <div>
          <h3 class="text-sm text-zinc-500 mb-1">Telemetry</h3>
          <%= if telem == [] do %>
            <div class="text-sm text-zinc-400">N/A</div>
          <% else %>
            <ul class="text-xs space-y-1">
              <%= for t <- telem do %>
                <li><code><%= t %></code></li>
              <% end %>
            </ul>
          <% end %>
        </div>

        <div>
          <h3 class="text-sm text-zinc-500 mb-1">Config (examples)</h3>
          <%= if confs == [] do %>
            <div class="text-sm text-zinc-400">No examples</div>
          <% else %>
            <ul class="text-xs space-y-1">
              <%= for c <- confs do %>
                <li><code><%= c %></code></li>
              <% end %>
            </ul>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  defp default_subtitle(:lifg), do: "Competitive Sense Selection"
  defp default_subtitle(_),     do: "Module Summary"

  defp default_region_desc(:lifg),
    do:
      "Selects the winning sense per token from si.sense_candidates with boundary guards and " <>
      "scoring (lex_fit, rel_prior, activation, intent_bias)."

  defp default_region_desc(_), do: "Overview of the selected module."

  # --- Live state ------------------------------------------------------------
  defp live_state_panel(assigns) do
    st   = assigns[:state] || %{}
    proc = Map.get(st, :process) || Map.get(st, :process_name) || "N/A"
    pid  = Map.get(st, :pid) || "N/A"
    q    = Map.get(st, :queue, 0)
    cur  = Map.get(st, :current) || Map.get(st, :current_msg)

    ~H"""
    <div class="border rounded-xl p-4 shadow-sm">
      <h2 class="font-semibold mb-2">Live state</h2>

      <div class="text-sm space-y-1 mb-2">
        <div><span class="opacity-60">process:</span> <code><%= proc %></code></div>
        <div><span class="opacity-60">pid:</span> <code><%= pid %></code></div>
        <div><span class="opacity-60">queue:</span> <%= q %></div>
        <div><span class="opacity-60">current:</span> <%= inspect(cur || :idle) %></div>
      </div>

      <div class="text-sm text-zinc-500">State</div>
      <pre class="text-xs bg-zinc-50 p-2 rounded border mt-1 overflow-x-auto">
<%= inspect(st[:state] || st, limit: :infinity) %>
      </pre>
    </div>
    """
  end

  # --- Mood panel ------------------------------------------------------------
  defp mood_panel(assigns) do
    mood     = assigns[:mood] || %{levels: %{}, derived: %{}, tone: :neutral}
    levels   = Map.get(mood, :levels, %{}) || %{}
    derived  = Map.get(mood, :derived, %{}) || %{}
    tone     = Map.get(mood, :tone, :neutral) || :neutral

    ~H"""
    <div class="border rounded-xl p-4 shadow-sm">
      <div class="flex items-center justify-between mb-3">
        <h2 class="font-semibold">Mood</h2>
        <span class={[
          "text-xs px-2 py-1 rounded border",
          tone_class(tone)
        ]}><%= Atom.to_string(tone) %></span>
      </div>

      <div class="grid grid-cols-2 gap-4">
        <div>
          <h3 class="text-sm text-zinc-500 mb-1">Neuromodulators</h3>
          <.kv label="DA"   value={fmt(levels[:da])} />
          <.kv label="5HT"  value={fmt(levels[:"5ht"])} />
          <.kv label="GLU"  value={fmt(levels[:glu])} />
          <.kv label="NE"   value={fmt(levels[:ne])} />
        </div>

        <div>
          <h3 class="text-sm text-zinc-500 mb-1">Derived</h3>
          <.kv label="Exploration" value={fmt(derived[:exploration])} />
          <.kv label="Inhibition"  value={fmt(derived[:inhibition])} />
          <.kv label="Vigilance"   value={fmt(derived[:vigilance])} />
          <.kv label="Plasticity"  value={fmt(derived[:plasticity])} />
        </div>
      </div>
    </div>
    """
  end

  # --- LIFG last decision ----------------------------------------------------
  defp lifg_panel(assigns) do
    lifg_last  = assigns[:lifg_last] || %{}
    meta       = Map.get(lifg_last, :meta, %{}) || %{}
    guards     = Map.get(lifg_last, :guards, %{}) || %{}
    choices    = Map.get(lifg_last, :choices, []) || []

    sentence   = Map.get(lifg_last, :sentence)
    intent     = Map.get(lifg_last, :intent)
    confidence = Map.get(lifg_last, :confidence)
    tokens     = Map.get(lifg_last, :tokens, []) || []
    updated_ms = Map.get(lifg_last, :updated_ms_ago) || Map.get(lifg_last, :updated_ago_ms)

    ~H"""
    <div class="border rounded-xl p-4 shadow-sm">
      <div class="flex items-center justify-between mb-2">
        <h2 class="font-semibold">LIFG — Decision (last)</h2>
        <span class="text-xs text-zinc-500">
          <%= if is_number(updated_ms), do: "#{updated_ms} ms ago", else: "—" %>
        </span>
      </div>

      <div class="text-sm grid grid-cols-1 md:grid-cols-2 gap-3">
        <div>
          <div><span class="opacity-60">sentence:</span> <%= sentence || "—" %></div>
          <div><span class="opacity-60">intent:</span> <%= intent || "—" %></div>
          <div><span class="opacity-60">confidence:</span> <%= if is_number(confidence), do: fmt_pct(confidence), else: "—" %></div>
          <div class="mt-1">
            <span class="opacity-60">guards:</span>
            chargrams=<%= guards[:chargram_violation] || 0 %>,
            boundary_drops=<%= guards[:boundary_drops] || guards[:boundary_drop] || 0 %>
          </div>
        </div>

        <div>
          <div class="opacity-60">meta</div>
          <div class="text-xs">
            margin_threshold=<%= meta[:margin_threshold] || "—" %>,
            scores_mode=<%= meta[:scores_mode] || "—" %>
          </div>
        </div>
      </div>

      <div class="mt-3">
        <h3 class="text-sm text-zinc-500 mb-1">Tokens</h3>
        <%= if tokens == [] do %>
          <div class="text-sm text-zinc-400">None</div>
        <% else %>
          <ul class="text-sm list-disc ml-5">
            <%= for t <- tokens do %><li><%= inspect(t) %></li><% end %>
          </ul>
        <% end %>
      </div>

      <div class="mt-3">
        <h3 class="text-sm text-zinc-500 mb-1">Choices</h3>
        <%= if choices == [] do %>
          <div class="text-sm text-zinc-400">No finalists</div>
        <% else %>
          <ul class="list-disc ml-5 text-sm">
            <%= for c <- choices do %>
              <li><%= inspect(c) %></li>
            <% end %>
          </ul>
        <% end %>
      </div>
    </div>
    """
  end

  # --- Hippocampus / Snapshot ------------------------------------------------
  defp hippo_panel(assigns) do
    hippo   = assigns[:hippo] || %{window: []}
    _metrics = Map.get(assigns, :metrics, %{}) || %{}

    ~H"""
    <div class="border rounded-xl p-4 shadow-sm">
      <h2 class="font-semibold mb-3">Hippocampus</h2>

      <div class="text-sm text-zinc-600 mb-2">
        <div>Window size: <%= (hippo[:window] || []) |> length() %></div>
      </div>

      <div class="text-sm text-zinc-500">Metrics:</div>
      <pre class="text-xs bg-zinc-50 p-2 rounded border mt-1"><%= inspect(@metrics) %></pre>
    </div>
    """
  end

  # Explicit attrs help dialyzer/compile warnings
  attr :snapshot, :any, default: nil
  defp snapshot_panel(assigns) do
    ~H"""
    <div class="border rounded-xl p-4 shadow-sm">
      <h2 class="font-semibold mb-3">Process Snapshot</h2>
      <%= if is_nil(@snapshot) do %>
        <div class="text-sm text-zinc-400">No snapshot available</div>
      <% else %>
        <pre class="text-xs bg-zinc-50 p-2 rounded border"><%= inspect(@snapshot, limit: :infinity) %></pre>
      <% end %>
    </div>
    """
  end

  # --- small UI helpers ------------------------------------------------------
  attr :label, :any, required: true
  attr :value, :any, required: true
  defp kv(assigns) do
    ~H"""
    <div class="flex items-center justify-between py-0.5">
      <span class="text-sm text-zinc-500"><%= @label %></span>
      <span class="font-mono text-sm"><%= @value %></span>
    </div>
    """
  end

  defp tone_class(:positive), do: "border-green-400 text-green-600"
  defp tone_class(:negative), do: "border-red-400 text-red-600"
  defp tone_class(_),         do: "border-zinc-300 text-zinc-600"

  defp fmt(nil), do: "—"
  defp fmt(v) when is_number(v), do: :io_lib.format("~.3f", [v]) |> IO.iodata_to_binary()
  defp fmt(v), do: to_string(v)

  defp fmt_pct(nil), do: "--"
  defp fmt_pct(v) when is_integer(v), do: "#{v}%"
  defp fmt_pct(v) when is_float(v), do: :io_lib.format("~.1f%", [v]) |> IO.iodata_to_binary()
  defp fmt_pct(v), do: to_string(v)

  # Region select options:
  # - If caller supplies @regions, use that list.
  # - Else, list keys from the registry in a sensible order.
  defp region_options([]) do
    # Prefer a stable, readable order in the dropdown
    preferred = [
      :lifg, :pmtg, :hippocampus, :thalamus, :basal_ganglia, :ofc, :acc,
      :dlpfc, :vmpfc, :dmpfc, :fpc, :prefrontal, :atl, :salience,
      :temporal, :frontal, :cerebellum
    ]

    keys =
      preferred
      |> Enum.filter(&RegionRegistry.available?/1)
      |> Kernel.++(RegionRegistry.keys() -- preferred)
      |> Enum.uniq()

    Enum.map(keys, &{&1, pretty_label(&1)})
  end

  defp region_options(pairs) do
    Enum.map(pairs, fn
      {key, label, _color, _region} -> {key, label}
      {key, label} -> {key, label}
      atom when is_atom(atom) -> {atom, pretty_label(atom)}
      _other -> {:lifg, "LIFG"}
    end)
  end

  defp pretty_label(:lifg),           do: "LIFG"
  defp pretty_label(:pmtg),           do: "PMTG"
  defp pretty_label(:ofc),            do: "OFC"
  defp pretty_label(:acc),            do: "ACC"
  defp pretty_label(:dlpfc),          do: "DLPFC"
  defp pretty_label(:vmpfc),          do: "VMPFC"
  defp pretty_label(:dmpfc),          do: "DMPFC"
  defp pretty_label(:fpc),            do: "FPC"
  defp pretty_label(:atl),            do: "ATL"
  defp pretty_label(:salience),       do: "Salience"
  defp pretty_label(:temporal),       do: "Temporal"
  defp pretty_label(:frontal),        do: "Frontal"
  defp pretty_label(:cerebellum),     do: "Cerebellum"
  defp pretty_label(:thalamus),       do: "Thalamus"
  defp pretty_label(:basal_ganglia),  do: "Basal Ganglia"
  defp pretty_label(:hippocampus),    do: "Hippocampus"
  defp pretty_label(:prefrontal),     do: "Prefrontal"
  defp pretty_label(other),           do: other |> Atom.to_string() |> String.upcase()

  # --- internal: viewBox assignment from optional override -------------------
  defp assign_viewbox(assigns) do
    vb = BaseArt.viewbox(Map.get(assigns, :svg_base))
    Map.put(assigns, :vb, vb)
  end
end

