defmodule SymbrellaWeb.BrainLive do
  @moduledoc """
  LiveView: Interactive human brain (lateral left view).

  • Color-coded regions (Frontal/LIFG, Temporal/PMTG, Hippocampus, Thalamus, OFC, Cerebellum, Occipital, Parietal)
  • Click a region to reveal module details in the pane below
  • Iconic, hand-drawn vector paths for an "organ" look (not abstract ellipses)
  """
  use SymbrellaWeb, :live_view

  @regions [
    # id, label, color, module_key
    {:lifg,        "LIFG (Broca’s area)",         "#f44336", :lifg},
    {:ofc,         "OFC (orbitofrontal cortex)",  "#ff9800", :ofc},
    {:pmtg,        "PMTG (mid temporal)",         "#7e57c2", :pmtg},
    {:hippocampus, "Hippocampus",                  "#00897b", :hippocampus},
    {:thalamus,    "Thalamus",                     "#1976d2", :thalamus},
    {:cerebellum,  "Cerebellum",                   "#6d4c41", :cerebellum},
    {:occipital,   "Occipital lobe",               "#66bb6a", :occipital},
    {:parietal,    "Parietal lobe",                "#42a5f5", :parietal},
    {:temporal,    "Temporal lobe",                "#9575cd", :temporal},
    {:frontal,     "Frontal lobe",                 "#ef5350", :frontal}
  ]

  @module_info %{
    lifg: %{
      title: "LIFG — Competitive Sense Selection",
      modules: ~w(Brain.LIFG Brain.LIFG.Stage1 Core.LIFG.Input)a,
      summary: """
      Selects the winning sense per token from `si.sense_candidates`, with boundary guards and scoring:
      lex_fit, rel_prior, activation, intent_bias (+ episodes boost).
      """,
      telemetry: ~w(brain.lifg.stage1.start brain.lifg.stage1.stop brain.lifg.chargram_violation brain.lifg.boundary_drop),
      config: [
        {:brain, :lifg_stage1_weights, %{lex_fit: 0.40, rel_prior: 0.30, activation: 0.20, intent_bias: 0.10}},
        {:brain, :lifg_stage1_scores_mode, :all}
      ]
    },
    pmtg: %{
      title: "PMTG — Controlled Semantic Retrieval",
      modules: ~w(Brain.PMTG)a,
      summary: "Interfaces selection demands (from LIFG) with knowledge access dynamics; surfaces near-winners into `si.sense_candidates`.",
      telemetry: ~w(brain.pmtg.no_mwe_senses),
      config: [ {:brain, :pmtg_margin_threshold, 0.15}, {:brain, :pmtg_mode, :boost} ]
    },
    hippocampus: %{
      title: "Hippocampus — Episodic Write/Recall",
      modules: ~w(Brain.Hippocampus Db.Episode)a,
      summary: "Writes episodes at ATL finalize; recalls by norms overlap + recency half-life; biases LIFG.",
      telemetry: ~w(brain.hippo.write brain.hippo.recall),
      config: [ {:db, :pgvector, true} ]
    },
    thalamus: %{
      title: "Thalamus — Routing / Attention Gating",
      modules: ~w(Brain.Thalamus)a,
      summary: "Gates signals, routes salience to regions, coordinates focus windows.",
      telemetry: ~w(brain.thalamus.route),
      config: []
    },
    ofc: %{
      title: "OFC — Valuation / Feedback",
      modules: ~w(Brain.OFC)a,
      summary: "Scores outcomes and feeds back to bias future selection and phrasing.",
      telemetry: ~w(brain.ofc.feedback),
      config: []
    },
    cerebellum: %{
      title: "Cerebellum — Sequencing / Prediction",
      modules: ~w(Brain.Cerebellum)a,
      summary: "Smooths timing, sequencing of multi-step outputs; light predictive adjustments.",
      telemetry: ~w(brain.cerebellum.tick),
      config: []
    },
    occipital: %{
      title: "Occipital — Visual Context (future)",
      modules: [],
      summary: "Reserved: visual feature hooks / context fusion.",
      telemetry: [],
      config: []
    },
    parietal: %{
      title: "Parietal — Integration Hub (future)",
      modules: [],
      summary: "Reserved: multi-modal integration and spatial attention.",
      telemetry: [],
      config: []
    },
    temporal: %{
      title: "Temporal — Semantics Bedrock",
      modules: ~w(Brain.PMTG Db.BrainCell Core.Lexicon)a,
      summary: "Home base for lexical & semantic knowledge access (with PMTG).",
      telemetry: [],
      config: []
    },
    frontal: %{
      title: "Frontal — Control & Planning",
      modules: ~w(Brain.LIFG Brain.OFC)a,
      summary: "Executive control for language selection and valuation.",
      telemetry: [],
      config: []
    }
  }

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:selected, :lifg)
     |> assign(:regions, @regions)
     |> assign(:module_info, @module_info)}
  end

  @impl true
  def handle_event("select_region", %{"region" => region}, socket) do
    region_atom =
      region
      |> String.to_existing_atom()

    {:noreply, assign(socket, :selected, region_atom)}
  rescue
    _ -> {:noreply, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="mx-auto max-w-6xl px-4 py-6">
      <div class="mb-4 flex items-center justify-between gap-4">
        <h1 class="text-2xl font-semibold">Brain Regions ↔ Symbrella Modules</h1>
        <div class="text-sm opacity-70">Left-lateral human brain (vector, hand-drawn)</div>
      </div>

      <div class="grid grid-cols-1 lg:grid-cols-2 gap-6 items-start">
        <!-- SVG brain -->
        <div class="w-full">
          <.brain_svg selected={@selected} />
          <div class="mt-2 text-xs opacity-70">Click any colored region to view details.</div>
        </div>

        <!-- Info pane -->
        <div id="info" class="w-full">
          <.info_panel selected={@selected} data={@module_info[@selected]} />
          <div class="mt-4 rounded-2xl border p-4">
            <h3 class="font-medium mb-2">Quick jump</h3>
            <div class="flex flex-wrap gap-2">
              <%= for {id, label, color, _} <- @regions do %>
                <button
                  type="button"
                  phx-click="select_region"
                  phx-value-region={id}
                  class={[
                    "rounded-full px-3 py-1 text-sm border hover:shadow transition",
                    if(@selected == id, do: "ring-2 ring-offset-2", else: "")
                  ]}
                  style={"border-color: #{color}; #{if(@selected == id, do: "ring-color: #{color};", else: "")}"}
                >
                  <span class="mr-2 inline-block h-3 w-3 rounded-full align-middle" style={"background: #{color}"}></span>
                  <%= label %>
                </button>
              <% end %>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  ## Components

  attr :selected, :atom, required: true
  def brain_svg(assigns) do
    ~H"""
    <svg
      xmlns="http://www.w3.org/2000/svg"
      viewBox="0 0 1100 750"
      class="w-full h-auto rounded-2xl border bg-white"
      aria-labelledby="title desc"
      role="img"
    >
      <title id="title">Human brain, left lateral view</title>
      <desc id="desc">Interactive diagram. Click colored regions to reveal corresponding Symbrella module details.</desc>

      <!-- Base silhouette -->
      <defs>
        <filter id="shadow" x="-20%" y="-20%" width="140%" height="140%">
          <feDropShadow dx="0" dy="2" stdDeviation="6" flood-opacity="0.18"/>
        </filter>
        <clipPath id="brain-clip">
          <!-- Organic outer contour -->
          <path d="M144,372 C140,250 230,170 360,140 C440,120 540,110 640,135 C740,160 820,210 865,270
                   C915,340 910,420 892,470 C872,525 878,570 852,598 C820,632 760,618 740,598
                   C730,588 710,590 700,595 C670,610 618,620 560,618 C495,615 440,598 400,580
                   C360,562 330,560 300,562 C240,566 200,552 178,520 C156,488 150,438 144,372 z"/>
        </clipPath>
      </defs>

      <!-- Tissue fill -->
      <g filter="url(#shadow)" clip-path="url(#brain-clip)">
        <rect x="120" y="110" width="800" height="560" fill="#f7f4ef"/>
        <!-- light sulci texture -->
        <g stroke="#e6e0d8" stroke-width="4" fill="none" opacity="0.8">
          <path d="M220,220 C340,200 460,220 580,250"/>
          <path d="M220,280 C360,260 520,300 640,320"/>
          <path d="M240,340 C360,330 520,360 660,370"/>
          <path d="M260,400 C380,400 520,430 660,430"/>
          <path d="M280,460 C400,470 540,490 660,500"/>
        </g>
      </g>

      <!-- Regions (each is a click target with a tasteful organ shape) -->
      <!-- Frontal lobe -->
      <.region
        id="frontal"
        label="Frontal lobe"
        selected={@selected == :frontal or @selected == :lifg or @selected == :ofc}
        color="#ef5350"
        d="M180,360 C200,260 300,190 420,170 C520,155 600,170 640,190
           C640,210 600,240 560,260 C520,280 470,300 430,320 C360,355 300,380 240,385
           C210,387 190,380 180,360 z"
      />

      <!-- LIFG (Broca’s) subregion -->
      <.region
        id="lifg"
        label="LIFG (Broca’s area)"
        selected={@selected == :lifg}
        color="#f44336"
        d="M350,400 C340,375 360,350 400,330 C440,315 485,320 510,340
           C520,355 515,380 490,400 C465,420 420,430 390,425 C370,420 355,415 350,400 z"
      />

      <!-- OFC subregion -->
      <.region
        id="ofc"
        label="OFC"
        selected={@selected == :ofc}
        color="#ff9800"
        d="M360,450 C355,430 370,415 400,405 C430,398 465,402 485,415
           C495,430 488,447 465,460 C440,472 405,475 385,468 C370,463 362,458 360,450 z"
      />

      <!-- Parietal lobe -->
      <.region
        id="parietal"
        label="Parietal"
        selected={@selected == :parietal}
        color="#42a5f5"
        d="M520,250 C590,220 660,230 720,265 C760,290 790,330 780,380
           C740,395 700,398 660,395 C610,392 570,380 540,360 C515,345 510,310 520,250 z"
      />

      <!-- Temporal lobe -->
      <.region
        id="temporal"
        label="Temporal"
        selected={@selected == :temporal or @selected == :pmtg or @selected == :hippocampus}
        color="#9575cd"
        d="M330,520 C340,485 380,465 440,460 C500,460 560,470 600,490
           C640,510 640,545 600,565 C560,585 500,595 440,592 C390,588 350,570 330,520 z"
      />

      <!-- PMTG subregion -->
      <.region
        id="pmtg"
        label="PMTG"
        selected={@selected == :pmtg}
        color="#7e57c2"
        d="M460,520 C470,505 500,500 530,505 C560,512 580,525 575,540
           C565,555 540,565 510,565 C485,562 470,552 465,540 C462,533 460,526 460,520 z"
      />

      <!-- Hippocampus -->
      <.region
        id="hippocampus"
        label="Hippocampus"
        selected={@selected == :hippocampus}
        color="#00897b"
        d="M520,560 C530,548 555,545 575,550 C595,557 600,570 590,580
           C575,592 550,595 532,590 C522,585 518,575 520,560 z"
      />

      <!-- Thalamus (deep nucleus blob) -->
      <.region
        id="thalamus"
        label="Thalamus"
        selected={@selected == :thalamus}
        color="#1976d2"
        d="M600,420 C600,405 615,395 635,395 C650,395 665,405 665,420
           C665,435 650,445 635,445 C615,445 600,435 600,420 z"
      />

      <!-- Occipital lobe -->
      <.region
        id="occipital"
        label="Occipital"
        selected={@selected == :occipital}
        color="#66bb6a"
        d="M710,340 C740,325 780,330 810,355 C825,370 830,395 818,420
           C800,450 760,455 730,440 C710,430 700,410 700,390 C700,370 705,355 710,340 z"
      />

      <!-- Cerebellum (distinct organ look) -->
      <.region
        id="cerebellum"
        label="Cerebellum"
        selected={@selected == :cerebellum}
        color="#6d4c41"
        d="M720,570 C740,540 790,532 835,545 C875,555 900,580 895,610
           C888,642 850,660 810,657 C770,654 735,635 725,605 C722,595 720,585 720,570 z"
      />

      <!-- Outline stroke for clarity -->
      <use href="#brain-clip" fill="none" stroke="#b7b0a7" stroke-width="3"/>
    </svg>
    """
  end

  attr :id, :string, required: true
  attr :label, :string, required: true
  attr :selected, :boolean, required: true
  attr :color, :string, required: true
  attr :d, :string, required: true
  def region(assigns) do
    ~H"""
    <g
      phx-click="select_region"
      phx-value-region={@id}
      class="cursor-pointer transition"
      role="button"
      aria-label={"#{@label} (click for details)"}
    >
      <path
        d={@d}
        fill={@color}
        fill-opacity={if @selected, do: "0.55", else: "0.28"}
        stroke={@color}
        stroke-width={if @selected, do: "3", else: "2"}
      />
    </g>
    """
  end

  attr :selected, :atom, required: true
  attr :data, :map, required: true
  def info_panel(assigns) do
    ~H"""
    <div class="rounded-2xl border p-5 bg-white">
      <div class="flex items-start justify-between gap-4">
        <h2 class="text-xl font-semibold"><%= @data.title %></h2>
        <span class="inline-flex items-center gap-2 rounded-full border px-3 py-1 text-xs">
          <span class="opacity-60">Selected:</span>
          <code class="font-mono"><%= @selected %></code>
        </span>
      </div>
      <p class="mt-2 text-sm leading-relaxed opacity-90"><%= @data.summary %></p>

      <div class="mt-4 grid grid-cols-1 md:grid-cols-3 gap-4">
        <div>
          <h3 class="font-medium mb-1">Modules</h3>
          <ul class="text-sm space-y-1">
            <%= if Enum.empty?(@data.modules) do %>
              <li class="opacity-60">—</li>
            <% else %>
              <%= for m <- @data.modules do %>
                <li><code class="px-1 py-0.5 rounded bg-zinc-50 border"><%= inspect(m) %></code></li>
              <% end %>
            <% end %>
          </ul>
        </div>

        <div>
          <h3 class="font-medium mb-1">Telemetry</h3>
          <ul class="text-sm space-y-1">
            <%= if Enum.empty?(@data.telemetry) do %>
              <li class="opacity-60">—</li>
            <% else %>
              <%= for t <- @data.telemetry do %>
                <li><code class="px-1 py-0.5 rounded bg-zinc-50 border"><%= t %></code></li>
              <% end %>
            <% end %>
          </ul>
        </div>

        <div>
          <h3 class="font-medium mb-1">Config (examples)</h3>
          <ul class="text-sm space-y-1">
            <%= if Enum.empty?(@data.config) do %>
              <li class="opacity-60">—</li>
            <% else %>
              <%= for {app, key, val} <- @data.config do %>
                <li>
                  <code class="px-1 py-0.5 rounded bg-zinc-50 border">
                    config :<%= app %>, <%= key %>, <%= inspect(val) %>
                  </code>
                </li>
              <% end %>
            <% end %>
          </ul>
        </div>
      </div>
    </div>
    """
  end
end

