defmodule SymbrellaWeb.BrainLive do
  use SymbrellaWeb, :live_view

  @moduledoc """
  Interactive brain silhouette (lateral view) with labeled cognitive regions.
  Hover to preview, click to toggle selection highlight.
  """

  @palette %{
    lifg:        "#8b5cf6", # violet
    atl:         "#ef4444", # red
    mtg_sts:     "#f59e0b", # amber
    tpj_ag:      "#06b6d4", # cyan
    mpfc:        "#ec4899", # pink
    ofc_vmpfc:   "#14b8a6", # teal
    hippocampus: "#eab308", # yellow
    thalamus:    "#64748b"  # slate
  }

  # Stylized region paths (not ellipses), tuned for viewBox 0 0 800 480
  @regions [
    %{
      id: "lifg",
      label: "LIFG / Broca",
      color: @palette.lifg,
      desc:
        "Stage-1 sense selection & ambiguity pruning; early control/selection over candidate senses.",
      d: "M 520 250 C 560 240, 600 220, 630 240 C 645 260, 640 290, 610 305 C 585 320, 545 320, 520 300 C 505 285, 505 265, 520 250 Z",
      anchor: {650, 258}
    },
    %{
      id: "atl",
      label: "ATL (Anterior Temporal Lobe)",
      color: @palette.atl,
      desc: "Hub-like semantic integration; converges modality-specific inputs at the temporal pole.",
      d: "M 560 320 C 600 320, 640 330, 660 350 C 650 370, 620 385, 590 380 C 560 370, 540 350, 545 335 C 550 325, 555 322, 560 320 Z",
      anchor: {670, 350}
    },
    %{
      id: "mtg_sts",
      label: "MTG / STS",
      color: @palette.mtg_sts,
      desc: "Lexical-semantic access & combinatorics; temporal dynamics for context fit.",
      d: "M 420 300 C 460 290, 520 295, 560 310 C 560 330, 520 345, 470 340 C 440 335, 420 325, 420 300 Z",
      anchor: {585, 315}
    },
    %{
      id: "tpj_ag",
      label: "TPJ / Angular Gyrus",
      color: @palette.tpj_ag,
      desc: "Conceptual integration & controlled retrieval; discourse-level glue.",
      d: "M 360 230 C 400 220, 450 225, 470 245 C 455 260, 430 275, 395 275 C 370 270, 355 255, 360 230 Z",
      anchor: {485, 245}
    },
    %{
      id: "mpfc",
      label: "mPFC",
      color: @palette.mpfc,
      desc: "Goal/task schemas & global intent constraints over interpretation.",
      d: "M 540 140 C 580 130, 620 130, 640 150 C 630 165, 600 175, 565 170 C 550 165, 542 155, 540 140 Z",
      anchor: {660, 150}
    },
    %{
      id: "ofc_vmpfc",
      label: "OFC / vmPFC",
      color: @palette.ofc_vmpfc,
      desc: "Valuation & pragmatic bias; resolves among plausible readings via reward/utility signals.",
      d: "M 560 200 C 600 205, 635 220, 650 240 C 635 250, 610 260, 580 260 C 560 255, 550 240, 552 220 Z",
      anchor: {665, 240}
    },
    %{
      id: "hippocampus",
      label: "Hippocampus",
      color: @palette.hippocampus,
      desc: "Associative recall & episode binding; links current semantics to episodic traces.",
      d: "M 460 340 C 480 335, 505 340, 515 355 C 505 365, 485 370, 465 365 C 455 357, 455 347, 460 340 Z",
      anchor: {535, 355}
    },
    %{
      id: "thalamus",
      label: "Thalamus",
      color: @palette.thalamus,
      desc: "Relay/gating & rhythmic gain control across cortico-cortical loops.",
      d: "M 410 250 C 430 245, 450 250, 455 265 C 445 275, 425 278, 410 270 C 405 262, 405 255, 410 250 Z",
      anchor: {470, 267}
    }
  ]

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(
       regions: @regions,
       hovered: nil,
       selected: MapSet.new()
     )}
  end

  @impl true
  def handle_event("toggle", %{"id" => id}, socket) do
    sel =
      if MapSet.member?(socket.assigns.selected, id),
        do: MapSet.delete(socket.assigns.selected, id),
        else: MapSet.put(socket.assigns.selected, id)

    {:noreply, assign(socket, selected: sel)}
  end

  @impl true
  def handle_event("hover", %{"id" => id}, socket),
    do: {:noreply, assign(socket, hovered: id)}

  @impl true
  def handle_event("leave", _params, socket),
    do: {:noreply, assign(socket, hovered: nil)}

  # --- view helpers

  defp opacities(id, hovered, selected) do
    cond do
      hovered == id -> {"0.9", "1.0"}
      MapSet.member?(selected, id) -> {"0.7", "1.0"}
      true -> {"0.32", "0.85"}
    end
  end

  defp label_color(hex) do
    with <<?#, r1::binary-size(2), g1::binary-size(2), b1::binary-size(2)>> <- String.downcase(hex),
         {r, ""} <- Integer.parse(r1, 16),
         {g, ""} <- Integer.parse(g1, 16),
         {b, ""} <- Integer.parse(b1, 16) do
      lum = 0.2126 * r + 0.7152 * g + 0.0722 * b
      if lum > 140, do: "#0f172a", else: "#f8fafc"
    else
      _ -> "#0f172a"
    end
  end

  defp region_for(id), do: Enum.find(@regions, &(&1.id == id))

  @impl true
  def render(assigns) do
    ~H"""
    <div class="mx-auto max-w-6xl w-full h-[calc(100svh-6rem)] grid grid-cols-1 lg:grid-cols-[1fr_320px] gap-6 p-4">
      <!-- Canvas -->
      <div class="relative rounded-2xl border border-slate-800/60 bg-[var(--color-panel)] shadow">
        <svg viewBox="0 0 800 480" class="w-full h-full">
          <!-- Background -->
          <rect x="0" y="0" width="800" height="480" fill="transparent"/>

          <!-- Brain silhouette (stylized, not too detailed) -->
          <g>
            <path
              d="M 130 240
                 C 120 170, 170 110, 260 80
                 C 350 50, 480 60, 560 110
                 C 620 150, 680 210, 690 260
                 C 700 300, 680 350, 640 380
                 C 600 410, 540 430, 470 440
                 C 400 450, 320 440, 260 410
                 C 230 400, 210 380, 200 360
                 C 190 340, 170 325, 150 320
                 C 120 310, 110 280, 120 260 Z"
              fill="#0b1220" fill-opacity="0.55"
              stroke="#0ea5e9" stroke-opacity="0.25" stroke-width="2"
            />
            <!-- Brainstem (subtle) -->
            <path d="M 200 360 Q 210 390 245 410 Q 255 450 300 465"
                  fill="none" stroke="#0ea5e9" stroke-opacity="0.25" stroke-width="7" stroke-linecap="round"/>
          </g>

          <!-- Regions -->
          <%= for r <- @regions do %>
            <% {fill_op, stroke_op} = opacities(r.id, @hovered, @selected) %>
            <g
              phx-click="toggle" phx-value-id={r.id}
              phx-mouseenter="hover" phx-value-id={r.id}
              phx-mouseleave="leave"
              class="cursor-pointer"
            >
              <path d={r.d}
                    fill={r.color} fill-opacity={fill_op}
                    stroke={r.color} stroke-opacity={stroke_op} stroke-width="2.5"/>
              <% {lx, ly} = r.anchor %>
              <line x1={lx - 18} y1={ly} x2={lx} y2={ly}
                    stroke={r.color} stroke-opacity="0.75" stroke-width="2"/>
              <rect x={lx + 2} y={ly - 14} rx="6" ry="6" width="220" height="28"
                    fill={r.color} fill-opacity="0.18" stroke={r.color} stroke-opacity="0.5" stroke-width="1"/>
              <text x={lx + 12} y={ly + 6} font-size="13" font-weight="600"
                    fill={label_color(r.color)} style="font-family: ui-sans-serif, system-ui;">
                <%= r.label %>
              </text>
            </g>
          <% end %>

          <!-- Hover badge -->
          <%= if @hovered do %>
            <% r = region_for(@hovered) %>
            <% {bx, by} = r.anchor %>
            <g>
              <rect x={bx - 140} y={max(by - 94, 12)} width="280" height="78" rx="10" ry="10"
                    fill="#0b1220" fill-opacity="0.95" stroke="#1f2937" stroke-width="1.25"/>
              <text x={bx - 128} y={max(by - 74, 32)} font-size="13" font-weight="700" fill="#e5e7eb"
                    style="font-family: ui-sans-serif, system-ui;"><%= r.label %></text>
              <text x={bx - 128} y={max(by - 56, 48)} font-size="12" fill="#cbd5e1"
                    style="font-family: ui-sans-serif, system-ui;">
                <tspan><%= String.slice(r.desc, 0, 52) %></tspan>
                <tspan x={bx - 128} dy="16"><%= String.slice(r.desc, 52, 52) %></tspan>
                <tspan x={bx - 128} dy="16"><%= String.slice(r.desc, 104, 52) %></tspan>
              </text>
            </g>
          <% end %>
        </svg>
      </div>

      <!-- Legend / details -->
      <aside class="rounded-2xl border border-slate-800/60 bg-[var(--color-panel)] p-4 space-y-4 shadow">
        <h2 class="text-sm font-semibold opacity-80">Brain chain regions</h2>

        <div class="space-y-2 max-h-[calc(100%-3rem)] overflow-y-auto pr-1">
          <%= for r <- @regions do %>
            <% selected? = MapSet.member?(@selected, r.id) %>
            <button
              phx-click="toggle" phx-value-id={r.id}
              phx-mouseenter="hover" phx-value-id={r.id}
              phx-mouseleave="leave"
              class={[
                "w-full text-left rounded-xl border px-3 py-2 transition",
                selected? && "border-white/30 bg-white/5",
                !selected? && "border-slate-800/70 hover:border-slate-700/80 bg-transparent"
              ]}
            >
              <div class="flex items-center gap-2">
                <span class="inline-block w-3.5 h-3.5 rounded-full" style={"background: #{r.color}"} />
                <span class="text-sm font-medium"><%= r.label %></span>
              </div>
              <p class="mt-1 text-xs opacity-80 leading-snug"><%= r.desc %></p>
            </button>
          <% end %>
        </div>

        <div class="pt-2 text-xs opacity-70">
          Tip: hover to preview, click to toggle highlight.
        </div>
      </aside>
    </div>
    """
  end
end

