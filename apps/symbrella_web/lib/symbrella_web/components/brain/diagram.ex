defmodule SymbrellaWeb.Components.Brain.Diagram do
  @moduledoc "Interactive left-lateral brain diagram (SVG) with optional HUD overlay."
  use SymbrellaWeb, :html

  alias SymbrellaWeb.Components.IntentChip, as: IntentChip
  alias SymbrellaWeb.Components.MoodChip, as: MoodChip

  # ViewBox constants (module attributes)
  @vb_w 1100
  @vb_h 750

  # HUD box geometry inside the SVG viewBox
  @hud_w 440
  @hud_h 56
  @hud_pad 16

  # Public API:
  #   :selected   — atom region id to highlight
  #   :show_hud   — toggle overlay chips (default: true)
  #   :intent     — intent label (atom or string)
  #   :keyword    — keyword string
  #   :confidence — float (0..1 or 0..100)
  #   :mood       — map with either:
  #                  %{exploration:, inhibition:, vigilance:, plasticity:} (0..1 or 0..100)
  #                or nested:
  #                  %{derived: %{exploration:, inhibition:, vigilance:, plasticity:}}
  #   :mood_ping  — optional %{exploration: bool, inhibition: bool, vigilance: bool, plasticity: bool}
  attr :selected, :atom, required: true
  attr :show_hud, :boolean, default: true
  attr :intent, :any, default: :none
  attr :keyword, :string, default: nil
  attr :confidence, :any, default: 0.0
  attr :mood, :map, default: %{}
  attr :mood_ping, :map, default: %{}

  def brain_diagram(assigns) do
    # Normalize intent
    chip_intent =
      case assigns.intent do
        i when is_atom(i) ->
          i

        i when is_binary(i) ->
          try do
            String.to_existing_atom(i)
          rescue
            _ -> :unknown
          end

        _ ->
          :none
      end

    # Normalize confidence → 0..1
    chip_conf =
      case assigns.confidence do
        v when is_number(v) and v > 1.0 -> min(1.0, v / 100.0)
        v when is_number(v) -> max(0.0, min(1.0, v))
        _ -> 0.0
      end

    # Normalize mood → 0..1
    m = assigns.mood || %{}
    derived = Map.get(m, :derived) || Map.get(m, "derived") || %{}

    chip_mood = %{
      exploration:
        to01(
          m[:exploration] || m["exploration"] || derived[:exploration] || derived["exploration"]
        ),
      inhibition:
        to01(m[:inhibition] || m["inhibition"] || derived[:inhibition] || derived["inhibition"]),
      vigilance:
        to01(m[:vigilance] || m["vigilance"] || derived[:vigilance] || derived["vigilance"]),
      plasticity:
        to01(m[:plasticity] || m["plasticity"] || derived[:plasticity] || derived["plasticity"])
    }

    # HUD placement
    hud_x = @vb_w - @hud_w - @hud_pad
    hud_y = @vb_h - @hud_h - @hud_pad

    # Locals for HEEx (avoid KeyError by not using @vb_w/@vb_h inside ~H)
    _vb_w = @vb_w
    _vb_h = @vb_h

    assigns =
      assigns
      |> assign(:chip_intent, chip_intent)
      |> assign(:chip_conf, chip_conf)
      |> assign(:chip_keyword, assigns.keyword)
      |> assign(:chip_mood, chip_mood)
      |> assign(:hud_x, hud_x)
      |> assign(:hud_y, hud_y)
      |> assign(:hud_w, @hud_w)
      |> assign(:hud_h, @hud_h)

    ~H"""
    <svg
      xmlns="http://www.w3.org/2000/svg"
      viewBox={"0 0 #{@vb_w} #{@vb_h}"}
      class="w-full h-auto rounded-2xl border bg-white"
      aria-labelledby="title desc"
      role="img"
    >
      <title id="title">Human brain, left lateral view</title>
      <desc id="desc">Interactive diagram. Click colored regions to reveal details.</desc>

      <defs>
        <filter id="shadow" x="-20%" y="-20%" width="140%" height="140%">
          <feDropShadow dx="0" dy="2" stdDeviation="6" flood-opacity="0.18" />
        </filter>
        <clipPath id="brain-clip">
          <path d="M144,372 C140,250 230,170 360,140 C440,120 540,110 640,135 C740,160 820,210 865,270
                   C915,340 910,420 892,470 C872,525 878,570 852,598 C820,632 760,618 740,598
                   C730,588 710,590 700,595 C670,610 618,620 560,618 C495,615 440,598 400,580
                   C360,562 330,560 300,562 C240,566 200,552 178,520 C156,488 150,438 144,372 z" />
        </clipPath>
      </defs>

      <g filter="url(#shadow)" clip-path="url(#brain-clip)">
        <rect x="120" y="110" width="800" height="560" fill="#f7f4ef" />
        <g stroke="#e6e0d8" stroke-width="4" fill="none" opacity="0.8">
          <path d="M220,220 C340,200 460,220 580,250" />
          <path d="M220,280 C360,260 520,300 640,320" />
          <path d="M240,340 C360,330 520,360 660,370" />
          <path d="M260,400 C380,400 520,430 660,430" />
          <path d="M280,460 C400,470 540,490 660,500" />
        </g>
      </g>

      <.region
        id="frontal"
        label="Frontal lobe"
        selected={@selected in [:frontal, :lifg, :ofc]}
        color="#ef5350"
        d="M180,360 C200,260 300,190 420,170 C520,155 600,170 640,190
           C640,210 600,240 560,260 C520,280 470,300 430,320 C360,355 300,380 240,385
           C210,387 190,380 180,360 z"
      />

      <.region
        id="lifg"
        label="LIFG (Broca’s area)"
        selected={@selected == :lifg}
        color="#f44336"
        d="M350,400 C340,375 360,350 400,330 C440,315 485,320 510,340
           C520,355 515,380 490,400 C465,420 420,430 390,425 C370,420 355,415 350,400 z"
      />

      <.region
        id="ofc"
        label="OFC"
        selected={@selected == :ofc}
        color="#ff9800"
        d="M360,450 C355,430 370,415 400,405 C430,398 465,402 485,415
           C495,430 488,447 465,460 C440,472 405,475 385,468 C370,463 362,458 360,450 z"
      />

      <.region
        id="parietal"
        label="Parietal"
        selected={@selected == :parietal}
        color="#42a5f5"
        d="M520,250 C590,220 660,230 720,265 C760,290 790,330 780,380
           C740,395 700,398 660,395 C610,392 570,380 540,360 C515,345 510,310 520,250 z"
      />

      <.region
        id="temporal"
        label="Temporal"
        selected={@selected in [:temporal, :pmtg, :hippocampus]}
        color="#9575cd"
        d="M330,520 C340,485 380,465 440,460 C500,460 560,470 600,490
           C640,510 640,545 600,565 C560,585 500,595 440,592 C390,588 350,570 330,520 z"
      />

      <.region
        id="pmtg"
        label="PMTG"
        selected={@selected == :pmtg}
        color="#7e57c2"
        d="M460,520 C470,505 500,500 530,505 C560,512 580,525 575,540
           C565,555 540,565 510,565 C485,562 470,552 465,540 C462,533 460,526 460,520 z"
      />

      <.region
        id="hippocampus"
        label="Hippocampus"
        selected={@selected == :hippocampus}
        color="#00897b"
        d="M520,560 C530,548 555,545 575,550 C595,557 600,570 590,580
           C575,592 550,595 532,590 C522,585 518,575 520,560 z"
      />

      <.region
        id="thalamus"
        label="Thalamus"
        selected={@selected == :thalamus}
        color="#1976d2"
        d="M600,420 C600,405 615,395 635,395 C650,395 665,405 665,420
           C665,435 650,445 635,445 C615,445 600,435 600,420 z"
      />

      <.region
        id="occipital"
        label="Occipital"
        selected={@selected == :occipital}
        color="#66bb6a"
        d="M710,340 C740,325 780,330 810,355 C825,370 830,395 818,420
           C800,450 760,455 730,440 C710,430 700,410 700,390 C700,370 705,355 710,340 z"
      />

      <.region
        id="cerebellum"
        label="Cerebellum"
        selected={@selected == :cerebellum}
        color="#6d4c41"
        d="M720,570 C740,540 790,532 835,545 C875,555 900,580 895,610
           C888,642 850,660 810,657 C770,654 735,635 725,605 C722,595 720,585 720,570 z"
      />

      <use href="#brain-clip" fill="none" stroke="#b7b0a7" stroke-width="3" />
      
    <!-- ─── HUD overlay (bottom-right) ───────────────────────────────── -->
      <%= if @show_hud do %>
        <foreignObject x={@hud_x} y={@hud_y} width={@hud_w} height={@hud_h} pointer-events="none">
          <div
            xmlns="http://www.w3.org/1999/xhtml"
            class="w-full h-full flex items-center justify-end gap-2"
            style="pointer-events:auto"
          >
            <IntentChip.intent_chip
              intent={@chip_intent}
              keyword={@chip_keyword}
              confidence={@chip_conf}
              class="bg-white/80"
            />
            <MoodChip.mood_chip
              mood={@chip_mood}
              ping={@mood_ping}
              class="bg-white/80"
            />
          </div>
        </foreignObject>
      <% end %>
    </svg>
    """
  end

  # Region path sub-component (uses "select-region" to match existing handlers)
  attr :id, :string, required: true
  attr :label, :string, required: true
  attr :selected, :boolean, required: true
  attr :color, :string, required: true
  attr :d, :string, required: true

  def region(assigns) do
    ~H"""
    <g
      phx-click="select-region"
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

  # ——— util ———
  defp to01(nil), do: nil
  defp to01(v) when is_number(v) and v > 1.0, do: min(1.0, v / 100.0)
  defp to01(v) when is_number(v), do: max(0.0, min(1.0, v))
  defp to01(_), do: nil
end
