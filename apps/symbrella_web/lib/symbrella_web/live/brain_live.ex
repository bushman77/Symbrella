defmodule SymbrellaWeb.BrainLive do
  @moduledoc """
  LiveView: Interactive human brain (lateral left view).

  • Color-coded regions (Frontal/LIFG, Temporal/PMTG, Hippocampus, Thalamus, OFC, Cerebellum, Occipital, Parietal)
  • Click a region to reveal module details in the pane below
  • Shows the live state of the selected region (PID, queue, current function, state snapshot)
  • Adds refresh/auto-refresh + Hippocampus controls (recall source, reset, seed)
  • Subscribes to Hippocampus telemetry to render latest recall metrics
  • ⚡ Subscribes to CycleClock telemetry for snappy phase/HZ/S updates in the HUD
  • 🧭 Shows latest intent in the header (pulled from Brain.latest_intent/0 each cycle; telemetry also supported)
  """

  use SymbrellaWeb, :live_view
  alias Brain.Introspect
  alias Brain.Hippocampus
  alias Brain.LIFG
  alias SymbrellaWeb.Components.CycleHUD

  @regions [
    {:lifg, "LIFG (Broca’s area)", "#f44336", :lifg},
    {:ofc, "OFC (orbitofrontal cortex)", "#ff9800", :ofc},
    {:pmtg, "PMTG (mid temporal)", "#7e57c2", :pmtg},
    {:hippocampus, "Hippocampus", "#00897b", :hippocampus},
    {:thalamus, "Thalamus", "#1976d2", :thalamus},
    {:cerebellum, "Cerebellum", "#6d4c41", :cerebellum},
    {:occipital, "Occipital lobe", "#66bb6a", :occipital},
    {:parietal, "Parietal lobe", "#42a5f5", :parietal},
    {:temporal, "Temporal lobe", "#9575cd", :temporal},
    {:frontal, "Frontal lobe", "#ef5350", :frontal}
  ]

  @region_full_names %{
    lifg: "Left Inferior Frontal Gyrus (Broca’s area)",
    ofc: "Orbitofrontal Cortex",
    pmtg: "Posterior Middle Temporal Gyrus",
    hippocampus: "Hippocampus",
    thalamus: "Thalamus",
    cerebellum: "Cerebellum",
    occipital: "Occipital Lobe",
    parietal: "Parietal Lobe",
    temporal: "Temporal Lobe",
    frontal: "Frontal Lobe"
  }

  @module_info %{
    lifg: %{
      title: "LIFG — Competitive Sense Selection",
      modules: ~w(Brain.LIFG Brain.LIFG.Stage1 Core.LIFG.Input)a,
      summary: """
      Selects the winning sense per token from `si.sense_candidates`, with boundary guards and scoring:
      lex_fit, rel_prior, activation, intent_bias (+ episodes boost).
      """,
      telemetry:
        ~w(brain.lifg.stage1.start brain.lifg.stage1.stop brain.lifg.chargram_violation brain.lifg.boundary_drop),
      config: [
        {:brain, :lifg_stage1_weights,
         %{lex_fit: 0.40, rel_prior: 0.30, activation: 0.20, intent_bias: 0.10}},
        {:brain, :lifg_stage1_scores_mode, :all}
      ]
    },
    pmtg: %{
      title: "PMTG — Controlled Semantic Retrieval",
      modules: ~w(Brain.PMTG)a,
      summary:
        "Interfaces selection demands (from LIFG) with knowledge access dynamics; surfaces near-winners into `si.sense_candidates`.",
      telemetry: ~w(brain.pmtg.no_mwe_senses),
      config: [{:brain, :pmtg_margin_threshold, 0.15}, {:brain, :pmtg_mode, :boost}]
    },
    hippocampus: %{
      title: "Hippocampus — Episodic Write/Recall",
      modules: ~w(Brain.Hippocampus Db.Episode)a,
      summary:
        "Writes episodes at ATL finalize; recalls by norms overlap + recency half-life; biases LIFG.",
      telemetry: ~w(brain.hippo.write brain.hippo.recall),
      config: [{:db, :pgvector, true}]
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

  # ────────────────────────────────────────────────────────────────────────────
  # LiveView lifecycle
  # ────────────────────────────────────────────────────────────────────────────

  @impl true
  def mount(_params, _session, socket) do
    selected = :lifg
    _ = Brain.CycleClock.ensure_started()

    # Base assigns (before we attach anything)
    socket =
      socket
      |> assign(:selected, selected)
      |> assign(:regions, @regions)
      |> assign(:module_info, @module_info)
      |> assign(:full_names, @region_full_names)
      |> assign(:snapshot, Introspect.snapshot(selected))
      |> assign(:hippo, safe_snapshot_hippo())
      |> assign(:hippo_metrics, nil)
      |> assign(:lifg_last, safe_last_lifg())
      |> assign(:intent, nil)
      |> assign(:auto, false)
      |> assign(:timer_ref, nil)
      |> assign(:telemetry_hippo_id, nil)
      |> assign(:telemetry_cycle_id, nil)
      |> assign(:telemetry_intent_ids, [])
      |> assign(:cycle_nonce, 0)

    if connected?(socket) do
      # Attach telemetry only when connected to avoid orphan handlers during disconnected mount
      hippo_handler_id =
        "brain-live-hippo-#{System.unique_integer([:positive])}-#{System.system_time(:microsecond)}"

      :ok =
        :telemetry.attach(
          hippo_handler_id,
          [:brain, :hippocampus, :recall],
          &__MODULE__.telemetry_handler/4,
          %{pid: self()}
        )

      cycle_handler_id =
        "brain-live-cycle-#{System.unique_integer([:positive])}-#{System.system_time(:microsecond)}"

      :ok =
        :telemetry.attach(
          cycle_handler_id,
          [:brain, :cycle, :tick],
          &__MODULE__.cycle_telemetry_handler/4,
          %{pid: self()}
        )

      # Intent telemetry (optional fast-path; cycle pull remains the source of truth)
      intent_id_core =
        "brain-live-intent-core-#{System.unique_integer([:positive])}-#{System.system_time(:microsecond)}"

      _ =
        :telemetry.attach(
          intent_id_core,
          [:core, :intent, :selected],
          &__MODULE__.intent_telemetry_handler/4,
          %{pid: self()}
        )

      intent_id_brain =
        "brain-live-intent-brain-#{System.unique_integer([:positive])}-#{System.system_time(:microsecond)}"

      _ =
        :telemetry.attach(
          intent_id_brain,
          [:brain, :intent, :selected],
          &__MODULE__.intent_telemetry_handler/4,
          %{pid: self()}
        )

      # Seed intent from Brain so the chip has content immediately
      init_intent =
        case Process.whereis(Brain) do
          nil -> nil
          _ -> Brain.latest_intent()
        end

      {:ok,
       socket
       |> assign(:telemetry_hippo_id, hippo_handler_id)
       |> assign(:telemetry_cycle_id, cycle_handler_id)
       |> assign(:telemetry_intent_ids, [intent_id_core, intent_id_brain])
       |> assign(:intent, init_intent)}
    else
      {:ok, socket}
    end
  end

  @impl true
  def terminate(_reason, socket) do
    if id = socket.assigns[:telemetry_hippo_id], do: :telemetry.detach(id)
    if id = socket.assigns[:telemetry_cycle_id], do: :telemetry.detach(id)

    case socket.assigns[:telemetry_intent_ids] do
      ids when is_list(ids) -> Enum.each(ids, &:telemetry.detach/1)
      _ -> :ok
    end

    if ref = socket.assigns[:timer_ref], do: :timer.cancel(ref)
    :ok
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Telemetry bridges
  # ────────────────────────────────────────────────────────────────────────────

  @doc false
  def telemetry_handler(_event, measurements, metadata, %{pid: pid}) when is_pid(pid) do
    send(pid, {:hippo_telemetry, measurements, metadata})
  end

  def telemetry_handler(_, _, _, _), do: :ok

  @doc false
  def cycle_telemetry_handler(_event, _meas, _meta, %{pid: pid}) when is_pid(pid) do
    send(pid, :cycle_tick)
  end

  def cycle_telemetry_handler(_, _, _, _), do: :ok

  @doc false
  def intent_telemetry_handler(_event, meas, meta, %{pid: pid}) when is_pid(pid) do
    send(pid, {:intent_selected, meas, meta})
  end

  def intent_telemetry_handler(_, _, _, _), do: :ok

  # ────────────────────────────────────────────────────────────────────────────
  # Events
  # ────────────────────────────────────────────────────────────────────────────

  @impl true
  def handle_event("select_region", %{"region" => region}, socket) do
    region_atom = region |> String.to_existing_atom()
    snap = Introspect.snapshot(region_atom)
    {:noreply,
     socket
     |> assign(:selected, region_atom)
     |> assign(:snapshot, snap)
     |> assign(:lifg_last, safe_last_lifg())}
  rescue
    _ -> {:noreply, socket}
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    {:noreply, assign(socket, refresh_all(socket.assigns))}
  end

  @impl true
  def handle_event("toggle_auto", _params, socket) do
    case socket.assigns.auto do
      false ->
        {:ok, ref} = :timer.send_interval(1000, :tick)
        {:noreply, socket |> assign(:auto, true) |> assign(:timer_ref, ref)}

      true ->
        if socket.assigns.timer_ref, do: :timer.cancel(socket.assigns.timer_ref)
        {:noreply, socket |> assign(:auto, false) |> assign(:timer_ref, nil)}
    end
  end

  # Hippocampus controls
  @impl true
  def handle_event("hippo_source", %{"v" => v}, socket) do
    :ok = Hippocampus.configure(recall_source: v)
    {:noreply, assign(socket, :hippo, safe_snapshot_hippo())}
  end

  @impl true
  def handle_event("hippo_reset", _params, socket) do
    :ok = Hippocampus.reset()
    {:noreply, socket |> assign(:hippo, safe_snapshot_hippo()) |> assign(:hippo_metrics, nil)}
  end

  @impl true
  def handle_event("hippo_seed", _params, socket) do
    now = System.system_time(:second)

    for i <- 1..5 do
      slate = %{
        winners: [%{lemma: "demo_#{i}", norm: "demo_#{i}"}],
        tokens: [%{n: 1, phrase: "demo #{i}"}],
        score: :rand.uniform()
      }

      meta = %{tenant: "demo", ts: now - i}
      Hippocampus.encode(slate, meta)
    end

    {:noreply, assign(socket, :hippo, safe_snapshot_hippo())}
  end

  # Auto-refresh + telemetry ticks
  @impl true
  def handle_info(:tick, socket) do
    {:noreply, assign(socket, refresh_all(socket.assigns))}
  end

  @impl true
  def handle_info({:hippo_telemetry, meas, meta}, socket) do
    compact =
      %{
        at_ms: System.system_time(:millisecond),
        source: meta[:source] || meta["source"],
        k: meas[:k] || meas["k"],
        mem_k: meas[:mem_k] || meas["mem_k"],
        db_k: meas[:db_k] || meas["db_k"],
        had_embedding?: meas[:had_embedding?] || meas["had_embedding?"],
        top_score: meas[:top_score] || meas["top_score"],
        avg_age_ms: meas[:avg_age_ms] || meas["avg_age_ms"]
      }
      |> Enum.reject(fn {_k, v} -> is_nil(v) end)
      |> Map.new()

    {:noreply, assign(socket, :hippo_metrics, compact)}
  end

  # Optional fast-path: immediate update when intent telemetry fires
  @impl true
  def handle_info({:intent_selected, meas, meta}, socket) do
    now = System.system_time(:millisecond)

    intent =
      meta[:intent] || meas[:intent] || meta["intent"] || meas["intent"] || nil

    keyword =
      meta[:keyword] || meas[:keyword] || meta["keyword"] || meas["keyword"] || nil

    confidence =
      meas[:confidence] || meta[:confidence] || meas["confidence"] || meta["confidence"] || nil

    norm_intent =
      cond do
        is_atom(intent) -> intent
        is_binary(intent) ->
          try do
            String.to_existing_atom(intent)
          rescue
            _ -> String.to_atom(intent)
          end
        true -> nil
      end

    norm_conf =
      cond do
        is_number(confidence) -> confidence * 1.0
        is_binary(confidence) ->
          case Float.parse(confidence) do
            {f, _} -> f
            _ -> nil
          end
        true -> nil
      end

    compact = %{
      at_ms: now,
      intent: norm_intent,
      keyword: (if is_binary(keyword), do: keyword, else: to_string(keyword || "")),
      confidence: norm_conf
    }

    {:noreply, assign(socket, :intent, compact)}
  end

  # Cycle-driven source of truth: pull from Brain.latest_intent/0 every tick
  @impl true
  def handle_info(:cycle_tick, socket) do
    latest =
      case Process.whereis(Brain) do
        nil -> socket.assigns.intent
        _ -> Brain.latest_intent() || socket.assigns.intent
      end

    socket =
      if latest != socket.assigns.intent do
        socket
        |> assign(:intent, latest)
        |> assign(:cycle_nonce, socket.assigns.cycle_nonce + 1)
      else
        assign(socket, :cycle_nonce, socket.assigns.cycle_nonce + 1)
      end

    {:noreply, socket}
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Helpers
  # ────────────────────────────────────────────────────────────────────────────

  defp refresh_all(assigns) do
    %{
      snapshot: Introspect.snapshot(assigns.selected),
      hippo: safe_snapshot_hippo(),
      lifg_last: safe_last_lifg()
    }
  end

  defp safe_snapshot_hippo do
    try do
      Hippocampus.snapshot()
    rescue
      _ ->
        %{
          window_keep: 0,
          window: [],
          last: nil,
          opts: %{
            half_life_ms: 0,
            recall_limit: 0,
            min_jaccard: 0.0,
            recall_source: :memory
          }
        }
    end
  end

  defp safe_last_lifg do
    try do
      case LIFG.last() do
        :empty -> nil
        m when is_map(m) -> m
        _ -> nil
      end
    rescue
      _ -> nil
    end
  end

  defp ms_ago_system(at_ms) when is_integer(at_ms) do
    max(System.system_time(:millisecond) - at_ms, 0)
  end

  defp ms_ago(at_ms) when is_integer(at_ms) do
    now = System.monotonic_time(:millisecond)
    max(now - at_ms, 0)
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Render
  # ────────────────────────────────────────────────────────────────────────────

  @impl true
  def render(assigns) do
    ~H"""
    <div class="mx-auto max-w-6xl px-4 py-6">
      <div class="mb-4 flex flex-col gap-2">
        <div class="flex items-center justify-between gap-4">
          <h1 class="text-2xl font-semibold">Brain Regions ↔ Symbrella Modules</h1>

          <div class="flex items-center gap-2">
            <CycleHUD.cycle_hud />
            <.intent_chip intent={@intent} />
            <button
              phx-click="refresh"
              class="rounded-md border px-3 py-1 text-sm hover:bg-zinc-50">
              Refresh
            </button>
            <button
              phx-click="toggle_auto"
              class={["rounded-md border px-3 py-1 text-sm", @auto && "bg-emerald-50 border-emerald-300"]}>
              <%= if @auto, do: "Auto: ON", else: "Auto: OFF" %>
            </button>
          </div>
        </div>

        <div class="text-sm opacity-70">
          Left-lateral human brain (vector, hand-drawn)
        </div>
      </div>

      <div class="grid grid-cols-1 lg:grid-cols-2 gap-6 items-start">
        <div class="w-full">
          <.brain_svg selected={@selected} />
          <div class="mt-2 text-xs opacity-70">Click any colored region to view details.</div>
        </div>

        <div id="info" class="w-full">
          <.info_panel
            selected={@selected}
            data={@module_info[@selected]}
            snapshot={@snapshot}
            hippo={@hippo}
            hippo_metrics={@hippo_metrics}
            full_names={@full_names} />

          <!-- LIFG Decision Card -->
          <%= if @selected == :lifg do %>
            <.lifg_decision last={@lifg_last} />
          <% end %>

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
                  <span
                    class="mr-2 inline-block h-3 w-3 rounded-full align-middle"
                    style={"background: #{color}"}
                  >
                  </span>
                  {label}
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
      <desc id="desc">
        Interactive diagram. Click colored regions to reveal corresponding Symbrella module details.
      </desc>

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
        selected={@selected == :frontal or @selected == :lifg or @selected == :ofc}
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
        selected={@selected == :temporal or @selected == :pmtg or @selected == :hippocampus}
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

  # ── FIXED: Bind vars explicitly so HEEx scope is clear
  attr :intent, :map, default: nil
  def intent_chip(assigns) do
    ~H"""
    <% intent = @intent %>
    <% conf = if intent, do: intent[:confidence], else: nil %>
    <% kw = if intent, do: (intent[:keyword] || ""), else: "" %>

    <span
      class={[
        "inline-flex items-center gap-2 rounded-full border px-3 py-1 text-xs",
        conf_class(intent)
      ]}
      title="Latest intent (Brain source-of-truth)"
    >
      <span class="opacity-60">Intent:</span>
      <code class="font-mono">
        <%= if intent && intent.intent, do: intent.intent, else: "—" %>
      </code>

      <%= if kw != "" do %>
        <span class="opacity-60">•</span>
        <span><%= kw %></span>
      <% end %>

      <%= if is_number(conf) do %>
        <span class="opacity-60">•</span>
        <span><%= :erlang.float_to_binary(conf * 100.0, [:compact, {:decimals, 1}]) %>%</span>
      <% end %>

      <%= if intent && is_integer(intent[:at_ms]) do %>
        <span class="opacity-60">•</span>
        <span class="opacity-60">
          updated <%= System.system_time(:millisecond) - intent.at_ms %> ms ago
        </span>
      <% end %>
    </span>
    """
  end

  defp conf_class(%{confidence: c}) when is_number(c) do
    cond do
      c >= 0.75 -> "bg-emerald-50 border-emerald-300"
      c >= 0.50 -> "bg-amber-50 border-amber-300"
      true -> "bg-zinc-50 border-zinc-300"
    end
  end

  defp conf_class(_), do: "bg-zinc-50 border-zinc-200"

  attr :selected, :atom, required: true
  attr :data, :map, required: true
  attr :snapshot, :map, default: nil
  attr :hippo, :map, default: nil
  attr :hippo_metrics, :map, default: nil
  attr :full_names, :map, default: %{}
  def info_panel(assigns) do
    ~H"""
    <div class="rounded-2xl border p-5 bg-white">
      <div class="flex items-start justify-between gap-4">
        <div>
          <h2 class="text-xl font-semibold">{@data.title}</h2>
          <div class="mt-1 text-xs opacity-70">
            <%= Map.get(@full_names, @selected, "—") %>
          </div>
        </div>
        <span class="inline-flex items-center gap-2 rounded-full border px-3 py-1 text-xs">
          <span class="opacity-60">Selected:</span>
          <code class="font-mono">{@selected}</code>
        </span>
      </div>
      <p class="mt-2 text-sm leading-relaxed opacity-90">{@data.summary}</p>

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

      <div class="mt-6 rounded-2xl border p-4">
        <div class="flex items-center justify-between gap-3">
          <h3 class="font-medium">Live state</h3>
          <div class="text-xs opacity-70">
            <%= if @snapshot && @snapshot.running? do %>
              <span class="inline-flex items-center gap-2">
                <span class="inline-block h-2 w-2 rounded-full bg-emerald-500"></span>
                running
              </span>
            <% else %>
              <span class="inline-flex items-center gap-2">
                <span class="inline-block h-2 w-2 rounded-full bg-rose-500"></span>
                down
              </span>
            <% end %>
          </div>
        </div>

        <%= if @snapshot do %>
          <div class="mt-3 grid grid-cols-1 md:grid-cols-2 gap-4">
            <div class="text-sm">
              <h4 class="font-medium mb-1">Process</h4>
              <ul class="space-y-1">
                <li><span class="opacity-60">module:</span> <code><%= inspect(@snapshot.module) %></code></li>
                <li><span class="opacity-60">pid:</span> <code><%= inspect(@snapshot.pid) %></code></li>
                <%= if is_map(@snapshot.info) do %>
                  <li><span class="opacity-60">queue:</span> <code><%= @snapshot.info[:message_queue_len] || 0 %></code></li>
                  <li><span class="opacity-60">current:</span> <code><%= inspect(@snapshot.info[:current_function]) %></code></li>
                <% end %>
              </ul>
            </div>

            <div class="text-sm">
              <h4 class="font-medium mb-1">State</h4>
              <div class="rounded border bg-zinc-50 p-2 overflow-auto max-h-56">
                <pre class="text-xs whitespace-pre-wrap">
<%= inspect(@snapshot.state, pretty: true, width: 80, limit: :infinity, printable_limit: 500) %>
                </pre>
              </div>
            </div>
          </div>
        <% else %>
          <div class="mt-2 text-sm opacity-70">— no snapshot —</div>
        <% end %>
      </div>

      <%= if @selected == :hippocampus and is_map(@hippo) do %>
        <div class="mt-4 rounded-2xl border p-4">
          <div class="flex items-center justify-between">
            <h3 class="font-medium">Hippocampus — window &amp; knobs</h3>
            <div class="flex items-center gap-2">
              <button phx-click="hippo_reset" class="rounded-md border px-2.5 py-1 text-xs hover:bg-zinc-50">Reset</button>
              <button phx-click="hippo_seed"  class="rounded-md border px-2.5 py-1 text-xs hover:bg-zinc-50">Seed 5</button>
            </div>
          </div>

          <div class="mt-3 grid grid-cols-1 md:grid-cols-3 gap-3 text-sm">
            <div class="rounded-lg border p-3">
              <div class="opacity-60 text-xs mb-1">Window</div>
              <div>size: <b><%= length(@hippo.window || []) %></b> / keep: <b><%= @hippo.window_keep %></b></div>
              <div>last write: <%= case @hippo.last do
                {at, _} when is_integer(at) -> "#{ms_ago(at)} ms ago"
                _ -> "—"
              end %></div>
            </div>

            <div class="rounded-lg border p-3">
              <div class="opacity-60 text-xs mb-1">Knobs</div>
              <div>half_life_ms: <b><%= get_in(@hippo, [:opts, :half_life_ms]) %></b></div>
              <div>recall_limit: <b><%= get_in(@hippo, [:opts, :recall_limit]) %></b></div>
              <div>min_jaccard: <b><%= get_in(@hippo, [:opts, :min_jaccard]) %></b></div>
            </div>

            <div class="rounded-lg border p-3">
              <div class="opacity-60 text-xs mb-1">Recall source</div>
              <form phx-change="hippo_source" class="flex items-center gap-2">
                <select name="v" class="rounded-md border px-2 py-1 text-sm">
                  <%= for opt <- ~w(memory db hybrid)a do %>
                    <option value={opt} selected={get_in(@hippo, [:opts, :recall_source]) == opt}><%= opt %></option>
                  <% end %>
                </select>
                <span class="text-xs opacity-70">switches backend</span>
              </form>
            </div>
          </div>

          <div class="mt-3 rounded-lg border p-3 text-sm">
            <div class="opacity-60 text-xs mb-1">Latest recall (telemetry)</div>
            <%= if is_map(@hippo_metrics) do %>
              <div class="grid grid-cols-2 md:grid-cols-4 gap-x-4 gap-y-1">
                <div>source: <b><%= @hippo_metrics[:source] || "?" %></b></div>
                <div>k: <b><%= @hippo_metrics[:k] || "—" %></b></div>
                <div>mem_k: <b><%= @hippo_metrics[:mem_k] || "—" %></b></div>
                <div>db_k: <b><%= @hippo_metrics[:db_k] || "—" %></b></div>
                <%= if Map.has_key?(@hippo_metrics, :had_embedding?) do %>
                  <div>had_embedding?: <b><%= inspect(@hippo_metrics[:had_embedding?]) %></b></div>
                <% end %>
                <%= if Map.has_key?(@hippo_metrics, :top_score) do %>
                  <div>top_score: <b><%= :erlang.float_to_binary(@hippo_metrics[:top_score] * 1.0, [:compact, decimals: 3]) %></b></div>
                <% end %>
                <%= if Map.has_key?(@hippo_metrics, :avg_age_ms) do %>
                  <div>avg_age_ms: <b><%= @hippo_metrics[:avg_age_ms] %></b></div>
                <% end %>
                <div class="col-span-2 md:col-span-4 text-xs opacity-60">
                  <%= ts = @hippo_metrics[:at_ms] || 0 %>
                  updated: <%= if ts > 0, do: "#{System.system_time(:millisecond) - ts} ms ago", else: "—" %>
                </div>
              </div>
            <% else %>
              <div class="opacity-70">— no recall events yet —</div>
            <% end %>
          </div>

          <details class="mt-3 rounded-lg border p-3">
            <summary class="cursor-pointer text-sm font-medium">State inspector</summary>
            <pre class="mt-2 overflow-auto text-xs"><%= inspect(@hippo, pretty: true, limit: :infinity) %></pre>
          </details>
        </div>
      <% end %>
    </div>
    """
  end

  # ===== NEW: LIFG Decision Card =====

  attr :last, :map, default: nil
  def lifg_decision(assigns) do
    ~H"""
    <div class="mt-4 rounded-2xl border p-4">
      <div class="flex items-center justify-between gap-2">
        <h3 class="font-medium">LIFG Decision (last)</h3>
        <div class="text-xs opacity-70">
          <%= if @last && is_integer(@last[:ts_ms]) do %>
            updated <%= ms_ago_system(@last.ts_ms) %> ms ago
          <% else %>
            — no snapshot —
          <% end %>
        </div>
      </div>

      <%= if is_map(@last) do %>
        <div class="mt-2 grid grid-cols-1 md:grid-cols-2 gap-3 text-sm">
          <div class="rounded-lg border p-3 bg-zinc-50">
            <div><span class="opacity-60">sentence:</span> <b><%= @last[:si_sentence] || "—" %></b></div>
            <div class="mt-1"><span class="opacity-60">intent:</span> <b><%= @last[:intent] || "—" %></b></div>
            <div class="mt-1">
              <span class="opacity-60">confidence:</span>
              <b><%= if is_number(@last[:confidence]), do: :erlang.float_to_binary(@last.confidence * 100.0, [:compact, {:decimals, 1}]) <> "%", else: "—" %></b>
            </div>
            <div class="mt-1">
              <span class="opacity-60">guards:</span>
              <b>chargrams=<%= get_in(@last, [:guards, :chargram_violation]) || 0 %></b>,
              <b>boundary_drops=<%= length(get_in(@last, [:guards, :rejected_by_boundary]) || []) %></b>
            </div>
          </div>

          <div class="rounded-lg border p-3">
            <div class="opacity-60 text-xs mb-1">Tokens</div>
  <%  tokens = (@last[:tokens] || []) %>
            <%= if tokens == [] do %>
              <div class="text-xs opacity-70">— none —</div>
            <% else %>
              <ul class="space-y-1">
                <%= for t <- tokens do %>
                  <li>
                    <code class="px-1 py-0.5 rounded bg-zinc-50 border">
                      [<%= t[:index] %>]
                      <%= if t[:mw], do: "«#{t[:phrase]}»", else: "'#{t[:phrase]}'" %>
                      <%= if t[:n], do: "·n=#{t[:n]}", else: "" %>
                    </code>
                  </li>
                <% end %>
              </ul>
            <% end %>
          </div>
        </div>

        <div class="mt-3 rounded-lg border p-3">
          <div class="opacity-60 text-xs mb-2">Per-token choices & top scores</div>


 <%  choices  = (@last[:choices]  || []) %>
 <%  finalists = (@last[:finalists] || []) %>

          <%= if choices == [] do %>
            <div class="text-xs opacity-70">— no choices captured —</div>
          <% else %>
            <div class="space-y-3">
              <%= for ch <- choices do %>
                <% idx = ch[:token_index] %>
                <% rank = Enum.find(finalists, fn f -> f[:token_index] == idx end) %>
                <div class="rounded-md border p-2">
                  <div class="text-sm">
                    <b>token <%= idx %>:</b>
                    chosen_id=<code class="px-1 py-0.5 rounded bg-zinc-50 border"><%= ch[:chosen_id] || "?" %></code>
                    · margin=<b><%= :erlang.float_to_binary((ch[:margin] || 0.0) * 1.0, [:compact, {:decimals, 3}]) %></b>
                  </div>
                  <%= if is_map(ch[:scores]) and map_size(ch[:scores]) > 0 do %>
                    <div class="mt-1 text-xs opacity-70">scores (top 3):</div>
                    <ul class="text-xs grid grid-cols-1 md:grid-cols-3 gap-1">
                      <%= for {id, s} <- (rank && rank[:ranking] || ch[:scores] |> Enum.sort_by(fn {_id, s} -> -s end)) |> Enum.take(3) do %>
                        <li>
                          <code class="px-1 py-0.5 rounded bg-zinc-50 border"><%= id %></code>
                          <span class="opacity-60">=</span>
                          <b><%= :erlang.float_to_binary(s * 1.0, [:compact, {:decimals, 3}]) %></b>
                        </li>
                      <% end %>
                    </ul>
                  <% else %>
                    <div class="mt-1 text-xs opacity-70">— no score map —</div>
                  <% end %>
                </div>
              <% end %>
            </div>
          <% end %>
        </div>
      <% else %>
        <div class="text-sm opacity-70 mt-2">No LIFG snapshot captured yet. Trigger a probe or interact with the system to populate this card.</div>
      <% end %>
    </div>
    """
  end
end

