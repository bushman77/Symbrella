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

  # UI pieces (split into focused components)
  alias SymbrellaWeb.Components.CycleHUD
  alias SymbrellaWeb.Components.Brain.Diagram,      as: BrainDiagram
  alias SymbrellaWeb.Components.Brain.InfoPanel,     as: BrainInfoPanel
  alias SymbrellaWeb.Components.Brain.LIFGDecision,  as: BrainLIFGDecision
  alias SymbrellaWeb.Components.Brain.HippoPanel,    as: BrainHippoPanel
  alias SymbrellaWeb.Components.Brain.IntentChip,    as: BrainIntentChip
  alias SymbrellaWeb.Components.MoodChip

  # ────────────────────────────────────────────────────────────────────────────
  # Region metadata (kept local; no Brain.Legend module)
  # ────────────────────────────────────────────────────────────────────────────

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

    # Prime mood from current snapshot so UI isn't empty at first render
    %{levels: lv} = Brain.MoodCore.snapshot()
    init_mood_levels = %{
      :da    => Map.get(lv, :da, 0.5),
      :"5ht" => Map.get(lv, :"5ht", 0.5),
      :ne    => Map.get(lv, :ne, 0.5),
      :glu   => Map.get(lv, :glu, 0.5)
    }

    init_mood_derived = %{
      exploration: 0.6 * init_mood_levels.da + 0.4 * init_mood_levels.ne,
      inhibition:  init_mood_levels[:"5ht"],
      vigilance:   init_mood_levels.ne,
      plasticity:  0.5 * init_mood_levels.da + 0.5 * init_mood_levels.glu
    }

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
      |> assign(:telemetry_mood_id, nil)
      |> assign(:cycle_nonce, 0)
      |> assign(:mood_levels, init_mood_levels)
      |> assign(:mood_derived, init_mood_derived)
      |> assign(:mood_tone, :neutral)        # NEW: tone default

    if connected?(socket) do
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

      # Mood telemetry
      mood_handler_id =
        "brain-live-mood-#{System.unique_integer([:positive])}-#{System.system_time(:microsecond)}"

      :ok =
        :telemetry.attach(
          mood_handler_id,
          [:brain, :mood, :update],
          &__MODULE__.mood_telemetry_handler/4,
          %{pid: self()}
        )

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

      init_intent =
        case Process.whereis(Brain) do
          nil -> nil
          _ -> Brain.latest_intent()
        end

      {:ok,
       socket
       |> assign(:telemetry_hippo_id, hippo_handler_id)
       |> assign(:telemetry_cycle_id, cycle_handler_id)
       |> assign(:telemetry_mood_id, mood_handler_id)
       |> assign(:telemetry_intent_ids, [intent_id_core, intent_id_brain])
       |> assign(:intent, init_intent)
       |> assign(:mood_tone, tone_for_intent(extract_label(init_intent)))}
    else
      {:ok, socket}
    end
  end

  @impl true
  def terminate(_reason, socket) do
    if id = socket.assigns[:telemetry_hippo_id], do: :telemetry.detach(id)
    if id = socket.assigns[:telemetry_cycle_id], do: :telemetry.detach(id)
    if id = socket.assigns[:telemetry_mood_id],  do: :telemetry.detach(id)

    case socket.assigns[:telemetry_intent_ids] do
      ids when is_list(ids) -> Enum.each(ids, &:telemetry.detach/1)
      _ -> :ok
    end

    if ref = socket.assigns[:timer_ref], do: :timer.cancel(ref)
    :ok
  end

  # Telemetry → LiveView mailbox bridge for mood updates
  def mood_telemetry_handler(_event, meas, _meta, %{pid: pid}) when is_pid(pid) do
    send(pid, {:mood_update, meas})
  end

  @impl true
  def handle_info({:mood_update, meas}, socket) do
    levels = %{
      :da    => meas[:da],
      :"5ht" => meas[:"5ht"],
      :ne    => meas[:ne],
      :glu   => meas[:glu]
    }

    derived = %{
      exploration: meas[:exploration],
      inhibition:  meas[:inhibition],
      vigilance:   meas[:vigilance],
      plasticity:  meas[:plasticity]
    }

    {:noreply, assign(socket, mood_levels: levels, mood_derived: derived)}
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

  # Intent telemetry → update chip + tone
  @impl true
  def handle_info({:intent_selected, meas, meta}, socket) do
    now = System.system_time(:millisecond)

    intent =
      meta[:intent] || meas[:intent] || meta["intent"] || meas["intent"] || nil

    keyword =
      meta[:keyword] || meas[:keyword] || meta["keyword"] || meas["keyword"] || ""

    text =
      meta[:text] || meas[:text] || meta["text"] || meas["text"] || keyword

    confidence =
      meas[:confidence] || meta[:confidence] || meas["confidence"] || meta["confidence"] || nil

    source = meta[:source] || meas[:source] || :core

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

    {label_fixed, conf_fixed} = normalize_intent_label(norm_intent, text, norm_conf)

    compact = %{
      at_ms: now,
      intent: label_fixed,
      keyword: (if is_binary(keyword), do: keyword, else: to_string(keyword || "")),
      confidence: conf_fixed,
      source: source
    }

    {:noreply,
     socket
     |> assign(:intent, compact)
     |> assign(:mood_tone, tone_for_intent(label_fixed))}
  end

  # Cycle-driven source of truth
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
        |> assign(:mood_tone, tone_for_intent(extract_label(latest)))
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

  # Intent normalization: keep :abuse / :insult intact, only gate :ask
  defp normalize_intent_label(label, text, conf) do
    label1 =
      case label do
        :ask ->
          if looks_like_question?(text), do: :ask, else: :unknown
        nil ->
          :unknown
        other ->
          other
      end

    conf1 = conf || 0.0
    label2 =
      if conf1 < 0.60 and label1 == :ask, do: :unknown, else: label1

    {label2, conf1}
  end

  defp looks_like_question?(s) when is_binary(s) do
    String.contains?(s, "?") ||
      Regex.match?(
        ~r/^\s*(who|what|when|where|why|how|do|does|did|can|could|will|would|should|is|are|am|have|has|had|may|might|was|were)\b/i,
        s
      )
  end
  defp looks_like_question?(_), do: false

  # tone helpers
  defp tone_for_intent(:abuse),  do: :danger
  defp tone_for_intent(:insult), do: :warning
  defp tone_for_intent(_),       do: :neutral

  defp extract_label(nil), do: nil
  defp extract_label(%{intent: i}) when is_atom(i), do: i
  defp extract_label(i) when is_atom(i), do: i
  defp extract_label(_), do: nil

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

          <div class="flex items-center gap-2 flex-wrap">
            <CycleHUD.cycle_hud />
            <BrainIntentChip.intent_chip intent={@intent} class="bg-white/70" />
            <MoodChip.mood_chip levels={@mood_levels} derived={@mood_derived} tone={@mood_tone} />
            <button phx-click="refresh" class="rounded-md border px-3 py-1 text-sm hover:bg-zinc-50">
              Refresh
            </button>
            <button
              phx-click="toggle_auto"
              class={["rounded-md border px-3 py-1 text-sm", @auto && "bg-emerald-50 border-emerald-300"]}>
              <%= if @auto, do: "Auto: ON", else: "Auto: OFF" %>
            </button>
          </div>
        </div>
        <div class="text-sm opacity-70">Left-lateral human brain (vector, hand-drawn)</div>
      </div>

      <div class="grid grid-cols-1 lg:grid-cols-2 gap-6 items-start">
        <div class="w-full">
          <BrainDiagram.brain_diagram selected={@selected} />
          <div class="mt-2 text-xs opacity-70">Click any colored region to view details.</div>
        </div>

        <div id="info" class="w-full">
          <BrainInfoPanel.info_panel
            selected={@selected}
            data={@module_info[@selected]}
            snapshot={@snapshot}
            hippo={@hippo}
            hippo_metrics={@hippo_metrics}
            full_names={@full_names} />

          <%= if @selected == :hippocampus do %>
            <BrainHippoPanel.hippo_panel hippo={@hippo} hippo_metrics={@hippo_metrics} />
          <% end %>

          <%= if @selected == :lifg do %>
            <BrainLIFGDecision.lifg_decision last={@lifg_last} />
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
                  <span class="mr-2 inline-block h-3 w-3 rounded-full align-middle" style={"background: #{color}"} />
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
end

