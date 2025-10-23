defmodule SymbrellaWeb.BrainLive do
  @moduledoc """
  LiveView for the Brain dashboard. All HTML is delegated to `SymbrellaWeb.BrainHTML`.

  • Safe defaults so first render never crashes (even if upstream is quiet)
  • Subscribes to Brain.Bus for blackboard + HUD topics (clock/intent/mood/lifg)
  • Telemetry bridge for mood updates
  • Loads inline brain SVG from priv/static/images/brain.svg when available
  """

  use SymbrellaWeb, :live_view
  require Logger

  alias SymbrellaWeb.BrainHTML
  alias Brain.Bus

  @blackboard_topic "brain:blackboard"
  @hud_topics ~w(brain:clock brain:intent brain:mood brain:lifg)

  @mood_defaults %{
    levels: %{da: 0.35, "5ht": 0.50, glu: 0.40, ne: 0.50},
    derived: %{exploration: 0.41, inhibition: 0.50, vigilance: 0.50, plasticity: 0.375},
    tone: :neutral
  }

  # ---------------------------------------------------------------------------
  # LiveView lifecycle
  # ---------------------------------------------------------------------------

  @impl true
  def mount(_params, _session, socket) do
    socket =
      socket
      # Core assigns (safe defaults)
      |> assign_new(:selected,       fn -> :lifg end)
      |> assign_new(:regions,        fn -> [] end)
      |> assign_new(:module_info,    fn -> %{} end)
      |> assign_new(:intent,         fn -> %{} end)
      |> assign_new(:snapshot,       fn -> nil end)
      |> assign_new(:lifg_last,      fn -> %{} end)
      |> assign_new(:hippo,          fn -> %{window: []} end)
      |> assign_new(:hippo_metrics,  fn -> %{} end)
      |> assign_new(:region,         fn -> %{} end)
      |> assign_new(:region_state,   fn -> %{workspace: []} end)
      |> assign_new(:auto,           fn -> false end)
      |> assign_new(:clock,          fn -> %{} end)
      # Mood assigns + alias
      |> assign_new(:mood_levels,  fn -> @mood_defaults.levels  end)
      |> assign_new(:mood_derived, fn -> @mood_defaults.derived end)
      |> assign_new(:mood_tone,    fn -> @mood_defaults.tone    end)
      |> ensure_mood_alias()
      # SVG (inline if present, else nil so HTML falls back to /images)
      |> assign_new(:brain_svg, fn -> load_brain_svg() end)
      # Telemetry
      |> attach_mood_telemetry()

    # PubSub subscriptions after websocket connects
    if connected?(socket) do
      :ok = Bus.subscribe(@blackboard_topic)
      Enum.each(@hud_topics, &Bus.subscribe/1)
    end

    {:ok, socket}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    socket =
      case Map.get(params, "r") do
        nil -> socket
        r when is_binary(r) ->
          sel =
            try do
              String.to_existing_atom(r)
            rescue
              ArgumentError -> socket.assigns.selected
            end

          assign(socket, :selected, sel)
      end

    {:noreply, socket}
  end

  # ---------------------------------------------------------------------------
  # Events
  # ---------------------------------------------------------------------------

  # Dropdown change or overlay click -> select region
  @impl true
  def handle_event("select-region", params, socket) do
    r =
      Map.get(params, "region") ||
        Map.get(params, "value") ||
        Map.get(params, "r")

    sel =
      cond do
        is_atom(r) -> r
        is_binary(r) ->
          try do
            String.to_existing_atom(r)
          rescue
            ArgumentError -> socket.assigns.selected
          end

        true ->
          socket.assigns.selected
      end

    {:noreply, assign(socket, :selected, sel)}
  end

  # Refresh button is present; keep it no-op (or tie into your clock later)
  @impl true
  def handle_event("refresh", _params, socket), do: {:noreply, socket}

  # ---------------------------------------------------------------------------
  # Incoming telemetry + PubSub
  # ---------------------------------------------------------------------------

  # Mood telemetry bridge
  @impl true
  def handle_info({:mood_update, meas, _meta}, socket) do
    levels = %{
      da:   Map.get(meas, :da,   0.5),
      "5ht": Map.get(meas, :"5ht", 0.5),
      glu:  Map.get(meas, :glu,  0.5),
      ne:   Map.get(meas, :ne,   0.5)
    }

    derived = %{
      exploration: Map.get(meas, :exploration, 0.5),
      inhibition:  Map.get(meas, :inhibition,  0.5),
      vigilance:   Map.get(meas, :vigilance,   0.5),
      plasticity:  Map.get(meas, :plasticity,  0.5)
    }

    tone = socket.assigns[:mood_tone] || :neutral

    {:noreply,
     socket
     |> assign(:mood_levels, levels)
     |> assign(:mood_derived, derived)
     |> assign(:mood_tone, tone)
     |> assign(:mood, %{levels: levels, derived: derived, tone: tone})}
  end

  # Blackboard feed (current and legacy tags)
  @impl true
  def handle_info({tag, env}, socket) when tag in [:blackboard, :brain_bus] do
    {:noreply,
     update(socket, :region_state, fn st ->
       st = st || %{}
       ws = [env | (st[:workspace] || [])] |> Enum.take(100)
       Map.put(st, :workspace, ws)
     end)}
  end

  # HUD topics
  @impl true
  def handle_info({:clock, m}, socket),  do: {:noreply, assign(socket, :clock,  m || %{})}
  @impl true
  def handle_info({:intent, m}, socket), do: {:noreply, assign(socket, :intent, m || %{})}
  @impl true
  def handle_info({:mood, m}, socket) do
    # Accept PubSub mood frames too (optional; telemetry will also drive UI)
    mood = m || %{}
    levels  = Map.get(mood, :levels,  socket.assigns[:mood_levels]  || @mood_defaults.levels)
    derived = Map.get(mood, :derived, socket.assigns[:mood_derived] || @mood_defaults.derived)
    tone    = Map.get(mood, :tone,    socket.assigns[:mood_tone]    || @mood_defaults.tone)

    {:noreply,
     socket
     |> assign(:mood_levels, levels)
     |> assign(:mood_derived, derived)
     |> assign(:mood_tone, tone)
     |> assign(:mood, %{levels: levels, derived: derived, tone: tone})}
  end

  @impl true
  def handle_info({:lifg_update, m}, socket),
    do: {:noreply, assign(socket, :lifg_last, m || %{})}

  @impl true
  def handle_info(_msg, socket), do: {:noreply, socket}

  # ---------------------------------------------------------------------------
  # Termination
  # ---------------------------------------------------------------------------

  @impl true
  def terminate(_reason, %{assigns: %{telemetry_mood_id: id}}) when is_binary(id) do
    :telemetry.detach(id)
    :ok
  end

  def terminate(_reason, _socket), do: :ok

  # ---------------------------------------------------------------------------
  # Internal helpers
  # ---------------------------------------------------------------------------

  # Attaches to [:brain, :mood, :update] and forwards as {:mood_update, meas, meta}
  defp attach_mood_telemetry(socket) do
    id = "brain-live-mood-#{System.unique_integer([:positive])}"
    :ok = :telemetry.attach(id, [:brain, :mood, :update], &__MODULE__.on_mood_event/4, self())
    assign(socket, :telemetry_mood_id, id)
  end

  def on_mood_event(_event, meas, meta, pid) when is_pid(pid) do
    send(pid, {:mood_update, meas, meta})
  end

  def on_mood_event(_, _, _, _), do: :ok

  # Back-compat alias: synthesize @mood from split fields if missing
  defp ensure_mood_alias(socket) do
    a = socket.assigns

    mood =
      a[:mood] ||
        %{
          levels: Map.get(a, :mood_levels,  @mood_defaults.levels),
          derived: Map.get(a, :mood_derived, @mood_defaults.derived),
          tone: Map.get(a, :mood_tone, @mood_defaults.tone)
        }

    assign(socket, :mood, mood)
  end

  # Load /priv/static/images/brain.svg if present; else return nil
  defp load_brain_svg do
    with {:ok, priv} <- safe_priv_dir(:symbrella_web),
         path <- Path.join([priv, "static", "images", "brain.svg"]),
         true <- File.exists?(path),
         {:ok, svg} <- File.read(path) do
      svg
    else
      _ -> nil
    end
  end

  defp safe_priv_dir(app) do
    try do
      {:ok, :code.priv_dir(app) |> List.to_string()}
    rescue
      _ -> :error
    end
  end

  # ---------------------------------------------------------------------------
  # Render (delegated)
  # ---------------------------------------------------------------------------

  @impl true
  def render(assigns) do
    assigns =
      if Map.has_key?(assigns, :mood),
        do: assigns,
        else: Map.put(assigns, :mood, @mood_defaults)

    BrainHTML.brain(assigns)
  end
end

