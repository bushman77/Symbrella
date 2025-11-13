defmodule SymbrellaWeb.BrainLive.MoodHud do
  @moduledoc """
  Mood HUD for BrainLive.

  Extracted responsibilities:
    • Default mood assigns and seeding into the socket
    • Telemetry attach/detach (brain:mood events)
    • Event and handle_info helpers to keep LiveView tiny
    • Function component `<.mood_hud />` (compact, always-visible HUD)

  Usage (in BrainLive):
    import SymbrellaWeb.BrainLive.MoodHud, only: [mood_hud: 1]

    socket =
      socket
      |> SymbrellaWeb.BrainLive.MoodHud.seed()
      |> (fn s -> if connected?(s), do: SymbrellaWeb.BrainLive.MoodHud.attach(s), else: s end).()

    # In render:
    <.mood_hud mood={@mood} />

    # In terminate/2:
    _ = SymbrellaWeb.BrainLive.MoodHud.detach(socket)

    # In handle_info/2:
    def handle_info({:mood_event, _, _} = msg, socket), do: SymbrellaWeb.BrainLive.MoodHud.on_info(msg, socket)
    def handle_info({:mood_update, _, _} = msg, socket), do: SymbrellaWeb.BrainLive.MoodHud.on_info(msg, socket)
    def handle_info({:mood, _} = msg, socket),          do: SymbrellaWeb.BrainLive.MoodHud.on_info(msg, socket)
  """

  use SymbrellaWeb, :html
  require Logger

  @defaults %{
    levels: %{da: 0.35, "5ht": 0.50, glu: 0.40, ne: 0.50},
    derived: %{exploration: 0.41, inhibition: 0.50, vigilance: 0.50, plasticity: 0.375},
    tone: :neutral
  }

  # ----- Public API -----------------------------------------------------------

  def defaults, do: @defaults

  @doc "Seed default mood assigns on mount."
  def seed(socket) do
    socket
    |> Phoenix.Component.assign_new(:mood_levels, fn -> @defaults.levels end)
    |> Phoenix.Component.assign_new(:mood_derived, fn -> @defaults.derived end)
    |> Phoenix.Component.assign_new(:mood_tone, fn -> @defaults.tone end)
    |> Phoenix.Component.assign_new(:telemetry_mood_id, fn -> nil end)
    |> ensure_alias()
  end

  @doc "Attach telemetry handlers once (call only when connected?)."
  def attach(socket) do
    if socket.assigns[:telemetry_mood_id] do
      socket
    else
      id = "brain-mood-ui:#{inspect(self())}"

      events = [
        [:brain, :mood, :updated],
        [:brain, :mood, :tick],
        # legacy/alternate spelling
        [:brain, :mood, :update]
      ]

      try do
        if Code.ensure_loaded?(:telemetry) do
          :ok = :telemetry.attach_many(id, events, &__MODULE__.handle_telemetry/4, self())
        end
      rescue
        _ -> :ok
      end

      Phoenix.Component.assign(socket, :telemetry_mood_id, id)
    end
  end

  @doc "Detach telemetry handler if present."
  def detach(socket) do
    if id = socket.assigns[:telemetry_mood_id] do
      :telemetry.detach(id)
    end

    socket
  end

  @doc """
  `handle_info` helper. Return `{:noreply, socket}` for mood-related messages,
  or `:ignore` for non-mood messages (so your LiveView can fall through).
  """
  def on_info({:mood_event, meas, meta}, socket) do
    mood =
      meta[:mood] ||
        %{
          levels: Map.get(meta, :levels, %{}),
          derived: Map.get(meta, :derived, %{}),
          tone: Map.get(meta, :tone, :neutral)
        }

    {:noreply,
     socket
     |> Phoenix.Component.assign(:mood, mood)
     |> Phoenix.Component.assign(:mood_last, %{at: System.system_time(:millisecond), meas: meas})}
  end

  def on_info({:mood_update, meas, _meta}, socket) do
    levels = %{
      da: Map.get(meas, :da, 0.5),
      "5ht": Map.get(meas, :"5ht", 0.5),
      glu: Map.get(meas, :glu, 0.5),
      ne: Map.get(meas, :ne, 0.5)
    }

    derived = %{
      exploration: Map.get(meas, :exploration, 0.5),
      inhibition: Map.get(meas, :inhibition, 0.5),
      vigilance: Map.get(meas, :vigilance, 0.5),
      plasticity: Map.get(meas, :plasticity, 0.5)
    }

    tone = socket.assigns[:mood_tone] || :neutral

    {:noreply,
     socket
     |> Phoenix.Component.assign(:mood_levels, levels)
     |> Phoenix.Component.assign(:mood_derived, derived)
     |> Phoenix.Component.assign(:mood_tone, tone)
     |> Phoenix.Component.assign(:mood, %{levels: levels, derived: derived, tone: tone})}
  end

  def on_info({:mood, m}, socket) do
    mood = m || %{}

    levels = Map.get(mood, :levels, socket.assigns[:mood_levels] || @defaults.levels)
    derived = Map.get(mood, :derived, socket.assigns[:mood_derived] || @defaults.derived)
    tone = Map.get(mood, :tone, socket.assigns[:mood_tone] || @defaults.tone)

    {:noreply,
     socket
     |> Phoenix.Component.assign(:mood_levels, levels)
     |> Phoenix.Component.assign(:mood_derived, derived)
     |> Phoenix.Component.assign(:mood_tone, tone)
     |> Phoenix.Component.assign(:mood, %{levels: levels, derived: derived, tone: tone})}
  end

  def on_info(_msg, _socket), do: :ignore

  # ----- Telemetry callback fns (to be registered via attach/1) --------------

  # Canonical: send {:mood_event, measurements, metadata}
  def handle_telemetry(_event, measurements, metadata, lv_pid) when is_pid(lv_pid) do
    send(lv_pid, {:mood_event, measurements, metadata})
    :ok
  end

  # Non-PID callers: ignore safely
  def handle_telemetry(_event, _m, _md, _other), do: :ok

  # Legacy/alternate (kept for compatibility if you want to wire it elsewhere):
  def legacy_telemetry(_event, meas, meta, pid) when is_pid(pid) do
    send(pid, {:mood_update, meas, meta})
    :ok
  end

  def legacy_telemetry(_event, _meas, _meta, _other), do: :ok

  # ----- Function component: compact Mood HUD --------------------------------

  @doc "Compact, always-visible mood HUD chip."
  attr :mood, :map, default: @defaults

  def mood_hud(assigns) do
    mood = Map.get(assigns, :mood, @defaults) || @defaults

    assigns =
      assigns
      |> Phoenix.Component.assign(:tone, Map.get(mood, :tone, :neutral))
      |> Phoenix.Component.assign(:derived, Map.get(mood, :derived, %{}))

    ~H"""
    <div class="fixed bottom-3 right-3 z-30">
      <div class="text-xs px-2 py-1 rounded-md border bg-white/80 backdrop-blur flex gap-2 items-center shadow">
        <span class={["px-1 rounded border", tone_class(@tone)]}>{to_string(@tone)}</span>
        <span class="opacity-70">Expl</span><span class="font-mono"><%= fmt(@derived[:exploration]) %></span>
        <span class="opacity-70">Inhib</span><span class="font-mono"><%= fmt(@derived[:inhibition]) %></span>
        <span class="opacity-70">Vigil</span><span class="font-mono"><%= fmt(@derived[:vigilance]) %></span>
        <span class="opacity-70">Plast</span><span class="font-mono"><%= fmt(@derived[:plasticity]) %></span>
      </div>
    </div>
    """
  end

  # ----- Internals ------------------------------------------------------------

  defp ensure_alias(socket) do
    a = socket.assigns

    mood =
      a[:mood] ||
        %{
          levels: Map.get(a, :mood_levels, @defaults.levels),
          derived: Map.get(a, :mood_derived, @defaults.derived),
          tone: Map.get(a, :mood_tone, @defaults.tone)
        }

    Phoenix.Component.assign(socket, :mood, mood)
  end

  defp tone_class(:positive), do: "border-green-400 text-green-700"
  defp tone_class(:negative), do: "border-red-400 text-red-700"
  defp tone_class(_), do: "border-zinc-300 text-zinc-700"

  defp fmt(nil), do: "—"
  defp fmt(v) when is_number(v), do: :io_lib.format("~.3f", [v]) |> IO.iodata_to_binary()
  defp fmt(v), do: to_string(v)
end
