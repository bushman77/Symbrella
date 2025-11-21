defmodule SymbrellaWeb.BrainLive.MoodHud do
  @moduledoc """
  Mood HUD for BrainLive.

  Responsibilities:
  • Default mood assigns and seeding into the socket
  • Telemetry attach/detach (brain:mood events)
  • Event and handle_info helpers to keep LiveView tiny
  • Function component `<.mood_hud />` (compact, always-visible HUD)
  """

  use SymbrellaWeb, :html
  require Logger

  @defaults %{
    levels: %{
      da: 0.35,
      "5ht": 0.50,
      glu: 0.40,
      ne: 0.50
    },
    derived: %{
      exploration: 0.41,
      inhibition: 0.50,
      vigilance: 0.50,
      plasticity: 0.375
    },
    tone: :neutral
  }

  # ---------------------------------------------------------------------------
  # Public API
  # ---------------------------------------------------------------------------

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
        # canonical event emitted by Brain.MoodCore.emit/4:
        [:brain, :mood, :update]
      ]

      try do
        if Code.ensure_loaded?(:telemetry) do
          :ok =
            :telemetry.attach_many(
              id,
              events,
              &__MODULE__.handle_telemetry/4,
              self()
            )
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
  `handle_info` helper.

  Returns `{:noreply, socket}` for mood-related messages, or `:ignore` for
  non-mood messages (so your LiveView can fall through).
  """

  # NEW: correctly derive mood + tone from Brain.MoodCore telemetry measurements.
  def on_info({:mood_event, meas, meta}, socket) do
    levels0 = socket.assigns[:mood_levels] || @defaults.levels
    derived0 = socket.assigns[:mood_derived] || @defaults.derived
    tone0 = socket.assigns[:mood_tone] || @defaults.tone

    # If metadata carries a full mood struct, use it directly.
    mood_from_meta = meta[:mood] || meta["mood"]

    {levels, derived, tone} =
      cond do
        is_map(mood_from_meta) ->
          m = mood_from_meta

          levels =
            Map.get(m, :levels) ||
              Map.get(m, "levels") ||
              levels0

          derived =
            Map.get(m, :derived) ||
              Map.get(m, "derived") ||
              %{
                exploration:
                  Map.get(m, :exploration, Map.get(m, "exploration", derived0[:exploration])),
                inhibition:
                  Map.get(m, :inhibition, Map.get(m, "inhibition", derived0[:inhibition])),
                vigilance:
                  Map.get(m, :vigilance, Map.get(m, "vigilance", derived0[:vigilance])),
                plasticity:
                  Map.get(m, :plasticity, Map.get(m, "plasticity", derived0[:plasticity]))
              }

          tone_raw =
            Map.get(m, :tone) ||
              Map.get(m, "tone") ||
              Map.get(m, :tone_hint) ||
              Map.get(m, "tone_hint")

          tone = tone_raw || derive_tone_from_derived(derived, tone0)

          {levels, derived, tone}

        true ->
          # Otherwise derive from measurements (this is what MoodCore.emit/4 sends).
          levels =
            Map.get(meta, :levels) ||
              Map.get(meta, "levels") ||
              Map.get(meas, :levels) ||
              Map.get(meas, "levels") ||
              %{
                da: Map.get(meas, :da, levels0[:da]),
                "5ht": Map.get(meas, :"5ht", levels0[:"5ht"]),
                glu: Map.get(meas, :glu, levels0[:glu]),
                ne: Map.get(meas, :ne, levels0[:ne])
              }

          derived =
            Map.get(meta, :derived) ||
              Map.get(meta, "derived") ||
              Map.get(meas, :derived) ||
              Map.get(meas, "derived") ||
              %{
                exploration: Map.get(meas, :exploration, derived0[:exploration]),
                inhibition: Map.get(meas, :inhibition, derived0[:inhibition]),
                vigilance: Map.get(meas, :vigilance, derived0[:vigilance]),
                plasticity: Map.get(meas, :plasticity, derived0[:plasticity])
              }

          tone_raw =
            Map.get(meta, :tone) ||
              Map.get(meta, "tone") ||
              Map.get(meta, :tone_hint) ||
              Map.get(meta, "tone_hint") ||
              Map.get(meas, :tone) ||
              Map.get(meas, "tone")

          tone = tone_raw || derive_tone_from_derived(derived, tone0)

          {levels, derived, tone}
      end

    mood = %{levels: levels, derived: derived, tone: tone}

    {:noreply,
     socket
     |> Phoenix.Component.assign(:mood_levels, levels)
     |> Phoenix.Component.assign(:mood_derived, derived)
     |> Phoenix.Component.assign(:mood_tone, tone)
     |> Phoenix.Component.assign(:mood, mood)
     |> Phoenix.Component.assign(:mood_last, %{
       at: System.system_time(:millisecond),
       meas: meas,
       meta: meta
     })}
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

    tone0 = socket.assigns[:mood_tone] || :neutral
    tone = derive_tone_from_derived(derived, tone0)

    {:noreply,
     socket
     |> Phoenix.Component.assign(:mood_levels, levels)
     |> Phoenix.Component.assign(:mood_derived, derived)
     |> Phoenix.Component.assign(:mood_tone, tone)
     |> Phoenix.Component.assign(:mood, %{levels: levels, derived: derived, tone: tone})}
  end

  def on_info({:mood, m}, socket) do
    mood = m || %{}

    levels =
      Map.get(mood, :levels, socket.assigns[:mood_levels] || @defaults.levels)

    derived =
      Map.get(mood, :derived, socket.assigns[:mood_derived] || @defaults.derived)

    tone0 = socket.assigns[:mood_tone] || @defaults.tone

    tone =
      Map.get(mood, :tone) ||
        Map.get(mood, :tone_hint) ||
        derive_tone_from_derived(derived, tone0)

    {:noreply,
     socket
     |> Phoenix.Component.assign(:mood_levels, levels)
     |> Phoenix.Component.assign(:mood_derived, derived)
     |> Phoenix.Component.assign(:mood_tone, tone)
     |> Phoenix.Component.assign(:mood, %{levels: levels, derived: derived, tone: tone})}
  end

  def on_info(_msg, _socket), do: :ignore

  # ---------------------------------------------------------------------------
  # Telemetry callback fns (to be registered via attach/1)
  # ---------------------------------------------------------------------------

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

  # ---------------------------------------------------------------------------
  # Function component: compact Mood HUD
  # ---------------------------------------------------------------------------

  @doc "Compact, always-visible mood HUD chip."
  attr :mood, :map, default: @defaults
  def mood_hud(assigns) do
    mood = Map.get(assigns, :mood, @defaults) || @defaults

    tone =
      Map.get(mood, :tone) ||
        Map.get(mood, :tone_hint) ||
        :neutral

    derived = Map.get(mood, :derived, %{})

    assigns =
      assigns
      |> Phoenix.Component.assign(:tone, tone)
      |> Phoenix.Component.assign(:derived, derived)

    ~H"""
    <span
      class={[
        "text-xs px-2 py-1 rounded-md border inline-flex items-center gap-1 shadow-sm backdrop-blur-sm",
        tone_class(@tone)
      ]}
    >
      <span class={["w-2 h-2 rounded-full mr-1", tone_dot_class(@tone)]}></span>

      <span class="font-semibold mr-1">Mood</span>

      <span class="opacity-70">Expl</span>
      <span class="tabular-nums"><%= fmt(@derived[:exploration]) %></span>

      <span class="opacity-70 ml-1">Inhib</span>
      <span class="tabular-nums"><%= fmt(@derived[:inhibition]) %></span>

      <span class="opacity-70 ml-1">Vigil</span>
      <span class="tabular-nums"><%= fmt(@derived[:vigilance]) %></span>

      <span class="opacity-70 ml-1">Plast</span>
      <span class="tabular-nums"><%= fmt(@derived[:plasticity]) %></span>

      <span class="ml-2 text-[10px] font-semibold tracking-wide uppercase opacity-80">
        <%= tone_label(@tone) %>
      </span>
    </span>
    """
  end

  # ---------------------------------------------------------------------------
  # Internals
  # ---------------------------------------------------------------------------

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

  # Take derived indices and turn them into a tone, mirroring MoodCore.choose_tone/1
  defp derive_tone_from_derived(%{vigilance: vig, inhibition: inh, exploration: exp}, _fallback) do
    cond do
      vig > 0.65 -> :deescalate
      inh > 0.65 and exp < 0.35 -> :cool
      exp > 0.45 and inh >= 0.40 -> :warm
      true -> :neutral
    end
  end

  defp derive_tone_from_derived(_other, fallback), do: fallback || :neutral

  # Color scheme per tone (chip-level)

  defp tone_class(:warm),
    do: "border-green-400 text-green-700"

  defp tone_class(:cool),
    do: "border-sky-400 text-sky-700"

  defp tone_class(:deescalate),
    do: "border-red-400 text-red-700"

  # Backwards-compat / legacy names if anything else sets them
  defp tone_class(:positive),
    do: "border-green-400 text-green-700"

  defp tone_class(:negative),
    do: "border-red-400 text-red-700"

  defp tone_class(_),
    do: "border-zinc-300 text-zinc-700"

  # Tiny dot color per tone
  defp tone_dot_class(tone) do
    case normalize_tone(tone) do
      :warm -> "bg-amber-400 dark:bg-amber-300"
      :cool -> "bg-sky-400 dark:bg-sky-300"
      :deescalate -> "bg-rose-400 dark:bg-rose-300"
      :neutral -> "bg-zinc-400 dark:bg-zinc-300"
    end
  end

  defp tone_label(tone) do
    case normalize_tone(tone) do
      :warm -> "Warm"
      :cool -> "Cool"
      :deescalate -> "De-escalate"
      :neutral -> "Neutral"
      nil -> "Neutral"
    end
  end

  defp normalize_tone(nil), do: :neutral
  defp normalize_tone(t) when is_atom(t), do: t

  defp normalize_tone(t) when is_binary(t) do
    case t |> String.trim() |> String.downcase() do
      "warm" -> :warm
      "cool" -> :cool
      "deescalate" -> :deescalate
      "de-escalate" -> :deescalate
      "de_escalate" -> :deescalate
      "neutral" -> :neutral
      _ -> :neutral
    end
  end

  defp normalize_tone(_), do: :neutral

  defp fmt(nil), do: "—"

  defp fmt(v) when is_number(v),
    do: :io_lib.format("~.3f", [v]) |> IO.iodata_to_binary()

  defp fmt(v), do: to_string(v)
end

