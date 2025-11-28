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
    tone: :neutral,
    tone_since: 0
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
    |> Phoenix.Component.assign_new(:mood_tone_since, fn -> @defaults.tone_since end)
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

  # Canonical path: {:mood_event, measurements, metadata}
  def on_info({:mood_event, meas, meta}, socket) do
    prev_mood = socket.assigns[:mood] || @defaults

    mood_raw =
      case meta[:mood] || meta["mood"] do
        %{} = meta_mood ->
          normalize_mood(meta_mood, prev_mood)

        _ ->
          build_mood_from_meas(prev_mood, meas, meta || %{})
      end

    mood = smooth_tone(mood_raw, prev_mood)

    {:noreply,
     socket
     |> assign_mood(mood)
     |> Phoenix.Component.assign(:mood_last, %{
       at: System.system_time(:millisecond),
       meas: meas,
       meta: meta
     })}
  end

  # Legacy path: {:mood_update, measurements, metadata}
  def on_info({:mood_update, meas, meta}, socket) do
    prev_mood = socket.assigns[:mood] || @defaults
    mood_raw = build_mood_from_meas(prev_mood, meas, meta || %{})
    mood = smooth_tone(mood_raw, prev_mood)

    {:noreply, assign_mood(socket, mood)}
  end

  # Direct mood struct: {:mood, %{levels/derived/tone/...}}
  def on_info({:mood, m}, socket) do
    prev_mood = socket.assigns[:mood] || @defaults
    mood_raw = normalize_mood(m || %{}, prev_mood)
    mood = smooth_tone(mood_raw, prev_mood)

    {:noreply, assign_mood(socket, mood)}
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
    <span class={[
      "text-xs px-2 py-1 rounded-md border inline-flex items-center gap-1 shadow-sm backdrop-blur-sm",
      tone_class(@tone)
    ]}>
      <span class={["w-2 h-2 rounded-full mr-1", tone_dot_class(@tone)]}></span>

      <span class="font-semibold mr-1">Mood</span>

      <span class="opacity-70">Expl</span>
      <span class="tabular-nums">{fmt(@derived[:exploration])}</span>

      <span class="opacity-70 ml-1">Inhib</span>
      <span class="tabular-nums">{fmt(@derived[:inhibition])}</span>

      <span class="opacity-70 ml-1">Vigil</span>
      <span class="tabular-nums">{fmt(@derived[:vigilance])}</span>

      <span class="opacity-70 ml-1">Plast</span>
      <span class="tabular-nums">{fmt(@derived[:plasticity])}</span>

      <span class="ml-2 text-[10px] font-semibold tracking-wide uppercase opacity-80">
        {tone_label(@tone)}
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
          tone: Map.get(a, :mood_tone, @defaults.tone),
          tone_since: Map.get(a, :mood_tone_since, @defaults.tone_since)
        }

    assign_mood(socket, mood)
  end

  defp assign_mood(socket, mood) do
    mood = mood || @defaults

    levels = Map.get(mood, :levels, @defaults.levels)
    derived = Map.get(mood, :derived, @defaults.derived)
    tone = Map.get(mood, :tone, @defaults.tone)
    tone_since = Map.get(mood, :tone_since, @defaults.tone_since)

    mood = %{
      levels: levels,
      derived: derived,
      tone: tone,
      tone_since: tone_since
    }

    socket
    |> Phoenix.Component.assign(:mood_levels, levels)
    |> Phoenix.Component.assign(:mood_derived, derived)
    |> Phoenix.Component.assign(:mood_tone, tone)
    |> Phoenix.Component.assign(:mood_tone_since, tone_since)
    |> Phoenix.Component.assign(:mood, mood)
  end

  defp normalize_mood(mood_map, prev_mood) do
    prev_levels = Map.get(prev_mood, :levels, @defaults.levels)
    prev_derived = Map.get(prev_mood, :derived, @defaults.derived)
    prev_tone = Map.get(prev_mood, :tone, @defaults.tone)
    prev_since = Map.get(prev_mood, :tone_since, @defaults.tone_since)

    levels =
      Map.get(mood_map, :levels) ||
        Map.get(mood_map, "levels") ||
        prev_levels

    derived =
      Map.get(mood_map, :derived) ||
        Map.get(mood_map, "derived") ||
        %{
          exploration:
            Map.get(
              mood_map,
              :exploration,
              Map.get(mood_map, "exploration", prev_derived[:exploration])
            ),
          inhibition:
            Map.get(
              mood_map,
              :inhibition,
              Map.get(mood_map, "inhibition", prev_derived[:inhibition])
            ),
          vigilance:
            Map.get(
              mood_map,
              :vigilance,
              Map.get(mood_map, "vigilance", prev_derived[:vigilance])
            ),
          plasticity:
            Map.get(
              mood_map,
              :plasticity,
              Map.get(mood_map, "plasticity", prev_derived[:plasticity])
            )
        }

    tone_raw =
      Map.get(mood_map, :tone) ||
        Map.get(mood_map, "tone") ||
        Map.get(mood_map, :tone_hint) ||
        Map.get(mood_map, "tone_hint")

    tone =
      case tone_raw do
        nil -> derive_tone_from_derived(derived, prev_tone)
        t -> t
      end

    %{
      levels: levels,
      derived: derived,
      tone: tone,
      tone_since:
        Map.get(
          mood_map,
          :tone_since,
          Map.get(mood_map, "tone_since", prev_since)
        )
    }
  end

  defp build_mood_from_meas(prev_mood, meas, meta) do
    prev_levels = Map.get(prev_mood, :levels, @defaults.levels)
    prev_derived = Map.get(prev_mood, :derived, @defaults.derived)
    prev_tone = Map.get(prev_mood, :tone, @defaults.tone)
    prev_since = Map.get(prev_mood, :tone_since, @defaults.tone_since)

    levels =
      Map.get(meta, :levels) ||
        Map.get(meta, "levels") ||
        Map.get(meas, :levels) ||
        Map.get(meas, "levels") ||
        %{
          da: Map.get(meas, :da, prev_levels[:da]),
          "5ht": Map.get(meas, :"5ht", prev_levels[:"5ht"]),
          glu: Map.get(meas, :glu, prev_levels[:glu]),
          ne: Map.get(meas, :ne, prev_levels[:ne])
        }

    derived =
      Map.get(meta, :derived) ||
        Map.get(meta, "derived") ||
        Map.get(meas, :derived) ||
        Map.get(meas, "derived") ||
        %{
          exploration: Map.get(meas, :exploration, prev_derived[:exploration]),
          inhibition: Map.get(meas, :inhibition, prev_derived[:inhibition]),
          vigilance: Map.get(meas, :vigilance, prev_derived[:vigilance]),
          plasticity: Map.get(meas, :plasticity, prev_derived[:plasticity])
        }

    tone_raw =
      Map.get(meta, :tone) ||
        Map.get(meta, "tone") ||
        Map.get(meta, :tone_hint) ||
        Map.get(meta, "tone_hint") ||
        Map.get(meas, :tone) ||
        Map.get(meas, "tone")

    tone =
      case tone_raw do
        nil -> derive_tone_from_derived(derived, prev_tone)
        t -> t
      end

    %{
      levels: levels,
      derived: derived,
      tone: tone,
      tone_since: prev_since
    }
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

  # ----------------- Tone smoothing -----------------

  defp smooth_tone(%{} = new_mood, %{} = prev_mood) do
    now_ms = System.monotonic_time(:millisecond)

    ui_cfg = Application.get_env(:brain, :mood_ui, [])
    ignore_deesc_ms = ui_cfg[:ignore_transient_deescalate_ms] || 300
    min_dwell_ms = ui_cfg[:min_tone_dwell_ms] || 200

    last_tone = Map.get(prev_mood, :tone, :neutral)
    last_since = Map.get(prev_mood, :tone_since, now_ms)
    new_tone = Map.get(new_mood, :tone, last_tone)

    cond do
      # No change → keep existing start time
      new_tone == last_tone ->
        Map.put(new_mood, :tone_since, last_since)

      # Very short, more-intense spike? (e.g. deescalate) -> ignore.
      tone_rank(new_tone) > tone_rank(last_tone) and now_ms - last_since < min_dwell_ms ->
        new_mood
        |> Map.put(:tone, last_tone)
        |> Map.put(:tone_since, last_since)

      # Special case: quick deescalate blip after neutral/warm → ignore
      last_tone in [:neutral, :warm] and new_tone == :deescalate and
          now_ms - last_since < ignore_deesc_ms ->
        new_mood
        |> Map.put(:tone, last_tone)
        |> Map.put(:tone_since, last_since)

      # Otherwise accept the new tone and start its timer
      true ->
        Map.put(new_mood, :tone_since, now_ms)
    end
  end

  defp tone_rank(:neutral), do: 0
  defp tone_rank(:warm), do: 1
  defp tone_rank(:cool), do: 1
  defp tone_rank(:deescalate), do: 2
  defp tone_rank(_), do: 0

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
