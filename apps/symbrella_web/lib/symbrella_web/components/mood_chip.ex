defmodule SymbrellaWeb.Components.MoodChip do
  @moduledoc """
  MoodChip — tiny UI widget showing the four mood indices as bars.

  Usage:
      <MoodChip.mood_chip mood={@mood} ping={@mood_ping} class="bg-white/70" />

  Assigns:
    :mood  -> %{exploration:, inhibition:, vigilance:, plasticity:} (0.0..1.0)
              Optionally may also include :tone_hint or :tone.
    :ping  -> optional map of booleans, keys: :exploration, :inhibition, :vigilance, :plasticity
              when true, that bar “pings” with a quick glow. Defaults to none.
    :tone  -> optional tone hint (:warm | :cool | :deescalate | :neutral or string).
              If omitted, we’ll try to infer from mood[:tone_hint] / mood[:tone].
  """

  use Phoenix.Component

  @doc "Renders a compact mood chip with 4 bars, numeric tooltips, and optional tone badge."
  attr :mood, :map, required: true
  attr :ping, :map, default: %{}
  attr :class, :string, default: ""
  attr :tone, :any, default: nil

  def mood_chip(assigns) do
    ~H"""
    <div class={[
      "rounded-xl shadow-sm border border-black/5 px-3 py-2 flex items-center justify-between gap-3",
      @class
    ]}>
      <div class="flex items-center gap-3">
        <div class="text-xs font-semibold tracking-wide opacity-70">Mood</div>

        {bar("Expl", :exploration, @mood[:exploration] || @mood["exploration"], @ping)}
        {bar("Inhib", :inhibition, @mood[:inhibition] || @mood["inhibition"], @ping)}
        {bar("Vigil", :vigilance, @mood[:vigilance] || @mood["vigilance"], @ping)}
        {bar("Plast", :plasticity, @mood[:plasticity] || @mood["plasticity"], @ping)}
      </div>

      <% tone =
        @tone ||
          @mood[:tone_hint] || @mood["tone_hint"] ||
          @mood[:tone] || @mood["tone"] %>

      <%= if tone do %>
        <span class={tone_class(tone)} title={"Tone: " <> tone_label(tone)}>
          {tone_label(tone)}
        </span>
      <% end %>
    </div>
    """
  end

  defp bar(label, key, val, ping) do
    v = to_float_01(val || 0.5)
    pct = trunc(v * 100)
    width = "#{max(6, pct)}%"
    glow? = (ping || %{})[key] == true

    assigns = %{label: label, pct: pct, width: width, glow?: glow?}

    ~H"""
    <div class="flex items-center gap-1">
      <span class="text-[10px] tabular-nums opacity-60 w-8 text-right">
        {@label}
      </span>
      <div
        class="w-20 h-2 rounded bg-neutral-200 overflow-hidden"
        title={"#{@label}: #{@pct}%"}
        aria-label={"#{@label} #{@pct}%"}
      >
        <div
          class={[
            "h-2 rounded transition-all duration-300",
            @glow? && "animate-[moodping_600ms_ease-out_1]"
          ]}
          style={"width: #{@width}"}
        >
        </div>
      </div>
      <span class="text-[10px] w-7 tabular-nums text-right opacity-60">
        {@pct}%
      </span>
    </div>
    """
  end

  # --- Tone helpers -------------------------------------------------------

  defp tone_class(tone) do
    base =
      "inline-flex items-center rounded-full px-2 py-0.5 text-[10px] " <>
        "font-semibold tracking-wide uppercase"

    case normalize_tone(tone) do
      :warm ->
        [
          base,
          "bg-amber-100 text-amber-800 dark:bg-amber-900/40 dark:text-amber-100"
        ]

      :cool ->
        [
          base,
          "bg-sky-100 text-sky-800 dark:bg-sky-900/40 dark:text-sky-100"
        ]

      :deescalate ->
        [
          base,
          "bg-rose-100 text-rose-800 dark:bg-rose-900/40 dark:text-rose-100"
        ]

      :neutral ->
        [
          base,
          "bg-slate-100 text-slate-700 dark:bg-slate-800 dark:text-slate-100"
        ]
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

  defp normalize_tone(nil), do: nil

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

  # --- Utility -----------------------------------------------------------

  defp to_float_01(x) when is_number(x), do: max(0.0, min(1.0, x * 1.0))

  defp to_float_01(x) when is_binary(x) do
    case Float.parse(x) do
      {v, _} -> to_float_01(v)
      _ -> 0.5
    end
  end

  defp to_float_01(_), do: 0.5
end
