defmodule SymbrellaWeb.Components.MoodChip do
  @moduledoc """
  MoodChip — tiny UI widget showing the four mood indices as bars.

  Usage:
      <MoodChip.mood_chip mood={@mood} ping={@mood_ping} class="bg-white/70" />

  Assigns:
    :mood  -> %{exploration:, inhibition:, vigilance:, plasticity:} (0.0..1.0)
    :ping  -> optional map of booleans, keys: :exploration, :inhibition, :vigilance, :plasticity
              when true, that bar “pings” with a quick glow. Defaults to none.
  """

  use Phoenix.Component

  @doc "Renders a compact mood chip with 4 bars and numeric tooltips."
  attr :mood, :map, required: true
  attr :ping, :map, default: %{}
  attr :class, :string, default: ""
  def mood_chip(assigns) do
    ~H"""
    <div class={["rounded-xl shadow-sm border border-black/5 px-3 py-2 flex items-center gap-3", @class]}>
      <div class="text-xs font-semibold tracking-wide opacity-70">Mood</div>

      <%= bar("Expl", :exploration, @mood[:exploration] || @mood["exploration"], @ping) %>
      <%= bar("Inhib", :inhibition,  @mood[:inhibition]  || @mood["inhibition"],  @ping) %>
      <%= bar("Vigil", :vigilance,   @mood[:vigilance]   || @mood["vigilance"],   @ping) %>
      <%= bar("Plast", :plasticity,  @mood[:plasticity]  || @mood["plasticity"],  @ping) %>
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
      <span class="text-[10px] tabular-nums opacity-60 w-8 text-right"><%= @label %></span>
      <div class="w-20 h-2 rounded bg-neutral-200 overflow-hidden" title={"#{@label}: #{@pct}%"} aria-label={"#{@label} #{@pct}%"}>
        <div class={["h-2 rounded transition-all duration-300", @glow? && "animate-[moodping_600ms_ease-out_1]"]}
             style={"width: #{@width}"}></div>
      </div>
      <span class="text-[10px] w-7 tabular-nums text-right opacity-60"><%= @pct %>%</span>
    </div>
    """
  end

  # Utility

  defp to_float_01(x) when is_number(x), do: max(0.0, min(1.0, x * 1.0))
  defp to_float_01(x) when is_binary(x) do
    case Float.parse(x) do
      {v, _} -> to_float_01(v)
      _ -> 0.5
    end
  end
  defp to_float_01(_), do: 0.5
end

