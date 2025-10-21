defmodule SymbrellaWeb.Components.MoodChip do
  @moduledoc """
  Compact visualization of MoodCore levels + derived signals.
  Accepts an optional `:tone` to tint the badge:
    :neutral | :warning | :danger
  """
  use Phoenix.Component

  attr :levels,  :map, default: %{}
  attr :derived, :map, default: %{}
  attr :tone,    :atom, default: :neutral
  attr :class,   :string, default: ""

  def mood_chip(assigns) do
    lv = assigns.levels || %{}
    dv = assigns.derived || %{}

    assigns =
      assigns
      |> assign(:expl, fmtf(Map.get(dv, :exploration)))
      |> assign(:inhib, fmtf(Map.get(dv, :inhibition)))
      |> assign(:vigil, fmtf(Map.get(dv, :vigilance)))
      |> assign(:plast, fmtf(Map.get(dv, :plasticity)))
      |> assign(:tone_cls, tone_class(assigns.tone))

    ~H"""
    <div class={[
           "flex items-center gap-2 rounded-2xl px-3 py-2 shadow-sm border text-[10px]",
           @tone_cls,
           @class
         ]}>
      <span class="font-semibold tracking-wide">Mood</span>
      <span>Expl: <%= @expl %></span>
      <span>Inhib: <%= @inhib %></span>
      <span>Vigil: <%= @vigil %></span>
      <span>Plast: <%= @plast %></span>
    </div>
    """
  end

  # styling
  defp tone_class(:danger),  do: "bg-rose-50 border-rose-200 text-rose-700"
  defp tone_class(:warning), do: "bg-amber-50 border-amber-200 text-amber-700"
  defp tone_class(_),        do: "bg-white/70 border-black/5 text-zinc-800"

  # format helper (0..1 floats)
  defp fmtf(nil), do: "--"
  defp fmtf(v) when is_integer(v), do: :erlang.float_to_binary(v * 1.0, decimals: 2)
  defp fmtf(v) when is_float(v),   do: :erlang.float_to_binary(v, decimals: 2)
  defp fmtf(_), do: "--"
end

