defmodule SymbrellaWeb.Components.CycleHUD do
  @moduledoc """
  Compact clock HUD showing seq/Hz/Δt/phase pulled from Brain.CycleClock.snapshot/0.
  Safe formatting for integers/floats/nils to avoid float_to_binary errors.
  """
  use Phoenix.Component
  alias Brain.CycleClock

  attr :class, :string, default: ""

  def cycle_hud(assigns) do
    snap = CycleClock.snapshot()
    assigns = assign(assigns, snap: snap)

    ~H"""
    <div class={"flex items-center gap-2 rounded-2xl px-3 py-2 shadow-sm border border-black/5 #{ @class }"}>
      <span class="text-xs font-semibold tracking-wide">Clock</span>
      <span class="text-[10px] tabular-nums">seq <%= @snap.seq %></span>
      <span class="text-[10px] tabular-nums">Hz <%= fmtf(@snap.hz, 2) %></span>
      <span class="text-[10px] tabular-nums">Δt <%= fmt_ms(@snap.dt_ms) %>ms</span>
      <span class="text-[10px] tabular-nums">ϕ <%= fmtf(@snap.phase, 2) %></span>
    </div>
    """
  end

  # ---- formatting helpers ----

  defp fmtf(v, d \\ 2)
  defp fmtf(v, d) when is_integer(v), do: :erlang.float_to_binary(v * 1.0, decimals: d)
  defp fmtf(v, d) when is_float(v),   do: :erlang.float_to_binary(v, decimals: d)
  defp fmtf(_v, _d),                  do: "--"

  defp fmt_ms(v) when is_integer(v), do: Integer.to_string(v)
  defp fmt_ms(v) when is_float(v),   do: :erlang.float_to_binary(v, decimals: 1)
  defp fmt_ms(_),                    do: "--"
end

