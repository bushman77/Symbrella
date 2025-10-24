defmodule SymbrellaWeb.Components.CycleHUD do
  @moduledoc """
  Minimal HUD to display current brain cycle phase + tempo + strength.

  Drop into any LiveView template:  <.cycle_hud />
  """
  use Phoenix.Component

  def cycle_hud(assigns) do
    phase = safe_phase()
    hz    = safe_hz()

    metrics =
      try do
        state =
          if Code.ensure_loaded?(Brain) and function_exported?(Brain, :get_state, 0),
            do: Brain.get_state(),
            else: %{}

        Brain.CycleMetrics.snapshot(state)
      rescue
        _ -> %{strength: 0.0, hz: hz, order: []}
      end

    assigns =
      assigns
      |> Map.put(:phase, phase)
      |> Map.put(:hz, safe_num(hz))
      |> Map.put(:strength, safe_num(metrics[:strength] || metrics["strength"] || 0.0))
      |> Map.put(:order, metrics[:order] || metrics["order"] || [])

    ~H"""
    <div class="flex items-center gap-3 text-sm font-medium px-3 py-2 rounded-2xl shadow-md bg-neutral-900/60 border border-neutral-700">
      <span class="uppercase tracking-wide">Phase:</span>
      <span class="px-2 py-0.5 rounded-md bg-neutral-800 border border-neutral-700"><%= @phase %></span>
      <span>•</span>
      <span>Tempo: <%= f2(@hz) %> Hz</span>
      <span>•</span>
      <span>Cycle S: <%= f2(@strength) %></span>
      <%= if @order != [] do %>
        <span>•</span>
        <span>Order: <%= Enum.map(@order, &to_string/1) |> Enum.join(" → ") %></span>
      <% end %>
    </div>
    """
  end

  # ── helpers ──────────────────────────────────────────────────────────

  defp safe_phase do
    try do
      if Code.ensure_loaded?(Brain.CycleClock) and function_exported?(Brain.CycleClock, :phase, 0),
        do: Brain.CycleClock.phase(),
        else: :unknown
    rescue
      _ -> :unknown
    end
  end

  defp safe_hz do
    try do
      if Code.ensure_loaded?(Brain.CycleClock) and function_exported?(Brain.CycleClock, :hz, 0),
        do: Brain.CycleClock.hz(),
        else: 0.0
    rescue
      _ -> 0.0
    end
  end

  defp safe_num(v) when is_number(v), do: v * 1.0
  defp safe_num(_), do: 0.0

  # format float with 2 decimals (no default args warning)
  defp f2(v), do: :erlang.float_to_binary(safe_num(v), decimals: 2)
end

