defmodule SymbrellaWeb.Components.CycleHUD do
  @moduledoc """
  Minimal HUD to display current brain cycle phase + tempo + strength.

  Drop into any LiveView template:  <.cycle_hud />
  """
  use Phoenix.Component

  def cycle_hud(assigns) do
    phase = Brain.CycleClock.phase()
    hz    = Brain.CycleClock.hz()
    # Try to fetch a snapshot; fall back gracefully if Brain.get_state/0 isn't available.
    metrics =
      try do
        state =
          if function_exported?(Brain, :get_state, 0),
            do: Brain.get_state(),
            else: %{}

        Brain.CycleMetrics.snapshot(state)
      rescue
        _ -> %{strength: 0.0, hz: hz, order: []}
      end

    assigns =
      assigns
      |> Map.put(:phase, phase)
      |> Map.put(:hz, hz)
      |> Map.put(:strength, Float.round(metrics.strength, 2))
      |> Map.put(:order, Enum.map(metrics.order, &to_string/1))

    ~H"""
    <div class="flex items-center gap-3 text-sm font-medium px-3 py-2 rounded-2xl shadow-md bg-neutral-900/60 border border-neutral-700">
      <span class="uppercase tracking-wide">Phase:</span>
      <span class="px-2 py-0.5 rounded-md bg-neutral-800 border border-neutral-700"><%= @phase %></span>
      <span>•</span>
      <span>Tempo: <%= :erlang.float_to_binary(@hz, decimals: 2) %> Hz</span>
      <span>•</span>
      <span>Cycle S: <%= :erlang.float_to_binary(@strength, decimals: 2) %></span>
      <%= if @order != [] do %>
        <span>•</span>
        <span>Order: <%= Enum.join(@order, " → ") %></span>
      <% end %>
    </div>
    """
  end
end

