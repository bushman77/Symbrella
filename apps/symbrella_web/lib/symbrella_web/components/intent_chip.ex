# apps/symbrella_web/lib/symbrella_web/components/intent_chip.ex
defmodule SymbrellaWeb.Components.IntentChip do
  @moduledoc """
  Tiny HUD chip for intent/keyword/confidence.
  Non-intrusive; drop anywhere in your LiveViews.
  """
  use Phoenix.Component

  @levels %{
    high: "bg-emerald-50 border-emerald-300",
    mid:  "bg-amber-50 border-amber-300",
    low:  "bg-rose-50 border-rose-300"
  }

  attr :intent, :atom, default: :none
  attr :keyword, :string, default: nil
  attr :confidence, :float, default: 0.0
  attr :class, :string, default: nil

  def intent_chip(assigns) do
    level =
      cond do
        assigns.confidence >= 0.66 -> :high
        assigns.confidence >= 0.33 -> :mid
        true -> :low
      end

    assigns = assign(assigns, :tone, Map.fetch!(@levels, level))

    ~H"""
    <div class={["inline-flex items-center gap-2 rounded-full border px-3 py-1 text-xs", @tone, @class]}>
      <span class="opacity-70">intent</span>
      <code class="px-1 rounded bg-white border"><%= inspect(@intent) %></code>
      <%= if @keyword do %>
        <span class="opacity-70">kw</span>
        <code class="px-1 rounded bg-white border"><%= @keyword %></code>
      <% end %>
      <span class="opacity-70">conf</span>
      <code class="px-1 rounded bg-white border">
        <%= :erlang.float_to_binary(@confidence * 1.0, [:compact, decimals: 2]) %>
      </code>
    </div>
    """
  end
end

