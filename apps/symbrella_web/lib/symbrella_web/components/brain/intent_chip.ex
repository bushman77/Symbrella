defmodule SymbrellaWeb.Components.Brain.IntentChip do
  @moduledoc "Compact chip displaying latest intent/keyword/confidence."
  use SymbrellaWeb, :html

  attr :intent, :map, default: nil
  def intent_chip(assigns) do
    ~H"""
    <% intent = @intent %>
    <% conf = if intent, do: intent[:confidence], else: nil %>
    <% kw = if intent, do: (intent[:keyword] || ""), else: "" %>

    <span class={[
      "inline-flex items-center gap-2 rounded-full border px-3 py-1 text-xs",
      conf_class(intent)
    ]} title="Latest intent (Brain source-of-truth)">
      <span class="opacity-60">Intent:</span>
      <code class="font-mono"><%= if intent && intent.intent, do: intent.intent, else: "—" %></code>

      <%= if kw != "" do %>
        <span class="opacity-60">•</span><span><%= kw %></span>
      <% end %>

      <%= if is_number(conf) do %>
        <span class="opacity-60">•</span>
        <span><%= :erlang.float_to_binary(conf * 100.0, [:compact, {:decimals, 1}]) %>%</span>
      <% end %>

      <%= if intent && is_integer(intent[:at_ms]) do %>
        <span class="opacity-60">•</span>
        <span class="opacity-60">updated <%= System.system_time(:millisecond) - intent.at_ms %> ms ago</span>
      <% end %>
    </span>
    """
  end

  defp conf_class(%{confidence: c}) when is_number(c) do
    cond do
      c >= 0.75 -> "bg-emerald-50 border-emerald-300"
      c >= 0.50 -> "bg-amber-50 border-amber-300"
      true -> "bg-zinc-50 border-zinc-300"
    end
  end
  defp conf_class(_), do: "bg-zinc-50 border-zinc-200"
end

