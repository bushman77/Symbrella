defmodule SymbrellaWeb.Components.Brain.HippoPanel do
  @moduledoc "Hippocampus controls + latest recall metrics (pure render; events handled by LiveView)."
  use SymbrellaWeb, :html

  attr :hippo, :map, default: %{}
  attr :hippo_metrics, :map, default: nil
  def hippo_panel(assigns) do
    ~H"""
    <div class="mt-4 rounded-2xl border p-4">
      <div class="flex items-center justify-between">
        <h3 class="font-medium">Hippocampus — window &amp; knobs</h3>
        <div class="flex items-center gap-2">
          <button phx-click="hippo_reset" class="rounded-md border px-2.5 py-1 text-xs hover:bg-zinc-50">Reset</button>
          <button phx-click="hippo_seed"  class="rounded-md border px-2.5 py-1 text-xs hover:bg-zinc-50">Seed 5</button>
        </div>
      </div>

      <div class="mt-3 grid grid-cols-1 md:grid-cols-3 gap-3 text-sm">
        <div class="rounded-lg border p-3">
          <div class="opacity-60 text-xs mb-1">Window</div>
          <div>size: <b><%= length(@hippo.window || []) %></b> / keep: <b><%= @hippo.window_keep %></b></div>
          <div>last write: <%= case @hippo.last do
            {at, _} when is_integer(at) -> "#{max(System.monotonic_time(:millisecond) - at, 0)} ms ago"
            _ -> "—"
          end %></div>
        </div>

        <div class="rounded-lg border p-3">
          <div class="opacity-60 text-xs mb-1">Knobs</div>
          <div>half_life_ms: <b><%= get_in(@hippo, [:opts, :half_life_ms]) %></b></div>
          <div>recall_limit: <b><%= get_in(@hippo, [:opts, :recall_limit]) %></b></div>
          <div>min_jaccard: <b><%= get_in(@hippo, [:opts, :min_jaccard]) %></b></div>
        </div>

        <div class="rounded-lg border p-3">
          <div class="opacity-60 text-xs mb-1">Recall source</div>
          <form phx-change="hippo_source" class="flex items-center gap-2">
            <select name="v" class="rounded-md border px-2 py-1 text-sm">
              <%= for opt <- ~w(memory db hybrid)a do %>
                <option value={opt} selected={get_in(@hippo, [:opts, :recall_source]) == opt}><%= opt %></option>
              <% end %>
            </select>
            <span class="text-xs opacity-70">switches backend</span>
          </form>
        </div>
      </div>

      <div class="mt-3 rounded-lg border p-3 text-sm">
        <div class="opacity-60 text-xs mb-1">Latest recall (telemetry)</div>
        <%= if is_map(@hippo_metrics) do %>
          <div class="grid grid-cols-2 md:grid-cols-4 gap-x-4 gap-y-1">
            <div>source: <b><%= @hippo_metrics[:source] || "?" %></b></div>
            <div>k: <b><%= @hippo_metrics[:k] || "—" %></b></div>
            <div>mem_k: <b><%= @hippo_metrics[:mem_k] || "—" %></b></div>
            <div>db_k: <b><%= @hippo_metrics[:db_k] || "—" %></b></div>
            <%= if Map.has_key?(@hippo_metrics, :had_embedding?) do %>
              <div>had_embedding?: <b><%= inspect(@hippo_metrics[:had_embedding?]) %></b></div>
            <% end %>
            <%= if Map.has_key?(@hippo_metrics, :top_score) do %>
              <div>top_score: <b><%= :erlang.float_to_binary(@hippo_metrics[:top_score] * 1.0, [:compact, decimals: 3]) %></b></div>
            <% end %>
            <%= if Map.has_key?(@hippo_metrics, :avg_age_ms) do %>
              <div>avg_age_ms: <b><%= @hippo_metrics[:avg_age_ms] %></b></div>
            <% end %>
            <div class="col-span-2 md:col-span-4 text-xs opacity-60">
              <%= ts = @hippo_metrics[:at_ms] || 0 %>
              updated: <%= if ts > 0, do: "#{System.system_time(:millisecond) - ts} ms ago", else: "—" %>
            </div>
          </div>
        <% else %>
          <div class="opacity-70">— no recall events yet —</div>
        <% end %>
      </div>

      <details class="mt-3 rounded-lg border p-3">
        <summary class="cursor-pointer text-sm font-medium">State inspector</summary>
        <pre class="mt-2 overflow-auto text-xs"><%= inspect(@hippo, pretty: true, limit: :infinity) %></pre>
      </details>
    </div>
    """
  end
end

