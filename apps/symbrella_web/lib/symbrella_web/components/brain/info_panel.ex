defmodule SymbrellaWeb.Components.Brain.InfoPanel do
  @moduledoc "Right-side info panel: title, summary, modules, telemetry, config, and live snapshot."
  use SymbrellaWeb, :html

  attr :selected, :atom, required: true
  attr :data, :map, required: true
  attr :snapshot, :map, default: nil
  attr :hippo, :map, default: nil
  attr :hippo_metrics, :map, default: nil
  attr :full_names, :map, default: %{}

  def info_panel(assigns) do
    ~H"""
    <div class="rounded-2xl border p-5 bg-white">
      <div class="flex items-start justify-between gap-4">
        <div>
          <h2 class="text-xl font-semibold">{@data.title}</h2>
          <div class="mt-1 text-xs opacity-70">{Map.get(@full_names, @selected, "—")}</div>
        </div>
        <span class="inline-flex items-center gap-2 rounded-full border px-3 py-1 text-xs">
          <span class="opacity-60">Selected:</span>
          <code class="font-mono">{@selected}</code>
        </span>
      </div>

      <p class="mt-2 text-sm leading-relaxed opacity-90">{@data.summary}</p>

      <div class="mt-4 grid grid-cols-1 md:grid-cols-3 gap-4">
        <div>
          <h3 class="font-medium mb-1">Modules</h3>
          <ul class="text-sm space-y-1">
            <%= if Enum.empty?(@data.modules) do %>
              <li class="opacity-60">—</li>
            <% else %>
              <%= for m <- @data.modules do %>
                <li><code class="px-1 py-0.5 rounded bg-zinc-50 border">{inspect(m)}</code></li>
              <% end %>
            <% end %>
          </ul>
        </div>

        <div>
          <h3 class="font-medium mb-1">Telemetry</h3>
          <ul class="text-sm space-y-1">
            <%= if Enum.empty?(@data.telemetry) do %>
              <li class="opacity-60">—</li>
            <% else %>
              <%= for t <- @data.telemetry do %>
                <li><code class="px-1 py-0.5 rounded bg-zinc-50 border">{t}</code></li>
              <% end %>
            <% end %>
          </ul>
        </div>

        <div>
          <h3 class="font-medium mb-1">Config (examples)</h3>
          <ul class="text-sm space-y-1">
            <%= if Enum.empty?(@data.config) do %>
              <li class="opacity-60">—</li>
            <% else %>
              <%= for {app, key, val} <- @data.config do %>
                <li>
                  <code class="px-1 py-0.5 rounded bg-zinc-50 border">
                    config :{app}, {key}, {inspect(val)}
                  </code>
                </li>
              <% end %>
            <% end %>
          </ul>
        </div>
      </div>

      <div class="mt-6 rounded-2xl border p-4">
        <div class="flex items-center justify-between gap-3">
          <h3 class="font-medium">Live state</h3>
          <div class="text-xs opacity-70">
            <%= if @snapshot && @snapshot.running? do %>
              <span class="inline-flex items-center gap-2">
                <span class="inline-block h-2 w-2 rounded-full bg-emerald-500"></span> running
              </span>
            <% else %>
              <span class="inline-flex items-center gap-2">
                <span class="inline-block h-2 w-2 rounded-full bg-rose-500"></span> down
              </span>
            <% end %>
          </div>
        </div>

        <%= if @snapshot do %>
          <div class="mt-3 grid grid-cols-1 md:grid-cols-2 gap-4">
            <div class="text-sm">
              <h4 class="font-medium mb-1">Process</h4>
              <ul class="space-y-1">
                <li>
                  <span class="opacity-60">module:</span> <code>{inspect(@snapshot.module)}</code>
                </li>
                <li><span class="opacity-60">pid:</span> <code>{inspect(@snapshot.pid)}</code></li>
                <%= if is_map(@snapshot.info) do %>
                  <li>
                    <span class="opacity-60">queue:</span>
                    <code>{@snapshot.info[:message_queue_len] || 0}</code>
                  </li>
                  <li>
                    <span class="opacity-60">current:</span>
                    <code>{inspect(@snapshot.info[:current_function])}</code>
                  </li>
                <% end %>
              </ul>
            </div>

            <div class="text-sm">
              <h4 class="font-medium mb-1">State</h4>
              <div class="rounded border bg-zinc-50 p-2 overflow-auto max-h-56">
                <pre class="text-xs whitespace-pre-wrap">
                  <%= inspect(@snapshot.state, pretty: true, width: 80, limit: :infinity, printable_limit: 500) %>
                </pre>
              </div>
            </div>
          </div>
        <% else %>
          <div class="mt-2 text-sm opacity-70">— no snapshot —</div>
        <% end %>
      </div>
    </div>
    """
  end
end
