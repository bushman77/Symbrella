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
    data = assigns[:data] || %{}
    snap = assigns[:snapshot]

    title = mget(data, :title) || "—"
    summary = mget(data, :summary) || ""
    modules = mget(data, :modules) || []
    telemetry = mget(data, :telemetry) || []
    config = mget(data, :config) || []

    full_name = Map.get(assigns[:full_names] || %{}, assigns[:selected]) || "—"

    running? =
      truthy?(mget(snap || %{}, :running?) || mget(snap || %{}, :running) || false)

    info = if is_map(mget(snap || %{}, :info)), do: mget(snap || %{}, :info), else: nil

    assigns =
      assigns
      |> assign(:title, title)
      |> assign(:summary, summary)
      |> assign(:modules, modules)
      |> assign(:telemetry, telemetry)
      |> assign(:config, config)
      |> assign(:full_name, full_name)
      |> assign(:running?, running?)
      |> assign(:snap_mod, mget(snap || %{}, :module))
      |> assign(:snap_pid, mget(snap || %{}, :pid))
      |> assign(:snap_info, info)
      |> assign(:snap_state, mget(snap || %{}, :state))

    ~H"""
    <div class="rounded-2xl border p-5 bg-white dark:bg-neutral-900/70 dark:border-neutral-800">
      <div class="flex items-start justify-between gap-4">
        <div>
          <h2 class="text-xl font-semibold text-zinc-900 dark:text-zinc-100">{@title}</h2>
          <div class="mt-1 text-xs opacity-70 text-zinc-700 dark:text-zinc-300">{@full_name}</div>
        </div>

        <span class="inline-flex items-center gap-2 rounded-full border px-3 py-1 text-xs dark:border-neutral-700 dark:text-zinc-200">
          <span class="opacity-60">Selected:</span>
          <code class="font-mono">{@selected}</code>
        </span>
      </div>

      <p class="mt-2 text-sm leading-relaxed opacity-90 text-zinc-800 dark:text-zinc-200">
        {@summary}
      </p>

      <div class="mt-4 grid grid-cols-1 md:grid-cols-3 gap-4">
        <div>
          <h3 class="font-medium mb-1 text-zinc-900 dark:text-zinc-100">Modules</h3>
          <ul class="text-sm space-y-1">
            <%= if Enum.empty?(@modules) do %>
              <li class="opacity-60">—</li>
            <% else %>
              <%= for m <- @modules do %>
                <li>
                  <code class="px-1 py-0.5 rounded bg-zinc-50 border dark:bg-neutral-950/40 dark:border-neutral-800">
                    {inspect(m)}
                  </code>
                </li>
              <% end %>
            <% end %>
          </ul>
        </div>

        <div>
          <h3 class="font-medium mb-1 text-zinc-900 dark:text-zinc-100">Telemetry</h3>
          <ul class="text-sm space-y-1">
            <%= if Enum.empty?(@telemetry) do %>
              <li class="opacity-60">—</li>
            <% else %>
              <%= for t <- @telemetry do %>
                <li>
                  <code class="px-1 py-0.5 rounded bg-zinc-50 border dark:bg-neutral-950/40 dark:border-neutral-800">
                    {t}
                  </code>
                </li>
              <% end %>
            <% end %>
          </ul>
        </div>

        <div>
          <h3 class="font-medium mb-1 text-zinc-900 dark:text-zinc-100">Config (examples)</h3>
          <ul class="text-sm space-y-1">
            <%= if Enum.empty?(@config) do %>
              <li class="opacity-60">—</li>
            <% else %>
              <%= for {app, key, val} <- @config do %>
                <li>
                  <code class="px-1 py-0.5 rounded bg-zinc-50 border dark:bg-neutral-950/40 dark:border-neutral-800">
                    config :{app}, {key}, {inspect(val)}
                  </code>
                </li>
              <% end %>
            <% end %>
          </ul>
        </div>
      </div>

      <div class="mt-6 rounded-2xl border p-4 dark:border-neutral-800">
        <div class="flex items-center justify-between gap-3">
          <h3 class="font-medium text-zinc-900 dark:text-zinc-100">Live state</h3>
          <div class="text-xs opacity-70">
            <%= if @snapshot && @running? do %>
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
              <h4 class="font-medium mb-1 text-zinc-900 dark:text-zinc-100">Process</h4>
              <ul class="space-y-1 text-zinc-800 dark:text-zinc-200">
                <li>
                  <span class="opacity-60">module:</span>
                  <code>{inspect(@snap_mod)}</code>
                </li>
                <li>
                  <span class="opacity-60">pid:</span>
                  <code>{inspect(@snap_pid)}</code>
                </li>

                <%= if is_map(@snap_info) do %>
                  <li>
                    <span class="opacity-60">queue:</span>
                    <code>
                      {@snap_info[:message_queue_len] || @snap_info["message_queue_len"] || 0}
                    </code>
                  </li>
                  <li>
                    <span class="opacity-60">current:</span>
                    <code>
                      {inspect(@snap_info[:current_function] || @snap_info["current_function"])}
                    </code>
                  </li>
                <% end %>
              </ul>
            </div>

            <div class="text-sm">
              <h4 class="font-medium mb-1 text-zinc-900 dark:text-zinc-100">State</h4>

              <details class="rounded border dark:border-neutral-800 bg-zinc-50 dark:bg-neutral-950/40">
                <summary class="cursor-pointer px-3 py-2 text-xs opacity-80 select-none">
                  Inspect full state
                </summary>
                <div class="px-3 pb-3 overflow-auto max-h-56">
                  <pre class="text-xs whitespace-pre-wrap"><%= inspect(@snap_state, pretty: true, width: 80, limit: :infinity, printable_limit: 500) %></pre>
                </div>
              </details>
            </div>
          </div>
        <% else %>
          <div class="mt-2 text-sm opacity-70">— no snapshot —</div>
        <% end %>
      </div>
    </div>
    """
  end

  # --- tiny helpers ----------------------------------------------------------

  defp mget(nil, _k), do: nil
  defp mget(m, k) when is_map(m), do: Map.get(m, k) || Map.get(m, to_string(k))
  defp mget(_, _), do: nil

  defp truthy?(true), do: true
  defp truthy?("true"), do: true
  defp truthy?("1"), do: true
  defp truthy?(1), do: true
  defp truthy?(_), do: false
end
