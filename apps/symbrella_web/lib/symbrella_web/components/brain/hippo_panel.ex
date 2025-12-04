defmodule SymbrellaWeb.Components.Brain.HippoPanel do
  @moduledoc "Hippocampus controls + latest recall metrics (pure render; events handled by LiveView)."
  use SymbrellaWeb, :html

  attr :hippo, :map, default: %{}
  attr :hippo_metrics, :map, default: nil

  def hippo_panel(assigns) do
    hippo = assigns[:hippo] || %{}
    opts = Map.get(hippo, :opts, %{}) || %{}

    window = Map.get(hippo, :window, []) || []
    keep = Map.get(hippo, :window_keep) || Map.get(opts, :window_keep) || "—"

    last = Map.get(hippo, :last)

    source =
      opts
      |> Map.get(:recall_source, "memory")
      |> normalize_source_str()

    assigns =
      assigns
      |> assign(:window, window)
      |> assign(:keep, keep)
      |> assign(:last, last)
      |> assign(:opts, opts)
      |> assign(:source, source)
      |> assign(
        :metrics,
        if(is_map(assigns[:hippo_metrics]), do: assigns[:hippo_metrics], else: nil)
      )

    ~H"""
    <div class="mt-4 rounded-2xl border p-4">
      <div class="flex items-center justify-between">
        <h3 class="font-medium">Hippocampus — window &amp; knobs</h3>
        <div class="flex items-center gap-2">
          <button
            phx-click="hippo_reset"
            class="rounded-md border px-2.5 py-1 text-xs hover:bg-zinc-50"
          >
            Reset
          </button>
          <button
            phx-click="hippo_seed"
            class="rounded-md border px-2.5 py-1 text-xs hover:bg-zinc-50"
          >
            Seed 5
          </button>
        </div>
      </div>

      <div class="mt-3 grid grid-cols-1 md:grid-cols-3 gap-3 text-sm">
        <div class="rounded-lg border p-3">
          <div class="opacity-60 text-xs mb-1">Window</div>
          <div>size: <b>{length(@window)}</b> / keep: <b>{@keep}</b></div>
          <div>last write: {format_last_write(@last)}</div>
        </div>

        <div class="rounded-lg border p-3">
          <div class="opacity-60 text-xs mb-1">Knobs</div>
          <div>half_life_ms: <b>{Map.get(@opts, :half_life_ms, "—")}</b></div>
          <div>recall_limit: <b>{Map.get(@opts, :recall_limit, "—")}</b></div>
          <div>min_jaccard: <b>{Map.get(@opts, :min_jaccard, "—")}</b></div>
          <%= if Map.has_key?(@opts, :recall_min_sim) do %>
            <div>recall_min_sim: <b>{Map.get(@opts, :recall_min_sim, "—")}</b></div>
          <% end %>
        </div>

        <div class="rounded-lg border p-3">
          <div class="opacity-60 text-xs mb-1">Recall source</div>
          <form phx-change="hippo_source" class="flex items-center gap-2">
            <select name="v" class="rounded-md border px-2 py-1 text-sm">
              <%= for opt <- ~w(memory db hybrid) do %>
                <option value={opt} selected={@source == opt}>
                  {opt}
                </option>
              <% end %>
            </select>
            <span class="text-xs opacity-70">switches backend</span>
          </form>
        </div>
      </div>

      <div class="mt-3 rounded-lg border p-3 text-sm">
        <div class="opacity-60 text-xs mb-1">Latest recall (telemetry)</div>

        <%= if is_map(@metrics) do %>
          <div class="grid grid-cols-2 md:grid-cols-4 gap-x-4 gap-y-1">
            <div>source: <b>{@metrics[:source] || "?"}</b></div>
            <div>k: <b>{@metrics[:k] || "—"}</b></div>
            <div>mem_k: <b>{@metrics[:mem_k] || "—"}</b></div>
            <div>db_k: <b>{@metrics[:db_k] || "—"}</b></div>

            <%= if Map.has_key?(@metrics, :had_embedding?) do %>
              <div>had_embedding?: <b>{inspect(@metrics[:had_embedding?])}</b></div>
            <% end %>

            <%= if is_number(@metrics[:top_score]) do %>
              <div>
                top_score: <b>{float3(@metrics[:top_score])}</b>
              </div>
            <% end %>

            <%= if is_integer(@metrics[:avg_age_ms]) do %>
              <div>avg_age_ms: <b>{@metrics[:avg_age_ms]}</b></div>
            <% end %>

            <div class="col-span-2 md:col-span-4 text-xs opacity-60">
              {format_updated(@metrics[:at_ms])}
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

  defp normalize_source_str(v) when is_atom(v), do: v |> Atom.to_string() |> String.downcase()
  defp normalize_source_str(v) when is_binary(v), do: v |> String.downcase()
  defp normalize_source_str(_), do: "memory"

  defp format_last_write({at, _payload}) when is_integer(at) do
    now = System.monotonic_time(:millisecond)
    diff = max(now - at, 0)
    "#{diff} ms ago"
  end

  defp format_last_write(_), do: "—"

  defp format_updated(at_ms) when is_integer(at_ms) and at_ms > 0 do
    diff = max(System.system_time(:millisecond) - at_ms, 0)
    "updated: #{diff} ms ago"
  end

  defp format_updated(_), do: "updated: —"

  defp float3(v) when is_number(v),
    do: :erlang.float_to_binary(v * 1.0, [:compact, decimals: 3])

  defp float3(_), do: "—"
end
