defmodule SymbrellaWeb.Components.Brain.LIFGDecision do
  @moduledoc "Card showing the most recent LIFG decision snapshot."
  use SymbrellaWeb, :html

  attr :last, :map, default: nil

  def lifg_decision(assigns) do
    ~H"""
    <div class="mt-4 rounded-2xl border p-4">
      <div class="flex items-center justify-between gap-2">
        <h3 class="font-medium">LIFG Decision (last)</h3>
        <div class="text-xs opacity-70">
          <%= if @last && is_integer(@last[:ts_ms]) do %>
            updated {max(System.system_time(:millisecond) - @last.ts_ms, 0)} ms ago
          <% else %>
            — no snapshot —
          <% end %>
        </div>
      </div>

      <%= if is_map(@last) do %>
        <div class="mt-2 grid grid-cols-1 md:grid-cols-2 gap-3 text-sm">
          <div class="rounded-lg border p-3 bg-zinc-50">
            <div><span class="opacity-60">sentence:</span> <b>{@last[:si_sentence] || "—"}</b></div>
            <div class="mt-1">
              <span class="opacity-60">intent:</span> <b>{@last[:intent] || "—"}</b>
            </div>
            <div class="mt-1">
              <span class="opacity-60">confidence:</span>
              <b>
                {if is_number(@last[:confidence]),
                  do:
                    :erlang.float_to_binary(@last.confidence * 100.0, [:compact, {:decimals, 1}]) <>
                      "%",
                  else: "—"}
              </b>
            </div>
            <div class="mt-1">
              <span class="opacity-60">guards:</span>
              <b>chargrams=<%= get_in(@last, [:guards, :chargram_violation]) || 0 %></b>,
              <b>boundary_drops={length(get_in(@last, [:guards, :rejected_by_boundary]) || [])}</b>
            </div>
          </div>

          <div class="rounded-lg border p-3">
            <div class="opacity-60 text-xs mb-1">Tokens</div>
            <% tokens = @last[:tokens] || [] %>
            <%= if tokens == [] do %>
              <div class="text-xs opacity-70">— none —</div>
            <% else %>
              <ul class="space-y-1">
                <%= for t <- tokens do %>
                  <li>
                    <code class="px-1 py-0.5 rounded bg-zinc-50 border">
                      [{t[:index]}] {if t[:mw], do: "«#{t[:phrase]}»", else: "'#{t[:phrase]}'"}
                      {if t[:n], do: "·n=#{t[:n]}", else: ""}
                    </code>
                  </li>
                <% end %>
              </ul>
            <% end %>
          </div>
        </div>

        <div class="mt-3 rounded-lg border p-3">
          <div class="opacity-60 text-xs mb-2">Per-token choices & top scores</div>
          <% choices = @last[:choices] || [] %>
          <% finalists = @last[:finalists] || [] %>

          <%= if choices == [] do %>
            <div class="text-xs opacity-70">— no choices captured —</div>
          <% else %>
            <div class="space-y-3">
              <%= for ch <- choices do %>
                <% idx = ch[:token_index] %>
                <% rank = Enum.find(finalists, fn f -> f[:token_index] == idx end) %>
                <div class="rounded-md border p-2">
                  <div class="text-sm">
                    <b>token {idx}:</b>
                    chosen_id=<code class="px-1 py-0.5 rounded bg-zinc-50 border"><%= ch[:chosen_id] || "?" %></code> · margin=<b><%= :erlang.float_to_binary((ch[:margin] || 0.0) * 1.0, [:compact, {:decimals, 3}]) %></b>
                  </div>
                  <%= if is_map(ch[:scores]) and map_size(ch[:scores]) > 0 do %>
                    <div class="mt-1 text-xs opacity-70">scores (top 3):</div>
                    <ul class="text-xs grid grid-cols-1 md:grid-cols-3 gap-1">
                      <%= for {id, s} <- (rank && rank[:ranking] || ch[:scores] |> Enum.sort_by(fn {_id, s} -> -s end)) |> Enum.take(3) do %>
                        <li>
                          <code class="px-1 py-0.5 rounded bg-zinc-50 border">{id}</code>
                          <span class="opacity-60">=</span>
                          <b>{:erlang.float_to_binary(s * 1.0, [:compact, {:decimals, 3}])}</b>
                        </li>
                      <% end %>
                    </ul>
                  <% else %>
                    <div class="mt-1 text-xs opacity-70">— no score map —</div>
                  <% end %>
                </div>
              <% end %>
            </div>
          <% end %>
        </div>
      <% else %>
        <div class="text-sm opacity-70 mt-2">No LIFG snapshot captured yet.</div>
      <% end %>
    </div>
    """
  end
end
