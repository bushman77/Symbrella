# apps/symbrella_web/lib/symbrella_web/components/brain/sense_slate.ex
defmodule SymbrellaWeb.Components.Brain.SenseSlate do
  @moduledoc """
  SenseSlate — UI for inspecting per-token sense selection.

  Expected shape (best-effort; tolerant of drift):
    tokens: list of maps/structs with token index + raw/norm info
    senses: map keyed by token index (integer or string), values like:
      %{
        winner: %{id:, pos:, gloss:, total:},
        margin: number,
        candidates: [%{id:, pos:, gloss:, total:, factors: %{...}}]
        drops: %{boundary:, chargram:, no_cand:},
        episodes: [%{episode_id:, boost:}]
      }

  Optional:
    selected_i: token index currently selected
    phx_click/phx_target: allow token selection via LiveView events
  """

  use Phoenix.Component

  attr :tokens, :list, default: []
  attr :senses, :map, default: %{}
  attr :selected_i, :integer, default: nil

  attr :phx_click, :string, default: nil
  attr :phx_target, :any, default: nil

  attr :class, :string, default: ""

  def sense_slate(assigns) do
    selected_i = assigns.selected_i || default_selected(assigns.tokens, assigns.senses)

    assigns =
      assigns
      |> assign(:selected_i, selected_i)
      |> assign(:selected_entry, sense_entry(assigns.senses, selected_i))
      |> assign(:selected_token, token_by_index(assigns.tokens, selected_i))

    ~H"""
    <div class={[
      "rounded-2xl border p-4 bg-white dark:bg-neutral-900/70 dark:border-neutral-800",
      @class
    ]}>
      <div class="flex items-start justify-between gap-3">
        <div>
          <h3 class="text-sm font-semibold text-zinc-900 dark:text-zinc-100">Sense selection</h3>
          <div class="text-xs opacity-70 text-zinc-700 dark:text-zinc-300">
            Winner + margin + top candidates (click a token to inspect)
          </div>
        </div>

        <%= if is_integer(@selected_i) do %>
          <span class="inline-flex items-center gap-2 rounded-full border px-3 py-1 text-xs dark:border-neutral-700 dark:text-zinc-200">
            <span class="opacity-60">token:</span>
            <code class="font-mono">{@selected_i}</code>
          </span>
        <% end %>
      </div>
      
    <!-- Token strip -->
      <div class="mt-3 flex flex-wrap gap-2">
        <%= if Enum.empty?(@tokens) do %>
          <span class="text-xs opacity-60">— no tokens —</span>
        <% else %>
          <%= for tok <- @tokens do %>
            <% i = token_index(tok) %>
            <% entry = sense_entry(@senses, i) %>
            <% winner = mget(entry, :winner) %>
            <% margin = mget(entry, :margin) %>
            <% flags = token_flags(entry) %>

            <button
              type="button"
              phx-click={@phx_click}
              phx-target={@phx_target}
              phx-value-i={i}
              class={[
                "text-left rounded-xl border px-3 py-2 shadow-sm transition",
                "bg-white dark:bg-neutral-950/40 dark:border-neutral-800",
                @selected_i == i && "ring-1 ring-black/20 dark:ring-white/20"
              ]}
              title={token_title(tok, winner, margin, flags)}
            >
              <div class="flex items-center gap-2">
                <span class="text-xs font-medium text-zinc-900 dark:text-zinc-100">
                  {token_raw(tok)}
                </span>

                <%= if winner do %>
                  <code class="text-[10px] px-1 rounded bg-zinc-50 border dark:bg-neutral-900 dark:border-neutral-700">
                    {short_id(mget(winner, :id))}{pos_suffix(mget(winner, :pos))}
                  </code>
                <% else %>
                  <span class="text-[10px] opacity-60">no winner</span>
                <% end %>

                <%= if is_number(margin) do %>
                  <span class="text-[10px] opacity-70">m={fmt(margin, 2)}</span>
                <% end %>
              </div>

              <%= if flags != [] do %>
                <div class="mt-1 flex flex-wrap gap-1">
                  <%= for f <- flags do %>
                    <span class="text-[10px] px-1.5 py-0.5 rounded-full border opacity-80 dark:border-neutral-700">
                      {f}
                    </span>
                  <% end %>
                </div>
              <% end %>
            </button>
          <% end %>
        <% end %>
      </div>
      
    <!-- Selected token detail -->
      <div class="mt-4 grid grid-cols-1 lg:grid-cols-3 gap-4">
        <div class="lg:col-span-1 rounded-2xl border p-4 dark:border-neutral-800">
          <h4 class="text-sm font-semibold text-zinc-900 dark:text-zinc-100">Token</h4>

          <%= if @selected_token do %>
            <ul class="mt-2 text-xs space-y-1 text-zinc-800 dark:text-zinc-200">
              <li><span class="opacity-60">raw:</span> <code>{token_raw(@selected_token)}</code></li>
              <li>
                <span class="opacity-60">norm:</span> <code>{token_norm(@selected_token)}</code>
              </li>
              <li>
                <span class="opacity-60">span:</span>
                <code>{inspect(token_span(@selected_token))}</code>
              </li>
              <li>
                <span class="opacity-60">mwe?:</span>
                <code>{inspect(token_mwe?(@selected_token))}</code>
              </li>
            </ul>
          <% else %>
            <div class="mt-2 text-xs opacity-60">— no selection —</div>
          <% end %>
        </div>

        <div class="lg:col-span-2 rounded-2xl border p-4 dark:border-neutral-800">
          <h4 class="text-sm font-semibold text-zinc-900 dark:text-zinc-100">Selection</h4>

          <%= if is_map(@selected_entry) do %>
            <% winner = mget(@selected_entry, :winner) || %{} %>
            <% margin = mget(@selected_entry, :margin) %>
            <% drops = mget(@selected_entry, :drops) || %{} %>
            <% episodes = mget(@selected_entry, :episodes) || [] %>
            <% candidates = mget(@selected_entry, :candidates) || [] %>

            <div class="mt-2 flex flex-wrap items-center gap-2 text-xs">
              <span class="opacity-70">winner</span>
              <code class="px-2 py-0.5 rounded bg-zinc-50 border dark:bg-neutral-900 dark:border-neutral-700">
                {mget(winner, :id) || "—"}{pos_suffix(mget(winner, :pos))}
              </code>

              <%= if is_binary(mget(winner, :gloss)) and mget(winner, :gloss) != "" do %>
                <span class="opacity-70">·</span>
                <span class="opacity-90">{truncate(mget(winner, :gloss), 96)}</span>
              <% end %>

              <%= if is_number(margin) do %>
                <span class="opacity-70">· margin</span>
                <code class="px-2 py-0.5 rounded bg-zinc-50 border dark:bg-neutral-900 dark:border-neutral-700">
                  {fmt(margin, 3)}
                </code>
              <% end %>
            </div>

            <div class="mt-3 flex flex-wrap gap-2 text-[10px] opacity-80">
              <span class="rounded-full border px-2 py-0.5 dark:border-neutral-700">
                boundary drops: {mget(drops, :boundary) || 0}
              </span>
              <span class="rounded-full border px-2 py-0.5 dark:border-neutral-700">
                chargram drops: {mget(drops, :chargram) || 0}
              </span>
              <span class="rounded-full border px-2 py-0.5 dark:border-neutral-700">
                no-cand: {mget(drops, :no_cand) || 0}
              </span>

              <%= if episodes != [] do %>
                <span class="rounded-full border px-2 py-0.5 dark:border-neutral-700">
                  episodes: {length(episodes)}
                </span>
              <% end %>
            </div>

            <div class="mt-4 overflow-auto">
              <table class="w-full text-xs">
                <thead class="text-[10px] uppercase opacity-70">
                  <tr>
                    <th class="text-left py-2 pr-2">#</th>
                    <th class="text-left py-2 pr-2">sense</th>
                    <th class="text-left py-2 pr-2">pos</th>
                    <th class="text-left py-2 pr-2">total</th>
                    <th class="text-left py-2 pr-2">gloss</th>
                  </tr>
                </thead>
                <tbody>
                  <%= if Enum.empty?(candidates) do %>
                    <tr>
                      <td colspan="5" class="py-2 opacity-60">— no candidates —</td>
                    </tr>
                  <% else %>
                    <%= for {c, idx} <- Enum.with_index(Enum.take(candidates, 8), 1) do %>
                      <tr class="border-t dark:border-neutral-800">
                        <td class="py-2 pr-2 tabular-nums opacity-70">{idx}</td>
                        <td class="py-2 pr-2 font-mono">{mget(c, :id) || "—"}</td>
                        <td class="py-2 pr-2">{inspect(mget(c, :pos) || :_)}</td>
                        <td class="py-2 pr-2 tabular-nums">{fmt(mget(c, :total), 3)}</td>
                        <td class="py-2 pr-2">{truncate(mget(c, :gloss), 84)}</td>
                      </tr>

                      <%= if is_map(mget(c, :factors)) do %>
                        <tr>
                          <td></td>
                          <td colspan="4" class="pb-2 pr-2">
                            <div class="text-[10px] opacity-80">
                              {factors_inline(mget(c, :factors))}
                            </div>
                          </td>
                        </tr>
                      <% end %>
                    <% end %>
                  <% end %>
                </tbody>
              </table>
            </div>

            <%= if episodes != [] do %>
              <div class="mt-4 text-xs">
                <h5 class="font-semibold text-zinc-900 dark:text-zinc-100">Episode influence</h5>
                <ul class="mt-1 space-y-1">
                  <%= for e <- Enum.take(episodes, 6) do %>
                    <li class="opacity-90">
                      <code class="font-mono">{mget(e, :episode_id) || "—"}</code>
                      <span class="opacity-70"> boost </span>
                      <code class="font-mono">{fmt(mget(e, :boost), 3)}</code>
                    </li>
                  <% end %>
                </ul>
              </div>
            <% end %>
          <% else %>
            <div class="mt-2 text-xs opacity-60">— no sense data for selected token —</div>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  # --- tolerant getters -------------------------------------------------------

  defp mget(nil, _k), do: nil
  defp mget(m, k) when is_map(m), do: Map.get(m, k) || Map.get(m, to_string(k))
  defp mget(_, _), do: nil

  defp sense_entry(senses, nil) when is_map(senses), do: nil

  defp sense_entry(senses, i) when is_map(senses),
    do: Map.get(senses, i) || Map.get(senses, to_string(i))

  defp sense_entry(_, _), do: nil

  defp default_selected(tokens, senses) do
    case Enum.find(tokens, fn t -> is_map(sense_entry(senses, token_index(t))) end) do
      nil ->
        case tokens do
          [t | _] -> token_index(t)
          _ -> nil
        end

      t ->
        token_index(t)
    end
  end

  defp token_by_index(tokens, i) when is_list(tokens) and is_integer(i) do
    Enum.find(tokens, fn t -> token_index(t) == i end)
  end

  defp token_by_index(_, _), do: nil

  defp token_index(tok) do
    mget(tok, :i) || mget(tok, :index) || mget(tok, :token_index) || 0
  end

  defp token_raw(tok), do: mget(tok, :raw) || mget(tok, :text) || mget(tok, :token) || "—"
  defp token_norm(tok), do: mget(tok, :norm) || mget(tok, :normalized) || ""
  defp token_span(tok), do: mget(tok, :span) || {mget(tok, :start), mget(tok, :end)}
  defp token_mwe?(tok), do: mget(tok, :mwe?) || mget(tok, :mw) || false

  defp token_flags(entry) do
    drops = mget(entry, :drops) || %{}
    flags = []

    flags = if (mget(drops, :boundary) || 0) > 0, do: ["boundary_drop" | flags], else: flags
    flags = if (mget(drops, :chargram) || 0) > 0, do: ["chargram_drop" | flags], else: flags
    flags = if (mget(drops, :no_cand) || 0) > 0, do: ["no_cand" | flags], else: flags

    episodes = mget(entry, :episodes) || []
    flags = if is_list(episodes) and episodes != [], do: ["episode+" | flags], else: flags

    Enum.reverse(flags)
  end

  defp token_title(tok, winner, margin, flags) do
    base = "#{token_raw(tok)} · i=#{token_index(tok)}"
    w = if winner, do: " · winner=#{mget(winner, :id)}", else: ""
    m = if is_number(margin), do: " · margin=#{fmt(margin, 3)}", else: ""
    f = if flags != [], do: " · #{Enum.join(flags, ", ")}", else: ""
    base <> w <> m <> f
  end

  defp short_id(nil), do: "—"

  defp short_id(id) when is_binary(id) do
    if byte_size(id) > 18, do: String.slice(id, 0, 18) <> "…", else: id
  end

  defp short_id(other), do: inspect(other)

  defp pos_suffix(nil), do: ""
  defp pos_suffix(pos) when is_atom(pos), do: "/" <> Atom.to_string(pos)
  defp pos_suffix(pos) when is_binary(pos), do: "/" <> pos
  defp pos_suffix(_), do: ""

  defp truncate(nil, _n), do: ""

  defp truncate(s, n) when is_binary(s) do
    if byte_size(s) > n, do: String.slice(s, 0, n) <> "…", else: s
  end

  defp truncate(other, n), do: truncate(inspect(other), n)

  defp fmt(nil, _d), do: "—"
  defp fmt(x, d) when is_integer(x), do: fmt(x * 1.0, d)
  defp fmt(x, d) when is_float(x), do: :erlang.float_to_binary(x, decimals: d)
  defp fmt(x, _d) when is_binary(x), do: x
  defp fmt(other, _d), do: inspect(other)

  defp factors_inline(factors) when is_map(factors) do
    factors
    |> Enum.sort_by(fn {k, _} -> to_string(k) end)
    |> Enum.map(fn {k, v} -> "#{k}=#{fmt(v, 3)}" end)
    |> Enum.join(" · ")
  end

  defp factors_inline(_), do: ""
end
