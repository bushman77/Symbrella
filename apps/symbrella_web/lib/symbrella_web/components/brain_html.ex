# apps/symbrella_web/lib/symbrella_web/brain_html.ex
defmodule SymbrellaWeb.BrainHTML do
  @moduledoc """
  Presentation helpers for the Brain dashboard (HUD, status, mood, etc.).

  The primary surface (`brain/1`, `brain_header/1`, `brain_map/1`) now lives in
  `SymbrellaWeb.Components.Brain.Panels`. This module exposes reusable components
  and provides a compatibility delegate for `brain/1`.
  """

  use SymbrellaWeb, :html

  alias SymbrellaWeb.Components.Brain.Panels

  @doc """
  Delegates to `SymbrellaWeb.Components.Brain.Panels.brain/1`.
  """
  def brain(assigns), do: Panels.brain(assigns)

  # --- HUD row ---------------------------------------------------------------

  attr :clock, :map, default: %{}
  attr :intent, :map, default: %{}
  attr :mood, :map, default: %{derived: %{}}
  attr :auto, :any, default: false
  # region / brain snapshot (optional; used as fallback source)
  attr :snapshot, :any, default: nil
  # working memory summary (optional)
  attr :wm, :any, default: nil
  # attention/meta (optional; supports self-name hit)
  attr :attention, :any, default: nil
  # SelfPortrait snapshot (optional)
  attr :self_portrait, :any, default: nil

  def hud_row(assigns) do
    snap = assigns[:snapshot] || %{}

    clock =
      nonempty_map(assigns[:clock]) ||
        nonempty_map(mget(snap, :clock)) ||
        %{}

    intent =
      nonempty_map(assigns[:intent]) ||
        nonempty_map(mget(snap, :intent)) ||
        %{}

    # Prefer LiveView's :mood; fall back to snapshot.mood only if it's empty
    mood =
      case assigns[:mood] do
        %{} = m when map_size(m) > 0 ->
          m

        _ ->
          nonempty_map(mget(snap, :mood)) ||
            %{levels: %{}, derived: %{}, tone: :neutral}
      end

    # Support both shapes:
    #   - nested: mood.derived[:exploration]
    #   - flat:   mood[:exploration]
    expl = mood_val(mood, :exploration)
    inhib = mood_val(mood, :inhibition)
    vigil = mood_val(mood, :vigilance)
    plast = mood_val(mood, :plasticity)

    d = mget(mood, :derived) || %{}
    seq = mget(clock, :seq) || "—"
    hz = mget(clock, :hz) || "—"
    dtms = mget(clock, :dt_ms) || mget(clock, :dt) || "—"
    phi = mget(clock, :phi) || "—"

    label =
      mget(intent, :label) ||
        mget(intent, :intent) ||
        mget(intent, :name) ||
        mget(intent, :type)

    kw =
      mget(intent, :keyword) ||
        mget(intent, :kw) ||
        mget(intent, :keyword_norm)

    src = mget(intent, :source) || mget(intent, :src)
    conf = mget(intent, :confidence) || mget(intent, :score) || mget(intent, :prob)

    # WM summary: allow passing wm directly or via snapshot.wm / snapshot.workspace / etc
    wm =
      nonempty_map(normalize_wm(assigns[:wm])) ||
        nonempty_map(normalize_wm(mget(snap, :wm))) ||
        %{}

    wm_size =
      mget(wm, :size) ||
        mget(wm, :count) ||
        mget(wm, :n)

    wm_cap =
      mget(wm, :capacity) ||
        mget(wm, :cap) ||
        mget(wm, :limit) ||
        mget(wm, :max)

    wm_ratio =
      cond do
        is_integer(wm_size) and is_integer(wm_cap) -> "#{wm_size}/#{wm_cap}"
        is_number(wm_size) and is_number(wm_cap) -> "#{wm_size}/#{wm_cap}"
        true -> "—/—"
      end

    attention =
      nonempty_map(assigns[:attention]) ||
        nonempty_map(mget(snap, :attention)) ||
        %{}

    self_match =
      mget_in(attention, [:self_name, :match]) ||
        mget_in(attention, [:self_name, :value]) ||
        mget_in(attention, [:self, :match]) ||
        mget_in(attention, [:self, :value]) ||
        mget_in(snap, [:attention, :self_name, :match]) ||
        mget_in(snap, [:attention, :self_name, :value])

    self_hit =
      truthy?(mget_in(attention, [:self_name, :hit?])) or
        truthy?(mget_in(attention, [:self_name, :hit])) or
        truthy?(mget_in(attention, [:self, :hit?])) or
        truthy?(mget_in(attention, [:self, :hit])) or
        self_hit?(snap) or
        truthy?(assigns[:self_hit])

    # SelfPortrait: prefer explicit assigns, fall back to snapshot.self_portrait
    sp =
      nonempty_map(assigns[:self_portrait]) ||
        nonempty_map(mget(snap, :self_portrait)) ||
        %{}

    sp_traits = mget(sp, :traits) || %{}
    sp_patterns = mget(sp, :patterns) || %{}

    sp_conf = mget(sp_traits, :confidence_baseline)
    sp_stab = mget(sp_traits, :stability)
    sp_cur = mget(sp_traits, :curiosity_bias)

    sp_bd = mget(sp_patterns, :boundary_drops) || 0
    sp_cg = mget(sp_patterns, :chargram_violations) || 0
    sp_no_mwe = mget(sp_patterns, :no_mwe_senses) || 0

    show_self_portrait? =
      (is_map(sp_traits) and map_size(sp_traits) > 0) or
        (is_map(sp_patterns) and map_size(sp_patterns) > 0) or
        self_hit

    assigns =
      assigns
      |> assign(:seq, seq)
      |> assign(:hz, hz)
      |> assign(:dtms, dtms)
      |> assign(:phi, phi)
      |> assign(:label, label)
      |> assign(:kw, kw)
      |> assign(:src, src)
      |> assign(:conf, conf)
      |> assign(:d, d)
      |> assign(:expl, expl)
      |> assign(:inhib, inhib)
      |> assign(:vigil, vigil)
      |> assign(:plast, plast)
      |> assign(:auto_on, assigns[:auto] in [true, "on", "ON"])
      |> assign(:wm_ratio, wm_ratio)
      |> assign(:self_hit, self_hit)
      |> assign(:self_match, self_match)
      |> assign(:show_self_portrait, show_self_portrait?)
      |> assign(:sp_conf, sp_conf)
      |> assign(:sp_stab, sp_stab)
      |> assign(:sp_cur, sp_cur)
      |> assign(:sp_bd, sp_bd)
      |> assign(:sp_cg, sp_cg)
      |> assign(:sp_no_mwe, sp_no_mwe)

    ~H"""
    <div class="flex flex-wrap items-center gap-2">
      <.chip>
        <span class="font-semibold">Clock</span>
        <span class="opacity-70">seq</span> {@seq}
        <span class="opacity-70">Hz</span> {fmt(@hz)}
        <span class="opacity-70">Δt</span> {@dtms}ms <span class="opacity-70">ϕ</span> {fmt(@phi)}
      </.chip>

      <.chip>
        <span class="font-semibold">Intent</span>
        <%= if is_binary(@label) do %>
          <code class="px-1">{@label}</code>
        <% else %>
          <span>—</span>
        <% end %>
        <%= if is_binary(@kw) do %>
          · {@kw}
        <% end %>
        <%= if is_number(@conf) do %>
          · {fmt_pct(@conf)}
        <% else %>
          · --
        <% end %>
        <%= if is_binary(@src) do %>
          <span class="opacity-60">( {@src} )</span>
        <% end %>
      </.chip>

      <.chip>
        <span class="font-semibold">Mood</span>
        Expl: {fmt(@expl)} · Inhib: {fmt(@inhib)} · Vigil: {fmt(@vigil)} · Plast: {fmt(@plast)}
      </.chip>

      <.chip>
        <span class="font-semibold">WM</span>
        <span class="font-mono">{@wm_ratio}</span>
      </.chip>

      <%= if @show_self_portrait do %>
        <.chip>
          <span class="font-semibold">Self</span>

          <%= if @self_hit and is_binary(@self_match) and @self_match != "" do %>
            <code class="px-1">{@self_match}</code>
          <% end %>
          · <span class="opacity-70">conf</span> {fmt(@sp_conf)} ·
          <span class="opacity-70">stab</span> {fmt(@sp_stab)} ·
          <span class="opacity-70">cur</span> {fmt(@sp_cur)} ·
          <span class="opacity-70">bd</span> {to_string(@sp_bd)} ·
          <span class="opacity-70">cg</span> {to_string(@sp_cg)} ·
          <span class="opacity-70">no_mwe</span> {to_string(@sp_no_mwe)}
        </.chip>
      <% end %>

      <.chip>
        <button class="text-xs border px-2 py-1 rounded" phx-click="refresh">Refresh</button>
        <span class="opacity-60">Auto:</span> {(@auto_on && "ON") || "OFF"}
      </.chip>
    </div>
    """
  end

  # HUD chip component (internal)
  slot :inner_block, required: true

  defp chip(assigns) do
    ~H"""
    <span class="text-xs px-2 py-1 rounded-md border bg-white/70 text-zinc-700 flex items-center gap-1">
      {render_slot(@inner_block)}
    </span>
    """
  end

  # --- Blackboard panel ------------------------------------------------------

  attr :events, :list, default: []
  attr :filter, :string, default: ""
  attr :limit, :integer, default: 50

  def blackboard_panel(assigns) do
    now = System.system_time(:millisecond)
    q = (assigns[:filter] || "") |> String.trim() |> String.downcase()

    normalized =
      (assigns[:events] || [])
      |> Enum.map(&normalize_bb_event/1)

    filtered =
      if q == "" do
        normalized
      else
        Enum.filter(normalized, fn ev ->
          to_string(ev.tag) |> String.downcase() |> String.contains?(q) or
            ev.preview |> String.downcase() |> String.contains?(q)
        end)
      end

    view = filtered |> Enum.take(assigns[:limit] || 50)
    decorated = decorate_bb_view(view)

    assigns =
      assigns
      |> assign(:now, now)
      |> assign(:q, assigns[:filter] || "")
      |> assign(:count, length(normalized))
      |> assign(:fcount, length(filtered))
      |> assign(:view, view)
      |> assign(:decorated, decorated)

    ~H"""
    <div class="rounded-xl border border-gray-200 dark:border-gray-700 p-4 bg-white/75 dark:bg-neutral-900/70">
      <div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3">
        <div>
          <div class="text-sm font-semibold text-gray-700 dark:text-gray-200">Blackboard Feed</div>
          <div class="text-xs text-gray-500 dark:text-gray-400">
            showing {@fcount} / {@count} (latest {@limit})
          </div>
        </div>

        <div class="flex items-center gap-2">
          <form phx-change="bb_filter" class="flex items-center gap-2">
            <input
              type="text"
              name="q"
              value={@q}
              phx-debounce="150"
              placeholder="filter (tag / text)…"
              class="w-56 rounded-md border border-gray-200 dark:border-gray-700 bg-white/80 dark:bg-neutral-900 px-2 py-1 text-sm"
            />
          </form>
          <button
            type="button"
            phx-click="bb_clear"
            class="rounded-md border border-gray-200 dark:border-gray-700 px-2.5 py-1 text-xs hover:bg-white dark:hover:bg-neutral-900"
          >
            Clear
          </button>
        </div>
      </div>

      <div class="mt-3 space-y-2">
        <%= if @view == [] do %>
          <div class="text-sm text-gray-500 dark:text-gray-400">— no events —</div>
        <% else %>
          <%= for item <- @decorated do %>
            <%= case item do %>
              <% {:sep, sep} -> %>
                <div class="my-2 flex items-center gap-2">
                  <div class="h-px flex-1 bg-gray-200 dark:bg-gray-700"></div>
                  <div class="text-[10px] uppercase tracking-wide text-gray-500 dark:text-gray-400">
                    {sep.label}
                  </div>
                  <div class="h-px flex-1 bg-gray-200 dark:bg-gray-700"></div>
                </div>
              <% {:ev, ev} -> %>
                <details
                  open
                  class="rounded-lg border border-gray-200 dark:border-gray-700 bg-white/60 dark:bg-neutral-950/20 p-2"
                >
                  <summary class="cursor-pointer text-sm">
                    <span class="font-mono text-xs px-2 py-0.5 rounded bg-black/5 dark:bg-white/5 mr-2">
                      {to_string(ev.tag)}
                    </span>

                    <%= if is_integer(ev.frame_seq) do %>
                      <span class="font-mono text-[10px] px-1.5 py-0.5 rounded bg-black/5 dark:bg-white/5 mr-2">
                        F{to_string(ev.frame_seq)}
                      </span>
                    <% end %>

                    <span class="opacity-70 mr-2">
                      {format_age(@now, ev.at_ms)}
                    </span>

                    <span class="text-gray-700 dark:text-gray-200">
                      {ev.preview}
                    </span>
                  </summary>

                  <pre class="mt-2 text-xs leading-5 overflow-x-auto p-2 rounded bg-black/5 dark:bg-white/5"><%= inspect(ev.env, pretty: true, width: 100, limit: :infinity) %></pre>
                </details>
            <% end %>
          <% end %>
        <% end %>
      </div>
    </div>
    """
  end

  defp normalize_bb_event(%{at_ms: at, tag: tag, env: env} = m) do
    env_map = if(is_map(env), do: env, else: %{})

    frame_seq = mget(env_map, :frame_seq)
    frame_ts_ms = ms_from_env(env_map)

    at_ms =
      cond do
        is_integer(at) and at > 0 -> at
        is_integer(frame_ts_ms) and frame_ts_ms > 0 -> frame_ts_ms
        true -> 0
      end

    %{
      id: Map.get(m, :id),
      at_ms: at_ms,
      frame_seq: if(is_integer(frame_seq), do: frame_seq, else: nil),
      frame_ts_ms: if(is_integer(frame_ts_ms), do: frame_ts_ms, else: 0),
      tag: tag || :event,
      env: env,
      preview: preview_env(env)
    }
  end

  defp normalize_bb_event(env) do
    env_map = if(is_map(env), do: env, else: %{})
    frame_seq = mget(env_map, :frame_seq)
    frame_ts_ms = ms_from_env(env_map)

    %{
      id: nil,
      at_ms: if(is_integer(frame_ts_ms) and frame_ts_ms > 0, do: frame_ts_ms, else: 0),
      frame_seq: if(is_integer(frame_seq), do: frame_seq, else: nil),
      frame_ts_ms: if(is_integer(frame_ts_ms), do: frame_ts_ms, else: 0),
      tag: :event,
      env: env,
      preview: preview_env(env)
    }
  end

  defp ms_from_env(%{} = env) do
    v =
      mget(env, :at_ms) ||
        mget(env, :ts_ms) ||
        mget(env, :frame_ts_ms)

    to_ms(v)
  end

  defp ms_from_env(_), do: 0

  defp to_ms(v) when is_integer(v), do: v

  defp to_ms(v) when is_binary(v) do
    case Integer.parse(v) do
      {i, _} -> i
      _ -> 0
    end
  end

  defp to_ms(_), do: 0

  defp decorate_bb_view(events) when is_list(events) do
    {items, _prev} =
      Enum.reduce(events, {[], nil}, fn ev, {acc, prev} ->
        acc2 =
          case separator_for(prev, ev) do
            nil -> acc
            sep -> [{:sep, sep} | acc]
          end

        {[{:ev, ev} | acc2], ev}
      end)

    Enum.reverse(items)
  end

  defp decorate_bb_view(_), do: []

  defp separator_for(nil, ev) do
    %{label: separator_label(:start, ev)}
  end

  defp separator_for(prev, ev) when is_map(prev) and is_map(ev) do
    prev_seq = prev[:frame_seq]
    ev_seq = ev[:frame_seq]

    cond do
      is_integer(prev_seq) and is_integer(ev_seq) and prev_seq != ev_seq ->
        %{label: separator_label(:frame, ev)}

      is_integer(prev[:at_ms]) and is_integer(ev[:at_ms]) and
          abs(prev[:at_ms] - ev[:at_ms]) > 1_500 ->
        %{label: separator_label(:timegap, ev)}

      true ->
        nil
    end
  end

  defp separator_for(_prev, _ev), do: nil

  defp separator_label(kind, ev) do
    seq = ev[:frame_seq]
    ts = ev[:frame_ts_ms]
    ts_s = if(is_integer(ts) and ts > 0, do: format_ts(ts), else: "—")

    base =
      cond do
        is_integer(seq) -> "Frame #{seq}"
        true -> "Frame"
      end

    case kind do
      :start -> "#{base} · #{ts_s}"
      :frame -> "#{base} · #{ts_s}"
      :timegap -> "— #{base} · #{ts_s} —"
      _ -> "#{base} · #{ts_s}"
    end
  end

  defp format_ts(ms) when is_integer(ms) and ms > 0 do
    try do
      ms
      |> DateTime.from_unix!(:millisecond)
      |> Calendar.strftime("%H:%M:%S")
    rescue
      _ -> "#{ms}ms"
    end
  end

  defp format_ts(_), do: "—"

  defp preview_env(env) do
    s = inspect(env, pretty: false, limit: 50, printable_limit: 300)
    if byte_size(s) > 120, do: binary_part(s, 0, 120) <> "…", else: s
  end

  defp format_age(now_ms, at_ms) when is_integer(at_ms) and at_ms > 0 do
    delta = max(now_ms - at_ms, 0)
    "#{delta}ms ago"
  end

  defp format_age(_now_ms, _at_ms), do: "—"

  # --- Working memory panel --------------------------------------------------

  attr :wm, :any, default: %{}

  def wm_panel(assigns) do
    wm = assigns[:wm] || %{}

    items =
      cond do
        is_map(wm) and is_list(wm[:items]) -> wm[:items]
        is_map(wm) and is_list(wm[:wm]) -> wm[:wm]
        is_map(wm) and is_list(wm[:value]) -> wm[:value]
        true -> []
      end

    empty? = items == []

    assigns =
      assigns
      |> assign(:wm, wm)
      |> assign(:items, items)
      |> assign(:empty?, empty?)

    ~H"""
    <div class="rounded-xl border border-gray-200 dark:border-gray-700 p-4 bg-white/75 dark:bg-neutral-900/70">
      <div class="flex items-center justify-between">
        <div class="text-sm font-semibold text-gray-700 dark:text-gray-200">Working Memory (WM)</div>
        <div class="text-xs text-gray-500 dark:text-gray-400">
          <%= if is_map(@wm) do %>
            <%= if @wm[:source] do %>
              source: <span class="font-mono">{to_string(@wm[:source])}</span>
            <% end %>
          <% else %>
            —
          <% end %>
        </div>
      </div>

      <%= if @empty? do %>
        <div class="mt-2 text-sm text-gray-500 dark:text-gray-400">— no WM observed yet —</div>
      <% else %>
        <pre class="mt-3 text-xs leading-5 overflow-x-auto p-2 rounded bg-black/5 dark:bg-white/5"><%= inspect(@wm, pretty: true, width: 100, limit: :infinity) %></pre>
      <% end %>
    </div>
    """
  end

  # --- Live state ------------------------------------------------------------

  attr :state, :any, default: %{}

  def live_state_panel(assigns) do
    st = assigns[:state] || %{}

    assigns =
      assigns
      |> assign(:proc, Map.get(st, :process) || Map.get(st, :process_name) || "N/A")
      |> assign(:pid, Map.get(st, :pid) || "N/A")
      |> assign(:q, Map.get(st, :queue, 0))
      |> assign(:cur, Map.get(st, :current) || Map.get(st, :current_msg))
      |> assign(:state_dump, Map.get(st, :state, st))

    ~H"""
    <div class="border rounded-xl p-4 shadow-sm">
      <h2 class="font-semibold mb-2">Live state</h2>

      <div class="text-sm space-y-1 mb-2">
        <div><span class="opacity-60">process:</span> <code>{inspect(@proc)}</code></div>
        <div><span class="opacity-60">pid:</span> <code>{inspect(@pid)}</code></div>
        <div><span class="opacity-60">queue:</span> {@q}</div>
        <div><span class="opacity-60">current:</span> {inspect(@cur || :idle)}</div>
      </div>

      <div class="text-sm text-zinc-500">State</div>
      <pre class="text-xs bg-zinc-50 p-2 rounded border mt-1 overflow-x-auto">
        <%= inspect(@state_dump, limit: :infinity) %>
      </pre>
    </div>
    """
  end

  # --- Module status ---------------------------------------------------------

  attr :status, :any, required: true
  attr :selected, :atom, default: :lifg

  def module_status_panel(assigns) do
    stat = assigns[:status] || %{}

    {tag_level, tag_text} =
      cond do
        Map.get(stat, :ok?, false) ->
          {:ok, "running"}

        stat[:error] in [:not_running, :noproc] ->
          {:warn, "not running"}

        true ->
          {:neutral, "unknown"}
      end

    assigns =
      assigns
      |> assign(:tag_level, tag_level)
      |> assign(:tag_text, tag_text)
      |> assign(:status, stat)

    ~H"""
    <div class="border rounded-xl p-4 shadow-sm">
      <div class="flex items-center justify-between mb-2">
        <h2 class="font-semibold">
          Module Status — {@selected |> to_string() |> String.upcase()}
        </h2>
        <span class={["text-xs px-2 py-1 rounded border", status_badge_class(@tag_level)]}>
          {@tag_text}
        </span>
      </div>

      <%= if @status == %{} do %>
        <div class="text-sm text-zinc-400">No status available</div>
      <% else %>
        <%= if Map.get(@status, :ok?) && not is_nil(Map.get(@status, :status)) do %>
          <div class="text-sm text-zinc-500 mb-1">Details</div>
          <pre class="text-xs bg-zinc-50 p-2 rounded border overflow-x-auto">
            <%= inspect(@status.status, limit: :infinity) %>
          </pre>
        <% else %>
          <div class="text-sm">
            {case @status[:error] do
              :not_running -> "Region process is not running."
              :noproc -> "No process registered for this region."
              {:exit, r} -> "Exited while fetching status: #{inspect(r)}"
              other -> "Status unavailable: #{inspect(other)}"
            end}
          </div>
        <% end %>
      <% end %>
    </div>
    """
  end

  defp status_badge_class(:ok), do: "border-green-400 text-green-600"
  defp status_badge_class(:warn), do: "border-amber-400 text-amber-600"
  defp status_badge_class(_neutral), do: "border-zinc-300 text-zinc-600"

  # --- Mood panel ------------------------------------------------------------

  def mood_panel(assigns) do
    mood = Map.get(assigns, :mood, %{levels: %{}, derived: %{}, tone: :neutral})

    assigns =
      assigns
      |> assign(:levels, Map.get(mood, :levels, %{}) || %{})
      |> assign(:derived, Map.get(mood, :derived, %{}) || %{})
      |> assign(:tone, Map.get(mood, :tone, :neutral) || :neutral)

    ~H"""
    <div class="border rounded-xl p-4 shadow-sm">
      <div class="flex items-center justify-between mb-3">
        <h2 class="font-semibold">Mood</h2>
        <span class={["text-xs px-2 py-1 rounded border", tone_class(@tone)]}>
          {to_string(@tone)}
        </span>
      </div>

      <div class="grid grid-cols-2 gap-4">
        <div>
          <h3 class="text-sm text-zinc-500 mb-1">Neuromodulators</h3>
          <.kv label="DA" value={fmt(@levels[:da])} />
          <.kv label="5HT" value={fmt(@levels[:"5ht"])} />
          <.kv label="GLU" value={fmt(@levels[:glu])} />
          <.kv label="NE" value={fmt(@levels[:ne])} />
        </div>

        <div>
          <h3 class="text-sm text-zinc-500 mb-1">Derived</h3>
          <.kv label="Exploration" value={fmt(@derived[:exploration])} />
          <.kv label="Inhibition" value={fmt(@derived[:inhibition])} />
          <.kv label="Vigilance" value={fmt(@derived[:vigilance])} />
          <.kv label="Plasticity" value={fmt(@derived[:plasticity])} />
        </div>
      </div>
    </div>
    """
  end

  # --- small UI helpers ------------------------------------------------------

  attr :label, :any, required: true
  attr :value, :any, required: true

  defp kv(assigns) do
    ~H"""
    <div class="flex items-center justify-between py-0.5">
      <span class="text-sm text-zinc-500">{@label}</span>
      <span class="font-mono text-sm">{@value}</span>
    </div>
    """
  end

  defp tone_class(:positive), do: "border-green-400 text-green-600"
  defp tone_class(:negative), do: "border-red-400 text-red-600"
  defp tone_class(_), do: "border-zinc-300 text-zinc-600"

  defp fmt(nil), do: "—"

  defp fmt(v) when is_number(v),
    do: :io_lib.format("~.3f", [v]) |> IO.iodata_to_binary()

  defp fmt(v), do: to_string(v)

  defp fmt_pct(nil), do: "--"
  defp fmt_pct(v) when is_integer(v), do: "#{v}%"

  # Treat floats in 0..1 as probability; otherwise treat as 0..100 already
  defp fmt_pct(v) when is_float(v) and v >= 0.0 and v <= 1.0,
    do: :io_lib.format("~.1f%", [v * 100.0]) |> IO.iodata_to_binary()

  defp fmt_pct(v) when is_float(v),
    do: :io_lib.format("~.1f%", [v]) |> IO.iodata_to_binary()

  defp fmt_pct(v), do: to_string(v)

  # --- tiny helpers for mixed-key maps + mood dims --------------------------

  defp mget(m, k) when is_map(m),
    do: Map.get(m, k) || Map.get(m, to_string(k))

  defp mget(_, _), do: nil

  defp mget_in(m, [k | rest]) when is_map(m) do
    case mget(m, k) do
      %{} = next -> mget_in(next, rest)
      other -> other
    end
  end

  defp mget_in(_, _), do: nil

  defp nonempty_map(%{} = m) when map_size(m) > 0, do: m
  defp nonempty_map(_), do: nil

  # Try nested `mood.derived[key]`, then flat `mood[key]`
  defp mood_val(%{} = mood, key) do
    derived = mget(mood, :derived) || %{}
    mget(derived, key) || mget(mood, key)
  end

  defp mood_val(_, _), do: nil

  defp truthy?(v) when v in [true, "true", "TRUE", "1", 1, true, :yes, "yes", "on", "ON"],
    do: true

  defp truthy?(_), do: false

  defp self_hit?(snap) do
    truthy?(mget_in(snap, [:attention, :self_name, :hit?])) or
      truthy?(mget_in(snap, [:attention, :self_name, :hit])) or
      truthy?(mget_in(snap, [:attention_stamp, :self_name_hit?])) or
      truthy?(mget_in(snap, [:attention, :self_hit]))
  end

  defp normalize_wm(nil), do: nil
  defp normalize_wm(%{} = wm), do: wm

  defp normalize_wm(other) when is_list(other) do
    # if someone passes a list of items, treat length as size
    %{size: length(other)}
  end

  defp normalize_wm(_), do: nil
end
