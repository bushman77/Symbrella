#apps/symbrella_web/lib/symbrella_web/components/brain_html.ex
defmodule SymbrellaWeb.BrainHTML do
  @moduledoc """
  Presentation helpers for the Brain dashboard (HUD, status, mood, etc.).

  The primary surface (`brain/1`, `brain_header/1`, `brain_map/1`) now lives in
  `SymbrellaWeb.Components.Brain.Panels`. This module exposes reusable components
  and provides a compatibility delegate for `brain/1`.
  """

  use SymbrellaWeb, :html

  alias SymbrellaWeb.Components.Brain.Panels

  # ---------------------------------------------------------------------------
  # Compatibility shim: keep existing call-sites working
  # ---------------------------------------------------------------------------
  @doc """
  Delegates to `SymbrellaWeb.Components.Brain.Panels.brain/1`.
  """
  def brain(assigns), do: Panels.brain(assigns)

  # --- HUD row ---------------------------------------------------------------
  # Public so Panels.brain/1 can call it

  attr :clock, :map, default: %{}
  attr :intent, :map, default: %{}
  attr :mood, :map, default: %{derived: %{}}
  attr :auto, :any, default: false
  # region / brain snapshot (optional; used as fallback source)
  attr :snapshot, :any, default: nil

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
    expl  = mood_val(mood, :exploration)
    inhib = mood_val(mood, :inhibition)
    vigil = mood_val(mood, :vigilance)
    plast = mood_val(mood, :plasticity)

    d    = mget(mood, :derived) || %{}
    seq  = mget(clock, :seq) || "—"
    hz   = mget(clock, :hz) || "—"
    dtms = mget(clock, :dt_ms) || mget(clock, :dt) || "—"
    phi  = mget(clock, :phi) || "—"

    label =
      mget(intent, :label) ||
        mget(intent, :intent) ||
        mget(intent, :name) ||
        mget(intent, :type)

    kw =
      mget(intent, :keyword) ||
        mget(intent, :kw) ||
        mget(intent, :keyword_norm)

    src  = mget(intent, :source) || mget(intent, :src)
    conf = mget(intent, :confidence) || mget(intent, :score) || mget(intent, :prob)

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

    ~H"""
    <div class="flex flex-wrap items-center gap-2">
      <.chip>
        <span class="font-semibold">Clock</span>
        <span class="opacity-70">seq</span> <%= @seq %>
        <span class="opacity-70">Hz</span> <%= fmt(@hz) %>
        <span class="opacity-70">Δt</span> <%= @dtms %>ms
        <span class="opacity-70">ϕ</span> <%= fmt(@phi) %>
      </.chip>

      <.chip>
        <span class="font-semibold">Intent</span>
        <%= if is_binary(@label) do %>
          <code class="px-1"><%= @label %></code>
        <% else %>
          <span>—</span>
        <% end %>
        <%= if is_binary(@kw) do %>· <%= @kw %><% end %>
        <%= if is_number(@conf) do %>· <%= fmt_pct(@conf) %><% else %>· --<% end %>
        <%= if is_binary(@src) do %>
          <span class="opacity-60">( <%= @src %> )</span>
        <% end %>
      </.chip>

      <.chip>
        <span class="font-semibold">Mood</span>
        Expl: <%= fmt(@expl) %>
        · Inhib: <%= fmt(@inhib) %>
        · Vigil: <%= fmt(@vigil) %>
        · Plast: <%= fmt(@plast) %>
      </.chip>

      <.chip>
        <button class="text-xs border px-2 py-1 rounded" phx-click="refresh">Refresh</button>
        <span class="opacity-60">Auto:</span> <%= @auto_on && "ON" || "OFF" %>
      </.chip>
    </div>
    """
  end

  # HUD chip component (internal)
  slot :inner_block, required: true
  defp chip(assigns) do
    ~H"""
    <span class="text-xs px-2 py-1 rounded-md border bg-white/70 text-zinc-700 flex items-center gap-1">
      <%= render_slot(@inner_block) %>
    </span>
    """
  end

  # --- Live state ------------------------------------------------------------
  # Public so Panels.brain/1 can call it

  attr :state, :any, default: %{}
  def live_state_panel(assigns) do
    st = assigns[:state] || %{}

    assigns =
      assigns
      |> assign(:proc, Map.get(st, :process) || Map.get(st, :process_name) || "N/A")
      |> assign(:pid,  Map.get(st, :pid) || "N/A")
      |> assign(:q,    Map.get(st, :queue, 0))
      |> assign(:cur,  Map.get(st, :current) || Map.get(st, :current_msg))
      |> assign(:state_dump, Map.get(st, :state, st))

    ~H"""
    <div class="border rounded-xl p-4 shadow-sm">
      <h2 class="font-semibold mb-2">Live state</h2>

      <div class="text-sm space-y-1 mb-2">
        <div><span class="opacity-60">process:</span> <code><%= inspect(@proc) %></code></div>
        <div><span class="opacity-60">pid:</span> <code><%= inspect(@pid) %></code></div>
        <div><span class="opacity-60">queue:</span> <%= @q %></div>
        <div><span class="opacity-60">current:</span> <%= inspect(@cur || :idle) %></div>
      </div>

      <div class="text-sm text-zinc-500">State</div>
      <pre class="text-xs bg-zinc-50 p-2 rounded border mt-1 overflow-x-auto">
        <%= inspect(@state_dump, limit: :infinity) %>
      </pre>
    </div>
    """
  end

  # --- Module status ---------------------------------------------------------
  # Public so Panels.brain/1 can call it

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
          Module Status — <%= @selected |> to_string() |> String.upcase() %>
        </h2>
        <span class={["text-xs px-2 py-1 rounded border", status_badge_class(@tag_level)]}>
          <%= @tag_text %>
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
            <%= case @status[:error] do
              :not_running -> "Region process is not running."
              :noproc      -> "No process registered for this region."
              {:exit, r}   -> "Exited while fetching status: #{inspect(r)}"
              other        -> "Status unavailable: #{inspect(other)}"
            end %>
          </div>
        <% end %>
      <% end %>
    </div>
    """
  end

  defp status_badge_class(:ok),      do: "border-green-400 text-green-600"
  defp status_badge_class(:warn),    do: "border-amber-400 text-amber-600"
  defp status_badge_class(_neutral), do: "border-zinc-300 text-zinc-600"

  # --- Mood panel ------------------------------------------------------------
  # Public so Panels.brain/1 can call it

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
          <%= to_string(@tone) %>
        </span>
      </div>

      <div class="grid grid-cols-2 gap-4">
        <div>
          <h3 class="text-sm text-zinc-500 mb-1">Neuromodulators</h3>
          <.kv label="DA"  value={fmt(@levels[:da])} />
          <.kv label="5HT" value={fmt(@levels[:"5ht"])} />
          <.kv label="GLU" value={fmt(@levels[:glu])} />
          <.kv label="NE"  value={fmt(@levels[:ne])} />
        </div>

        <div>
          <h3 class="text-sm text-zinc-500 mb-1">Derived</h3>
          <.kv label="Exploration" value={fmt(@derived[:exploration])} />
          <.kv label="Inhibition"  value={fmt(@derived[:inhibition])} />
          <.kv label="Vigilance"   value={fmt(@derived[:vigilance])} />
          <.kv label="Plasticity"  value={fmt(@derived[:plasticity])} />
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
      <span class="text-sm text-zinc-500"><%= @label %></span>
      <span class="font-mono text-sm"><%= @value %></span>
    </div>
    """
  end

  defp tone_class(:positive), do: "border-green-400 text-green-600"
  defp tone_class(:negative), do: "border-red-400 text-red-600"
  defp tone_class(_),         do: "border-zinc-300 text-zinc-600"

  defp fmt(nil), do: "—"
  defp fmt(v) when is_number(v),
    do: :io_lib.format("~.3f", [v]) |> IO.iodata_to_binary()
  defp fmt(v), do: to_string(v)

  defp fmt_pct(nil), do: "--"
  defp fmt_pct(v) when is_integer(v), do: "#{v}%"
  defp fmt_pct(v) when is_float(v),
    do: :io_lib.format("~.1f%", [v]) |> IO.iodata_to_binary()
  defp fmt_pct(v), do: to_string(v)

  # --- tiny helpers for mixed-key maps + mood dims --------------------------

  defp mget(m, k) when is_map(m),
    do: Map.get(m, k) || Map.get(m, to_string(k))
  defp mget(_, _), do: nil

  defp nonempty_map(%{} = m) when map_size(m) > 0, do: m
  defp nonempty_map(_), do: nil

  # Try nested `mood.derived[key]`, then flat `mood[key]`
  defp mood_val(%{} = mood, key) do
    derived = mget(mood, :derived) || %{}
    mget(derived, key) || mget(mood, key)
  end

  defp mood_val(_, _), do: nil
end

