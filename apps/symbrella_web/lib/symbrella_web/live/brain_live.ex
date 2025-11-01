defmodule SymbrellaWeb.BrainLive do
  @moduledoc """
  LiveView for the Brain dashboard. All HTML is delegated to `SymbrellaWeb.BrainHTML`.

  • Safe defaults so first render never crashes (even if upstream is quiet)
  • Subscribes to Brain.Bus for blackboard + HUD topics (clock/intent/mood/lifg)
  • Telemetry bridge for mood updates
  • Loads inline brain SVG from priv/static/images/brain.svg when available
  • ✅ Periodic, defensive region snapshot + status refresh
  • ✅ Selected-region status panel (prefers GenServer.call(mod, :status))
  • ✅ All-regions status grid (fully registry-driven, no hard-coding)
  • ✅ Intent chip stays in sync with brain state (PubSub + snapshot fallback)
  """

  use SymbrellaWeb, :live_view
  require Logger

  alias SymbrellaWeb.BrainHTML
  alias SymbrellaWeb.Region.Registry, as: RegionRegistry
  alias Brain.Bus
  # Optional (used if present):
  alias Brain.Introspect

  @blackboard_topic "brain:blackboard"
  @hud_topics ~w(brain:clock brain:intent brain:mood brain:lifg)
  @refresh_ms 500

  @mood_defaults %{
    levels: %{da: 0.35, "5ht": 0.50, glu: 0.40, ne: 0.50},
    derived: %{exploration: 0.41, inhibition: 0.50, vigilance: 0.50, plasticity: 0.375},
    tone: :neutral
  }

  # ---------------------------------------------------------------------------
  # LiveView lifecycle
  # ---------------------------------------------------------------------------

  @impl true
  def mount(_params, _session, socket) do
    _ = ensure_optional(Brain.Cerebellum)
    _ = ensure_optional(Brain.LIFG)   # <— make sure LIFG is up if available

    socket =
      socket
      # Core assigns (safe defaults)
      |> assign_new(:selected,       fn -> default_selected() end)
      |> assign_new(:regions,        fn -> [] end)
      |> assign_new(:module_info,    fn -> %{} end)
      |> assign_new(:intent,         fn -> %{} end)
      |> assign_new(:snapshot,       fn -> nil end)
      |> assign_new(:lifg_last,      fn -> %{} end)
      |> assign_new(:hippo,          fn -> %{window: []} end)
      |> assign_new(:hippo_metrics,  fn -> %{} end)
      |> assign_new(:region,         fn -> %{} end)
      |> assign_new(:region_state,   fn -> %{workspace: [], snapshot: %{}} end)
      |> assign_new(:region_status,  fn -> %{} end)
      |> assign_new(:auto,           fn -> false end)
      |> assign_new(:clock,          fn -> %{} end)
      |> assign_new(:mood_levels,  fn -> @mood_defaults.levels  end)
      |> assign_new(:mood_derived, fn -> @mood_defaults.derived end)
      |> assign_new(:mood_tone,    fn -> @mood_defaults.tone    end)
      |> assign_new(:telemetry_mood_id, fn -> nil end)   # track handler id for detach
      |> ensure_mood_alias()
      |> assign_new(:brain_svg, fn -> load_brain_svg() end)
      |> assign(:all_status, collect_all_region_status())
      |> maybe_seed_intent_from_brain()

    socket =
      if connected?(socket) do
        :ok = Bus.subscribe(@blackboard_topic)
        Enum.each(@hud_topics, &Bus.subscribe/1)
        :timer.send_interval(@refresh_ms, :refresh_selected)
        attach_mood_telemetry(socket)   # <— USE IT HERE ✅
      else
        socket
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    assigns =
      if Map.has_key?(assigns, :mood),
        do: assigns,
        else: Map.put(assigns, :mood, @mood_defaults)

    ~H"""
    <%= BrainHTML.brain(assigns) %>

    <.selected_region_status selected={@selected} status={@region_status} />
    <.regions_status_grid all_status={@all_status} />
    """
  end

  # ---------------------------------------------------------------------------
  # Telemetry bridge (mood)
  # ---------------------------------------------------------------------------

  # Call this from mount/3 once:
  # socket |> attach_mood_telemetry()
  defp attach_mood_telemetry(socket) do
    # Don’t attach twice on reconnect/code-reload
    if socket.assigns[:telemetry_mood_id] do
      socket
    else
      id = "brain-mood-ui:#{inspect(self())}"

      events = [
        [:brain, :mood, :updated],
        [:brain, :mood, :tick],
        [:brain, :mood, :update] # legacy/alt spelling
      ]

      try do
        if Code.ensure_loaded?(:telemetry) do
          :ok = :telemetry.attach_many(id, events, &__MODULE__.handle_mood_event/4, self())
        end
      rescue
        _ -> :ok
      end

      assign(socket, :telemetry_mood_id, id)
    end
  end

# -- Mood event handlers (grouped) ---------------------------------------------

# Canonical: send {:mood_event, measurements, metadata}
def handle_mood_event(_event, measurements, metadata, lv_pid) when is_pid(lv_pid) do
  send(lv_pid, {:mood_event, measurements, metadata})
  :ok
end

# Non-PID callers: ignore safely
def handle_mood_event(_event, _m, _md, _other), do: :ok

# Legacy/alternate: send {:mood_update, meas, meta}
# Keep this only if something in your UI is already matching on :mood_update.
def on_mood_event(_event, meas, meta, pid) when is_pid(pid) do
  send(pid, {:mood_update, meas, meta})
  :ok
end

# Non-PID callers: ignore safely
def on_mood_event(_event, _meas, _meta, _other), do: :ok

  @impl true
  def terminate(_reason, socket) do
    if id = socket.assigns[:telemetry_mood_id] do
      :telemetry.detach(id)
    end
    :ok
  end

  # ---------------------------------------------------------------------------
  # Params & UI events
  # ---------------------------------------------------------------------------

  defp default_selected do
    keys = RegionRegistry.keys()

    cond do
      Enum.member?(keys, :lifg) -> :lifg
      keys != [] -> hd(keys)
      true -> :lifg
    end
  end

  @impl true
  def handle_params(params, _uri, socket) do
    socket =
      case Map.get(params, "r") do
        nil -> socket
        r when is_binary(r) ->
          sel =
            try do
              String.to_existing_atom(r)
            rescue
              ArgumentError -> socket.assigns.selected
            end

          if RegionRegistry.available?(sel), do: assign(socket, :selected, sel), else: socket
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("select-region", params, socket) do
    # Expecting phx-value-region from the <path> tag
    r = Map.get(params, "region") || Map.get(params, "value") || Map.get(params, "r")

    sel =
      cond do
        is_atom(r) -> r
        is_binary(r) ->
          try do
            String.to_existing_atom(r)
          rescue
            ArgumentError -> socket.assigns.selected
          end

        true -> socket.assigns.selected
      end

    send(self(), :refresh_selected) # instant refresh on change
    {:noreply, assign(socket, :selected, sel)}
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    send(self(), :refresh_selected)
    {:noreply, socket}
  end

  # ---------------------------------------------------------------------------
  # Incoming telemetry + PubSub (ALL handle_info clauses grouped together)
  # ---------------------------------------------------------------------------

  @impl true
  def handle_info({:mood_event, meas, meta}, socket) do
    mood =
      meta[:mood] ||
        %{
          levels: Map.get(meta, :levels, %{}),
          derived: Map.get(meta, :derived, %{}),
          tone: Map.get(meta, :tone, :neutral)
        }

    {:noreply,
     socket
     |> assign(:mood, mood)
     |> assign(:mood_last, %{at: System.system_time(:millisecond), meas: meas})}
  end

  @impl true
  def handle_info({:mood_update, meas, _meta}, socket) do
    levels = %{
      da:    Map.get(meas, :da,    0.5),
      "5ht": Map.get(meas, :"5ht", 0.5),
      glu:   Map.get(meas, :glu,   0.5),
      ne:    Map.get(meas, :ne,    0.5)
    }

    derived = %{
      exploration: Map.get(meas, :exploration, 0.5),
      inhibition:  Map.get(meas, :inhibition,  0.5),
      vigilance:   Map.get(meas, :vigilance,   0.5),
      plasticity:  Map.get(meas, :plasticity,  0.5)
    }

    tone = socket.assigns[:mood_tone] || :neutral

    {:noreply,
     socket
     |> assign(:mood_levels, levels)
     |> assign(:mood_derived, derived)
     |> assign(:mood_tone, tone)
     |> assign(:mood, %{levels: levels, derived: derived, tone: tone})}
  end

  @impl true
  def handle_info({tag, env}, socket) when tag in [:blackboard, :brain_bus] do
    # Keep workspace; also opportunistically harvest intent if present in env
    socket =
      case extract_intent_any(env) do
        nil -> socket
        intent -> assign(socket, :intent, stamp_intent(intent, "blackboard"))
      end

    {:noreply,
     update(socket, :region_state, fn st ->
       st = st || %{}
       ws = [env | (st[:workspace] || [])] |> Enum.take(100)
       Map.put(st, :workspace, ws)
     end)}
  end

  @impl true
  def handle_info({:clock, m}, socket),
    do: {:noreply, assign(socket, :clock,  m || %{})}

  @impl true
  def handle_info({:intent, m}, socket) do
    # PubSub winner — normalize and stamp
    intent = normalize_intent(m, "bus")
    {:noreply, assign(socket, :intent, intent)}
  end

  @impl true
  def handle_info({:mood, m}, socket) do
    mood = m || %{}
    levels  = Map.get(mood, :levels,  socket.assigns[:mood_levels]  || @mood_defaults.levels)
    derived = Map.get(mood, :derived, socket.assigns[:mood_derived] || @mood_defaults.derived)
    tone    = Map.get(mood, :tone,    socket.assigns[:mood_tone]    || @mood_defaults.tone)

    {:noreply,
     socket
     |> assign(:mood_levels, levels)
     |> assign(:mood_derived, derived)
     |> assign(:mood_tone, tone)
     |> assign(:mood, %{levels: levels, derived: derived, tone: tone})}
  end

  @impl true
  def handle_info({:lifg_update, m}, socket),
    do: {:noreply, assign(socket, :lifg_last, m || %{})}

  # Periodic (and manual) refresh for the selected region
  @impl true
  def handle_info(:refresh_selected, socket) do
    sel = socket.assigns[:selected] || default_selected()
    {snapshot, status} = fetch_snapshot_and_status(sel)

    # Also pull global intent from the *brain state* so the HUD chip stays fresh
    socket =
      case fetch_global_intent() do
        nil -> socket
        intent -> assign(socket, :intent, stamp_intent(intent, "snapshot"))
      end

    socket =
      socket
      |> assign(:snapshot, snapshot)
      |> assign(:region_status, status || %{})
      |> assign(:all_status, collect_all_region_status()) # keep global grid fresh
      |> update(:region_state, fn st ->
        st = st || %{}
        ws = st[:workspace] || []
        st = Map.put(st, :snapshot, snapshot || %{})
        Map.put_new(st, :workspace, ws)
      end)

    {:noreply, socket}
  end

  @impl true
  def handle_info(_msg, socket), do: {:noreply, socket}

  # ---------------------------------------------------------------------------
  # Internal helpers
  # ---------------------------------------------------------------------------

  defp ensure_optional(mod) when is_atom(mod) do
    cond do
      Code.ensure_loaded?(mod) and function_exported?(mod, :ensure_started, 0) ->
        try do
          mod.ensure_started()
        catch
          _, _ -> :ok
        end

      true ->
        :ok
    end
  end

# --- Telemetry bridge (mood) handlers ---------------------------------------

  defp ensure_mood_alias(socket) do
    a = socket.assigns

    mood =
      a[:mood] ||
        %{
          levels: Map.get(a, :mood_levels,  @mood_defaults.levels),
          derived: Map.get(a, :mood_derived, @mood_defaults.derived),
          tone: Map.get(a, :mood_tone, @mood_defaults.tone)
        }

    assign(socket, :mood, mood)
  end

  defp load_brain_svg do
    with {:ok, priv} <- safe_priv_dir(:symbrella_web),
         path <- Path.join([priv, "static", "images", "brain.svg"]),
         true <- File.exists?(path),
         {:ok, svg} <- File.read(path) do
      svg
    else
      _ -> nil
    end
  end

  defp safe_priv_dir(app) do
    try do
      {:ok, :code.priv_dir(app) |> List.to_string()}
    rescue
      _ -> :error
    end
  end

  # ---- Region fetchers ------------------------------------------------------

  # Returns {snapshot_map, status_map}
  defp fetch_snapshot_and_status(region_key) do
    snap =
      cond do
        Code.ensure_loaded?(Introspect) and function_exported?(Introspect, :region_state, 1) ->
          case safe_call(fn -> Introspect.region_state(region_key) end) do
            {:ok, s} -> normalize_map(s)
            _ -> fallback_snapshot(region_key)
          end

        true ->
          fallback_snapshot(region_key)
      end

    {snap, get_region_status(region_key)}
  end

  defp fallback_snapshot(region_key) do
    cond do
      Code.ensure_loaded?(Introspect) and function_exported?(Introspect, :snapshot, 0) ->
        case safe_call(fn -> Introspect.snapshot() end) do
          {:ok, s} ->
            s = normalize_map(s)
            regs = s[:regions] || s["regions"] || %{}

            cond do
              is_map(regs) and map_size(regs) > 0 ->
                Map.get(regs, region_key) || Map.get(regs, to_string(region_key)) || %{}

              true ->
                direct_module_snapshot(region_key)
            end

          _ ->
            direct_module_snapshot(region_key)
        end

      true ->
        direct_module_snapshot(region_key)
    end
  end

  defp direct_module_snapshot(region_key) do
    mod = RegionRegistry.process_for(region_key) |> call_target()

    cond do
      is_nil(mod) ->
        %{}

      Code.ensure_loaded?(mod) and function_exported?(mod, :get_state, 0) ->
        case safe_call(fn -> mod.get_state() end) do
          {:ok, state} -> normalize_map(state)
          _ -> %{}
        end

      true ->
        %{}
    end
  end

  defp get_region_status(region_key) do
    mod = RegionRegistry.process_for(region_key) |> call_target()
    try_direct_status(mod, region_key)
  end

  # Builds a robust status map using (in order):
  # 1) GenServer.call(mod, :status, 150) if implemented
  # 2) mod.status/0 if implemented
  # 3) Process info fallback (pid, queue, current) so UI always has something
  defp try_direct_status(nil, _rk), do: %{}

  defp try_direct_status(mod, region_key) do
    base = base_status_from_process(mod, region_key)

    gs =
      case safe_call(fn -> GenServer.call(mod, :status, 150) end) do
        {:ok, s} when is_map(s) -> s
        _ ->
          if Code.ensure_loaded?(mod) and function_exported?(mod, :status, 0) do
            case safe_call(fn -> mod.status() end) do
              {:ok, s} when is_map(s) -> s
              _ -> %{}
            end
          else
            %{}
          end
      end

    Map.merge(base, gs)
  end

  defp base_status_from_process(mod, region_key) do
    {pid, reg_name} = pid_of(mod)

    case pid do
      pid when is_pid(pid) ->
        info = :erlang.process_info(pid, [:message_queue_len, :current_function])

        %{
          region: region_key,
          status: :up,
          pid: pid,
          registered_name: reg_name,
          queue: info[:message_queue_len] || 0,
          current: info[:current_function] || :idle
        }

      _ ->
        %{
          region: region_key,
          status: :down,
          pid: nil,
          queue: 0,
          current: :idle
        }
    end
  end

  defp pid_of(mod) when is_atom(mod) do
    cond do
      function_exported?(mod, :name, 0) and is_pid(Process.whereis(mod.name())) ->
        {Process.whereis(mod.name()), mod.name()}

      is_pid(Process.whereis(mod)) ->
        {Process.whereis(mod), mod}

      true ->
        {nil, nil}
    end
  end

  # Resolve misnamed/mis-cased module atoms from registries into the real target.
  # Today we fix Brain.Lifg → Brain.LIFG; add more here if needed.
  defp call_target(mod) when is_atom(mod) do
    cond do
      mod == Brain.Lifg and Code.ensure_loaded?(Brain.LIFG) -> Brain.LIFG
      true -> mod
    end
  end

  defp safe_call(fun) when is_function(fun, 0) do
    try do
      {:ok, fun.()}
    rescue
      e ->
        Logger.debug("BrainLive safe_call error: #{inspect(e)}")
        :error
    catch
      _ -> :error
    end
  end

  defp normalize_map(%{} = m), do: m
  defp normalize_map(other), do: %{value: other}

  # ---- All-regions aggregation ----------------------------------------------

  defp collect_all_region_status do
    RegionRegistry.keys()
    |> Enum.map(fn key ->
      mod = RegionRegistry.process_for(key) |> call_target()
      {key, status_of(key, mod)}
    end)
    |> Enum.into(%{})
  end

  defp status_of(key, mod) when is_atom(mod) or is_nil(mod) do
    mod = call_target(mod)
    s = try_direct_status(mod, key)
    {pid, _} = if mod, do: pid_of(mod), else: {nil, nil}

    s
    |> Map.put_new(:region, key)
    |> Map.put_new(:pid, pid)
    |> Map.put_new(:status, s[:status] || s["status"] || (if pid, do: :up, else: :down))
  end

  # ---------------------------------------------------------------------------
  # Render helpers (panels)
  # ---------------------------------------------------------------------------

  # --- Selected region status (uses :status map) -----------------------------

  attr :selected, :atom
  attr :status, :map, default: %{}
  defp selected_region_status(assigns) do
    ~H"""
    <div class="mt-6">
      <div class="text-sm font-semibold text-gray-700 dark:text-gray-200 mb-2">
        Selected Region — Status (<%= @selected %>)
      </div>

      <div class="rounded-xl border border-gray-200 dark:border-gray-700 p-4 bg-white/75 dark:bg-neutral-900/70">
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4 text-sm">
          <div>
            <div class="font-medium mb-1">Mode & Thresholds</div>
            <div>pMTG mode: <%= h(status_val(@status, :pmtg_mode)) %></div>
            <div>pMTG window_keep: <%= h(status_val(@status, :pmtg_window_keep)) %></div>
            <div>scores: <%= h(status_val(@status, :scores)) %></div>
            <div>margin_threshold: <%= h(status_val(@status, :margin_threshold)) %></div>
            <div>min_margin: <%= h(status_val(@status, :min_margin)) %></div>
            <div>acc_conflict_tau: <%= h(status_val(@status, :acc_conflict_tau)) %></div>
            <div>mwe_fallback: <%= h(status_val(@status, :mwe_fallback)) %></div>
          </div>

          <div>
            <div class="font-medium mb-1">Weights</div>
            <%= case status_val(@status, :weights) do %>
              <% %{} = w -> %>
                <ul class="space-y-0.5">
                  <%= for {k, v} <- w do %>
                    <li><%= to_string(k) %>: <%= h(v) %></li>
                  <% end %>
                </ul>
              <% _ -> %>
                <div class="text-gray-500 dark:text-gray-400">—</div>
            <% end %>
          </div>

          <div>
            <div class="font-medium mb-1">Process</div>
            <div>pid: <%= inspect(@status[:pid] || @status["pid"]) %></div>
            <div>queue: <%= h(@status[:queue] || @status["queue"]) %></div>
            <div>current: <%= h(@status[:current] || @status["current"]) %></div>
            <%= if rn = (@status[:registered_name] || @status["registered_name"]) do %>
              <div>registered_name: <%= h(rn) %></div>
            <% end %>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp status_val(m, k) when is_map(m), do: Map.get(m, k) || Map.get(m, to_string(k))
  defp status_val(_, _), do: nil

  # --- All-regions status grid ----------------------------------------------

  attr :all_status, :map, default: %{}
  defp regions_status_grid(assigns) do
    ~H"""
    <div class="mt-6">
      <div class="text-sm font-semibold text-gray-700 dark:text-gray-200 mb-2">
        All Regions — Live Status
      </div>
      <div class="grid grid-cols-1 md:grid-cols-3 gap-3">
        <%= for {k, s} <- @all_status do %>
          <div class="rounded-xl border border-gray-200 dark:border-gray-700 p-3 bg-white/70 dark:bg-neutral-900/70">
            <div class="flex items-center justify-between">
              <div class="text-sm font-medium"><%= RegionRegistry.label_for(k) %></div>
              <span class={"text-xs px-2 py-0.5 rounded-full " <> status_badge_class(s[:status])}>
                <%= (s[:status] || :unknown) |> to_string() %>
              </span>
            </div>
            <div class="mt-2 text-xs text-gray-600 dark:text-gray-400">
              <div>pid: <%= inspect(s[:pid]) %></div>
              <%= if is_number(s[:queue] || s["queue"]) do %>
                <div>queue: <%= s[:queue] || s["queue"] %></div>
              <% end %>
              <%= if s[:current] || s["current"] do %>
                <div>current: <%= h(s[:current] || s["current"]) %></div>
              <% end %>
              <%= if s[:store] && is_map(s[:store]) do %>
                <div class="mt-1">
                  store: size=<%= s[:store][:size] || s[:store]["size"] || 0 %>,
                  pending=<%= s[:store][:pending] || s[:store]["pending"] || 0 %>
                </div>
              <% end %>
            </div>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  defp status_badge_class(:down),
    do: "bg-red-100 text-red-700 dark:bg-red-900/30 dark:text-red-300"

  defp status_badge_class(:unknown),
    do: "bg-gray-100 text-gray-700 dark:bg-gray-800 dark:text-gray-300"

  defp status_badge_class(_),
    do: "bg-emerald-100 text-emerald-700 dark:bg-emerald-900/30 dark:text-emerald-300"

  # ---- HTML-safe formatter for odd values -----------------------------------

  # Tuples/PIDs/Maps/Lists → inspect/1 so HEEx won't choke.
  defp h(v) when is_tuple(v), do: inspect(v)
  defp h(v) when is_pid(v),   do: inspect(v)
  defp h(v) when is_map(v),   do: inspect(v)
  defp h(v) when is_list(v) and not is_binary(v), do: inspect(v)
  defp h(v), do: v

  # ---------------------------------------------------------------------------
  # Intent helpers (normalize + fallback from brain snapshot/state)
  # ---------------------------------------------------------------------------

  defp maybe_seed_intent_from_brain(socket) do
    cur = socket.assigns[:intent] || %{}

    if map_size(cur) > 0 do
      socket
    else
      case fetch_global_intent() do
        nil     -> socket
        intent  -> assign(socket, :intent, stamp_intent(intent, "snapshot"))
      end
    end
  end

  # Try Introspect.snapshot/0 first, else Brain.get_state/0
  defp fetch_global_intent do
    intent =
      cond do
        Code.ensure_loaded?(Introspect) and function_exported?(Introspect, :snapshot, 0) ->
          case safe_call(fn -> Introspect.snapshot() end) do
            {:ok, snap} -> extract_intent_any(snap)
            _ -> nil
          end

        true ->
          nil
      end

    case intent do
      nil ->
        if Code.ensure_loaded?(Brain) and function_exported?(Brain, :get_state, 0) do
          case safe_call(fn -> GenServer.call(Brain, :snapshot) end) do
            {:ok, st} -> extract_intent_any(st)
            _ -> nil
          end
        else
          nil
        end

      other ->
        other
    end
  end

  # Extracts intent from many possible shapes/locations
  defp extract_intent_any(nil), do: nil

  defp extract_intent_any(%{} = m) do
    direct = mget(m, :intent)

    nested =
      mget_in(m, [:core, :intent]) ||
      mget_in(m, [:si, :intent]) ||
      mget_in(m, [:semantic, :intent]) ||
      mget_in(m, [:latest, :intent]) ||
      mget_in(m, [:brain, :intent])

    case normalize_intent(direct || nested, "state") do
      %{} = i -> i
      _ -> nil
    end
  end

  defp extract_intent_any(v) when is_binary(v), do: normalize_intent(v, "state")
  defp extract_intent_any(_), do: nil

  # Normalizes intent payloads into a consistent HUD-friendly map
  defp normalize_intent(nil, _src), do: %{}

  defp normalize_intent(v, src) when is_binary(v),
    do: %{label: v, intent: v, source: src, updated_at: now_ms()}

  defp normalize_intent(%{} = m, src) do
    label = mget(m, :label) || mget(m, :intent) || mget(m, :name) || mget(m, :type)
    kw    = mget(m, :keyword) || mget(m, :key)
    conf  = mget(m, :confidence) || mget(m, :score) || mget(m, :prob)
    src0  = mget(m, :source) || src

    base = %{
      label: label,
      intent: label,
      keyword: kw,
      confidence: conf,
      source: src0,
      updated_at: now_ms()
    }

    # Merge any extra fields the producer might have provided
    Map.merge(base, m)
  end

  defp normalize_intent(v, src) when is_atom(v),
    do: normalize_intent(Atom.to_string(v), src)

  defp normalize_intent(v, src),
    do: normalize_intent(to_string(v), src)

  defp stamp_intent(%{} = m, src) do
    m
    |> Map.put_new(:source, src)
    |> Map.put(:updated_at, now_ms())
  end

  # Mixed atom/string key getters
  defp mget(m, k) when is_map(m), do: Map.get(m, k) || Map.get(m, to_string(k))
  defp mget(_, _), do: nil

  defp mget_in(m, [k | rest]) when is_map(m) do
    case mget(m, k) do
      %{} = next -> mget_in(next, rest)
      other -> other
    end
  end

  defp mget_in(_, _), do: nil

  defp now_ms, do: System.system_time(:millisecond)
end

