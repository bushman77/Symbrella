# apps/symbrella_web/lib/symbrella_web/live/brain_live.ex
defmodule SymbrellaWeb.BrainLive do
  @moduledoc """
  LiveView for the Brain dashboard.
  All page structure is delegated to `SymbrellaWeb.BrainHTML`.
  Mood telemetry + compact HUD are handled by `SymbrellaWeb.BrainLive.MoodHud`.

  • Safe defaults so first render never crashes (even if upstream is quiet)
  • Subscribes to Brain.Bus for blackboard + HUD topics (clock/intent/mood/lifg/wm)
  • Mood telemetry attach/detach delegated to MoodHud
  • Loads inline brain SVG from priv/static/images/brain.svg when available
  • ✅ Periodic, defensive region snapshot + status refresh
  • ✅ Selected-region status panel (prefers GenServer.call(mod, :status))
  • ✅ All-regions status grid (fully registry-driven, no hard-coding)
  • ✅ Region Summary is now registry-driven (RegionRegistry.defn/1 → assigns.region)
  • ✅ Blackboard feed + WM panel (filterable + tolerant of shape drift)
  """

  use SymbrellaWeb, :live_view
  require Logger

  alias SymbrellaWeb.BrainHTML
  alias SymbrellaWeb.Region.Registry, as: RegionRegistry
  alias SymbrellaWeb.BrainLive.MoodHud
  alias Brain.Bus
  # Optional (used if present):
  alias Brain.Introspect

  import SymbrellaWeb.BrainLive.MoodHud, only: [mood_hud: 1]

  @blackboard_topic "brain:blackboard"
  @hud_topics ~w(brain:clock brain:intent brain:mood brain:lifg brain:wm)
  @refresh_ms 500

  @impl true
  def mount(_params, _session, socket) do
    _ = ensure_optional(Brain.Cerebellum)
    _ = ensure_optional(Brain.LIFG)

    selected0 = default_selected()

    socket =
      socket
      |> assign_new(:selected, fn -> selected0 end)
      |> assign_new(:regions, fn -> RegionRegistry.keys() end)
      |> assign_new(:region, fn -> region_meta_for(selected0) end)
      |> assign_new(:module_info, fn -> %{} end)
      |> assign_new(:intent, fn -> %{} end)
      |> assign_new(:snapshot, fn -> nil end)
      |> assign_new(:lifg_last, fn -> %{} end)
      |> assign_new(:hippo, fn -> %{window: []} end)
      |> assign_new(:hippo_metrics, fn -> %{} end)
      |> assign_new(:region_state, fn -> %{workspace: [], snapshot: %{}} end)
      |> assign_new(:region_status, fn -> %{} end)
      |> assign_new(:auto, fn -> false end)
      |> assign_new(:clock, fn -> %{} end)
      |> assign_new(:svg_base, fn -> load_brain_svg() end)
      |> assign_new(:bb_filter, fn -> "" end)
      |> assign_new(:wm, fn -> %{} end)
      |> MoodHud.seed()
      |> assign(:all_status, collect_all_region_status())
      |> maybe_seed_intent_from_brain()
      |> refresh_selected()

    socket =
      if connected?(socket) do
        :ok = Bus.subscribe(@blackboard_topic)
        Enum.each(@hud_topics, &Bus.subscribe/1)
        :timer.send_interval(@refresh_ms, :refresh_selected)
        MoodHud.attach(socket)
      else
        socket
      end

    {:ok, socket}
  end

  @impl true
  def terminate(_reason, socket) do
    _ = MoodHud.detach(socket)
    :ok
  end

  @impl true
  def render(assigns) do
    assigns =
      if Map.has_key?(assigns, :mood),
        do: assigns,
        else: Map.put(assigns, :mood, MoodHud.defaults())

    ~H"""
    {BrainHTML.brain(assigns)}

    <!-- Compact Mood HUD overlay (extracted to MoodHud) -->
    <.mood_hud mood={@mood} />

    <.selected_region_status selected={@selected} status={@region_status} />
    <.regions_status_grid all_status={@all_status} />
    """
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
        nil ->
          socket

        r when is_binary(r) ->
          sel =
            try do
              String.to_existing_atom(r)
            rescue
              ArgumentError -> socket.assigns.selected
            end

          if RegionRegistry.available?(sel), do: assign(socket, :selected, sel), else: socket
      end

    send(self(), :refresh_selected)
    {:noreply, socket}
  end

  @impl true
  def handle_event("select-region", params, socket) do
    r = Map.get(params, "region") || Map.get(params, "value") || Map.get(params, "r")

    sel =
      cond do
        is_atom(r) ->
          r

        is_binary(r) ->
          try do
            String.to_existing_atom(r)
          rescue
            ArgumentError -> socket.assigns.selected
          end

        true ->
          socket.assigns.selected
      end

    send(self(), :refresh_selected)
    {:noreply, assign(socket, :selected, sel)}
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    send(self(), :refresh_selected)
    {:noreply, socket}
  end

  # Blackboard UI knobs
  @impl true
  def handle_event("bb_filter", %{"q" => q}, socket) when is_binary(q) do
    {:noreply, assign(socket, :bb_filter, q)}
  end

  def handle_event("bb_filter", _params, socket), do: {:noreply, socket}

  @impl true
  def handle_event("bb_clear", _params, socket) do
    {:noreply,
     update(socket, :region_state, fn st ->
       st = st || %{}
       st |> Map.put(:workspace, [])
     end)}
  end

  # ---------------------------------------------------------------------------
  # Incoming telemetry + PubSub
  # ---------------------------------------------------------------------------

  @impl true
  def handle_info(:refresh_selected, socket) do
    {:noreply, refresh_selected(socket)}
  end

  # Mood: delegate to MoodHud
  @impl true
  def handle_info({:mood_event, _, _} = msg, socket), do: MoodHud.on_info(msg, socket)

  @impl true
  def handle_info({:mood_update, _, _} = msg, socket), do: MoodHud.on_info(msg, socket)

  @impl true
  def handle_info({:mood, _} = msg, socket), do: MoodHud.on_info(msg, socket)

  # Blackboard / bus
  @impl true
  def handle_info({:blackboard, env}, socket), do: handle_blackboard(env, socket)

  @impl true
  def handle_info({:brain_bus, env}, socket), do: handle_blackboard(env, socket)

  # HUD topics
  @impl true
  def handle_info({:clock, m}, socket), do: {:noreply, assign(socket, :clock, m || %{})}

  @impl true
  def handle_info({:intent, m}, socket) do
    intent = normalize_intent(m, "bus")
    {:noreply, assign(socket, :intent, intent)}
  end

  @impl true
  def handle_info({:lifg_update, m}, socket) do
    lifg_last = m || %{}

    intent_from_lifg =
      lifg_last
      |> extract_intent_any()

    socket =
      socket
      |> assign(:lifg_last, lifg_last)
      |> maybe_assign_intent(intent_from_lifg, "lifg")
      |> maybe_assign_wm(extract_wm_any(lifg_last), "lifg")

    {:noreply, socket}
  end

  @impl true
  def handle_info({:wm, m}, socket) do
    {:noreply, maybe_assign_wm(socket, m, "bus")}
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

  # ---- Region meta (registry-driven) ----------------------------------------

  defp region_meta_for(region_key) when is_atom(region_key) do
    base =
      try do
        defn = RegionRegistry.defn(region_key)

        cond do
          is_map(defn) and Map.has_key?(defn, :__struct__) -> Map.from_struct(defn)
          is_map(defn) -> defn
          true -> %{}
        end
      rescue
        _ -> %{}
      end

    title =
      mget(base, :title) ||
        mget(base, :label) ||
        RegionRegistry.label_for(region_key) ||
        region_key |> Atom.to_string() |> String.upcase()

    %{
      key: region_key,
      title: title,
      subtitle: mget(base, :subtitle) || mget(base, :summary_title),
      desc: mget(base, :desc) || mget(base, :summary) || mget(base, :description),
      modules: listify(mget(base, :modules) || mget(base, :mods) || mget(base, :module)),
      telemetry: listify(mget(base, :telemetry) || mget(base, :telem)),
      config_examples:
        listify(mget(base, :config_examples) || mget(base, :config) || mget(base, :confs))
    }
  end

  defp region_meta_for(_), do: %{}

  defp mget(m, k) when is_map(m), do: Map.get(m, k) || Map.get(m, to_string(k))
  defp mget(_, _), do: nil

  defp listify(nil), do: []
  defp listify(v) when is_list(v), do: v
  defp listify(v), do: [v]

  # ---- Region fetchers ------------------------------------------------------

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

  defp try_direct_status(nil, _rk), do: %{}

  defp try_direct_status(mod, region_key) do
    base = base_status_from_process(mod, region_key)

    gs =
      case safe_call(fn -> GenServer.call(mod, :status, 150) end) do
        {:ok, s} when is_map(s) ->
          s

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
    |> Map.put_new(:status, s[:status] || s["status"] || if(pid, do: :up, else: :down))
  end

  # ---------------------------------------------------------------------------
  # Render helpers (panels)
  # ---------------------------------------------------------------------------

  attr :selected, :atom
  attr :status, :map, default: %{}

  defp selected_region_status(assigns) do
    ~H"""
    <div class="mt-6">
      <div class="text-sm font-semibold text-gray-700 dark:text-gray-200 mb-2">
        Selected Region — Status ({@selected})
      </div>

      <div class="rounded-xl border border-gray-200 dark:border-gray-700 p-4 bg-white/75 dark:bg-neutral-900/70">
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4 text-sm">
          <div>
            <div class="font-medium mb-1">Mode & Thresholds</div>
            <div>pMTG mode: {h(status_val(@status, :pmtg_mode))}</div>
            <div>pMTG window_keep: {h(status_val(@status, :pmtg_window_keep))}</div>
            <div>scores: {h(status_val(@status, :scores))}</div>
            <div>margin_threshold: {h(status_val(@status, :margin_threshold))}</div>
            <div>min_margin: {h(status_val(@status, :min_margin))}</div>
            <div>acc_conflict_tau: {h(status_val(@status, :acc_conflict_tau))}</div>
            <div>mwe_fallback: {h(status_val(@status, :mwe_fallback))}</div>
          </div>

          <div>
            <div class="font-medium mb-1">Weights</div>
            <%= case status_val(@status, :weights) do %>
              <% %{} = w -> %>
                <ul class="space-y-0.5">
                  <%= for {k, v} <- w do %>
                    <li>{to_string(k)}: {h(v)}</li>
                  <% end %>
                </ul>
              <% _ -> %>
                <div class="text-gray-500 dark:text-gray-400">—</div>
            <% end %>
          </div>

          <div>
            <div class="font-medium mb-1">Process</div>
            <div>pid: {inspect(@status[:pid] || @status["pid"])}</div>
            <div>queue: {h(@status[:queue] || @status["queue"])}</div>
            <div>current: {h(@status[:current] || @status["current"])}</div>
            <%= if rn = (@status[:registered_name] || @status["registered_name"]) do %>
              <div>registered_name: {h(rn)}</div>
            <% end %>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp status_val(m, k) when is_map(m), do: Map.get(m, k) || Map.get(m, to_string(k))
  defp status_val(_, _), do: nil

  defp refresh_selected(socket) do
    selected = socket.assigns[:selected] || default_selected()
    {snapshot, status} = fetch_snapshot_and_status(selected)

    socket
    |> assign(:snapshot, snapshot)
    |> assign(:region_status, status)
    |> assign(:all_status, collect_all_region_status())
    |> assign(:regions, RegionRegistry.keys())
    |> assign(:region, region_meta_for(selected))
    |> maybe_assign_wm(extract_wm_any(snapshot), "snapshot")
    |> update(:region_state, fn st ->
      st = st || %{}
      st |> Map.put(:snapshot, snapshot)
    end)
  end

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
              <div class="text-sm font-medium">{RegionRegistry.label_for(k)}</div>
              <span class={"text-xs px-2 py-0.5 rounded-full " <> status_badge_class(s[:status])}>
                {(s[:status] || :unknown) |> to_string()}
              </span>
            </div>
            <div class="mt-2 text-xs text-gray-600 dark:text-gray-400">
              <div>pid: {inspect(s[:pid])}</div>
              <%= if is_number(s[:queue] || s["queue"]) do %>
                <div>queue: {s[:queue] || s["queue"]}</div>
              <% end %>
              <%= if s[:current] || s["current"] do %>
                <div>current: {h(s[:current] || s["current"])}</div>
              <% end %>
              <%= if s[:store] && is_map(s[:store]) do %>
                <div class="mt-1">
                  store: size={s[:store][:size] || s[:store]["size"] || 0},
                  pending={s[:store][:pending] || s[:store]["pending"] || 0}
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

  defp h(v) when is_tuple(v), do: inspect(v)
  defp h(v) when is_pid(v), do: inspect(v)
  defp h(v) when is_map(v), do: inspect(v)
  defp h(v) when is_list(v) and not is_binary(v), do: inspect(v)
  defp h(v), do: v

  # ---------------------------------------------------------------------------
  # Intent helpers (unchanged)
  # ---------------------------------------------------------------------------

  defp maybe_seed_intent_from_brain(socket) do
    cur = socket.assigns[:intent] || %{}

    if map_size(cur) > 0 do
      socket
    else
      case fetch_global_intent() do
        nil -> socket
        intent -> assign(socket, :intent, stamp_intent(intent, "snapshot"))
      end
    end
  end

  defp fetch_global_intent do
    intent_from_introspect =
      if Code.ensure_loaded?(Introspect) and function_exported?(Introspect, :snapshot, 0) do
        case safe_call(fn -> Introspect.snapshot() end) do
          {:ok, snap} -> extract_intent_any(snap)
          _ -> nil
        end
      else
        nil
      end

    case intent_from_introspect do
      nil ->
        cond do
          Code.ensure_loaded?(Brain) and function_exported?(Brain, :snapshot, 0) ->
            case safe_call(fn -> Brain.snapshot() end) do
              {:ok, st} -> extract_intent_any(st)
              _ -> nil
            end

          true ->
            nil
        end

      other ->
        other
    end
  end

  defp extract_intent_any(nil), do: nil

  defp extract_intent_any(%{} = m) do
    direct = mget(m, :intent)

    nested =
      mget_in(m, [:core, :intent]) ||
        mget_in(m, [:si, :intent]) ||
        mget_in(m, [:semantic, :intent]) ||
        mget_in(m, [:latest, :intent]) ||
        mget_in(m, [:brain, :intent])

    last =
      mget(m, :last_intent) ||
        mget_in(m, [:brain, :last_intent]) ||
        mget_in(m, [:latest, :last_intent])

    candidate = direct || nested || last

    case normalize_intent(candidate, "state") do
      %{} = i -> i
      _ -> nil
    end
  end

  defp extract_intent_any(v) when is_binary(v), do: normalize_intent(v, "state")
  defp extract_intent_any(_), do: nil

  defp normalize_intent(nil, _src), do: %{}

  defp normalize_intent(v, src) when is_binary(v),
    do: %{label: v, intent: v, source: src, updated_at: now_ms()}

  defp normalize_intent(%{} = m, src) do
    raw_label =
      mget(m, :label) ||
        mget(m, :intent) ||
        mget(m, :name) ||
        mget(m, :type)

    kw = mget(m, :keyword) || mget(m, :key)
    conf = mget(m, :confidence) || mget(m, :score) || mget(m, :prob)
    src0 = mget(m, :source) || src

    label_str =
      case raw_label do
        nil -> nil
        v when is_binary(v) -> v
        v when is_atom(v) -> Atom.to_string(v)
        v -> to_string(v)
      end

    base = %{
      label: label_str,
      intent: Map.get(m, :intent, raw_label),
      keyword: kw,
      confidence: conf,
      source: src0,
      updated_at: now_ms()
    }

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

  defp mget_in(m, [k | rest]) when is_map(m) do
    case mget(m, k) do
      %{} = next -> mget_in(next, rest)
      other -> other
    end
  end

  defp mget_in(_, _), do: nil

  defp now_ms, do: System.system_time(:millisecond)

  # ---------------------------------------------------------------------------
  # WM helpers
  # ---------------------------------------------------------------------------

  defp extract_wm_any(nil), do: nil

  defp extract_wm_any(%{} = m) do
    direct = mget(m, :wm) || mget(m, :working_memory)

    nested =
      mget_in(m, [:core, :wm]) ||
        mget_in(m, [:brain, :wm]) ||
        mget_in(m, [:latest, :wm]) ||
        mget_in(m, [:state, :wm]) ||
        mget_in(m, [:regions, :wm])

    cand = direct || nested

    case cand do
      nil -> nil
      %{} = mm -> mm
      other -> %{value: other}
    end
  end

  defp extract_wm_any(other), do: %{value: other}

  defp maybe_assign_wm(socket, wm, src) do
    case wm do
      nil -> socket
      %{} = m -> assign(socket, :wm, Map.merge(%{source: src, updated_at: now_ms()}, m))
      _ -> assign(socket, :wm, %{source: src, updated_at: now_ms(), value: wm})
    end
  end

  defp maybe_assign_intent(socket, nil, _src), do: socket

  defp maybe_assign_intent(socket, %{} = intent, src),
    do: assign(socket, :intent, stamp_intent(intent, src))

  defp maybe_assign_intent(socket, intent, src),
    do: assign(socket, :intent, stamp_intent(normalize_intent(intent, src), src))

  # ---------------------------------------------------------------------------
  # Blackboard handler (now wraps events with timestamp + tag)
  # ---------------------------------------------------------------------------

  defp handle_blackboard(env, socket) do
    socket =
      socket
      |> maybe_assign_intent(extract_intent_any(env), "blackboard")
      |> maybe_assign_wm(extract_wm_any(env), "blackboard")

    bb_event = wrap_bb_event(env)

    {:noreply,
     update(socket, :region_state, fn st ->
       st = st || %{}
       ws = [bb_event | st[:workspace] || []] |> Enum.take(100)
       Map.put(st, :workspace, ws)
     end)}
  end

  defp wrap_bb_event(env) do
    %{
      id: :erlang.unique_integer([:positive, :monotonic]),
      at_ms: now_ms(),
      tag: bb_tag(env),
      env: env
    }
  end

  defp bb_tag(%{} = env) do
    cond do
      is_atom(mget(env, :region)) -> mget(env, :region)
      is_binary(mget(env, :region)) -> mget(env, :region)
      is_atom(mget(env, :tag)) -> mget(env, :tag)
      is_binary(mget(env, :tag)) -> mget(env, :tag)
      not is_nil(mget(env, :lifg)) -> :lifg
      not is_nil(mget(env, :pmtg)) -> :pmtg
      not is_nil(mget(env, :hippocampus)) -> :hippocampus
      not is_nil(mget(env, :mood)) -> :mood
      not is_nil(mget(env, :wm)) or not is_nil(mget(env, :working_memory)) -> :wm
      not is_nil(mget(env, :intent)) -> :intent
      true -> :event
    end
  end

  defp bb_tag(_), do: :event
end
