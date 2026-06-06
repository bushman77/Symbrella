defmodule Brain.Curiosity do
  @moduledoc """
  Curiosity — internal probe generator for Thalamus/BG/WM.

  Responsibilities (Phase 0):

    * Provide a `nudge/1` API that emits `[:curiosity, :proposal]` telemetry.
    * Keep a simple, monotonic probe sequence counter for traceability.
    * Optionally support a periodic tick loop (disabled by default) via `:tick_ms`.
    * Expose a lightweight `status/1` snapshot for the Brain dashboard.

  Pipeline:

    Brain.Curiosity.nudge/1
      → [:curiosity, :proposal]
      → Brain.Thalamus (OFC blend + ACC brake + mood bias)
      → Brain.BasalGanglia.decide/4
      → WM insertion (via BG), visible in Brain.snapshot_wm/0.
  """

  use Brain, region: :curiosity
  require Logger

  # ── Public API ──────────────────────────────────────────────────────────────

  @doc """
  Nudge curiosity to emit a single probe.

  Options (keyword or map):

    * :score   — base score in [0,1] (default 0.55)
    * :id      — probe id or prefix (default "curiosity|probe|alpha_zero")
    * :source  — tag for the probe source (default :curiosity)
    * :reason  — reason tag (default :curiosity, used in WM payload)
    * :seed    — optional arbitrary value to stash in the probe (not used yet)
  """
  @spec nudge(Keyword.t() | map()) :: :ok
  def nudge(opts \\ []) when is_list(opts) or is_map(opts) do
    GenServer.cast(__MODULE__, {:nudge, opts})
  end

  @doc """
  Return a compact status snapshot for UI/diagnostics.
  """
  @spec status(server :: pid() | atom()) :: {:ok, map()} | {:error, term()}
  def status(server \\ __MODULE__) do
    GenServer.call(server, :status)
  end

  # ── Region lifecycle ────────────────────────────────────────────────────────

  @impl GenServer
  def init(opts) do
    opts_kw = normalize_opts(opts)

    tick_ms =
      get_opt(opts_kw, :tick_ms, Application.get_env(:brain, :curiosity_tick_ms, 0))

    # Store effective tick_ms back into opts so effective_tick_ms/1 is stable.
    opts_kw = Keyword.put_new(opts_kw, :tick_ms, tick_ms)

    state = %{
      region: :curiosity,
      opts: opts_kw,
      seq: 0,
      last_probe_ms: nil,
      auto_timer: schedule_tick(tick_ms)
    }

    {:ok, state}
  end

  @impl GenServer
  def terminate(_reason, state) do
    if ref = Map.get(state, :auto_timer) do
      Process.cancel_timer(ref)
    end

    :ok
  end

  # ── GenServer calls ────────────────────────────────────────────────────────

  @impl GenServer
  def handle_call(:status, _from, %{} = state) do
    snapshot = %{
      region: Map.get(state, :region, :curiosity),
      seq: Map.get(state, :seq, 0),
      last_probe_ms: Map.get(state, :last_probe_ms),
      opts: Map.get(state, :opts, [])
    }

    {:reply, {:ok, snapshot}, state}
  end

  @impl GenServer
  def handle_call(msg, from, %{} = state) do
    Logger.warning("[Curiosity] Unknown call: #{inspect(msg)} from #{inspect(from)}")
    {:reply, {:error, {:unknown_call, msg}}, state}
  end

  # ── GenServer casts ────────────────────────────────────────────────────────

  @impl GenServer
  def handle_cast({:nudge, opts_in}, %{} = state) do
    opts = normalize_opts(opts_in)
    now_ms = System.system_time(:millisecond)

    {probe, base_score, self_state} = build_probe(state, opts)

    :telemetry.execute(
      [:curiosity, :proposal],
      %{
        score: base_score,
        uncertainty: Map.get(self_state, :uncertainty, 0.0),
        novelty: Map.get(self_state, :novelty, 0.0)
      },
      %{probe: probe, reason: Map.get(self_state, :reason)}
    )

    {:noreply,
     state
     |> bump_seq()
     |> Map.put(:last_probe_ms, now_ms)}
  end

  # ── GenServer infos ────────────────────────────────────────────────────────

  @impl GenServer
  def handle_info(:tick, %{} = state) do
    {probe, base_score, self_state} = build_probe(state, %{})

    :telemetry.execute(
      [:curiosity, :proposal],
      %{
        score: base_score,
        uncertainty: Map.get(self_state, :uncertainty, 0.0),
        novelty: Map.get(self_state, :novelty, 0.0)
      },
      %{probe: probe, reason: Map.get(self_state, :reason)}
    )

    next_timer =
      state
      |> effective_tick_ms()
      |> schedule_tick()

    {:noreply,
     state
     |> bump_seq()
     |> Map.put(:last_probe_ms, System.system_time(:millisecond))
     |> Map.put(:auto_timer, next_timer)}
  end

  @impl GenServer
  def handle_info(_msg, %{} = state), do: {:noreply, state}

  # ── Probe construction ─────────────────────────────────────────────────────

  defp build_probe(state, opts) do
    seq = Map.get(state, :seq, 0)

    self_state = curiosity_self_state(opts)

    default_score = if has_opt?(opts, :self_state), do: :auto, else: 0.55

    base =
      case get_opt(opts, :score, default_score) do
        v when is_number(v) -> clamp01(v)
        _ -> curiosity_score(self_state)
      end

    raw_id = get_opt(opts, :id, "curiosity|probe|alpha_zero") |> to_string()
    id = ensure_probe_id(raw_id, seq)

    source = get_opt(opts, :source, :curiosity)
    reason = get_opt(opts, :reason, :curiosity)
    seed = get_opt(opts, :seed, nil)

    probe = %{
      id: id,
      source: source,
      reason: reason,
      seed: seed,
      score: base
    }

    {probe, base, self_state}
  end

  defp curiosity_self_state(opts) do
    case get_opt(opts, :self_state, nil) do
      %{} = state ->
        normalize_self_state(state)

      _ ->
        runtime_self_state()
    end
  end

  defp runtime_self_state do
    required = [Brain, Brain.Meta, Brain.MoodCore, Brain.SelfPortrait]

    if Enum.all?(required, &(Process.whereis(&1) != nil)) do
      Brain.Introspection.snapshot()
      |> normalize_self_state()
      |> Map.put_new(:reason, :runtime_self_state)
    else
      %{
        uncertainty: 0.5,
        novelty: 0.3,
        exploration: 0.4,
        dopamine: 0.4,
        reason: :default_self_state
      }
    end
  end

  defp normalize_self_state(%{} = state) do
    mood = map_get(state, :mood, %{})
    mood_map = map_get(mood, :mood, mood)
    levels = map_get(mood, :levels, %{})

    %{
      uncertainty: state |> map_get(:uncertainty, 0.5) |> number_or(0.5) |> clamp01(),
      novelty: state |> map_get(:novelty, inferred_novelty(state)) |> number_or(0.3) |> clamp01(),
      exploration: mood_map |> map_get(:exploration, 0.4) |> number_or(0.4) |> clamp01(),
      dopamine: levels |> map_get(:da, 0.4) |> number_or(0.4) |> clamp01(),
      reason: map_get(state, :reason, :self_state)
    }
  end

  defp curiosity_score(%{} = self_state) do
    uncertainty = Map.get(self_state, :uncertainty, 0.5)
    novelty = Map.get(self_state, :novelty, 0.3)
    exploration = Map.get(self_state, :exploration, 0.4)
    dopamine = Map.get(self_state, :dopamine, 0.4)

    clamp01(0.25 + uncertainty * 0.30 + novelty * 0.20 + exploration * 0.15 + dopamine * 0.10)
  end

  defp inferred_novelty(%{} = state) do
    recent_errors = state |> map_get(:recent_errors, []) |> List.wrap()
    active_goals = state |> map_get(:active_goals, []) |> List.wrap()

    clamp01(length(recent_errors) * 0.12 + length(active_goals) * 0.08)
  end

  defp ensure_probe_id(id, seq) do
    # If caller already gives a fully-qualified probe id, respect it.
    if String.contains?(id, "|probe|") do
      id
    else
      "#{id}|probe|#{seq}"
    end
  end

  defp bump_seq(state), do: Map.update(state, :seq, 1, &(&1 + 1))

  # ── Helpers ────────────────────────────────────────────────────────────────

  defp clamp01(x) when is_number(x) do
    x = x * 1.0
    max(0.0, min(1.0, x))
  end

  defp clamp01(_), do: 0.0

  defp schedule_tick(ms) when is_integer(ms) and ms > 0 do
    Process.send_after(self(), :tick, ms)
  end

  defp schedule_tick(_), do: nil

  defp effective_tick_ms(%{opts: opts}) do
    get_opt(opts, :tick_ms, Application.get_env(:brain, :curiosity_tick_ms, 0))
  end

  # Accepts keyword or map; normalize_opts ensures keyword in state
  defp get_opt(opts, key, default) when is_list(opts), do: Keyword.get(opts, key, default)
  defp get_opt(%{} = opts, key, default), do: Map.get(opts, key, default)
  defp get_opt(_opts, _key, default), do: default

  defp has_opt?(opts, key) when is_list(opts), do: Keyword.has_key?(opts, key)
  defp has_opt?(%{} = opts, key), do: Map.has_key?(opts, key) or Map.has_key?(opts, to_string(key))
  defp has_opt?(_opts, _key), do: false

  defp map_get(%{} = map, key, default) when is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_map, _key, default), do: default

  defp number_or(value, _default) when is_integer(value), do: value * 1.0
  defp number_or(value, _default) when is_float(value), do: value
  defp number_or(_value, default), do: default

  defp normalize_opts(opts) when is_list(opts) do
    if Keyword.keyword?(opts), do: opts, else: []
  end

  defp normalize_opts(%{} = opts), do: Map.to_list(opts)
  defp normalize_opts(_), do: []
end
