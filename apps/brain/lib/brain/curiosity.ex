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

    {probe, base_score} = build_probe(state, opts)

    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: base_score},
      %{probe: probe}
    )

    {:noreply,
     state
     |> bump_seq()
     |> Map.put(:last_probe_ms, now_ms)}
  end

  # ── GenServer infos ────────────────────────────────────────────────────────

  @impl GenServer
  def handle_info(:tick, %{} = state) do
    {probe, base_score} = build_probe(state, %{})

    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: base_score},
      %{probe: probe}
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

    base =
      case get_opt(opts, :score, 0.55) do
        v when is_number(v) -> clamp01(v)
        _ -> 0.55
      end

    raw_id = get_opt(opts, :id, "curiosity|probe|alpha_zero") |> to_string()
    id = ensure_probe_id(raw_id, seq)

    source = get_opt(opts, :source, :curiosity)
    reason = get_opt(opts, :reason, :curiosity)
    seed   = get_opt(opts, :seed, nil)

    probe = %{
      id: id,
      source: source,
      reason: reason,
      seed: seed,
      score: base
    }

    {probe, base}
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

  defp normalize_opts(opts) when is_list(opts) do
    if Keyword.keyword?(opts), do: opts, else: []
  end

  defp normalize_opts(%{} = opts), do: Map.to_list(opts)
  defp normalize_opts(_), do: []
end

