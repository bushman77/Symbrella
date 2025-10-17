# lib/curiosity.ex
defmodule Curiosity do
  @moduledoc """
  Curiosity — minimal, self-contained background worker.

  Phase A goals:
  • Gentle periodic tick (default: every 5 minutes).
  • Compute an LC-like gain g ∈ [0,1] from memory & running task load.
  • Emit telemetry on each tick; back off when memory crosses a soft ceiling.
  • Publish a lightweight **proposal** event that brain regions may subscribe to.
  • Stay standalone (no Core/Brain deps). Easy to wire into Symbrella.Application.

  Telemetry events:
  • [:curiosity, :tick] — measurements: duration_ms, mem_bytes, gain, tasks_started, next_in_ms
  • [:curiosity, :backoff] — metadata: reason, mem_bytes, mem_soft_bytes
  • [:curiosity, :proposal] — measurements: gain, mem_bytes, tasks_running, score, next_in_ms
                               metadata: %{probe: %{id, source, kind, score, meta}, reason, v}

  Public API:
  • start_link/1, child_spec/1
  • snapshot/0 — returns current state (map)
  • nudge/0 — request an immediate tick (does not block)
  """

  use GenServer
  require Logger

  @tick :curiosity_tick

  # ---------- Defaults (override via child opts) ----------
  @default_interval_ms     300_000                    # 5 minutes
  @default_max_concurrency 2                          # reserved for future probes
  @default_mem_soft_bytes  350 * 1024 * 1024          # 350 MB soft ceiling
  @default_jitter_ms       15_000                     # +/- jitter on each schedule
  @default_cooldown_factor 1.5                        # backoff multiplier when hot
  @default_min_interval_ms 60_000                     # never faster than 1 minute
  @default_max_interval_ms 900_000                    # never slower than 15 minutes

  @type state :: %{
          interval_ms: non_neg_integer(),
          next_interval_ms: non_neg_integer(),
          min_interval_ms: non_neg_integer(),
          max_interval_ms: non_neg_integer(),
          jitter_ms: non_neg_integer(),
          cooldown_factor: number(),
          max_concurrency: pos_integer(),
          mem_soft_bytes: pos_integer(),
          last_tick_at: integer() | nil
        }

  # ---------- Public ----------

  @doc "Start as a supervised worker."
  @spec start_link(Keyword.t()) :: GenServer.on_start()
  def start_link(opts \\ []),
    do: GenServer.start_link(__MODULE__, opts, name: Keyword.get(opts, :name, __MODULE__))

  @doc "Standard OTP child spec."
  @spec child_spec(Keyword.t()) :: Supervisor.child_spec()
  def child_spec(opts),
    do: %{id: __MODULE__, start: {__MODULE__, :start_link, [opts]}, type: :worker, restart: :permanent, shutdown: 5_000}

  @doc "Light snapshot of internal counters."
  @spec snapshot(server :: GenServer.server()) :: state
  def snapshot(server \\ __MODULE__), do: GenServer.call(server, :snapshot)

  @doc "Request an immediate tick (out-of-band)."
  @spec nudge(server :: GenServer.server()) :: :ok
  def nudge(server \\ __MODULE__), do: GenServer.cast(server, :nudge)

  # ---------- GenServer ----------

  @impl GenServer
  def init(opts) do
    interval_ms   = Keyword.get(opts, :interval_ms, @default_interval_ms)
    max_conc      = Keyword.get(opts, :max_concurrency, @default_max_concurrency)
    mem_soft      = Keyword.get(opts, :mem_soft_bytes, @default_mem_soft_bytes)
    jitter_ms     = Keyword.get(opts, :jitter_ms, @default_jitter_ms)
    cooldown      = Keyword.get(opts, :cooldown_factor, @default_cooldown_factor)
    min_int       = Keyword.get(opts, :min_interval_ms, @default_min_interval_ms)
    max_int       = Keyword.get(opts, :max_interval_ms, @default_max_interval_ms)

    # Local, bounded task pool for future probes
    {:ok, _pid} =
      Task.Supervisor.start_link(name: task_sup(), max_children: max_conc, restart: :transient)

    st = %{
      interval_ms: clamp_interval(interval_ms, min_int, max_int),
      next_interval_ms: clamp_interval(interval_ms, min_int, max_int),
      min_interval_ms: min_int,
      max_interval_ms: max_int,
      jitter_ms: max(jitter_ms, 0),
      cooldown_factor: max(cooldown, 1.0),
      max_concurrency: max_conc,
      mem_soft_bytes: mem_soft,
      last_tick_at: nil
    }

    # Kick the first tick
    send(self(), @tick)
    {:ok, st}
  end

  @impl GenServer
  def handle_call(:snapshot, _from, st), do: {:reply, st, st}

  @impl GenServer
  def handle_cast(:nudge, st) do
    Process.send_after(self(), @tick, 0)
    {:noreply, st}
  end

  @impl GenServer
  def handle_info(@tick, st) do
    t0 = System.monotonic_time()

    # 1) LC-like gain from memory + (future) running tasks
    mem_total = :erlang.memory(:total)
    running   = running_tasks()
    gain      = lc_gain(mem_total, st.mem_soft_bytes, running)

    # 2) (Future) BG gate & probes — Phase A only emits telemetry + proposal
    tasks_started = 0
    duration_ms = delta_ms(t0)

    # Emit periodic tick
    :telemetry.execute([:curiosity, :tick], %{count: 1}, %{
      duration_ms: duration_ms,
      mem_bytes: mem_total,
      gain: gain,
      tasks_started: tasks_started,
      next_in_ms: st.next_interval_ms
    })

    # ---- NEW: Proposal broadcast (Route A — no Brain deps) ----
    idle_bonus = 0.10
    score = clamp01(0.6 * gain + 0.4 * idle_bonus)

    probe = %{
      id: "curiosity|probe|#{now_ms()}",
      source: :curiosity,
      kind: :idle_probe,
      score: score,
      meta: %{last_tick_at: st.last_tick_at, jitter_ms: st.jitter_ms}
    }

    :telemetry.execute(
      [:curiosity, :proposal],
      %{
        gain: gain,
        mem_bytes: mem_total,
        tasks_running: running,
        score: score,
        next_in_ms: st.next_interval_ms
      },
      %{probe: probe, reason: :idle, v: 1}
    )
    # -----------------------------------------------------------

    # Adaptive schedule: back off when memory is hot, relax when cool
    next_interval_ms =
      if mem_total >= st.mem_soft_bytes do
        :telemetry.execute([:curiosity, :backoff], %{count: 1}, %{
          reason: :memory_high,
          mem_bytes: mem_total,
          mem_soft_bytes: st.mem_soft_bytes
        })

        grow_with_cooldown(st.next_interval_ms, st.cooldown_factor, st.max_interval_ms)
      else
        relax_toward(st.next_interval_ms, st.interval_ms, st.cooldown_factor, st.min_interval_ms)
      end
      |> add_jitter(st.jitter_ms)
      |> clamp_interval(st.min_interval_ms, st.max_interval_ms)

    # Re-schedule next tick
    Process.send_after(self(), @tick, next_interval_ms)

    {:noreply, %{st | last_tick_at: now_ms(), next_interval_ms: next_interval_ms}}
  end

  # ---------- Internal helpers ----------

  defp task_sup, do: __MODULE__.TaskSup

  defp running_tasks() do
    case Process.whereis(task_sup()) do
      nil -> 0
      pid ->
        %{active: active} = Supervisor.count_children(pid)
        active
    end
  end

  # LC gain: 1.0 when memory is cool & no tasks; tapers toward 0 as load rises.
  @spec lc_gain(non_neg_integer(), pos_integer(), non_neg_integer()) :: float()
  defp lc_gain(mem_bytes, soft_ceiling_bytes, running_tasks) do
    mem_ratio =
      mem_bytes
      |> Kernel./(soft_ceiling_bytes |> max(1))
      |> min(2.0)
      |> max(0.0)

    load_penalty = min(running_tasks * 0.15, 0.6)
    raw = 1.0 - (0.7 * min(mem_ratio, 1.0)) - load_penalty
    clamp01(raw)
  end

  defp grow_with_cooldown(current_ms, factor, max_ms) do
    round(min(current_ms * factor, max_ms))
  end

  defp relax_toward(current_ms, target_ms, factor, min_ms) do
    # Move partway back toward the base interval to avoid sudden jumps
    step = max(factor - 1.0, 0.5) # gentler than grow
    new_ms = current_ms - round((current_ms - target_ms) / (1.0 + step))
    max(new_ms, min_ms)
  end

  defp add_jitter(ms, 0), do: ms
  defp add_jitter(ms, jitter) do
    # +/- jitter uniformly
    offset = :rand.uniform(2 * jitter + 1) - (jitter + 1)
    max(ms + offset, 0)
  end

  defp clamp_interval(ms, min_ms, max_ms), do: ms |> max(min_ms) |> min(max_ms)

  defp clamp01(x) when x < 0.0, do: 0.0
  defp clamp01(x) when x > 1.0, do: 1.0
  defp clamp01(x), do: x

  defp now_ms, do: System.system_time(:millisecond)

  defp delta_ms(t0native) do
    System.convert_time_unit(System.monotonic_time() - t0native, :native, :millisecond)
  end

  @doc "Simple sanity check."
  @spec hello() :: :world
  def hello, do: :world
end

