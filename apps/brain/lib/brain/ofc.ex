# apps/brain/lib/brain/ofc.ex
defmodule Brain.OFC do
  @moduledoc """
  OFC/vmPFC — valuation region for curiosity proposals.

  • Subscribes to `[:curiosity, :proposal]` (no compile-time deps on Curiosity).
  • Computes a quick expected-value-of-information (EVI) from:
      - `score` (proposal’s intrinsic value)
      - `gain` (LC-like arousal)
      - lightweight costs from `tasks_running` and memory pressure
  • Emits `[:brain, :ofc, :value]` with the computed value; **no gating or launching** here.

  Notes:
  - Keeps acyclic deps (db ← brain ← core ← web).
  - Uses `use Brain, region: :ofc` to remain a first-class region.
  - Maintains a simple EMA baseline for memory to normalize pressure across hosts.
  """

  use Brain, region: :ofc
  require Logger

  @handler_prefix "brain-ofc-curiosity-"
  @ema_alpha 0.10

  # ---- Region lifecycle ----

  @impl GenServer
  def init(opts) do
    state =
      %{
        region: :ofc,
        opts: Map.new(opts),
        stats: %{},
        # rolling baseline for mem_bytes (EMA)
        mem_baseline: nil
      }

    handler_id = unique_handler_id()

    :ok =
      :telemetry.attach(
        handler_id,
        [:curiosity, :proposal],
        &__MODULE__.on_curiosity/4,
        %{pid: self()}
      )

    {:ok, Map.put(state, :handler_id, handler_id)}
  end

  @impl GenServer
  def terminate(_reason, %{handler_id: id}) when is_binary(id) do
    :telemetry.detach(id)
    :ok
  end

  def terminate(_reason, _state), do: :ok

  # ---- Telemetry → GenServer bridge ----

  @doc false
  def on_curiosity(_event, measurements, metadata, %{pid: pid}) when is_pid(pid) do
    send(pid, {:curiosity_proposal, measurements, metadata})
  end

  def on_curiosity(_event, _meas, _meta, _cfg), do: :ok

  # ---- Region message handling ----

  @impl GenServer
  def handle_info({:curiosity_proposal, meas, meta}, state) do
    # Pull features (defensive to atom/string keys)
    gain          = get_num(meas, :gain, 0.0)
    score         = get_num(meas, :score, 0.0)
    mem_bytes     = get_num(meas, :mem_bytes, 0)
    tasks_running = get_num(meas, :tasks_running, 0)

    probe = meta_get(meta, :probe, %{})
    probe_id = to_string(Map.get(probe, :id) || Map.get(probe, "id") || "curiosity|probe|unknown")
    kind     = Map.get(probe, :kind)   || Map.get(probe, "kind")
    source   = Map.get(probe, :source) || Map.get(probe, "source")

    # Update EMA baseline for memory
    {baseline, state2} = update_mem_baseline(state, mem_bytes)

    # Compute lightweight costs
    load_penalty =
      tasks_running
      |> Kernel.*(0.10)     # each running task costs 0.10
      |> min(0.50)          # cap load penalty

    mem_ratio =
      if baseline && baseline > 0 do
        mem_bytes / baseline
      else
        1.0
      end
      |> min(2.0)
      |> max(0.0)

    # only penalize when above baseline; scaled gently
    memory_penalty =
      if mem_ratio > 1.0 do
        # up to +0.25 penalty at 2.0× baseline
        0.25 * min(mem_ratio - 1.0, 1.0)
      else
        0.0
      end

    value =
      0.5 * score +
      0.5 * gain -
      load_penalty -
      memory_penalty
      |> clamp01()

    :telemetry.execute(
      [:brain, :ofc, :value],
      %{
        value: value,
        gain: gain,
        score: score,
        mem_bytes: mem_bytes,
        tasks_running: tasks_running
      },
      %{
        probe_id: probe_id,
        kind: kind,
        source: source,
        mem_baseline: baseline,
        penalties: %{load: load_penalty, memory: memory_penalty},
        v: 1
      }
    )

    {:noreply, state2}
  end

  @impl GenServer
  def handle_info(_msg, state), do: {:noreply, state}

  # ---- Helpers ----

  defp unique_handler_id do
    @handler_prefix <>
      Integer.to_string(:erlang.unique_integer([:positive])) <>
      "-" <> Integer.to_string(System.system_time(:microsecond))
  end

  defp update_mem_baseline(%{mem_baseline: nil} = st, sample) when is_number(sample) and sample >= 0 do
    {sample, %{st | mem_baseline: sample}}
  end

  defp update_mem_baseline(%{mem_baseline: base} = st, sample) when is_number(sample) and sample >= 0 do
    ema = base + @ema_alpha * (sample - base)
    {ema, %{st | mem_baseline: ema}}
  end

  defp update_mem_baseline(st, _sample), do: {st.mem_baseline, st}

  defp get_num(map, key, default) do
    case {Map.get(map, key), Map.get(map, to_string(key))} do
      {v, _} when is_integer(v) -> v * 1.0
      {v, _} when is_float(v) -> v
      {_, v} when is_integer(v) -> v * 1.0
      {_, v} when is_float(v) -> v
      _ -> default * 1.0
    end
  end

  defp meta_get(meta, key, default \\ nil) do
    case {Map.get(meta, key), Map.get(meta, to_string(key))} do
      {nil, nil} -> default
      {v, _} -> v
      {_, v} -> v
    end
  end

  defp clamp01(x) when is_number(x), do: x |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0
end

