defmodule Brain.DLPFC do
  @moduledoc """
  DLPFC — execution/goal holder for tiny exploratory acts.

  Phase A (observer mode):
  • Subscribes to Thalamus decisions and Curiosity proposals.
  • Caches the most recent proposal (for future launch handoff / UI).
  • Emits lightweight telemetry.

  Phase B (after we patch Thalamus to stop launching):
  • Toggle `:act_on_thalamus` to true to have DLPFC call `Brain.focus/2`
    with a curiosity-labelled probe on `:allow | :boost`.

  Notes:
  - First-class region via `use Brain, region: :dlpfc`.
  - No compile-time deps on Curiosity; listens via telemetry.
  """

  use Brain, region: :dlpfc
  require Logger

  @t_handler "brain-dlpfc-thalamus-"
  @c_handler "brain-dlpfc-curiosity-"

  @typedoc "Runtime options controlling DLPFC behaviour."
  @type opts_t :: %{
          optional(:act_on_thalamus) => boolean()
        }

  # ─────────────── Public API ───────────────

  @doc """
  Update runtime options for DLPFC.

      :ok = Brain.DLPFC.set_opts(act_on_thalamus: true)
  """
  @spec set_opts(Keyword.t() | map()) :: :ok
  def set_opts(opts) when is_list(opts) or is_map(opts) do
    GenServer.cast(__MODULE__, {:set_opts, Map.new(opts)})
  end

  # ─────────────── Region lifecycle ───────────────

  @impl GenServer
  def init(opts) do
    state =
      %{
        region: :dlpfc,
        opts: Map.new(opts),
        stats: %{},
        # last seen curiosity-like probe (for UI / debugging only)
        last_probe: nil
      }

    th_id = unique(@t_handler)
    cu_id = unique(@c_handler)

    :ok =
      :telemetry.attach(
        th_id,
        [:brain, :thalamus, :curiosity, :decision],
        &__MODULE__.on_thalamus_decision/4,
        %{pid: self()}
      )

    :ok =
      :telemetry.attach(
        cu_id,
        [:curiosity, :proposal],
        &__MODULE__.on_curiosity_proposal/4,
        %{pid: self()}
      )

    {:ok, state |> Map.put(:th_handler, th_id) |> Map.put(:cu_handler, cu_id)}
  end

  @impl GenServer
  def terminate(_reason, %{th_handler: th, cu_handler: cu}) do
    if is_binary(th), do: :telemetry.detach(th)
    if is_binary(cu), do: :telemetry.detach(cu)
    :ok
  end

  def terminate(_reason, _), do: :ok

  # ─────────────── Telemetry bridges ───────────────

  @doc false
  def on_curiosity_proposal(_event, measurements, metadata, %{pid: pid}) when is_pid(pid) do
    send(pid, {:curiosity_proposal, measurements, metadata})
  end

  def on_curiosity_proposal(_, _, _, _), do: :ok

  @doc false
  def on_thalamus_decision(_event, measurements, metadata, %{pid: pid}) when is_pid(pid) do
    send(pid, {:thalamus_decision, measurements, metadata})
  end

  def on_thalamus_decision(_, _, _, _), do: :ok

  # ─────────────── GenServer casts ───────────────

  @impl GenServer
  def handle_cast({:set_opts, new_opts}, state) when is_map(new_opts) do
    merged = Map.merge(state.opts, new_opts)

    :telemetry.execute(
      [:brain, :dlpfc, :opts_updated],
      %{},
      %{opts: merged, v: 1}
    )

    {:noreply, %{state | opts: merged}}
  end

  @impl GenServer
  def handle_cast(_other, state), do: {:noreply, state}

  # ─────────────── Region messages ───────────────

  @impl GenServer
  def handle_info({:curiosity_proposal, meas, meta}, state) do
    # Cache the latest curiosity probe for UI / debugging.
    score = get_num(meas, :score, 0.0)

    base_probe = meta_get(meta, :probe, %{})

    probe =
      base_probe
      |> Map.put_new(:score, score)
      |> Map.put(:source, Map.get(base_probe, :source, :runtime))
      |> Map.put_new(:reason, :curiosity)

    :telemetry.execute(
      [:brain, :dlpfc, :proposal_cached],
      %{score: score},
      %{
        probe_id: probe[:id] || probe["id"],
        kind: probe[:kind] || probe["kind"],
        v: 1
      }
    )

    {:noreply, %{state | last_probe: probe}}
  end

  @impl GenServer
  def handle_info({:thalamus_decision, meas, meta}, state) do
    decision = meta_get(meta, :decision, nil)
    score = get_num(meas, :score, 0.0)
    act? = !!Map.get(state.opts, :act_on_thalamus, false)

    base_probe = meta_get(meta, :probe, %{})
    has_probe = is_map(base_probe) and map_size(base_probe) > 0

    probe =
      base_probe
      |> Map.put_new(:score, score)
      |> Map.put(:source, Map.get(base_probe, :source, :runtime))
      |> Map.put_new(:reason, :curiosity)

    :telemetry.execute(
      [:brain, :dlpfc, :heard],
      %{score: score},
      %{
        decision: decision,
        will_act: act?,
        has_probe: has_probe,
        v: 1
      }
    )

    state2 =
      case {act?, decision, has_probe} do
        {true, d, true} when d in [:allow, "allow", :boost, "boost"] ->
          _ = Brain.focus([probe], [])
          %{state | last_probe: probe}

        _ ->
          state
      end

    {:noreply, state2}
  end

  @impl GenServer
  def handle_info(_msg, state), do: {:noreply, state}

  # ─────────────── Helpers ───────────────

  defp unique(prefix) do
    prefix <>
      Integer.to_string(:erlang.unique_integer([:positive])) <>
      "-" <> Integer.to_string(System.system_time(:microsecond))
  end

  defp get_num(map, key, default) do
    case {Map.get(map, key), Map.get(map, to_string(key))} do
      {v, _} when is_integer(v) -> v * 1.0
      {v, _} when is_float(v) -> v
      {_, v} when is_integer(v) -> v * 1.0
      {_, v} when is_float(v) -> v
      _ -> default * 1.0
    end
  end

  defp meta_get(meta, key, default) do
    case {Map.get(meta, key), Map.get(meta, to_string(key))} do
      {nil, nil} -> default
      {v, _} when not is_nil(v) -> v
      {_, v} -> v
    end
  end
end

