# apps/brain/lib/brain/dlpfc.ex
defmodule Brain.DLPFC do
  @moduledoc """
  DLPFC — execution/goal holder for tiny exploratory acts.

  Phase A (observer mode):
  • Subscribes to Thalamus decisions and Curiosity proposals.
  • Caches the most recent proposal (for future launch handoff).
  • Emits lightweight telemetry; **does not** launch yet — avoids double-acting
    while Thalamus is the current launcher.

  Phase B (after we patch Thalamus to stop launching):
  • Toggle `:act_on_thalamus` to true to have DLPFC call `Brain.focus/2`
    with the cached probe on `:allow | :boost`.

  Notes:
  - First-class region via `use Brain, region: :dlpfc`.
  - No compile-time deps on Curiosity; listens via telemetry.
  """

  use Brain, region: :dlpfc
  require Logger

  @t_handler "brain-dlpfc-thalamus-"
  @c_handler "brain-dlpfc-curiosity-"

  # ─────────────── Region lifecycle ───────────────

  @impl GenServer
  def init(opts) do
    state =
      %{
        region: :dlpfc,
        opts: Map.new(opts),
        stats: %{},
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

  # ─────────────── Region messages ───────────────

  @impl GenServer
  def handle_info({:curiosity_proposal, meas, meta}, state) do
    # Cache the latest probe for potential launch handoff later
    score = get_num(meas, :score, 0.0)

    probe =
      meta_get(meta, :probe, %{})
      |> Map.put_new(:score, score)
      # preferred for BG / WM policy
      |> Map.put(:source, :runtime)
      |> Map.put(:reason, :curiosity)

    :telemetry.execute(
      [:brain, :dlpfc, :proposal_cached],
      %{score: score},
      %{probe_id: probe[:id] || probe["id"], kind: probe[:kind] || probe["kind"], v: 1}
    )

    {:noreply, %{state | last_probe: probe}}
  end

  @impl GenServer
  def handle_info({:thalamus_decision, meas, meta}, state) do
    decision = Map.get(meta, :decision) || Map.get(meta, "decision")
    score = get_num(meas, :score, 0.0)
    act? = !!Map.get(state.opts, :act_on_thalamus, false)

    :telemetry.execute(
      [:brain, :dlpfc, :heard],
      %{score: score},
      %{decision: decision, will_act: act?, has_probe: not is_nil(state.last_probe), v: 1}
    )

    # Phase A: observe only (act? defaults to false).
    # Phase B: flip :act_on_thalamus true AFTER Thalamus stops launching.
    state2 =
      case {act?, decision} do
        {true, d} when d in [:allow, "allow", :boost, "boost"] and is_map(state.last_probe) ->
          _ = Brain.focus([state.last_probe], [])
          state

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
