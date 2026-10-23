defmodule Brain.Neural.SignalReceptor do
  @moduledoc """
  A bounded, read-only Brain receptor for actual Axon shadow predictions.

  This is the nervous-system observation boundary, NOT an actuator. It retains
  only numeric signals, checkpoint/evaluation provenance and a short history.
  Neither its events nor its APIs change MoodCore, memory, LIFG, decisions,
  action policy or model weights. Predictions are not measurements of the
  future and never constitute verified facts.
  """

  use GenServer

  @prediction [:brain, :neural, :state_prediction]
  @evaluation [:brain, :neural, :state_evaluation]
  @events [@prediction, @evaluation]
  @keep 32

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Most recent neural event, or nil if there is no live checkpoint signal."
  def latest do
    if Process.whereis(__MODULE__), do: GenServer.call(__MODULE__, :latest), else: nil
  end

  @doc "Recent read-only neural events (newest first); no chat or memory text."
  def history(limit \\ 10) when is_integer(limit) and limit in 1..@keep do
    if Process.whereis(__MODULE__) do
      GenServer.call(__MODULE__, {:history, limit})
    else
      []
    end
  end

  def history(_), do: []

  @doc "Whether the live symbolic Brain has received a model forecast."
  def status do
    if Process.whereis(__MODULE__) do
      GenServer.call(__MODULE__, :status)
    else
      %{status: :unavailable, predictions: 0, evaluations: 0, latest: nil}
    end
  end

  @doc false
  def handle_neural_event(event, measurements, meta, pid)
      when is_pid(pid) and event in @events and is_map(measurements) and is_map(meta) do
    send(pid, {:neural_signal, event, measurements, meta})
    :ok
  end

  def handle_neural_event(_, _, _, _), do: :ok

  @impl true
  def init(_opts) do
    handler = {__MODULE__, self()}

    case :telemetry.attach_many(handler, @events, &__MODULE__.handle_neural_event/4, self()) do
      :ok ->
        {:ok, %{handler: handler, latest: nil, history: [],
          predictions: 0, evaluations: 0}}

      {:error, reason} ->
        {:stop, {:neural_signal_subscription_failed, reason}}
    end
  end

  @impl true
  def terminate(_reason, %{handler: handler}) do
    :telemetry.detach(handler)
    :ok
  end

  @impl true
  def handle_call(:latest, _from, state), do: {:reply, state.latest, state}

  def handle_call({:history, limit}, _from, state),
    do: {:reply, Enum.take(state.history, limit), state}

  def handle_call(:status, _from, state) do
    {:reply, %{
      status: if(state.predictions > 0, do: :receiving_shadow_signals, else: :awaiting_shadow_prediction),
      predictions: state.predictions,
      evaluations: state.evaluations,
      latest: state.latest,
      influence: :observation_only,
      deployment_authorized?: false
    }, state}
  end

  @impl true
  def handle_info({:neural_signal, event, measurements, meta}, state)
      when event in @events do
    # Accept only a supervised shadow signal with explicit uncertainty
    # boundaries. Never infer numeric confidence from the neural forecast.
    if valid_signal?(measurements, meta) do
      item = %{
        event: event,
        measurements: measurements,
        meta: meta,
        at_ms: System.system_time(:millisecond)
      }

      {:noreply, %{state |
        latest: item,
        history: Enum.take([item | state.history], @keep),
        predictions: state.predictions + if(event == @prediction, do: 1, else: 0),
        evaluations: state.evaluations + if(event == @evaluation, do: 1, else: 0)
      }}
    else
      {:noreply, state}
    end
  end

  def handle_info(_, state), do: {:noreply, state}

  defp valid_signal?(measurements, meta) do
    meta[:mode] == :shadow_only and meta[:source] == :axon_residual_v4 and
      meta[:verification] == :model_prediction_not_verified and
      meta[:deployment_authorized?] == false and
      is_integer(measurements[:count]) and measurements.count == 1
  end
end
