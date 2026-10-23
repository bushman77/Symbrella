defmodule SymbrellaWeb.NeuralShadow do
  @moduledoc """
  Opt-in, supervised, READ-ONLY Axon shadow inference.

  Receives the exact JSON-safe completed turns published by Core.TrainingArchive
  after persistence. Uses the same v4 projection and observation mask as the
  offline trainer, predicts the next state, then scores it when a later turn
  in the SAME session is observed.

  Does not modify response, memory, action selection, or neural weights.
  Never trained on the online observations. No user or assistant text goes
  into its JSONL prediction/evaluation audit.
  """

  use GenServer
  require Logger

  alias Brain.Neural.{StateTransitionBuilder, StateTransitionMask, StateTransitionResidual}

  @event [:core, :training_archive, :completed_turn]
  @neural_prediction_event [:brain, :neural, :state_prediction]
  @neural_evaluation_event [:brain, :neural, :state_evaluation]
  @signal_features [:confidence, :mood_da, :mood_5ht, :mood_glu, :mood_ne,
    :mood_exploration, :mood_inhibition, :mood_vigilance, :mood_plasticity]
  @max_checkpoint_bytes 5_000_000
  @max_sessions 128
  @portable_format :symbrella_residual_v4_safe_etf

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Status of the running shadow evaluator. It is disabled by default."
  def status do
    case Process.whereis(__MODULE__) do
      nil -> %{status: :disabled}
      _ -> GenServer.call(__MODULE__, :status)
    end
  end

  @doc """
  Validate a locally produced, session-held-out v4 residual checkpoint.
  Untrusted model files must never be supplied to Erlang term decoding.
  """
  def load_checkpoint(path) when is_binary(path) do
    with {:ok, %File.Stat{size: size}} when size > 0 and size <= @max_checkpoint_bytes <- File.stat(path),
         {:ok, binary} <- File.read(path),
         {:ok, artifact} <- decode_checkpoint(binary),
         :ok <- validate_checkpoint(artifact) do
      model = StateTransitionResidual.build_model(hidden_units: artifact.hidden_units)
      {_initialize, predict} = Axon.build(model)

      {:ok,
       %{
         artifact: artifact,
         predict: predict,
         sha256: :crypto.hash(:sha256, binary) |> Base.encode16(case: :lower)
       }}
    else
      {:error, reason} -> {:error, reason}
      _ -> {:error, :missing_or_oversized_checkpoint}
    end
  rescue
    _ -> {:error, :invalid_checkpoint}
  catch
    _, _ -> {:error, :invalid_checkpoint}
  end

  def load_checkpoint(_), do: {:error, :invalid_checkpoint_path}

  # Called by the archive telemetry handler. send/2 is non-blocking and this
  # callback NEVER evaluates a neural model in the LiveView/chat process.
  def handle_archive_event(@event, _measurements, %{turn: turn, events: events}, pid)
      when is_pid(pid) and is_map(turn) and is_list(events) do
    send(pid, {:archive_observed, turn, events})
    :ok
  end

  def handle_archive_event(_, _, _, _), do: :ok

  @impl true
  def init(opts) do
    conf = Application.get_env(:symbrella_web, __MODULE__, [])
    dir = Keyword.get(opts, :dir, Keyword.get(conf, :dir, "priv"))
    checkpoint =
      Keyword.get(opts, :checkpoint, Keyword.get(conf, :checkpoint,
        Path.join(dir, "state_transition_residual_v4_pilot.etf")
      ))
    log_path = Keyword.get(opts, :log_path, Path.join(dir, "shadow_predictions.jsonl"))

    base = %{
      status: :unavailable,
      reason: nil,
      dir: dir,
      checkpoint: checkpoint,
      log_path: log_path,
      model: nil,
      predictions: 0,
      evaluations: 0,
      skipped: 0,
      observed_pairs: 0,
      observed_cells: 0,
      residual_squared_error: 0.0,
      persistence_squared_error: 0.0,
      improved_pairs: 0,
      pending: %{},
      last_evaluation: nil,
      handler_id: nil
    }

    case load_checkpoint(checkpoint) do
      {:ok, model} ->
        # Handler IDs belong to the current shadow process, not a globally
        # reused constant. In tests/restarts a previous instance may be
        # terminated without its terminate/2 callback, leaving its handler
        # installed. Reusing the same static ID silently disabled the next
        # shadow instance with :already_exists.
        handler_id = {__MODULE__, self()}

        case :telemetry.attach(handler_id, @event, &__MODULE__.handle_archive_event/4, self()) do
          :ok ->
            {:ok, %{base | status: :shadow_only, model: model, handler_id: handler_id}}

          {:error, reason} ->
            Logger.warning("[NeuralShadow] unable to subscribe to archive events: #{inspect(reason)}")
            {:ok, %{base | reason: {:telemetry_attach_failed, reason}}}
        end

      {:error, :enoent} ->
        # A shadow checkpoint is optional until offline training has produced
        # an evaluated artifact. Its absence should not look like a crash.
        Logger.info("[NeuralShadow] unavailable: no checkpoint at #{checkpoint}")
        {:ok, %{base | reason: {:checkpoint_unavailable, :enoent}}}

      {:error, reason} ->
        Logger.warning("[NeuralShadow] disabled: checkpoint load failed (#{inspect(reason)})")
        {:ok, %{base | reason: {:checkpoint_unavailable, reason}}}
    end
  end

  @impl true
  def terminate(_reason, %{handler_id: handler_id}) do
    if handler_id, do: :telemetry.detach(handler_id)
    :ok
  end

  @impl true
  def handle_call(:status, _from, state) do
    {:reply, %{
      status: state.status,
      reason: state.reason,
      checkpoint: state.checkpoint,
      log_path: state.log_path,
      schema: StateTransitionBuilder.feature_schema_version(),
      model_version: if(state.model, do: state.model.artifact.model_version, else: nil),
      checkpoint_assessment: if(state.model, do: state.model.artifact[:assessment], else: nil),
      predictions: state.predictions,
      evaluations: state.evaluations,
      skipped: state.skipped,
      observed_pairs: state.observed_pairs,
      observed_cells: state.observed_cells,
      improved_pairs: state.improved_pairs,
      aggregate_residual_mse:
        if(state.observed_cells > 0, do: state.residual_squared_error / state.observed_cells, else: nil),
      aggregate_persistence_mse:
        if(state.observed_cells > 0, do: state.persistence_squared_error / state.observed_cells, else: nil),
      pending_sessions: map_size(state.pending),
      last_evaluation: state.last_evaluation
    }, state}
  end

  @impl true
  def handle_info({:archive_observed, turn, events}, state) do
    {:noreply, process_observation(state, turn, events)}
  rescue
    error ->
      Logger.warning("[NeuralShadow] ignored projection/prediction error: #{Exception.message(error)}")
      {:noreply, %{state | skipped: state.skipped + 1}}
  catch
    kind, _ ->
      Logger.warning("[NeuralShadow] ignored #{kind} during read-only inference")
      {:noreply, %{state | skipped: state.skipped + 1}}
  end

  def handle_info(_, state), do: {:noreply, state}

  defp process_observation(%{status: :shadow_only, model: model} = state, turn, events) do
    case StateTransitionBuilder.project_observation(turn, events) do
      {:ok, observation} ->
        session = observation.session_id
        last = Map.get(state.pending, session)

        # Ignore repeated archived turn notifications instead of treating a
        # duplicate as a genuine self-transition.
        if last && observation.turn_id in Map.get(last, :recent_ids, [last.turn_id]) do
          %{state | skipped: state.skipped + 1}
        else
          state = if last, do: score_previous(state, last, observation), else: state
          predict_future(state, model, observation)
        end

      {:error, reason} ->
        Logger.warning("[NeuralShadow] skipped invalid archived observation: #{inspect(reason)}")
        %{state | skipped: state.skipped + 1}
    end
  end

  defp process_observation(state, _turn, _events), do: state

  defp predict_future(state, model, observation) do
    x = Nx.tensor([observation.values], type: :f32)
    delta = model.predict.(model.artifact.params, x)
    predicted = StateTransitionResidual.predict_next(x, delta) |> Nx.to_flat_list()

    payload = %{
      record_type: "shadow_prediction",
      at: DateTime.utc_now() |> DateTime.to_iso8601(),
      model_version: model.artifact.model_version,
      model_sha256: model.sha256,
      feature_schema_version: observation.feature_schema_version,
      session_id: observation.session_id,
      from_turn_id: observation.turn_id,
      prediction: format_features(observation.feature_names, predicted),
      input_availability: format_features(observation.feature_names, observation.observed)
    }

    log_private(state.log_path, payload)
    # Hand real Axon forecasts to the structured Brain as an isolated
    # nerve-like signal. This does NOT drive MoodCore or response selection.
    emit_neural_prediction(model, observation, predicted)
    prior = Map.get(state.pending, observation.session_id)
    recent_ids =
      [observation.turn_id | if(prior, do: Map.get(prior, :recent_ids, [prior.turn_id]), else: [])]
      |> Enum.take(32)

    pending = Map.put(state.pending, observation.session_id, %{
      session_id: observation.session_id,
      turn_id: observation.turn_id,
      recent_ids: recent_ids,
      features: observation.values,
      observed: observation.observed,
      prediction: predicted
    })

    # Bound memory across long-running services with many inactive sessions.
    pending =
      if map_size(pending) > @max_sessions do
        pending |> Enum.take(-@max_sessions) |> Map.new()
      else
        pending
      end

    %{state | predictions: state.predictions + 1, pending: pending}
  end

  defp score_previous(state, last, observation) do
    mask =
      Enum.zip_with(last.observed, observation.observed, fn a, b ->
        if a == 1.0 and b == 1.0, do: 1.0, else: 0.0
      end)

    format = fn values -> Nx.tensor([values], type: :f32) end

    report =
      StateTransitionMask.metrics(
        observation.feature_names,
        format.(last.prediction),
        format.(last.features),
        format.(observation.values),
        format.(mask)
      )

    payload = %{
      record_type: "shadow_evaluation",
      at: DateTime.utc_now() |> DateTime.to_iso8601(),
      model_version: state.model.artifact.model_version,
      model_sha256: state.model.sha256,
      feature_schema_version: observation.feature_schema_version,
      session_id: observation.session_id,
      from_turn_id: last.turn_id,
      to_turn_id: observation.turn_id,
      valid_feature_pairs: report.valid_cells,
      residual_mse: report.model_mse,
      persistence_mse: report.persistence_mse,
      per_feature: report.per_feature,
      verdict:
        cond do
          report.valid_cells == 0 -> "no_common_observations"
          report.model_mse < report.persistence_mse -> "residual_better"
          report.model_mse > report.persistence_mse -> "persistence_better"
          true -> "tied"
        end
    }

    log_private(state.log_path, payload)
    emit_neural_evaluation(state.model, payload)

    # Aggregate squared errors over the same observed cells. Pairwise winning
    # counts alone can hide poor performance on larger or harder transitions.
    cells = report.valid_cells
    model_error = if cells > 0, do: report.model_mse * cells, else: 0.0
    baseline_error = if cells > 0, do: report.persistence_mse * cells, else: 0.0
    observed_increment = if cells > 0, do: 1, else: 0
    improved_increment = if payload.verdict == "residual_better", do: 1, else: 0

    %{state |
      evaluations: state.evaluations + 1,
      observed_pairs: state.observed_pairs + observed_increment,
      observed_cells: state.observed_cells + cells,
      residual_squared_error: state.residual_squared_error + model_error,
      persistence_squared_error: state.persistence_squared_error + baseline_error,
      improved_pairs: state.improved_pairs + improved_increment,
      last_evaluation: Map.take(payload, [:valid_feature_pairs, :residual_mse, :persistence_mse, :verdict])
    }
  end

  # Predictions are about the *next* completed turn, not about the current
  # actual neuromodulator levels. Omit any feature whose current observation
  # was not available, and never call this a calibrated truth estimate.
  defp emit_neural_prediction(model, observation, predicted) do
    rows =
      [observation.feature_names, observation.values, predicted, observation.observed]
      |> Enum.zip()
      |> Enum.flat_map(fn {feature, current, forecast, available} ->
        if feature in @signal_features and available == 1.0 and
             unit_number?(current) and unit_number?(forecast) do
          [{feature, current, forecast}]
        else
          []
        end
      end)

    if rows != [] do
      observed = Map.new(rows, fn {name, current, _} -> {name, current} end)
      forecast = Map.new(rows, fn {name, _, next} -> {name, next} end)
      delta = Map.new(rows, fn {name, current, next} -> {name, next - current} end)

      :telemetry.execute(
        @neural_prediction_event,
        %{count: 1, observed_features: length(rows)},
        %{
          source: :axon_residual_v4,
          mode: :shadow_only,
          verification: :model_prediction_not_verified,
          deployment_authorized?: false,
          model_version: model.artifact.model_version,
          model_sha256: model.sha256,
          checkpoint_assessment: get_in(model.artifact, [:assessment, :status]),
          feature_schema_version: observation.feature_schema_version,
          horizon: :next_completed_turn_in_same_session,
          observed: observed,
          forecast: forecast,
          predicted_change: delta
        }
      )
    end
  end

  defp emit_neural_evaluation(model, payload) do
    :telemetry.execute(
      @neural_evaluation_event,
      %{count: 1, valid_feature_pairs: payload.valid_feature_pairs},
      %{
        source: :axon_residual_v4,
        mode: :shadow_only,
        verification: :model_prediction_not_verified,
        deployment_authorized?: false,
        model_version: model.artifact.model_version,
        model_sha256: model.sha256,
        checkpoint_assessment: get_in(model.artifact, [:assessment, :status]),
        verdict: payload.verdict,
        residual_mse: payload.residual_mse,
        persistence_mse: payload.persistence_mse
      }
    )
  end

  defp unit_number?(value) when is_number(value),
    do: value >= 0.0 and value <= 1.0

  defp unit_number?(_), do: false

  defp format_features(names, values) do
    Enum.zip(names, values)
    |> Map.new(fn {name, value} -> {Atom.to_string(name), value} end)
  end

  defp log_private(path, record) do
    with :ok <- File.mkdir_p(Path.dirname(path)),
         :ok <- File.write(path, Jason.encode!(record) <> "\n", [:append, :binary]),
         :ok <- File.chmod(path, 0o600) do
      :ok
    else
      {:error, reason} ->
        Logger.warning("[NeuralShadow] cannot write private shadow audit: #{inspect(reason)}")
        {:error, reason}
    end
  end

  # Only known metadata and fixed-size tensor byte strings are decoded.
  # Unlike the old raw term_to_binary(Axon.ModelState), this portable ETF
  # contains no Nx structs / arbitrary backend atoms, so :safe works in a
  # freshly started BEAM without prior model training.
  defp decode_checkpoint(binary) do
    # Load only the known, local schema modules before safe ETF decoding:
    # feature-name and metadata atoms must already exist in this BEAM.
    _ = Code.ensure_loaded?(StateTransitionResidual)
    _ = Code.ensure_loaded?(Brain.Neural.StateTransitionAudit)
    _ = StateTransitionBuilder.feature_names()

    case :erlang.binary_to_term(binary, [:safe]) do
      %{format: @portable_format} = encoded ->
        decode_portable(encoded)

      legacy ->
        # Retain compatibility with older ETF files *when* the original
        # safe decoder can read them. No unsafe binary_to_term fallback.
        {:ok, legacy}
    end
  rescue
    _ -> {:error, :unsafe_or_corrupt_checkpoint}
  end

  defp decode_portable(%{
         weights: %{
           "dense_0" => %{"kernel" => k0, "bias" => b0},
           "dense_1" => %{"kernel" => k1, "bias" => b1}
         } = weights,
         hidden_units: hidden,
         byte_order: order
       } = encoded)
       when is_integer(hidden) and hidden in 1..64 and
              map_size(weights) == 2 do
    width = length(StateTransitionBuilder.feature_names())

    if order != System.endianness() do
      {:error, :unsupported_checkpoint_endianness}
    else
      with {:ok, first_kernel} <- unpack_tensor(k0, {width, hidden}),
           {:ok, first_bias} <- unpack_tensor(b0, {hidden}),
           {:ok, final_kernel} <- unpack_tensor(k1, {hidden, width}),
           {:ok, final_bias} <- unpack_tensor(b1, {width}) do
        data = %{
          "dense_0" => %{"kernel" => first_kernel, "bias" => first_bias},
          "dense_1" => %{"kernel" => final_kernel, "bias" => final_bias}
        }

        {:ok,
         encoded
         |> Map.drop([:format, :byte_order, :weights])
         |> Map.put(:params, Axon.ModelState.new(data))}
      end
    end
  end

  defp decode_portable(_), do: {:error, :invalid_portable_checkpoint}

  defp unpack_tensor(%{shape: shape, binary: bytes}, expected_shape)
       when is_tuple(shape) and is_binary(bytes) do
    expected_bytes = 4 * (expected_shape |> Tuple.to_list() |> Enum.product())

    if shape == expected_shape and byte_size(bytes) == expected_bytes do
      {:ok, bytes |> Nx.from_binary({:f, 32}) |> Nx.reshape(expected_shape)}
    else
      {:error, :invalid_portable_weights}
    end
  end

  defp unpack_tensor(_, _), do: {:error, :invalid_portable_weights}

  defp validate_checkpoint(artifact) when is_map(artifact) do
    names = StateTransitionBuilder.feature_names()

    if artifact[:model_version] == "state-transition-residual-v4" and
         artifact[:task] == :state_transition_residual and
         artifact[:feature_schema_version] == StateTransitionBuilder.feature_schema_version() and
         artifact[:feature_names] == names and
         artifact[:split_mode] == :session_holdout and
         artifact[:evaluated?] == true and
         artifact[:deployment_status] == :experimental_not_for_runtime and
         is_integer(artifact[:hidden_units]) and artifact[:hidden_units] in 1..64 and
         is_struct(artifact[:params], Axon.ModelState) do
      :ok
    else
      {:error, :incompatible_or_smoke_checkpoint}
    end
  end

  defp validate_checkpoint(_), do: {:error, :invalid_checkpoint}
end
