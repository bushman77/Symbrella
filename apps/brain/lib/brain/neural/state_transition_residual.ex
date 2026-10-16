defmodule Brain.Neural.StateTransitionResidual do
  @moduledoc """
  Experimental residual Axon forecast of the next observed cognitive state.

  Instead of fitting x(t+1) outright, learn delta = x(t+1) - x(t).
  At inference the state estimate is clip(x + predicted_delta, 0, 1).
  The output layer is zero-initialized, so the untrained model is exactly the
  persistence baseline. A trained model must beat persistence on a held-out
  session before its added complexity can be justified.

  Never loaded into live Symbrella. Inputs use the version-4 instrumented masked 24-feature
  projection from StateTransitionBuilder, NOT the raw object archive.
  """

  alias Brain.Neural.{StateTransitionAudit, StateTransitionBuilder, StateTransitionMask}

  @model_version "state-transition-residual-v4"
  @default_epochs 25
  @default_hidden 8
  @portable_format :symbrella_residual_v4_safe_etf

  @doc "Residual delta predictor with zero-initialized final layer."
  @spec build_model(keyword()) :: Axon.t()
  def build_model(opts \\ []) do
    width = length(StateTransitionBuilder.feature_names())
    hidden = Keyword.get(opts, :hidden_units, @default_hidden)

    Axon.input("current_state", shape: {nil, width})
    |> Axon.dense(hidden, activation: :relu)
    |> Axon.dense(width,
      kernel_initializer: :zeros,
      bias_initializer: :zeros
    )
  end

  @doc "Convert a predicted delta back to a valid bounded cognitive state."
  @spec predict_next(Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def predict_next(current_state, predicted_delta) do
    Nx.add(current_state, predicted_delta)
    |> Nx.clip(0.0, 1.0)
  end

  @spec train(map(), keyword()) :: {:ok, map()} | {:error, term()}
  def train(dataset, opts \\ [])

  def train(%{train: train, eval: evaluation, metadata: meta}, opts)
      when is_map(train) and is_map(evaluation) and is_map(meta) and is_list(opts) do
    epochs = Keyword.get(opts, :epochs, @default_epochs)
    hidden = Keyword.get(opts, :hidden_units, @default_hidden)
    names = StateTransitionBuilder.feature_names()

    with :ok <- validate_options(epochs, hidden),
         :ok <- validate_dataset(train, evaluation, meta, names) do
      model = build_model(hidden_units: hidden)
      observed_deltas = Nx.subtract(train.y, train.x)

      params =
        model
        |> Axon.Loop.trainer(&StateTransitionMask.loss/2, :sgd, log: 0)
        |> Axon.Loop.run(
          [{train.x, StateTransitionMask.targets(observed_deltas, train.mask)}],
          Axon.ModelState.empty(),
          epochs: epochs
        )

      {_init, predict} = Axon.build(model)
      train_delta = predict.(params, train.x)
      eval_delta = predict.(params, evaluation.x)

      train_next = predict_next(train.x, train_delta)
      eval_next = predict_next(evaluation.x, eval_delta)
      train_metrics = metrics(names, train_next, train.x, train.y, train.mask)
      eval_metrics = metrics(names, eval_next, evaluation.x, evaluation.y, evaluation.mask)

      result = %{
         model_version: @model_version,
         task: :state_transition_residual,
         feature_schema_version: StateTransitionBuilder.feature_schema_version(),
         feature_names: names,
         params: params,
         epochs: epochs,
         hidden_units: hidden,
         objective: :observed_next_minus_current,
         reconstruction: :clip_current_plus_predicted_delta,
         zero_initialized_residual?: true,
         train_examples: elem(Nx.shape(train.x), 0),
         eval_examples: elem(Nx.shape(evaluation.x), 0),
         train_sessions: meta.train_sessions,
         eval_sessions: meta.eval_sessions,
         split_mode: meta.split_mode,
         evaluated?: meta.split_mode == :session_holdout,
         train_mse: train_metrics.model_mse,
         eval_mse: eval_metrics.model_mse,
         persistence_train_mse: train_metrics.persistence_mse,
         persistence_eval_mse: eval_metrics.persistence_mse,
         train_per_feature: train_metrics.per_feature,
         eval_per_feature: eval_metrics.per_feature,
         valid_train_cells: train_metrics.valid_cells,
         valid_eval_cells: eval_metrics.valid_cells,
         mask_policy: :both_endpoints_observed_exclude_presence_indicators,
         deployment_status: :experimental_not_for_runtime
       }

      {:ok, Map.put(result, :assessment, StateTransitionAudit.assess(result))}
    end
  end

  def train(_, _), do: {:error, :invalid_dataset}

  @doc """
  Save a private, safe-to-decode ETF checkpoint without serializing Nx/Axon
  structs. Every model parameter is a fixed-shape, raw f32 tensor binary.
  The loader reconstructs Axon.ModelState.new/1 after validating dimensions.
  """
  @spec save(map(), Path.t()) :: :ok | {:error, term()}
  def save(%{model_version: @model_version, params: params, hidden_units: hidden} = artifact, path)
      when is_binary(path) and is_integer(hidden) and hidden in 1..64 do
    with {:ok, weights} <- portable_weights(params, hidden),
         :ok <- File.mkdir_p(Path.dirname(path)),
         :ok <- File.write(path, :erlang.term_to_binary(portable_artifact(artifact, weights)), [:binary]),
         :ok <- File.chmod(path, 0o600) do
      :ok
    end
  end

  def save(_, _), do: {:error, :invalid_artifact}

  defp portable_artifact(artifact, weights) do
    artifact
    |> Map.take([
      :model_version, :task, :feature_schema_version, :feature_names,
      :epochs, :hidden_units, :objective, :reconstruction,
      :zero_initialized_residual?, :assessment, :train_examples, :eval_examples,
      :train_sessions, :eval_sessions, :split_mode, :evaluated?,
      :train_mse, :eval_mse, :persistence_train_mse,
      :persistence_eval_mse, :mask_policy, :deployment_status
    ])
    |> Map.merge(%{
      format: @portable_format,
      byte_order: System.endianness(),
      weights: weights
    })
  end

  defp portable_weights(%Axon.ModelState{data: data}, hidden) when is_map(data) do
    width = length(StateTransitionBuilder.feature_names())

    with {:ok, first_kernel} <- packed_tensor(data, "dense_0", "kernel", {width, hidden}),
         {:ok, first_bias} <- packed_tensor(data, "dense_0", "bias", {hidden}),
         {:ok, final_kernel} <- packed_tensor(data, "dense_1", "kernel", {hidden, width}),
         {:ok, final_bias} <- packed_tensor(data, "dense_1", "bias", {width}) do
      {:ok, %{
        "dense_0" => %{"kernel" => first_kernel, "bias" => first_bias},
        "dense_1" => %{"kernel" => final_kernel, "bias" => final_bias}
      }}
    end
  end

  defp portable_weights(_, _), do: {:error, :invalid_model_weights}

  defp packed_tensor(data, layer, key, expected_shape) do
    case get_in(data, [layer, key]) do
      %Nx.Tensor{} = tensor ->
        if Nx.shape(tensor) == expected_shape and Nx.type(tensor) == {:f, 32} do
          {:ok, %{shape: expected_shape, binary: Nx.to_binary(tensor)}}
        else
          {:error, :invalid_model_weights}
        end

      _ ->
        {:error, :invalid_model_weights}
    end
  end

  @doc "Per-feature state MSE, excluding any unobserved transition targets."
  def metrics(names, predicted, current, observed, mask) do
    StateTransitionMask.metrics(names, predicted, current, observed, mask)
  end

  defp validate_options(epochs, hidden) do
    if is_integer(epochs) and epochs in 1..200 and
         is_integer(hidden) and hidden in 1..64,
      do: :ok,
      else: {:error, :invalid_training_options}
  end

  defp validate_dataset(train, evaluation, meta, names) do
    cond do
      meta[:task] != :state_transition ->
        {:error, :incorrect_training_task}

      meta[:feature_schema_version] != StateTransitionBuilder.feature_schema_version() ->
        {:error, :feature_schema_mismatch}

      true ->
        with :ok <- StateTransitionMask.validate_batch(train, names),
             :ok <- StateTransitionMask.validate_batch(evaluation, names) do
          cond do
            meta[:split_mode] == :in_sample_smoke ->
              :ok

            meta[:split_mode] != :session_holdout or
                MapSet.size(MapSet.new(meta[:train_sessions] || [])) == 0 or
                MapSet.size(MapSet.new(meta[:eval_sessions] || [])) == 0 or
                not MapSet.disjoint?(
                  MapSet.new(meta[:train_sessions] || []),
                  MapSet.new(meta[:eval_sessions] || [])
                ) ->
              {:error, :session_leakage}

            true ->
              :ok
          end
        end
    end
  end
end
