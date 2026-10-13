defmodule Brain.Neural.StateTransitionResidual do
  @moduledoc """
  Experimental residual Axon forecast of the next observed cognitive state.

  Instead of fitting x(t+1) outright, learn delta = x(t+1) - x(t).
  At inference the state estimate is clip(x + predicted_delta, 0, 1).
  The output layer is zero-initialized, so the untrained model is exactly the
  persistence baseline. A trained model must beat persistence on a held-out
  session before its added complexity can be justified.

  Never loaded into live Symbrella. Inputs use the version-2 24-feature
  projection from StateTransitionBuilder, NOT the raw object archive.
  """

  alias Brain.Neural.StateTransitionBuilder

  @model_version "state-transition-residual-v2"
  @default_epochs 25
  @default_hidden 8

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
        |> Axon.Loop.trainer(:mean_squared_error, :sgd, log: 0)
        |> Axon.Loop.run(
          [{train.x, observed_deltas}],
          Axon.ModelState.empty(),
          epochs: epochs
        )

      {_init, predict} = Axon.build(model)
      train_delta = predict.(params, train.x)
      eval_delta = predict.(params, evaluation.x)

      train_next = predict_next(train.x, train_delta)
      eval_next = predict_next(evaluation.x, eval_delta)
      train_metrics = metrics(names, train_next, train.x, train.y)
      eval_metrics = metrics(names, eval_next, evaluation.x, evaluation.y)

      {:ok,
       %{
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
         deployment_status: :experimental_not_for_runtime
       }}
    end
  end

  def train(_, _), do: {:error, :invalid_dataset}

  @doc "Write an isolated, private checkpoint, never replacing the direct pilot."
  @spec save(map(), Path.t()) :: :ok | {:error, term()}
  def save(%{model_version: @model_version, params: params} = artifact, path)
      when not is_nil(params) and is_binary(path) do
    with :ok <- File.mkdir_p(Path.dirname(path)),
         :ok <- File.write(path, :erlang.term_to_binary(artifact), [:binary]),
         :ok <- File.chmod(path, 0o600) do
      :ok
    end
  end

  def save(_, _), do: {:error, :invalid_artifact}

  @doc """
  Per-feature mean squared state error for model and persistence.
  All rows are in observed-state space [0, 1], not in delta space.
  """
  @spec metrics([atom()], Nx.Tensor.t(), Nx.Tensor.t(), Nx.Tensor.t()) :: map()
  def metrics(names, predicted, current, observed) when is_list(names) do
    model_squared = Nx.pow(Nx.subtract(predicted, observed), 2)
    baseline_squared = Nx.pow(Nx.subtract(current, observed), 2)

    model_per_feature = mean_columns(model_squared)
    baseline_per_feature = mean_columns(baseline_squared)

    breakdown =
      names
      |> Enum.zip(Enum.zip(model_per_feature, baseline_per_feature))
      |> Enum.map(fn {name, {model_error, base_error}} ->
        %{
          feature: name,
          model_mse: model_error,
          persistence_mse: base_error,
          improvement: base_error - model_error
        }
      end)

    %{
      model_mse: overall_mse(model_squared),
      persistence_mse: overall_mse(baseline_squared),
      per_feature: breakdown
    }
  end

  defp mean_columns(squared) do
    squared
    |> Nx.mean(axes: [0])
    |> Nx.to_list()
  end

  defp overall_mse(squared), do: squared |> Nx.mean() |> Nx.to_number() |> Kernel.*(1.0)

  defp validate_options(epochs, hidden) do
    if is_integer(epochs) and epochs in 1..200 and
         is_integer(hidden) and hidden in 1..64,
      do: :ok,
      else: {:error, :invalid_training_options}
  end

  defp validate_dataset(train, evaluation, meta, names) do
    width = length(names)
    training_shape = Nx.shape(train.x)
    evaluation_shape = Nx.shape(evaluation.x)

    cond do
      meta[:task] != :state_transition ->
        {:error, :incorrect_training_task}

      meta[:feature_schema_version] != StateTransitionBuilder.feature_schema_version() ->
        {:error, :feature_schema_mismatch}

      train[:feature_names] != names or evaluation[:feature_names] != names ->
        {:error, :feature_schema_mismatch}

      not match?({n, ^width} when n > 0, training_shape) or
          not match?({n, ^width} when n > 0, evaluation_shape) ->
        {:error, :invalid_feature_tensor_shape}

      Nx.shape(train.y) != training_shape or Nx.shape(evaluation.y) != evaluation_shape ->
        {:error, :invalid_target_shape}

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
