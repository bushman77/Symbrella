defmodule Brain.Neural.StateTransitionPilot do
  @moduledoc """
  Offline Axon forecast of a *next observed* Symbrella cognitive state.

  Self-supervision comes from recorded chronological turns within a session.
  Its loss is compared to the trivial 'next state stays like current state'
  baseline. This is research only, not a live cognitive prediction circuit.
  """

  alias Brain.Neural.StateTransitionBuilder

  @model_version "state-transition-pilot-v1"
  @epochs 25
  @hidden 8

  @spec train(map(), keyword()) :: {:ok, map()} | {:error, term()}
  def train(dataset, opts \\ [])

  def train(%{train: train, eval: evaluation, metadata: meta}, opts)
      when is_map(train) and is_map(evaluation) and is_map(meta) and is_list(opts) do
    epochs = Keyword.get(opts, :epochs, @epochs)
    hidden = Keyword.get(opts, :hidden_units, @hidden)
    features = StateTransitionBuilder.feature_names()
    width = length(features)

    with :ok <- validate_options(epochs, hidden),
         :ok <- validate_dataset(train, evaluation, meta, features, width) do
      model =
        Axon.input("current_state", shape: {nil, width})
        |> Axon.dense(hidden, activation: :relu)
        |> Axon.dense(width, activation: :sigmoid)

      params =
        model
        |> Axon.Loop.trainer(:mean_squared_error, :sgd, log: 0)
        |> Axon.Loop.run([{train.x, train.y}], Axon.ModelState.empty(), epochs: epochs)

      {_initialize, predict} = Axon.build(model)
      train_prediction = predict.(params, train.x)
      eval_prediction = predict.(params, evaluation.x)

      {:ok,
       %{
         model_version: @model_version,
         task: :state_transition,
         params: params,
         feature_names: features,
         feature_schema_version: StateTransitionBuilder.feature_schema_version(),
         epochs: epochs,
         hidden_units: hidden,
         train_examples: elem(Nx.shape(train.x), 0),
         eval_examples: elem(Nx.shape(evaluation.x), 0),
         train_sessions: meta.train_sessions,
         eval_sessions: meta.eval_sessions,
         split_mode: meta.split_mode,
         evaluated?: meta.split_mode == :session_holdout,
         train_mse: mse(train_prediction, train.y),
         eval_mse: mse(eval_prediction, evaluation.y),
         persistence_train_mse: mse(train.x, train.y),
         persistence_eval_mse: mse(evaluation.x, evaluation.y),
         deployment_status: :experimental_not_for_runtime
       }}
    end
  end

  def train(_, _), do: {:error, :invalid_dataset}

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

  defp validate_options(epochs, hidden) do
    if is_integer(epochs) and epochs in 1..200 and
         is_integer(hidden) and hidden in 1..64,
      do: :ok,
      else: {:error, :invalid_training_options}
  end

  defp validate_dataset(train, evaluation, meta, names, width) do
    train_shape = Nx.shape(train.x)
    eval_shape = Nx.shape(evaluation.x)
    train_y_shape = Nx.shape(train.y)
    eval_y_shape = Nx.shape(evaluation.y)

    cond do
      meta[:task] != :state_transition ->
        {:error, :incorrect_training_task}

      meta[:feature_schema_version] != StateTransitionBuilder.feature_schema_version() ->
        {:error, :feature_schema_mismatch}

      train[:feature_names] != names or evaluation[:feature_names] != names ->
        {:error, :feature_schema_mismatch}

      not match?({n, ^width} when n > 0, train_shape) or
          not match?({n, ^width} when n > 0, eval_shape) ->
        {:error, :invalid_feature_tensor_shape}

      train_shape != train_y_shape or eval_shape != eval_y_shape ->
        {:error, :invalid_target_shape}

      meta[:split_mode] == :in_sample_smoke ->
        :ok

      not MapSet.disjoint?(MapSet.new(meta[:train_sessions] || []),
            MapSet.new(meta[:eval_sessions] || [])) ->
        {:error, :session_leakage}

      true ->
        :ok
    end
  end

  defp mse(predictions, targets) do
    Nx.subtract(predictions, targets)
    |> Nx.pow(2)
    |> Nx.mean()
    |> Nx.to_number()
    |> Kernel.*(1.0)
  end
end
