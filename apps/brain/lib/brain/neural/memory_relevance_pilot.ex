defmodule Brain.Neural.MemoryRelevancePilot do
  @moduledoc """
  Explicitly experimental offline Axon memory-relevance learning.

  Consumes only Brain.Neural.TrainingBuilder reviewed Nx batches with
  :memory_relevance targets. Evaluates on session-held-out examples against
  the current hippocampal recall score feature. Never changes production
  weights, ranking, retrieval, or runtime supervision.
  """

  alias Brain.Neural.TrainingBuilder

  @model_version "memory-relevance-pilot-v1"
  @input_name "features"
  @default_hidden 4
  @default_epochs 25

  @spec train(map(), keyword()) :: {:ok, map()} | {:error, term()}
  def train(dataset, opts \\ [])

  def train(%{train: train, eval: evaluation, metadata: metadata}, opts)
      when is_map(train) and is_map(evaluation) and is_map(metadata) and is_list(opts) do
    epochs = Keyword.get(opts, :epochs, @default_epochs)
    hidden = Keyword.get(opts, :hidden_units, @default_hidden)

    with :ok <- validate_opts(epochs, hidden),
         :ok <- validate_data(train, evaluation, metadata) do
      model =
        Axon.input(@input_name, shape: {nil, length(TrainingBuilder.feature_names())})
        |> Axon.dense(hidden, activation: :relu)
        |> Axon.dense(1, activation: :sigmoid)

      # Same stable trainer API already used by Brain.SelfCalibration.Training.
      params =
        model
        |> Axon.Loop.trainer(:mean_squared_error, :sgd, log: 0)
        |> Axon.Loop.run([{train.x, train.y}], %{}, epochs: epochs)

      {_init, predict} = Axon.build(model)
      train_predictions = predict.(params, train.x) |> Nx.to_list() |> List.flatten()
      eval_predictions = predict.(params, evaluation.x) |> Nx.to_list() |> List.flatten()

      train_labels = train.y |> Nx.to_list() |> List.flatten()
      eval_labels = evaluation.y |> Nx.to_list() |> List.flatten()

      train_baseline = recall_scores(train)
      eval_baseline = recall_scores(evaluation)

      result = %{
        model_version: @model_version,
        feature_schema_version: TrainingBuilder.feature_schema_version(),
        input_name: @input_name,
        feature_names: TrainingBuilder.feature_names(),
        label_names: [:relevant],
        params: params,
        task: :memory_relevance,
        epochs: epochs,
        hidden_units: hidden,
        train_examples: length(train_labels),
        eval_examples: length(eval_labels),
        train_sessions: Map.get(metadata, :train_sessions, []),
        eval_sessions: Map.get(metadata, :eval_sessions, []),
        train_mse: mse(train_predictions, train_labels),
        eval_mse: mse(eval_predictions, eval_labels),
        baseline_train_mse: mse(train_baseline, train_labels),
        baseline_eval_mse: mse(eval_baseline, eval_labels),
        labels: %{
          train_positive: Enum.count(train_labels, &(&1 == 1.0)),
          eval_positive: Enum.count(eval_labels, &(&1 == 1.0))
        },
        evaluated?: metadata[:split_mode] != :in_sample_smoke,
        split_mode: Map.get(metadata, :split_mode, :session_holdout),
        deployment_status: :experimental_not_for_runtime
      }

      {:ok, result}
    end
  end

  def train(_, _), do: {:error, :invalid_dataset}

  @doc "Persist experimental weights and evaluation metadata in a private local file."
  @spec save(map(), Path.t()) :: :ok | {:error, term()}
  def save(%{model_version: @model_version, params: params} = artifact, path)
      when not is_nil(params) and is_binary(path) do
    dir = Path.dirname(path)
    with :ok <- File.mkdir_p(dir),
         :ok <- File.chmod(dir, 0o700),
         :ok <- File.write(path, :erlang.term_to_binary(artifact), [:binary]),
         :ok <- File.chmod(path, 0o600) do
      :ok
    end
  end

  def save(_, _), do: {:error, :invalid_artifact}

  defp validate_opts(epochs, hidden) do
    if is_integer(epochs) and epochs in 1..200 and
         is_integer(hidden) and hidden in 1..64 do
      :ok
    else
      {:error, :invalid_training_options}
    end
  end

  defp validate_data(train, eval_batch, metadata) do
    expected_features = TrainingBuilder.feature_names()
    count = length(expected_features)
    train_shape = Nx.shape(train.x)
    eval_shape = Nx.shape(eval_batch.x)
    train_y = Nx.shape(train.y)
    eval_y = Nx.shape(eval_batch.y)

    cond do
      metadata[:task] != :memory_relevance ->
        {:error, :incorrect_training_task}

      metadata[:feature_schema_version] != TrainingBuilder.feature_schema_version() ->
        {:error, :feature_schema_mismatch}

      train[:feature_names] != expected_features or
          eval_batch[:feature_names] != expected_features or
          train[:label_names] != [:relevant] or eval_batch[:label_names] != [:relevant] ->
        {:error, :feature_or_label_mismatch}

      not match?({n, ^count} when n > 0, train_shape) or
          not match?({n, ^count} when n > 0, eval_shape) ->
        {:error, :invalid_feature_tensor_shape}

      not match?({n, 1} when n > 0, train_y) or
          not match?({n, 1} when n > 0, eval_y) ->
        {:error, :invalid_label_tensor_shape}

      elem(train_shape, 0) != elem(train_y, 0) or
          elem(eval_shape, 0) != elem(eval_y, 0) ->
        {:error, :row_count_mismatch}

      metadata[:split_mode] == :in_sample_smoke ->
        :ok

      not MapSet.disjoint?(MapSet.new(metadata[:train_sessions] || []),
             MapSet.new(metadata[:eval_sessions] || [])) ->
        {:error, :session_leakage}

      true ->
        :ok
    end
  end

  defp recall_scores(batch) do
    index = Enum.find_index(TrainingBuilder.feature_names(), &(&1 == :recall_score))

    batch.x
    |> Nx.to_list()
    |> Enum.map(&Enum.at(&1, index))
  end

  defp mse(predictions, targets) do
    predictions
    |> Enum.zip(targets)
    |> Enum.reduce(0.0, fn {prediction, target}, acc ->
      acc + :math.pow(prediction - target, 2)
    end)
    |> Kernel./(length(targets))
  end
end
