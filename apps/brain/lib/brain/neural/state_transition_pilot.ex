defmodule Brain.Neural.StateTransitionPilot do
  @moduledoc """
  Offline Axon forecast of a *next observed* Symbrella cognitive state.

  Self-supervision comes from recorded chronological turns within a session.
  Its loss is compared to the trivial 'next state stays like current state'
  baseline. This is research only, not a live cognitive prediction circuit.
  """

  alias Brain.Neural.{StateTransitionBuilder, StateTransitionMask}

  @model_version "state-transition-pilot-v4"
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
        |> Axon.Loop.trainer(&StateTransitionMask.loss/2, :sgd, log: 0)
        |> Axon.Loop.run(
          [{train.x, StateTransitionMask.targets(train.y, train.mask)}],
          Axon.ModelState.empty(),
          epochs: epochs
        )

      {_initialize, predict} = Axon.build(model)
      train_prediction = predict.(params, train.x)
      eval_prediction = predict.(params, evaluation.x)
      train_metrics = StateTransitionMask.metrics(features, train_prediction, train.x, train.y, train.mask)
      eval_metrics = StateTransitionMask.metrics(features, eval_prediction, evaluation.x, evaluation.y, evaluation.mask)

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

  defp validate_dataset(train, evaluation, meta, names, _width) do
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
