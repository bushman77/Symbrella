defmodule Brain.SelfCalibration.Training do
  @moduledoc """
  Offline Axon training boundary for self-calibration.

  Training artifacts are advisory. They are not applied to Brain.SelfModel and
  must be evaluated and compared against the baseline before any future blend
  policy can consume them.
  """

  alias Brain.SelfCalibration.AxonModel

  @type status :: :initialized | :trained

  @type t :: %__MODULE__{
          model: Axon.t() | nil,
          params: term(),
          status: status(),
          source: atom(),
          model_version: String.t(),
          input_name: String.t(),
          feature_names: [atom()],
          label_names: [atom()],
          feature_schema_v: pos_integer(),
          metrics: map(),
          meta: map(),
          v: pos_integer()
        }

  defstruct model: nil,
            params: nil,
            status: :initialized,
            source: :axon,
            model_version: "axon-tiny-v1",
            input_name: "features",
            feature_names: [],
            label_names: [],
            feature_schema_v: 1,
            metrics: %{},
            meta: %{},
            v: 1

  @spec initialize(map(), keyword()) :: {:ok, t()} | {:error, term()}
  def initialize(batch, opts \\ [])

  def initialize(%{x: x, feature_names: feature_names, label_names: label_names}, opts)
      when is_list(feature_names) and is_list(label_names) do
    with :ok <- validate_schema(feature_names, label_names),
         {:ok, batch_size} <- batch_size(x) do
      hidden_units = Keyword.get(opts, :hidden_units, 8)
      model = AxonModel.build(hidden_units: hidden_units)
      {init_fn, _predict_fn} = Axon.build(model)
      params = init_fn.(Nx.template({batch_size, AxonModel.feature_count()}, :f32), %{})

      {:ok,
       %__MODULE__{
         model: model,
         params: params,
         status: :initialized,
         source: :axon,
         model_version: AxonModel.model_version(),
         input_name: AxonModel.input_name(),
         feature_names: feature_names,
         label_names: label_names,
         feature_schema_v: 1,
         metrics: %{},
         meta: %{
           batch_size: batch_size,
           hidden_units: hidden_units,
           trained?: false
         }
       }}
    end
  end

  def initialize(_batch, _opts), do: {:error, :invalid_batch}

  @spec train(map(), keyword()) :: {:ok, t()} | {:error, term()}
  def train(batch, opts \\ [])

  def train(%{x: x, y: y, feature_names: feature_names, label_names: label_names}, opts)
      when is_list(feature_names) and is_list(label_names) do
    with :ok <- validate_schema(feature_names, label_names),
         {:ok, batch_size} <- batch_size(x) do
      epochs = Keyword.get(opts, :epochs, 1)
      hidden_units = Keyword.get(opts, :hidden_units, 8)
      optimizer = Keyword.get(opts, :optimizer, :sgd)
      model = AxonModel.build(hidden_units: hidden_units)

      params =
        model
        |> Axon.Loop.trainer(:mean_squared_error, optimizer, log: 0)
        |> Axon.Loop.run([{x, y}], %{}, epochs: epochs)

      {:ok,
       %__MODULE__{
         model: model,
         params: params,
         status: :trained,
         source: :axon,
         model_version: AxonModel.model_version(),
         input_name: AxonModel.input_name(),
         feature_names: feature_names,
         label_names: label_names,
         feature_schema_v: 1,
         metrics: %{epochs: epochs},
         meta: %{
           batch_size: batch_size,
           hidden_units: hidden_units,
           optimizer: optimizer,
           trained?: true
         }
       }}
    end
  end

  def train(_batch, _opts), do: {:error, :invalid_batch}

  defp validate_schema(feature_names, label_names) do
    if feature_names == AxonModel.feature_names() and label_names == AxonModel.label_names() do
      :ok
    else
      {:error, :schema_mismatch}
    end
  end

  defp batch_size(x) do
    if Code.ensure_loaded?(Nx) and function_exported?(Nx, :shape, 1) do
      feature_count = AxonModel.feature_count()

      case Nx.shape(x) do
        {size, ^feature_count} when is_integer(size) and size > 0 ->
          {:ok, size}

        _ ->
          {:error, :invalid_feature_tensor}
      end
    else
      {:error, :nx_unavailable}
    end
  end
end
