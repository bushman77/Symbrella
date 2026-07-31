defmodule Db.NativeTokenWeights do
  @moduledoc """
  Builds token-weight rows for Symbrella-native training batches.

  This module is deliberately pure:

      token_id rows
        -> token weight rows
        -> telemetry stats

  It does not query the database.
  It does not create tensors.
  It does not train.
  It does not know about BrainCell schemas.

  Supported modes:

    * `:none` - every non-PAD token gets weight `1.0`
    * `:batch_idf` - batch-local inverse document frequency weighting

  The intended shape is:

      input_ids:      [[integer()]]
      token_weights:  [[float()]]

  where `token_weights` has the same row/column shape as `input_ids`.
  """

  @type token_id :: non_neg_integer()
  @type token_row :: [token_id()]
  @type weight_row :: [float()]
  @type mode :: :none | :batch_idf

  @type stats :: %{
          mode: mode(),
          row_count: non_neg_integer(),
          token_count: non_neg_integer(),
          zero_weight_count: non_neg_integer(),
          nonzero_weight_count: non_neg_integer(),
          min_token_weight: float(),
          max_token_weight: float(),
          avg_nonzero_token_weight: float(),
          unique_token_id_count: non_neg_integer()
        }

  @default_min_idf 0.25
  @default_max_idf 4.0
  @default_unk_weight 0.25

  @doc """
  Builds token weights for one set of rows.

  Options:

    * `:mode` - `:batch_idf`, `"batch_idf"`, `:none`, or `"none"`.
      Defaults to `:batch_idf`.
    * `:pad_id` - required PAD token id.
    * `:unk_id` - optional UNK token id.
    * `:min_idf` - minimum clamped IDF. Defaults to `0.25`.
    * `:max_idf` - maximum clamped IDF. Defaults to `4.0`.
    * `:unk_weight` - UNK weight for `:batch_idf`. Defaults to `0.25`.

  Returns:

      {weight_rows, stats}
  """
  @spec weights_for_rows([token_row()], keyword()) :: {[weight_row()], stats()}
  def weights_for_rows(rows, opts \\ []) when is_list(rows) do
    mode = normalize_mode(Keyword.get(opts, :mode, :batch_idf))
    pad_id = fetch_required!(opts, :pad_id)

    token_weights_by_id =
      build_token_weights_by_id(rows,
        mode: mode,
        pad_id: pad_id,
        unk_id: Keyword.get(opts, :unk_id),
        min_idf: Keyword.get(opts, :min_idf, @default_min_idf),
        max_idf: Keyword.get(opts, :max_idf, @default_max_idf),
        unk_weight: Keyword.get(opts, :unk_weight, @default_unk_weight)
      )

    weight_rows =
      rows
      |> Enum.map(fn row ->
        weights_for_row(row, token_weights_by_id,
          mode: mode,
          pad_id: pad_id,
          unk_id: Keyword.get(opts, :unk_id),
          unk_weight: Keyword.get(opts, :unk_weight, @default_unk_weight)
        )
      end)

    {weight_rows, stats(mode, rows, weight_rows)}
  end

  @doc """
  Builds token weights for left/right contrastive rows using one shared
  batch-local weighting map.

  This is preferred for contrastive training because both sides should use the
  same token weighting policy.

  Returns:

      {left_weight_rows, right_weight_rows, stats}
  """
  @spec weights_for_pair_rows([token_row()], [token_row()], keyword()) ::
          {[weight_row()], [weight_row()], stats()}
  def weights_for_pair_rows(left_rows, right_rows, opts \\ [])
      when is_list(left_rows) and is_list(right_rows) do
    mode = normalize_mode(Keyword.get(opts, :mode, :batch_idf))
    pad_id = fetch_required!(opts, :pad_id)

    all_rows = left_rows ++ right_rows

    token_weights_by_id =
      build_token_weights_by_id(all_rows,
        mode: mode,
        pad_id: pad_id,
        unk_id: Keyword.get(opts, :unk_id),
        min_idf: Keyword.get(opts, :min_idf, @default_min_idf),
        max_idf: Keyword.get(opts, :max_idf, @default_max_idf),
        unk_weight: Keyword.get(opts, :unk_weight, @default_unk_weight)
      )

    weight_opts = [
      mode: mode,
      pad_id: pad_id,
      unk_id: Keyword.get(opts, :unk_id),
      unk_weight: Keyword.get(opts, :unk_weight, @default_unk_weight)
    ]

    left_weight_rows =
      Enum.map(left_rows, fn row ->
        weights_for_row(row, token_weights_by_id, weight_opts)
      end)

    right_weight_rows =
      Enum.map(right_rows, fn row ->
        weights_for_row(row, token_weights_by_id, weight_opts)
      end)

    {left_weight_rows, right_weight_rows,
     stats(mode, all_rows, left_weight_rows ++ right_weight_rows)}
  end

  @doc """
  Builds the token-id -> token-weight map for a group of rows.

  Usually you want `weights_for_rows/2` or `weights_for_pair_rows/3` instead.
  """
  @spec build_token_weights_by_id([token_row()], keyword()) :: %{token_id() => float()}
  def build_token_weights_by_id(rows, opts \\ []) when is_list(rows) do
    mode = normalize_mode(Keyword.get(opts, :mode, :batch_idf))
    pad_id = fetch_required!(opts, :pad_id)

    case mode do
      :none ->
        rows
        |> flatten_unique_token_ids()
        |> Enum.reject(&(&1 == pad_id))
        |> Map.new(fn token_id -> {token_id, 1.0} end)

      :batch_idf ->
        min_idf = Keyword.get(opts, :min_idf, @default_min_idf)
        max_idf = Keyword.get(opts, :max_idf, @default_max_idf)

        document_sets =
          rows
          |> Enum.map(fn row ->
            row
            |> Enum.reject(&(&1 == pad_id))
            |> MapSet.new()
          end)

        document_count = length(document_sets)

        document_frequency =
          Enum.reduce(document_sets, %{}, fn token_ids, acc ->
            Enum.reduce(token_ids, acc, fn token_id, acc ->
              Map.update(acc, token_id, 1, &(&1 + 1))
            end)
          end)

        Map.new(document_frequency, fn {token_id, frequency} ->
          idf =
            :math.log((1 + document_count) / (1 + frequency)) + 1.0

          clamped_idf =
            idf
            |> Kernel.max(min_idf)
            |> Kernel.min(max_idf)

          {token_id, clamped_idf}
        end)
    end
  end

  @doc """
  Normalizes a user-facing mode value.
  """
  @spec normalize_mode(mode() | String.t()) :: mode()
  def normalize_mode(:none), do: :none
  def normalize_mode("none"), do: :none
  def normalize_mode(:batch_idf), do: :batch_idf
  def normalize_mode("batch_idf"), do: :batch_idf

  def normalize_mode(other) do
    raise ArgumentError, """
    invalid token weight mode: #{inspect(other)}

    Use:
      :batch_idf
      "batch_idf"
      :none
      "none"
    """
  end

  defp weights_for_row(row, token_weights_by_id, opts) do
    mode = Keyword.fetch!(opts, :mode)
    pad_id = Keyword.fetch!(opts, :pad_id)
    unk_id = Keyword.get(opts, :unk_id)
    unk_weight = Keyword.get(opts, :unk_weight, @default_unk_weight)

    Enum.map(row, fn token_id ->
      cond do
        token_id == pad_id ->
          0.0

        mode == :none ->
          1.0

        not is_nil(unk_id) and token_id == unk_id ->
          unk_weight

        true ->
          Map.get(token_weights_by_id, token_id, 1.0)
      end
    end)
  end

  defp stats(mode, rows, weight_rows) do
    flat_weights = List.flatten(weight_rows)
    nonzero_weights = Enum.reject(flat_weights, &(&1 == 0.0))

    base =
      %{
        mode: mode,
        row_count: length(rows),
        token_count: length(flat_weights),
        zero_weight_count: Enum.count(flat_weights, &(&1 == 0.0)),
        nonzero_weight_count: length(nonzero_weights),
        unique_token_id_count:
          rows
          |> flatten_unique_token_ids()
          |> length()
      }

    weight_stats =
      if nonzero_weights == [] do
        %{
          min_token_weight: 0.0,
          max_token_weight: 0.0,
          avg_nonzero_token_weight: 0.0
        }
      else
        %{
          min_token_weight: Enum.min(nonzero_weights),
          max_token_weight: Enum.max(nonzero_weights),
          avg_nonzero_token_weight: Enum.sum(nonzero_weights) / length(nonzero_weights)
        }
      end

    Map.merge(base, weight_stats)
  end

  defp flatten_unique_token_ids(rows) do
    rows
    |> List.flatten()
    |> Enum.uniq()
  end

  defp fetch_required!(opts, key) do
    case Keyword.fetch(opts, key) do
      {:ok, value} ->
        value

      :error ->
        raise ArgumentError, "missing required option #{inspect(key)}"
    end
  end
end
