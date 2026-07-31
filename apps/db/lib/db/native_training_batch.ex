defmodule Db.NativeTrainingBatch do
  @moduledoc """
  Builds Symbrella-native training batches from BrainCell sense rows.

  This module bridges:

      BrainCell rows
        -> sense_text
        -> persisted Db.NativeVocab token_ids
        -> padded input_ids
        -> attention_mask
        -> Nx tensors

  This does not train.
  This does not create embeddings.
  This does not use pretrained token IDs.
  """

  import Ecto.Query

  alias Db.BrainCell
  alias Db.NativeVocab

  @type example_meta :: %{
          id: String.t(),
          norm: String.t() | nil,
          pos: String.t() | nil,
          token_count: non_neg_integer(),
          token_id_count: non_neg_integer(),
          truncated?: boolean()
        }

  @type t :: %__MODULE__{
          examples: [example_meta()],
          batch_size: non_neg_integer(),
          max_tokens: pos_integer(),
          tensor_width: pos_integer(),
          pad_id: non_neg_integer(),
          truncated_count: non_neg_integer(),
          truncated_percent: float(),
          max_token_id_count_seen: non_neg_integer(),
          input_ids: Nx.Tensor.t(),
          attention_mask: Nx.Tensor.t()
        }

  defstruct [
    :examples,
    :batch_size,
    :max_tokens,
    :tensor_width,
    :pad_id,
    :truncated_count,
    :truncated_percent,
    :max_token_id_count_seen,
    :input_ids,
    :attention_mask
  ]

  @doc """
  Builds a batch from BrainCells matching one normalized word.

  Example:

      Db.NativeTrainingBatch.from_norm("there", limit: 25, max_tokens: 256)
  """
  @spec from_norm(String.t(), keyword()) :: {:ok, t()} | {:error, term()}
  def from_norm(norm, opts \\ []) when is_binary(norm) do
    limit = Keyword.get(opts, :limit, 25)

    normalized =
      norm
      |> String.downcase()
      |> String.trim()

    cells =
      Db.all(
        from(c in BrainCell,
          where: c.norm == ^normalized,
          order_by: [asc: c.id],
          limit: ^limit
        )
      )

    build(cells, opts)
  end

  @doc """
  Builds a batch from exact BrainCell ids.

  Example:

      Db.NativeTrainingBatch.from_ids(["there|noun|0", "there|adverb|1"])
  """
  @spec from_ids([String.t()], keyword()) :: {:ok, t()} | {:error, term()}
  def from_ids(ids, opts \\ []) when is_list(ids) do
    clean_ids =
      ids
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.uniq()

    cells =
      Db.all(
        from(c in BrainCell,
          where: c.id in ^clean_ids,
          order_by: [asc: c.id]
        )
      )

    build(cells, opts)
  end

  @doc """
  Builds a native training batch from already-loaded BrainCell structs.

  Options:

    * `:max_tokens` - max sequence length. Defaults to `256`.
    * `:pad_to` - `:max_tokens` or `:batch_max`. Defaults to `:max_tokens`.
    * `:learn?` - if true, learns missing vocab tokens before encoding. Defaults to `false`.

  During early development, `learn?: true` is useful.

  During real training, prefer `learn?: false` after the vocab is intentionally built/frozen.
  """
  @spec build([BrainCell.t()], keyword()) :: {:ok, t()} | {:error, term()}
  def build([], _opts), do: {:error, :no_cells}

  def build(cells, opts) when is_list(cells) do
    max_tokens = Keyword.get(opts, :max_tokens, 256)
    pad_to = Keyword.get(opts, :pad_to, :max_tokens)
    learn? = Keyword.get(opts, :learn?, false)

    with :ok <- validate_max_tokens(max_tokens),
         :ok <- validate_pad_to(pad_to) do
      texts = Enum.map(cells, &Db.BrainCellEmbeddings.sense_text/1)

      if learn? do
        {:ok, _result} = NativeVocab.learn_from_texts(texts)
      end

      encoded_examples =
        cells
        |> Enum.zip(texts)
        |> Enum.map(fn {cell, text} ->
          tokens = NativeVocab.tokenize(text)
          token_ids = NativeVocab.encode_text(text, max_tokens: max_tokens)

          %{
            cell: cell,
            text: text,
            tokens: tokens,
            token_ids: token_ids,
            token_count: length(tokens),
            token_id_count: length(token_ids),
            truncated?: length(tokens) + 2 > max_tokens
          }
        end)

      pad_id = NativeVocab.token_id("[PAD]")
      tensor_width = tensor_width(encoded_examples, max_tokens, pad_to)

      input_id_rows =
        Enum.map(encoded_examples, fn example ->
          example.token_ids
          |> Enum.take(tensor_width)
          |> pad_ids(tensor_width, pad_id)
        end)

      attention_mask_rows =
        Enum.map(encoded_examples, fn example ->
          example.token_ids
          |> Enum.take(tensor_width)
          |> attention_mask(tensor_width)
        end)

      examples =
        Enum.map(encoded_examples, fn example ->
          %{
            id: example.cell.id,
            norm: example.cell.norm,
            pos: example.cell.pos,
            token_count: example.token_count,
            token_id_count: example.token_id_count,
            truncated?: example.truncated?
          }
        end)

      batch_size = length(encoded_examples)

      truncated_count =
        Enum.count(examples, fn example -> example.truncated? end)

      truncated_percent =
        if batch_size > 0 do
          Float.round(truncated_count / batch_size * 100, 4)
        else
          0.0
        end

      max_token_id_count_seen =
        examples
        |> Enum.map(fn example -> example.token_id_count end)
        |> Enum.max(fn -> 0 end)

      batch = %__MODULE__{
        examples: examples,
        batch_size: batch_size,
        max_tokens: max_tokens,
        tensor_width: tensor_width,
        pad_id: pad_id,
        truncated_count: truncated_count,
        truncated_percent: truncated_percent,
        max_token_id_count_seen: max_token_id_count_seen,
        input_ids: Nx.tensor(input_id_rows, type: {:s, 64}),
        attention_mask: Nx.tensor(attention_mask_rows, type: {:s, 64})
      }

      {:ok, batch}
    end
  end

  defp validate_max_tokens(max_tokens) when is_integer(max_tokens) and max_tokens > 1, do: :ok
  defp validate_max_tokens(other), do: {:error, {:invalid_max_tokens, other}}

  defp validate_pad_to(:max_tokens), do: :ok
  defp validate_pad_to(:batch_max), do: :ok
  defp validate_pad_to(other), do: {:error, {:invalid_pad_to, other}}

  defp tensor_width(_encoded_examples, max_tokens, :max_tokens), do: max_tokens

  defp tensor_width(encoded_examples, max_tokens, :batch_max) do
    encoded_examples
    |> Enum.map(fn example -> length(example.token_ids) end)
    |> Enum.max()
    |> min(max_tokens)
  end

  defp pad_ids(ids, tensor_width, pad_id) do
    padding_needed = max(tensor_width - length(ids), 0)
    ids ++ List.duplicate(pad_id, padding_needed)
  end

  defp attention_mask(ids, tensor_width) do
    real_count = min(length(ids), tensor_width)

    List.duplicate(1, real_count) ++
      List.duplicate(0, max(tensor_width - real_count, 0))
  end
end
