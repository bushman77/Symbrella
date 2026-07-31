defmodule Db.NativeVocab do
  @moduledoc """
  Symbrella-native vocabulary context.

  This module owns the `token -> token_id` mapping used for native tensor
  training data.

  Boundary rule:
  - This does not train a model.
  - This does not create embeddings.
  - This does not use a pretrained tokenizer.
  - This only learns/stores stable Symbrella-native token IDs.
  """

  import Ecto.Query

  alias Db.NativeVocabToken

  @special_tokens [
    {"[PAD]", 0},
    {"[UNK]", 1},
    {"[BOS]", 2},
    {"[EOS]", 3}
  ]

  @doc """
  Returns the fixed special tokens.
  """
  def special_tokens, do: @special_tokens

  @doc """
  Tokenizes text using the current simple Symbrella-native tokenizer.

  This is intentionally simple for the first native vocab pass.
  """
  def tokenize(text) when is_binary(text) do
    text
    |> String.downcase()
    |> String.replace("_", " ")
    |> then(fn text ->
      Regex.scan(~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)?|[|:;,.!?()\[\]{}\/-]/u, text)
    end)
    |> List.flatten()
  end

  def tokenize(_), do: []

  @doc """
  Learns vocab entries from a list of sense_text strings.

  Existing token IDs are preserved. New tokens are appended after the current
  max token_id.

  Returns:

      {:ok, %{unique_seen: n, existing_updated: n, inserted: n, vocab_size: n}}
  """
  def learn_from_texts(texts, opts \\ []) when is_list(texts) do
    source = Keyword.get(opts, :source, "brain_cells")

    counts =
      texts
      |> Enum.flat_map(&tokenize/1)
      |> Enum.frequencies()

    Db.transaction(fn ->
      lock_vocab_table()
      ensure_special_tokens!()

      existing_tokens =
        counts
        |> Map.keys()
        |> existing_token_set()

      existing_updated = increment_existing_counts(existing_tokens, counts)

      new_counts =
        counts
        |> Map.drop(MapSet.to_list(existing_tokens))

      inserted = insert_new_tokens(new_counts, source)

      %{
        unique_seen: map_size(counts),
        existing_updated: existing_updated,
        inserted: inserted,
        vocab_size: vocab_size()
      }
    end)
  end

  @doc """
  Encodes raw text into persisted Symbrella-native token IDs.

  Adds `[BOS]` and `[EOS]`.
  Unknown tokens map to `[UNK]`.
  """
  def encode_text(text, opts \\ []) when is_binary(text) do
    text
    |> tokenize()
    |> encode_tokens(opts)
  end

  @doc """
  Encodes tokens into persisted Symbrella-native token IDs.

  Adds `[BOS]` and `[EOS]`.
  Unknown tokens map to `[UNK]`.
  """
  def encode_tokens(tokens, opts \\ []) when is_list(tokens) do
    max_tokens = Keyword.get(opts, :max_tokens, :infinity)

    vocab =
      tokens
      |> Enum.concat(Enum.map(@special_tokens, fn {token, _id} -> token end))
      |> ids_for_tokens()

    bos_id = Map.fetch!(vocab, "[BOS]")
    eos_id = Map.fetch!(vocab, "[EOS]")
    unk_id = Map.fetch!(vocab, "[UNK]")

    raw_ids =
      Enum.map(tokens, fn token ->
        Map.get(vocab, token, unk_id)
      end)

    ids = [bos_id | raw_ids] ++ [eos_id]

    truncate_ids(ids, max_tokens, eos_id)
  end

  @doc """
  Returns the token ID for one token.

  Unknown tokens resolve to `[UNK]`.
  """
  def token_id(token) when is_binary(token) do
    Db.one(
      from(v in NativeVocabToken,
        where: v.token == ^token,
        select: v.token_id
      )
    ) || unk_id()
  end

  @doc """
  Returns a preview of the persisted vocab ordered by token_id.
  """
  def preview(opts \\ []) do
    limit = Keyword.get(opts, :limit, 80)

    Db.all(
      from(v in NativeVocabToken,
        order_by: [asc: v.token_id],
        limit: ^limit,
        select: %{
          token: v.token,
          token_id: v.token_id,
          count: v.count,
          source: v.source
        }
      )
    )
  end

  @doc """
  Returns the total persisted vocab size.
  """
  def vocab_size do
    Db.aggregate(NativeVocabToken, :count, :id)
  end

  defp truncate_ids(ids, :infinity, _eos_id), do: ids

  defp truncate_ids(ids, max_tokens, eos_id) when is_integer(max_tokens) and max_tokens > 1 do
    if length(ids) > max_tokens do
      Enum.take(ids, max_tokens - 1) ++ [eos_id]
    else
      ids
    end
  end

  defp truncate_ids(ids, _invalid, _eos_id), do: ids

  defp ids_for_tokens(tokens) do
    tokens = tokens |> Enum.uniq()

    Db.all(
      from(v in NativeVocabToken,
        where: v.token in ^tokens,
        select: {v.token, v.token_id}
      )
    )
    |> Map.new()
  end

  defp unk_id do
    Db.one(
      from(v in NativeVocabToken,
        where: v.token == "[UNK]",
        select: v.token_id
      )
    ) || 1
  end

  defp lock_vocab_table do
    Ecto.Adapters.SQL.query!(
      Db,
      "LOCK TABLE symbrella_vocab_tokens IN SHARE ROW EXCLUSIVE MODE",
      []
    )
  end

  defp ensure_special_tokens! do
    now = now()

    rows =
      Enum.map(@special_tokens, fn {token, token_id} ->
        %{
          token: token,
          token_id: token_id,
          count: 0,
          source: "special",
          inserted_at: now,
          updated_at: now
        }
      end)

    Db.insert_all(
      NativeVocabToken,
      rows,
      on_conflict: :nothing,
      conflict_target: :token
    )
  end

  defp existing_token_set([]), do: MapSet.new()

  defp existing_token_set(tokens) do
    Db.all(
      from(v in NativeVocabToken,
        where: v.token in ^tokens,
        select: v.token
      )
    )
    |> MapSet.new()
  end

  defp increment_existing_counts(existing_tokens, counts) do
    now = now()

    Enum.reduce(existing_tokens, 0, fn token, acc ->
      inc = Map.fetch!(counts, token)

      {updated, _} =
        Db.update_all(
          from(v in NativeVocabToken, where: v.token == ^token),
          inc: [count: inc],
          set: [updated_at: now]
        )

      acc + updated
    end)
  end

  defp insert_new_tokens(new_counts, _source) when map_size(new_counts) == 0, do: 0

  defp insert_new_tokens(new_counts, source) do
    max_id =
      Db.one(
        from(v in NativeVocabToken,
          select: max(v.token_id)
        )
      ) || -1

    now = now()

    rows =
      new_counts
      |> Enum.sort_by(fn {token, count} -> {-count, token} end)
      |> Enum.with_index(max_id + 1)
      |> Enum.map(fn {{token, count}, token_id} ->
        %{
          token: token,
          token_id: token_id,
          count: count,
          source: source,
          inserted_at: now,
          updated_at: now
        }
      end)

    {inserted, _} =
      Db.insert_all(
        NativeVocabToken,
        rows,
        on_conflict: :nothing,
        conflict_target: :token
      )

    inserted
  end

  defp now do
    NaiveDateTime.utc_now()
    |> NaiveDateTime.truncate(:microsecond)
  end
end
