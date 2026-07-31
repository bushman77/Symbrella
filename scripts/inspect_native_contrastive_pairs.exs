# scripts/inspect_native_contrastive_pairs.exs
#
# Builds Symbrella-native contrastive training pairs.
#
# This does not train yet.
# This does not save embeddings.
# This does not use pretrained token IDs.
#
# It proves:
#   BrainCell rows
#     -> two semantic views per row
#     -> persisted native token IDs
#     -> left/right input tensors
#     -> positive pair labels by diagonal position
#
# Run:
#   mix run --no-start scripts/inspect_native_contrastive_pairs.exs --norm there --limit 25 --max-tokens 256
#
# If vocab is missing tokens:
#   mix run --no-start scripts/inspect_native_contrastive_pairs.exs --norm there --limit 25 --max-tokens 256 --learn

{:ok, _} = Application.ensure_all_started(:db)

import Ecto.Query

alias Db.BrainCell
alias Db.NativeVocab

{opts, _argv, invalid} =
  OptionParser.parse(System.argv(),
    switches: [
      norm: :string,
      limit: :integer,
      max_tokens: :integer,
      learn: :boolean
    ]
  )

if invalid != [] do
  IO.puts("""
  Invalid options:

  #{inspect(invalid, pretty: true)}

  Valid options:
    --norm there
    --limit 25
    --max-tokens 256
    --learn
  """)

  System.halt(1)
end

norm = Keyword.get(opts, :norm, "there")
limit = Keyword.get(opts, :limit, 25)
max_tokens = Keyword.get(opts, :max_tokens, 256)
learn? = Keyword.get(opts, :learn, false)

section = fn title ->
  IO.puts("\n" <> String.duplicate("=", 80))
  IO.puts(title)
  IO.puts(String.duplicate("=", 80))
end

format_list = fn
  nil ->
    ""

  list when is_list(list) ->
    list
    |> Enum.map(&to_string/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(", ")

  value ->
    to_string(value)
end

view_a = fn cell ->
  [
    "id: #{cell.id}",
    "word: #{cell.word || cell.norm}",
    "norm: #{cell.norm}",
    "part_of_speech: #{cell.pos}",
    "definition: #{cell.definition}"
  ]
  |> Enum.reject(fn line -> String.ends_with?(line, ": ") or String.ends_with?(line, ": nil") end)
  |> Enum.join("\n")
end

view_b = fn cell ->
  [
    "id: #{cell.id}",
    "word: #{cell.word || cell.norm}",
    "part_of_speech: #{cell.pos}",
    "example: #{cell.example}",
    "synonyms: #{format_list.(cell.synonyms)}",
    "antonyms: #{format_list.(cell.antonyms)}",
    "semantic_atoms: #{format_list.(cell.semantic_atoms)}"
  ]
  |> Enum.reject(fn line -> String.ends_with?(line, ": ") or String.ends_with?(line, ": nil") end)
  |> Enum.join("\n")
end

pad_ids = fn ids, width, pad_id ->
  ids ++ List.duplicate(pad_id, max(width - length(ids), 0))
end

mask_for = fn ids, width ->
  real_count = min(length(ids), width)
  List.duplicate(1, real_count) ++ List.duplicate(0, max(width - real_count, 0))
end

section.("Loading BrainCells")

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

if cells == [] do
  IO.puts("No BrainCells found for norm: #{inspect(norm)}")
  System.halt(1)
end

pairs =
  Enum.map(cells, fn cell ->
    left_text = view_a.(cell)
    right_text = view_b.(cell)

    %{
      id: cell.id,
      norm: cell.norm,
      pos: cell.pos,
      left_text: left_text,
      right_text: right_text,
      left_tokens: NativeVocab.tokenize(left_text),
      right_tokens: NativeVocab.tokenize(right_text)
    }
  end)

if learn? do
  texts =
    pairs
    |> Enum.flat_map(fn pair -> [pair.left_text, pair.right_text] end)

  {:ok, learn_result} = NativeVocab.learn_from_texts(texts)
  IO.inspect(learn_result, label: "learn_result", pretty: true)
end

pad_id = NativeVocab.token_id("[PAD]")

encoded_pairs =
  Enum.map(pairs, fn pair ->
    left_ids = NativeVocab.encode_text(pair.left_text, max_tokens: max_tokens)
    right_ids = NativeVocab.encode_text(pair.right_text, max_tokens: max_tokens)

    Map.merge(pair, %{
      left_ids: left_ids,
      right_ids: right_ids,
      left_token_id_count: length(left_ids),
      right_token_id_count: length(right_ids),
      left_truncated?: length(pair.left_tokens) + 2 > max_tokens,
      right_truncated?: length(pair.right_tokens) + 2 > max_tokens
    })
  end)

left_rows =
  Enum.map(encoded_pairs, fn pair ->
    pair.left_ids
    |> Enum.take(max_tokens)
    |> pad_ids.(max_tokens, pad_id)
  end)

right_rows =
  Enum.map(encoded_pairs, fn pair ->
    pair.right_ids
    |> Enum.take(max_tokens)
    |> pad_ids.(max_tokens, pad_id)
  end)

left_masks =
  Enum.map(encoded_pairs, fn pair ->
    pair.left_ids
    |> Enum.take(max_tokens)
    |> mask_for.(max_tokens)
  end)

right_masks =
  Enum.map(encoded_pairs, fn pair ->
    pair.right_ids
    |> Enum.take(max_tokens)
    |> mask_for.(max_tokens)
  end)

left_input_ids = Nx.tensor(left_rows, type: {:s, 64})
right_input_ids = Nx.tensor(right_rows, type: {:s, 64})
left_attention_mask = Nx.tensor(left_masks, type: {:s, 64})
right_attention_mask = Nx.tensor(right_masks, type: {:s, 64})

section.("Pair summary")

summary =
  Enum.map(encoded_pairs, fn pair ->
    %{
      id: pair.id,
      norm: pair.norm,
      pos: pair.pos,
      left_token_count: length(pair.left_tokens),
      right_token_count: length(pair.right_tokens),
      left_token_id_count: pair.left_token_id_count,
      right_token_id_count: pair.right_token_id_count,
      left_truncated?: pair.left_truncated?,
      right_truncated?: pair.right_truncated?
    }
  end)

IO.inspect(summary, pretty: true, limit: :infinity)

section.("First positive pair text")

first = hd(encoded_pairs)

IO.puts("LEFT VIEW")
IO.puts(first.left_text)
IO.puts("\nRIGHT VIEW")
IO.puts(first.right_text)

section.("Tensor shapes")

IO.inspect(
  %{
    left_input_ids: Nx.shape(left_input_ids),
    right_input_ids: Nx.shape(right_input_ids),
    left_attention_mask: Nx.shape(left_attention_mask),
    right_attention_mask: Nx.shape(right_attention_mask)
  },
  pretty: true
)

section.("Untrained contrastive loss inspection")

embedding_dim = 64
temperature = 0.07
batch_size = length(encoded_pairs)
vocab_size = NativeVocab.vocab_size()

max_input_id =
  [
    left_input_ids,
    right_input_ids
  ]
  |> Enum.flat_map(&Nx.to_flat_list/1)
  |> Enum.max()

if max_input_id >= vocab_size do
  raise """
  input_ids contain token_id #{max_input_id}, but vocab_size is #{vocab_size}.

  The persisted vocab is missing tokens. Re-run with --learn or rebuild the vocab.
  """
end

# Deterministic initialization for inspection only.
# These are not trained semantic embeddings yet.
embedding_table =
  {vocab_size, embedding_dim}
  |> Nx.iota(type: {:f, 32})
  |> Nx.multiply(0.01)
  |> Nx.sin()
  |> Nx.multiply(0.02)

encode = fn input_ids, attention_mask ->
  token_embeddings = Nx.take(embedding_table, input_ids, axis: 0)

  mask =
    attention_mask
    |> Nx.as_type({:f, 32})
    |> Nx.reshape({batch_size, max_tokens, 1})

  summed =
    token_embeddings
    |> Nx.multiply(mask)
    |> Nx.sum(axes: [1])

  counts =
    attention_mask
    |> Nx.as_type({:f, 32})
    |> Nx.sum(axes: [1])
    |> Nx.reshape({batch_size, 1})
    |> Nx.max(Nx.broadcast(1.0, {batch_size, 1}))

  pooled = Nx.divide(summed, counts)

  norms =
    pooled
    |> Nx.multiply(pooled)
    |> Nx.sum(axes: [1])
    |> Nx.add(1.0e-12)
    |> Nx.sqrt()
    |> Nx.reshape({batch_size, 1})

  Nx.divide(pooled, norms)
end

left_vectors = encode.(left_input_ids, left_attention_mask)
right_vectors = encode.(right_input_ids, right_attention_mask)

similarity =
  left_vectors
  |> Nx.dot(Nx.transpose(right_vectors))

similarity_values = Nx.to_flat_list(similarity)

losses =
  for i <- 0..(batch_size - 1) do
    row =
      similarity_values
      |> Enum.slice(i * batch_size, batch_size)

    logits = Enum.map(row, fn score -> score / temperature end)
    max_logit = Enum.max(logits)

    log_denom =
      logits
      |> Enum.map(fn logit -> :math.exp(logit - max_logit) end)
      |> Enum.sum()
      |> :math.log()
      |> Kernel.+(max_logit)

    positive_logit = Enum.at(logits, i)

    log_denom - positive_logit
  end

loss =
  losses
  |> Enum.sum()
  |> Kernel./(batch_size)

positive_scores =
  for i <- 0..(batch_size - 1) do
    similarity_values
    |> Enum.at(i * batch_size + i)
  end

hardest_negatives =
  for i <- 0..(batch_size - 1) do
    row =
      similarity_values
      |> Enum.slice(i * batch_size, batch_size)

    {hardest_j, hardest_score} =
      row
      |> Enum.with_index()
      |> Enum.reject(fn {_score, j} -> j == i end)
      |> Enum.max_by(fn {score, _j} -> score end)
      |> then(fn {score, j} -> {j, score} end)

    %{
      left: Enum.at(encoded_pairs, i).id,
      hardest_negative: Enum.at(encoded_pairs, hardest_j).id,
      score: hardest_score
    }
  end

IO.inspect(
  %{
    vocab_size: vocab_size,
    embedding_dim: embedding_dim,
    temperature: temperature,
    left_vectors: Nx.shape(left_vectors),
    right_vectors: Nx.shape(right_vectors),
    similarity: Nx.shape(similarity),
    untrained_loss: loss,
    avg_positive_score: Enum.sum(positive_scores) / batch_size
  },
  pretty: true
)

hardest_negatives
|> Enum.sort_by(fn row -> -row.score end)
|> Enum.take(10)
|> IO.inspect(label: "hardest_negatives", pretty: true, limit: :infinity)

section.("Positive labels")

positive_pairs =
  encoded_pairs
  |> Enum.with_index()
  |> Enum.map(fn {pair, index} ->
    %{
      index: index,
      left: pair.id,
      right: pair.id,
      target: 1
    }
  end)

IO.inspect(positive_pairs, pretty: true, limit: :infinity)

section.("Result")

IO.puts("""
Native contrastive pair inspection completed.

What this proves:
  BrainCell rows
    -> two views per exact sense id
    -> left/right native token tensors
    -> diagonal positive-pair labels

Next:
  train a tiny native encoder so left_i is close to right_i
  and left_i is farther from right_j where i != j.
""")
