# scripts/inspect_native_encoder_forward.exs
#
# First Symbrella-native encoder forward pass.
#
# This does not train.
# This does not create DB embeddings.
# This does not use a pretrained model.
#
# It proves:
#   persisted vocab token IDs
#     -> token embedding table
#     -> masked mean pooled sense vectors
#     -> cosine-like similarity matrix
#
# Run:
#   mix run --no-start scripts/inspect_native_encoder_forward.exs --norm there --limit 25 --max-tokens 256 --embedding-dim 64

{:ok, _} = Application.ensure_all_started(:db)

{opts, _argv, invalid} =
  OptionParser.parse(System.argv(),
    switches: [
      norm: :string,
      limit: :integer,
      max_tokens: :integer,
      embedding_dim: :integer
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
    --embedding-dim 64
  """)

  System.halt(1)
end

norm = Keyword.get(opts, :norm, "there")
limit = Keyword.get(opts, :limit, 25)
max_tokens = Keyword.get(opts, :max_tokens, 256)
embedding_dim = Keyword.get(opts, :embedding_dim, 64)

section = fn title ->
  IO.puts("\n" <> String.duplicate("=", 80))
  IO.puts(title)
  IO.puts(String.duplicate("=", 80))
end

section.("Loading native training batch")

{:ok, batch} =
  Db.NativeTrainingBatch.from_norm(norm,
    limit: limit,
    max_tokens: max_tokens,
    pad_to: :max_tokens
  )

IO.inspect(
  %{
    batch_size: batch.batch_size,
    max_tokens: batch.max_tokens,
    tensor_width: batch.tensor_width,
    truncated_count: batch.truncated_count,
    truncated_percent: batch.truncated_percent,
    max_token_id_count_seen: batch.max_token_id_count_seen
  },
  label: "batch_info",
  pretty: true
)

section.("Building deterministic native embedding table")

vocab_size = Db.NativeVocab.vocab_size()

flat_input_ids = Nx.to_flat_list(batch.input_ids)
max_input_id = Enum.max(flat_input_ids)

if max_input_id >= vocab_size do
  raise """
  input_ids contain token_id #{max_input_id}, but vocab_size is #{vocab_size}.

  The persisted vocab is missing tokens. Re-run vocab learning with --learn
  or rebuild the vocab before encoding.
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

IO.inspect(
  %{
    vocab_size: vocab_size,
    embedding_dim: embedding_dim,
    embedding_table_shape: Nx.shape(embedding_table),
    max_input_id: max_input_id
  },
  pretty: true
)

section.("Forward pass")

token_embeddings = Nx.take(embedding_table, batch.input_ids, axis: 0)

mask =
  batch.attention_mask
  |> Nx.as_type({:f, 32})
  |> Nx.reshape({batch.batch_size, batch.tensor_width, 1})

masked_embeddings = Nx.multiply(token_embeddings, mask)

summed =
  masked_embeddings
  |> Nx.sum(axes: [1])

counts =
  batch.attention_mask
  |> Nx.as_type({:f, 32})
  |> Nx.sum(axes: [1])
  |> Nx.reshape({batch.batch_size, 1})
  |> Nx.max(Nx.broadcast(1.0, {batch.batch_size, 1}))

pooled = Nx.divide(summed, counts)

norms =
  pooled
  |> Nx.multiply(pooled)
  |> Nx.sum(axes: [1])
  |> Nx.add(1.0e-12)
  |> Nx.sqrt()
  |> Nx.reshape({batch.batch_size, 1})

sense_vectors = Nx.divide(pooled, norms)

similarity = Nx.dot(sense_vectors, Nx.transpose(sense_vectors))

IO.inspect(token_embeddings, label: "token_embeddings", limit: 3)
IO.inspect(sense_vectors, label: "sense_vectors", limit: 3)
IO.inspect(similarity, label: "similarity", limit: 3)

section.("Shapes")

IO.inspect(
  %{
    input_ids: Nx.shape(batch.input_ids),
    attention_mask: Nx.shape(batch.attention_mask),
    token_embeddings: Nx.shape(token_embeddings),
    pooled: Nx.shape(pooled),
    sense_vectors: Nx.shape(sense_vectors),
    similarity: Nx.shape(similarity)
  },
  pretty: true
)

section.("Top off-diagonal similarity pairs")

ids = Enum.map(batch.examples, & &1.id)

similarity_values =
  similarity
  |> Nx.to_flat_list()

pairs =
  for i <- 0..(batch.batch_size - 1),
      j <- 0..(batch.batch_size - 1),
      i < j do
    index = i * batch.batch_size + j

    %{
      left: Enum.at(ids, i),
      right: Enum.at(ids, j),
      score: Enum.at(similarity_values, index)
    }
  end

pairs
|> Enum.sort_by(fn pair -> -pair.score end)
|> Enum.take(15)
|> IO.inspect(pretty: true, limit: :infinity)

section.("Result")

IO.puts("""
Native encoder forward pass completed.

What this proves:
  Db.NativeTrainingBatch
    -> input_ids / attention_mask
    -> native token embedding lookup
    -> masked pooled sense vectors
    -> similarity matrix

Important:
  These vectors are deterministic initialization only.
  They are not trained yet.
  Do not save these as BrainCell embeddings.

Next:
  add a real training objective.
""")
