# scripts/train_native_contrastive_toy.exs
#
# Tiny Symbrella-native contrastive training experiment.
#
# This trains only an in-memory token embedding table.
# It does not save model weights.
# It does not save BrainCell embeddings.
# It does not use pretrained token IDs.
#
# Goal:
#   Make left_i closer to right_i than to right_j where i != j.
#
# Examples:
#
#   Single norm:
#     mix run --no-start scripts/train_native_contrastive_toy.exs \
#       --norm there \
#       --limit 25 \
#       --max-tokens 256 \
#       --embedding-dim 64 \
#       --steps 100 \
#       --learning-rate 0.5
#
#   Mixed norms, semantic compact view, weighted pooling:
#     mix run --no-start scripts/train_native_contrastive_toy.exs \
#       --norms "there,dog,cat,house,water,fire,tree,river,hand,eye,walk,run,eat,sleep,think,speak,write,red,blue,happy,sad,child,sun,moon,stone" \
#       --max-tokens 256 \
#       --embedding-dim 64 \
#       --steps 200 \
#       --learning-rate 0.5 \
#       --view-mode semantic_compact \
#       --weight-mode batch_idf \
#       --learn
#
#   ID-only control:
#     mix run --no-start scripts/train_native_contrastive_toy.exs \
#       --norms "there,dog,cat,house,water,fire,tree,river,hand,eye,walk,run,eat,sleep,think,speak,write,red,blue,happy,sad,child,sun,moon,stone" \
#       --max-tokens 256 \
#       --embedding-dim 64 \
#       --steps 200 \
#       --learning-rate 0.5 \
#       --view-mode id_only \
#       --weight-mode batch_idf \
#       --learn

{:ok, _} = Application.ensure_all_started(:db)

import Ecto.Query

alias Db.BrainCell
alias Db.NativeTokenWeights
alias Db.NativeVocab

defmodule Symbrella.NativeContrastiveToy do
  @moduledoc false

  import Nx.Defn

  defn encode(embedding_table, input_ids, attention_mask, token_weights) do
    {batch_size, seq_len} = Nx.shape(input_ids)

    token_embeddings = Nx.take(embedding_table, input_ids, axis: 0)

    weighted_mask =
      attention_mask
      |> Nx.as_type({:f, 32})
      |> Nx.multiply(token_weights)
      |> Nx.reshape({batch_size, seq_len, 1})

    summed =
      token_embeddings
      |> Nx.multiply(weighted_mask)
      |> Nx.sum(axes: [1])

    counts =
      weighted_mask
      |> Nx.sum(axes: [1])
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

  defn contrastive_loss(
         embedding_table,
         left_input_ids,
         left_attention_mask,
         left_token_weights,
         right_input_ids,
         right_attention_mask,
         right_token_weights,
         positive_mask,
         temperature
       ) do
    left_vectors =
      encode(embedding_table, left_input_ids, left_attention_mask, left_token_weights)

    right_vectors =
      encode(embedding_table, right_input_ids, right_attention_mask, right_token_weights)

    logits =
      left_vectors
      |> Nx.dot(Nx.transpose(right_vectors))
      |> Nx.divide(temperature)

    row_max = Nx.reduce_max(logits, axes: [1], keep_axes: true)

    log_denom =
      logits
      |> Nx.subtract(row_max)
      |> Nx.exp()
      |> Nx.sum(axes: [1], keep_axes: true)
      |> Nx.log()
      |> Nx.add(row_max)

    positive_logits =
      logits
      |> Nx.multiply(positive_mask)
      |> Nx.sum(axes: [1])
      |> Nx.reshape({:auto, 1})

    losses = Nx.subtract(log_denom, positive_logits)

    Nx.mean(losses)
  end

  defn train_step(
         embedding_table,
         left_input_ids,
         left_attention_mask,
         left_token_weights,
         right_input_ids,
         right_attention_mask,
         right_token_weights,
         positive_mask,
         temperature,
         learning_rate
       ) do
    {loss, gradient} =
      value_and_grad(embedding_table, fn embedding_table ->
        contrastive_loss(
          embedding_table,
          left_input_ids,
          left_attention_mask,
          left_token_weights,
          right_input_ids,
          right_attention_mask,
          right_token_weights,
          positive_mask,
          temperature
        )
      end)

    updated_embedding_table =
      embedding_table
      |> Nx.subtract(Nx.multiply(gradient, learning_rate))

    {updated_embedding_table, loss}
  end

  defn similarity(
         embedding_table,
         left_input_ids,
         left_attention_mask,
         left_token_weights,
         right_input_ids,
         right_attention_mask,
         right_token_weights
       ) do
    left_vectors =
      encode(embedding_table, left_input_ids, left_attention_mask, left_token_weights)

    right_vectors =
      encode(embedding_table, right_input_ids, right_attention_mask, right_token_weights)

    Nx.dot(left_vectors, Nx.transpose(right_vectors))
  end
end

{opts, _argv, invalid} =
  OptionParser.parse(System.argv(),
    switches: [
      norm: :string,
      norms: :string,
      limit: :integer,
      max_tokens: :integer,
      embedding_dim: :integer,
      steps: :integer,
      learning_rate: :float,
      temperature: :float,
      view_mode: :string,
      weight_mode: :string,
      learn: :boolean
    ]
  )

if invalid != [] do
  IO.puts("""
  Invalid options:

  #{inspect(invalid, pretty: true)}

  Valid options:
    --norm there
    --norms "there,dog,cat"
    --limit 25
    --max-tokens 256
    --embedding-dim 64
    --steps 100
    --learning-rate 0.5
    --temperature 0.07
    --view-mode semantic
    --view-mode semantic_compact
    --view-mode id_only
    --view-mode word_pos
    --weight-mode batch_idf
    --weight-mode none
    --learn
  """)

  System.halt(1)
end

norm = Keyword.get(opts, :norm, "there")
norms = Keyword.get(opts, :norms)
limit = Keyword.get(opts, :limit, 25)
max_tokens = Keyword.get(opts, :max_tokens, 256)
embedding_dim = Keyword.get(opts, :embedding_dim, 64)
steps = Keyword.get(opts, :steps, 100)
learning_rate = Keyword.get(opts, :learning_rate, 0.5)
temperature = Keyword.get(opts, :temperature, 0.07)
view_mode = Keyword.get(opts, :view_mode, "semantic")
weight_mode = Keyword.get(opts, :weight_mode, "batch_idf")
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

{view_a, view_b} =
  case view_mode do
    "id_only" ->
      view = fn cell ->
        "sense_id: #{cell.id}"
      end

      {view, view}

    "word_pos" ->
      view = fn cell ->
        [
          "word: #{cell.word || cell.norm}",
          "norm: #{cell.norm}",
          "part_of_speech: #{cell.pos}"
        ]
        |> Enum.reject(fn line ->
          String.ends_with?(line, ": ") or String.ends_with?(line, ": nil")
        end)
        |> Enum.join("\n")
      end

      {view, view}

    "semantic_compact" ->
      left_view = fn cell ->
        [
          "#{cell.word || cell.norm}",
          "#{cell.pos}",
          "#{cell.definition}"
        ]
        |> Enum.reject(fn value ->
          is_nil(value) or String.trim(to_string(value)) == ""
        end)
        |> Enum.join("\n")
      end

      right_view = fn cell ->
        [
          "#{cell.word || cell.norm}",
          "#{cell.pos}",
          "#{cell.example}",
          "#{format_list.(cell.synonyms)}",
          "#{format_list.(cell.semantic_atoms)}"
        ]
        |> Enum.reject(fn value ->
          is_nil(value) or String.trim(to_string(value)) == ""
        end)
        |> Enum.join("\n")
      end

      {left_view, right_view}

    "semantic" ->
      left_view = fn cell ->
        [
          "id: #{cell.id}",
          "word: #{cell.word || cell.norm}",
          "norm: #{cell.norm}",
          "part_of_speech: #{cell.pos}",
          "definition: #{cell.definition}"
        ]
        |> Enum.reject(fn line ->
          String.ends_with?(line, ": ") or String.ends_with?(line, ": nil")
        end)
        |> Enum.join("\n")
      end

      right_view = fn cell ->
        [
          "id: #{cell.id}",
          "word: #{cell.word || cell.norm}",
          "part_of_speech: #{cell.pos}",
          "example: #{cell.example}",
          "synonyms: #{format_list.(cell.synonyms)}",
          "antonyms: #{format_list.(cell.antonyms)}",
          "semantic_atoms: #{format_list.(cell.semantic_atoms)}"
        ]
        |> Enum.reject(fn line ->
          String.ends_with?(line, ": ") or String.ends_with?(line, ": nil")
        end)
        |> Enum.join("\n")
      end

      {left_view, right_view}

    other ->
      raise """
      Invalid --view-mode #{inspect(other)}

      Use:
        --view-mode semantic
        --view-mode semantic_compact
        --view-mode id_only
        --view-mode word_pos
      """
  end

weight_mode =
  case weight_mode do
    "batch_idf" ->
      "batch_idf"

    "none" ->
      "none"

    other ->
      raise """
      Invalid --weight-mode #{inspect(other)}

      Use:
        --weight-mode batch_idf
        --weight-mode none
      """
  end

pad_ids = fn ids, width, pad_id ->
  ids ++ List.duplicate(pad_id, max(width - length(ids), 0))
end

mask_for = fn ids, width ->
  real_count = min(length(ids), width)

  List.duplicate(1, real_count) ++
    List.duplicate(0, max(width - real_count, 0))
end

section.("Loading BrainCells")

requested_norms =
  if is_binary(norms) do
    norms
    |> String.split(",", trim: true)
    |> Enum.map(fn requested_norm ->
      requested_norm
      |> String.downcase()
      |> String.trim()
    end)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  else
    [
      norm
      |> String.downcase()
      |> String.trim()
    ]
  end

cells =
  if is_binary(norms) do
    requested_norms
    |> Enum.map(fn requested_norm ->
      Db.one(
        from(c in BrainCell,
          where:
            c.norm == ^requested_norm and
              not is_nil(c.definition) and
              not is_nil(c.example),
          order_by: [asc: c.id],
          limit: 1
        )
      )
    end)
    |> Enum.reject(&is_nil/1)
  else
    [requested_norm] = requested_norms

    Db.all(
      from(c in BrainCell,
        where: c.norm == ^requested_norm,
        order_by: [asc: c.id],
        limit: ^limit
      )
    )
  end

if cells == [] do
  IO.puts("No BrainCells found for requested norms: #{inspect(requested_norms)}")
  System.halt(1)
end

if length(cells) < 2 do
  IO.puts("""
  Need at least 2 BrainCell rows for contrastive training.

  Found:
    #{length(cells)}

  Requested norms:
    #{inspect(requested_norms)}
  """)

  System.halt(1)
end

if is_binary(norms) and length(cells) < length(requested_norms) do
  found_norms =
    cells
    |> Enum.map(& &1.norm)
    |> MapSet.new()

  missing_norms =
    requested_norms
    |> Enum.reject(&MapSet.member?(found_norms, &1))

  IO.puts("""
  Warning: some requested norms were not found with both definition and example.

  Missing:
    #{inspect(missing_norms)}
  """)
end

batch_label =
  if is_binary(norms) do
    "mixed_norms: #{Enum.join(requested_norms, ",")}"
  else
    "norm: #{hd(requested_norms)}"
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
  texts = Enum.flat_map(pairs, fn pair -> [pair.left_text, pair.right_text] end)
  {:ok, learn_result} = NativeVocab.learn_from_texts(texts)
  IO.inspect(learn_result, label: "learn_result", pretty: true)
end

pad_id = NativeVocab.token_id("[PAD]")
unk_id = NativeVocab.token_id("[UNK]")

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

truncated_count =
  Enum.count(encoded_pairs, fn pair ->
    pair.left_truncated? or pair.right_truncated?
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

{left_weight_rows, right_weight_rows, token_weight_stats} =
  NativeTokenWeights.weights_for_pair_rows(left_rows, right_rows,
    mode: weight_mode,
    pad_id: pad_id,
    unk_id: unk_id
  )

batch_size = length(encoded_pairs)

positive_mask_rows =
  for i <- 0..(batch_size - 1) do
    for j <- 0..(batch_size - 1) do
      if i == j, do: 1.0, else: 0.0
    end
  end

left_input_ids = Nx.tensor(left_rows, type: {:s, 64})
right_input_ids = Nx.tensor(right_rows, type: {:s, 64})
left_attention_mask = Nx.tensor(left_masks, type: {:s, 64})
right_attention_mask = Nx.tensor(right_masks, type: {:s, 64})
left_token_weights = Nx.tensor(left_weight_rows, type: {:f, 32})
right_token_weights = Nx.tensor(right_weight_rows, type: {:f, 32})
positive_mask = Nx.tensor(positive_mask_rows, type: {:f, 32})
temperature_tensor = Nx.tensor(temperature, type: {:f, 32})
learning_rate_tensor = Nx.tensor(learning_rate, type: {:f, 32})

section.("Training setup")

vocab_size = NativeVocab.vocab_size()

flat_token_ids =
  [left_input_ids, right_input_ids]
  |> Enum.flat_map(&Nx.to_flat_list/1)

max_input_id = Enum.max(flat_token_ids)
unknown_token_count = Enum.count(flat_token_ids, &(&1 == unk_id))

if max_input_id >= vocab_size do
  raise """
  input_ids contain token_id #{max_input_id}, but vocab_size is #{vocab_size}.
  Re-run with --learn or rebuild the vocab.
  """
end

embedding_table =
  {vocab_size, embedding_dim}
  |> Nx.iota(type: {:f, 32})
  |> Nx.multiply(0.01)
  |> Nx.sin()
  |> Nx.multiply(0.02)

IO.inspect(
  Map.merge(
    %{
      batch_label: batch_label,
      view_mode: view_mode,
      weight_mode: weight_mode,
      batch_size: batch_size,
      max_tokens: max_tokens,
      vocab_size: vocab_size,
      embedding_dim: embedding_dim,
      steps: steps,
      learning_rate: learning_rate,
      temperature: temperature,
      truncated_count: truncated_count,
      unknown_token_count: unknown_token_count,
      left_input_ids: Nx.shape(left_input_ids),
      right_input_ids: Nx.shape(right_input_ids),
      left_token_weights: Nx.shape(left_token_weights),
      right_token_weights: Nx.shape(right_token_weights)
    },
    token_weight_stats
  ),
  pretty: true
)

section.("Training")

initial_loss =
  Symbrella.NativeContrastiveToy.contrastive_loss(
    embedding_table,
    left_input_ids,
    left_attention_mask,
    left_token_weights,
    right_input_ids,
    right_attention_mask,
    right_token_weights,
    positive_mask,
    temperature_tensor
  )
  |> Nx.to_number()

IO.puts("step=0 loss=#{Float.round(initial_loss, 6)}")

report_every = max(div(steps, 10), 1)

{embedding_table, final_loss} =
  Enum.reduce(1..steps, {embedding_table, initial_loss}, fn step, {embedding_table, _loss} ->
    {updated_embedding_table, loss_tensor} =
      Symbrella.NativeContrastiveToy.train_step(
        embedding_table,
        left_input_ids,
        left_attention_mask,
        left_token_weights,
        right_input_ids,
        right_attention_mask,
        right_token_weights,
        positive_mask,
        temperature_tensor,
        learning_rate_tensor
      )

    loss = Nx.to_number(loss_tensor)

    if rem(step, report_every) == 0 or step == 1 or step == steps do
      IO.puts("step=#{step} loss=#{Float.round(loss, 6)}")
    end

    {updated_embedding_table, loss}
  end)

section.("Post-training similarity check")

similarity =
  Symbrella.NativeContrastiveToy.similarity(
    embedding_table,
    left_input_ids,
    left_attention_mask,
    left_token_weights,
    right_input_ids,
    right_attention_mask,
    right_token_weights
  )

similarity_values = Nx.to_flat_list(similarity)

positive_scores =
  for i <- 0..(batch_size - 1) do
    Enum.at(similarity_values, i * batch_size + i)
  end

ranks =
  for i <- 0..(batch_size - 1) do
    row = Enum.slice(similarity_values, i * batch_size, batch_size)
    positive_score = Enum.at(row, i)

    rank =
      row
      |> Enum.count(fn score -> score > positive_score end)
      |> Kernel.+(1)

    %{
      id: Enum.at(encoded_pairs, i).id,
      positive_score: positive_score,
      rank: rank
    }
  end

top1_count = Enum.count(ranks, fn row -> row.rank == 1 end)
top5_count = Enum.count(ranks, fn row -> row.rank <= 5 end)

avg_rank =
  ranks
  |> Enum.map(& &1.rank)
  |> Enum.sum()
  |> Kernel./(batch_size)

hardest_negatives =
  for i <- 0..(batch_size - 1) do
    row = Enum.slice(similarity_values, i * batch_size, batch_size)

    {hardest_score, hardest_j} =
      row
      |> Enum.with_index()
      |> Enum.reject(fn {_score, j} -> j == i end)
      |> Enum.max_by(fn {score, _j} -> score end)

    positive_score = Enum.at(positive_scores, i)

    %{
      left: Enum.at(encoded_pairs, i).id,
      positive_score: positive_score,
      hardest_negative: Enum.at(encoded_pairs, hardest_j).id,
      hardest_negative_score: hardest_score,
      margin: positive_score - hardest_score
    }
  end

avg_positive_score = Enum.sum(positive_scores) / batch_size

avg_hardest_negative_score =
  hardest_negatives
  |> Enum.map(& &1.hardest_negative_score)
  |> Enum.sum()
  |> Kernel./(batch_size)

avg_margin =
  hardest_negatives
  |> Enum.map(& &1.margin)
  |> Enum.sum()
  |> Kernel./(batch_size)

IO.inspect(
  %{
    initial_loss: initial_loss,
    final_loss: final_loss,
    loss_delta: initial_loss - final_loss,
    top1_count: top1_count,
    top1_percent: Float.round(top1_count / batch_size * 100, 2),
    top5_count: top5_count,
    top5_percent: Float.round(top5_count / batch_size * 100, 2),
    avg_rank: avg_rank,
    avg_positive_score: avg_positive_score,
    avg_hardest_negative_score: avg_hardest_negative_score,
    avg_margin: avg_margin
  },
  pretty: true
)

hardest_negatives
|> Enum.sort_by(fn row -> row.margin end)
|> Enum.take(10)
|> IO.inspect(label: "weakest_margins", pretty: true, limit: :infinity)

ranks
|> Enum.sort_by(fn row -> row.rank end, :desc)
|> Enum.take(10)
|> IO.inspect(label: "worst_positive_ranks", pretty: true, limit: :infinity)

section.("Result")

IO.puts("""
Toy contrastive training completed.

Interpretation:
  If loss drops but top1/top5 stay at random baseline, the model collapsed.
  If top1/top5 improve and avg_margin becomes positive, the objective is separating pairs.

Important:
  This trained only an in-memory toy embedding table.
  Nothing was saved.
  These are not production BrainCell embeddings yet.
""")
