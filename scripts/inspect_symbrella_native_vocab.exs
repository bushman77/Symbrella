# scripts/inspect_symbrella_native_vocab.exs
#
# Purpose:
#   Inspect a Symbrella-native tokenizer/vocab path.
#
# What this does:
#   - loads BrainCell sense rows from the DB
#   - builds sense_text payloads
#   - tokenizes those strings with a simple Symbrella-native tokenizer
#   - builds a temporary vocabulary
#   - converts tokens into integer token_ids
#   - creates Nx tensors from those token_ids
#
# What this does NOT do:
#   - does not use a pretrained tokenizer
#   - does not use a pretrained model
#   - does not train yet
#   - does not mutate the DB
#   - does not save embeddings
#
# Run:
#   mix run scripts/inspect_symbrella_native_vocab.exs
#
# Examples:
#   mix run scripts/inspect_symbrella_native_vocab.exs --norm there --limit 25
#   mix run scripts/inspect_symbrella_native_vocab.exs --id "there|noun|0"
#   mix run scripts/inspect_symbrella_native_vocab.exs --norm there --limit 25 --max-tokens 64

import Ecto.Query

alias Db.BrainCell

{opts, _argv, invalid} =
  OptionParser.parse(System.argv(),
    switches: [
      id: :string,
      norm: :string,
      limit: :integer,
      max_tokens: :integer
    ]
  )

if invalid != [] do
  IO.puts("""
  Invalid options:

  #{inspect(invalid, pretty: true)}

  Valid options:
    --id "there|noun|0"
    --norm there
    --limit 25
    --max-tokens 64
  """)

  System.halt(1)
end

id = Keyword.get(opts, :id)
norm = Keyword.get(opts, :norm, "there")
limit = Keyword.get(opts, :limit, 10)
max_tokens = Keyword.get(opts, :max_tokens, 64 * 2)

section = fn title ->
  IO.puts("\n" <> String.duplicate("=", 80))
  IO.puts(title)
  IO.puts(String.duplicate("=", 80))
end

tokenize = fn text ->
  text
  |> String.downcase()
  |> String.replace("_", " ")
  |> then(fn text ->
    Regex.scan(~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)?|[|:;,.!?()\[\]{}\/-]/u, text)
  end)
  |> List.flatten()
end

load_cells = fn ->
  cond do
    is_binary(id) ->
      case Db.get(BrainCell, id) do
        nil ->
          IO.puts("No BrainCell found for id: #{inspect(id)}")
          System.halt(1)

        cell ->
          [cell]
      end

    is_binary(norm) ->
      normalized =
        norm
        |> String.downcase()
        |> String.trim()

      Db.all(
        from(c in BrainCell,
          where: c.norm == ^normalized,
          order_by: [asc: c.id],
          limit: ^limit
        )
      )

    true ->
      []
  end
end

cells = load_cells.()

if cells == [] do
  IO.puts("No BrainCell rows found.")
  System.halt(1)
end

examples =
  Enum.map(cells, fn cell ->
    text = Db.BrainCellEmbeddings.sense_text(cell)
    tokens = tokenize.(text)

    %{
      id: cell.id,
      norm: cell.norm,
      pos: cell.pos,
      sense_text: text,
      tokens: tokens
    }
  end)

special_tokens = ["[PAD]", "[UNK]", "[BOS]", "[EOS]"]

token_counts =
  examples
  |> Enum.flat_map(& &1.tokens)
  |> Enum.frequencies()

learned_tokens =
  token_counts
  |> Enum.sort_by(fn {token, count} -> {-count, token} end)
  |> Enum.map(fn {token, _count} -> token end)

vocab =
  special_tokens
  |> Enum.concat(learned_tokens)
  |> Enum.uniq()
  |> Enum.with_index()
  |> Map.new()

id_to_token =
  vocab
  |> Enum.map(fn {token, token_id} -> {token_id, token} end)
  |> Map.new()

encode = fn tokens ->
  unk_id = Map.fetch!(vocab, "[UNK]")
  bos_id = Map.fetch!(vocab, "[BOS]")
  eos_id = Map.fetch!(vocab, "[EOS]")

  raw_ids =
    tokens
    |> Enum.map(fn token -> Map.get(vocab, token, unk_id) end)

  ids = [bos_id | raw_ids] ++ [eos_id]

  if length(ids) > max_tokens do
    Enum.take(ids, max_tokens - 1) ++ [eos_id]
  else
    ids
  end
end

encoded_examples =
  Enum.map(examples, fn example ->
    token_ids = encode.(example.tokens)

    Map.put(example, :token_ids, token_ids)
  end)

pad_id = Map.fetch!(vocab, "[PAD]")

pad_to = fn ids, size ->
  padding_needed = max(size - length(ids), 0)
  ids ++ List.duplicate(pad_id, padding_needed)
end

attention_mask = fn ids, size ->
  real_count = min(length(ids), size)
  List.duplicate(1, real_count) ++ List.duplicate(0, max(size - real_count, 0))
end

tensor_width =
  encoded_examples
  |> Enum.map(fn example -> length(example.token_ids) end)
  |> Enum.max()
  |> min(max_tokens)

input_ids =
  encoded_examples
  |> Enum.map(fn example ->
    example.token_ids
    |> Enum.take(tensor_width)
    |> pad_to.(tensor_width)
  end)

attention_masks =
  encoded_examples
  |> Enum.map(fn example ->
    example.token_ids
    |> Enum.take(tensor_width)
    |> attention_mask.(tensor_width)
  end)

section.("Loaded BrainCell senses")

encoded_examples
|> Enum.map(fn example ->
  %{
    id: example.id,
    norm: example.norm,
    pos: example.pos,
    token_count: length(example.tokens),
    token_id_count: length(example.token_ids)
  }
end)
|> IO.inspect(pretty: true, limit: :infinity)

section.("First sense_text payload")

encoded_examples
|> hd()
|> Map.fetch!(:sense_text)
|> IO.puts()

section.("First tokenization result")

first = hd(encoded_examples)

IO.inspect(
  %{
    id: first.id,
    tokens: first.tokens,
    token_ids: first.token_ids
  },
  pretty: true,
  limit: :infinity
)

section.("Temporary Symbrella-native vocab preview")

vocab_preview =
  vocab
  |> Enum.sort_by(fn {_token, token_id} -> token_id end)
  |> Enum.take(80)

IO.inspect(vocab_preview, pretty: true, limit: :infinity)

section.("Token id lookup examples")

["there", "noun", "definition", "example", "place", "|", ":"]
|> Enum.map(fn token ->
  {token, Map.get(vocab, token, Map.fetch!(vocab, "[UNK]"))}
end)
|> IO.inspect(pretty: true)

section.("Nx tensor data")

input_ids_tensor = Nx.tensor(input_ids, type: {:s, 64})
attention_mask_tensor = Nx.tensor(attention_masks, type: {:s, 64})

IO.inspect(input_ids_tensor, label: "input_ids_tensor", limit: :infinity)
IO.inspect(attention_mask_tensor, label: "attention_mask_tensor", limit: :infinity)

section.("Decoded first tensor row")

decoded_first_row =
  input_ids
  |> hd()
  |> Enum.map(fn token_id -> Map.fetch!(id_to_token, token_id) end)

IO.inspect(decoded_first_row, pretty: true, limit: :infinity)

section.("Result")

IO.puts("""
Native tokenizer/vocab inspection completed.

What this proves:
  BrainCell sense_text
    -> Symbrella-native string tokens
    -> temporary vocab
    -> integer token_ids
    -> Nx tensors

Important:
  This vocab is temporary and sample-based.
  It is not yet the final persisted Symbrella training vocabulary.
  No DB writes were performed.
""")
