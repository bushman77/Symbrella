# scripts/learn_symbrella_native_vocab.exs
#
# Learns/persists Symbrella-native vocab tokens from BrainCell sense_text.
#
# This does not train.
# This does not create embeddings.
# This does not use pretrained token IDs.
#
# Run:
#   mix run scripts/learn_symbrella_native_vocab.exs --norm there --limit 25 --max-tokens 128

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
    --max-tokens 128
  """)

  System.halt(1)
end

id = Keyword.get(opts, :id)
norm = Keyword.get(opts, :norm, "there")
limit = Keyword.get(opts, :limit, 25)
max_tokens = Keyword.get(opts, :max_tokens, 128)

section = fn title ->
  IO.puts("\n" <> String.duplicate("=", 80))
  IO.puts(title)
  IO.puts(String.duplicate("=", 80))
end

cells =
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

if cells == [] do
  IO.puts("No BrainCell rows found.")
  System.halt(1)
end

texts = Enum.map(cells, &Db.BrainCellEmbeddings.sense_text/1)

section.("Learning persisted Symbrella vocab")

{:ok, result} = Db.NativeVocab.learn_from_texts(texts)

IO.inspect(result, label: "learn_result", pretty: true)

section.("Persisted vocab preview")

Db.NativeVocab.preview(limit: 80)
|> IO.inspect(pretty: true, limit: :infinity)

section.("Encoding batch using persisted vocab")

encoded =
  texts
  |> Enum.map(fn text ->
    Db.NativeVocab.encode_text(text, max_tokens: max_tokens)
  end)

pad_id = Db.NativeVocab.token_id("[PAD]")

tensor_width =
  encoded
  |> Enum.map(&length/1)
  |> Enum.max()
  |> min(max_tokens)

pad_to = fn ids ->
  padding_needed = max(tensor_width - length(ids), 0)
  ids ++ List.duplicate(pad_id, padding_needed)
end

mask_for = fn ids ->
  real_count = min(length(ids), tensor_width)
  List.duplicate(1, real_count) ++ List.duplicate(0, max(tensor_width - real_count, 0))
end

input_ids =
  Enum.map(encoded, fn ids ->
    ids
    |> Enum.take(tensor_width)
    |> pad_to.()
  end)

attention_masks =
  Enum.map(encoded, fn ids ->
    ids
    |> Enum.take(tensor_width)
    |> mask_for.()
  end)

input_ids_tensor = Nx.tensor(input_ids, type: {:s, 64})
attention_mask_tensor = Nx.tensor(attention_masks, type: {:s, 64})

IO.inspect(input_ids_tensor, label: "input_ids_tensor", limit: :infinity)
IO.inspect(attention_mask_tensor, label: "attention_mask_tensor", limit: :infinity)

section.("Result")

IO.puts("""
Persisted native vocab move completed.

What this proves:
BrainCell sense_text
-> Symbrella tokenizer
-> persisted token IDs
-> Nx tensor batch

Next:
use the persisted vocab to build repeatable training batches.
""")

Llm.stop_llama()
