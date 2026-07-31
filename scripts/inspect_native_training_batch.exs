# scripts/inspect_native_training_batch.exs
#
# Inspect a reusable Symbrella-native training batch.
#
# Run:
#   mix run scripts/inspect_native_training_batch.exs --norm there --limit 25 --max-tokens 128
#
# Also:
#   mix run scripts/inspect_native_training_batch.exs --norm there --limit 25 --max-tokens 128 --learn

{opts, _argv, invalid} =
  OptionParser.parse(System.argv(),
    switches: [
      norm: :string,
      limit: :integer,
      max_tokens: :integer,
      pad_to: :string,
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
    --max-tokens 128
    --pad-to max_tokens
    --pad-to batch_max
    --learn
  """)

  System.halt(1)
end

norm = Keyword.get(opts, :norm, "there")
limit = Keyword.get(opts, :limit, 25)
max_tokens = Keyword.get(opts, :max_tokens, 128)
learn? = Keyword.get(opts, :learn, false)

pad_to =
  case Keyword.get(opts, :pad_to, "max_tokens") do
    "max_tokens" ->
      :max_tokens

    "batch_max" ->
      :batch_max

    other ->
      IO.puts("Invalid --pad-to value: #{inspect(other)}")
      IO.puts("Use: max_tokens or batch_max")
      System.halt(1)
  end

section = fn title ->
  IO.puts("\n" <> String.duplicate("=", 80))
  IO.puts(title)
  IO.puts(String.duplicate("=", 80))
end

section.("Building native training batch")

{:ok, batch} =
  Db.NativeTrainingBatch.from_norm(norm,
    limit: limit,
    max_tokens: max_tokens,
    pad_to: pad_to,
    learn?: learn?
  )

IO.inspect(
  %{
    batch_size: batch.batch_size,
    max_tokens: batch.max_tokens,
    tensor_width: batch.tensor_width,
    pad_id: batch.pad_id,
    truncated_count: batch.truncated_count,
    truncated_percent: batch.truncated_percent,
    max_token_id_count_seen: batch.max_token_id_count_seen
  },
  label: "batch_info",
  pretty: true
)

section.("Examples")

IO.inspect(batch.examples, pretty: true, limit: :infinity)

section.("Tensors")

IO.inspect(batch.input_ids, label: "input_ids", limit: :infinity)
IO.inspect(batch.attention_mask, label: "attention_mask", limit: :infinity)

section.("Result")

IO.puts("""
Reusable native training batch completed.

This proves:
  Db.NativeTrainingBatch.from_norm/2
    -> persisted vocab token IDs
    -> padded input_ids tensor
    -> attention_mask tensor

This is now ready to feed into a small Symbrella-native model experiment.
""")
