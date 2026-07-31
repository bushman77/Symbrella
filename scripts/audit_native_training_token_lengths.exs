# scripts/audit_native_training_token_lengths.exs
#
# Audits BrainCell sense_text token lengths using the Symbrella-native tokenizer.
#
# This answers:
#   - What is the max token length in the DB?
#   - Which BrainCell rows are longest?
#   - How many rows would be truncated at a chosen max_tokens value?
#
# Run full DB audit:
#   mix run --no-start scripts/audit_native_training_token_lengths.exs --max-tokens 256
#
# Run sampled/limited audit:
#   mix run --no-start scripts/audit_native_training_token_lengths.exs --limit 100000 --max-tokens 256

{:ok, _} = Application.ensure_all_started(Db)

import Ecto.Query

alias Db.BrainCell
alias Db.NativeVocab

{opts, _argv, invalid} =
  OptionParser.parse(System.argv(),
    switches: [
      limit: :integer,
      max_tokens: :integer,
      top: :integer
    ]
  )

if invalid != [] do
  IO.puts("""
  Invalid options:

  #{inspect(invalid, pretty: true)}

  Valid options:
    --limit 100000
    --max-tokens 256
    --top 25
  """)

  System.halt(1)
end

limit = Keyword.get(opts, :limit)
max_tokens = Keyword.get(opts, :max_tokens, 256)
top_n = Keyword.get(opts, :top, 25)

section = fn title ->
  IO.puts("\n" <> String.duplicate("=", 80))
  IO.puts(title)
  IO.puts(String.duplicate("=", 80))
end

base_query =
  from(c in BrainCell,
    order_by: [asc: c.id]
  )

query =
  if is_integer(limit) and limit > 0 do
    from(c in base_query, limit: ^limit)
  else
    base_query
  end

empty_acc = %{
  total: 0,
  max_tokens_without_specials: 0,
  max_token_ids_with_specials: 0,
  over_limit: 0,
  histogram: %{},
  top: []
}

put_top = fn top, item ->
  [item | top]
  |> Enum.sort_by(fn row -> {-row.token_ids_with_specials, row.id} end)
  |> Enum.take(top_n)
end

section.("Auditing BrainCell native token lengths")

result =
  Db.transaction(
    fn ->
      query
      |> Db.stream(max_rows: 500)
      |> Enum.reduce(empty_acc, fn cell, acc ->
        sense_text = Db.BrainCellEmbeddings.sense_text(cell)
        tokens = NativeVocab.tokenize(sense_text)

        token_count = length(tokens)
        token_ids_with_specials = token_count + 2

        item = %{
          id: cell.id,
          norm: cell.norm,
          pos: cell.pos,
          token_count: token_count,
          token_ids_with_specials: token_ids_with_specials,
          would_truncate?: token_ids_with_specials > max_tokens
        }

        %{
          acc
          | total: acc.total + 1,
            max_tokens_without_specials: max(acc.max_tokens_without_specials, token_count),
            max_token_ids_with_specials:
              max(acc.max_token_ids_with_specials, token_ids_with_specials),
            over_limit:
              if(token_ids_with_specials > max_tokens,
                do: acc.over_limit + 1,
                else: acc.over_limit
              ),
            histogram: Map.update(acc.histogram, token_ids_with_specials, 1, &(&1 + 1)),
            top: put_top.(acc.top, item)
        }
      end)
    end,
    timeout: :infinity
  )
  |> case do
    {:ok, result} ->
      result

    {:error, reason} ->
      IO.inspect(reason, label: "audit failed")
      System.halt(1)
  end

percentile = fn histogram, total, pct ->
  target = Float.ceil(total * pct) |> trunc()

  histogram
  |> Enum.sort_by(fn {length, _count} -> length end)
  |> Enum.reduce_while(0, fn {length, count}, running ->
    next = running + count

    if next >= target do
      {:halt, length}
    else
      {:cont, next}
    end
  end)
end

p50 = percentile.(result.histogram, result.total, 0.50)
p90 = percentile.(result.histogram, result.total, 0.90)
p95 = percentile.(result.histogram, result.total, 0.95)
p99 = percentile.(result.histogram, result.total, 0.99)

section.("Summary")

IO.inspect(
  %{
    audited_rows: result.total,
    checked_max_tokens_setting: max_tokens,
    max_tokens_without_specials: result.max_tokens_without_specials,
    max_token_ids_with_specials: result.max_token_ids_with_specials,
    over_limit_count: result.over_limit,
    over_limit_percent:
      if(result.total > 0,
        do: Float.round(result.over_limit / result.total * 100, 4),
        else: 0.0
      ),
    p50_token_ids_with_specials: p50,
    p90_token_ids_with_specials: p90,
    p95_token_ids_with_specials: p95,
    p99_token_ids_with_specials: p99
  },
  pretty: true
)

section.("Longest BrainCell sense_text rows")

IO.inspect(result.top, pretty: true, limit: :infinity)

section.("Recommendation")

recommended =
  cond do
    result.max_token_ids_with_specials <= 128 -> 128
    result.max_token_ids_with_specials <= 256 -> 256
    result.max_token_ids_with_specials <= 384 -> 384
    result.max_token_ids_with_specials <= 512 -> 512
    true -> :bucket_or_truncate
  end

IO.inspect(
  %{
    recommended_exact_fit_default: recommended,
    note:
      "Use exact max only if it is sane. If a few dirty/extreme rows are huge, use p95/p99 plus bucketing/truncation."
  },
  pretty: true
)
