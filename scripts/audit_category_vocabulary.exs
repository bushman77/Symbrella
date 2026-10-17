# Read-only vocabulary census: which "w" categories are lexical, and which
# are Wiktionary maintenance? Counts category VALUES and distinct source SENSES.
#
#   mix run scripts/audit_category_vocabulary.exs
#   mix run scripts/audit_category_vocabulary.exs --limit-lines 100000 --top 30
#   mix run scripts/audit_category_vocabulary.exs --limit-lines 0
#
# "Likely maintenance" is a conservative heuristic, NOT a deletion policy.
# No database writes, importer changes or lexical promotion.

alias Db.JSONL.CategoryVocabulary

argv =
  case System.argv() do
    ["--" | rest] -> rest
    args -> args
  end

{options, rest, invalid} =
  OptionParser.parse(argv,
    strict: [limit_lines: :integer, top: :integer, progress: :integer]
  )

if rest != [] or invalid != [] do
  raise ArgumentError,
    "Usage: mix run scripts/audit_category_vocabulary.exs " <>
      "[--limit-lines N] [--top N] [--progress N]"
end

limit = Keyword.get(options, :limit_lines, 20_000)
top = Keyword.get(options, :top, 15)
progress = Keyword.get(options, :progress, 20_000)

if limit < 0 or top < 0 or top > 100 or progress < 0 do
  raise ArgumentError, "Require limit_lines >= 0, top 0..100 and progress >= 0"
end

path = Db.JSONL.IO.default_path()
unless File.regular?(path), do: raise("Missing Wiktextract JSONL: #{path}")

IO.puts("\n=== Wiktextract category vocabulary audit ===")
IO.puts("Mode: READ ONLY")
IO.puts("Source: #{path}")
IO.puts("Lines: #{if(limit == 0, do: "entire source", else: "first #{limit}")}")
IO.puts("Per-sense classification counts may OVERLAP.")
IO.puts("Other-review categories contain both legitimate topics and metadata.")

stream = File.stream!(path, [], :line)
stream = if limit == 0, do: stream, else: Stream.take(stream, limit)

{summary, lines, invalid_json} =
  stream
  |> Stream.with_index(1)
  |> Enum.reduce({CategoryVocabulary.new(), 0, 0}, fn {line, number},
                                                       {acc, _previous, bad} ->
    {acc, bad} =
      case Jason.decode(line) do
        {:ok, %{} = entry} -> {CategoryVocabulary.ingest(acc, entry), bad}
        _ -> {acc, bad + 1}
      end

    if progress > 0 and rem(number, progress) == 0 do
      IO.puts(:stderr,
        "Scanned #{number} lines; #{acc.source_senses} senses, " <>
          "#{acc.raw_category_values} raw categories"
      )
    end

    {acc, number, bad}
  end)

IO.puts("\n=== Per-sense evidence exposure ===")
IO.inspect(%{
  source_lines: lines,
  invalid_json_lines: invalid_json,
  source_entries_with_senses: summary.entries,
  source_senses: summary.source_senses,
  senses_with_no_categories: summary.senses_with_no_categories,
  senses_with_only_disamb_or_maintenance:
    summary.senses_with_only_disamb_or_maintenance,
  source_senses_by_class: summary.senses_by_class
}, pretty: true, width: 120)

IO.puts("\n=== Per-category VALUE exposure ===")
IO.inspect(%{
  raw_category_values: summary.raw_category_values,
  values_by_class: summary.category_values_by_class
}, pretty: true, width: 120)

for class <- [:other_review, :likely_maintenance, :place, :topic, :untyped] do
  IO.puts("\n=== Top #{top} category names: #{class} ===")

  summary
  |> CategoryVocabulary.top_names(class, top)
  |> Enum.with_index(1)
  |> Enum.each(fn {{name, count}, index} ->
    IO.puts("#{index}. #{name} — #{count} values")
  end)
end

IO.puts("\nNo database rows were changed.")
IO.puts("Do not treat ranked categories as an approved lexical allowlist.")
