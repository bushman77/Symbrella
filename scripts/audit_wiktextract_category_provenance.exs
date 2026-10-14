# Audit category provenance WITHOUT creating or modifying any BrainCell.
#
#   mix run scripts/audit_wiktextract_category_provenance.exs
#   mix run scripts/audit_wiktextract_category_provenance.exs --limit-lines 100000
#   mix run scripts/audit_wiktextract_category_provenance.exs --limit-lines 0
#
# Some source="w" categories refer to Wiktionary page maintenance, not lexical
# meaning. This script conservatively reports raw source/kind counts and
# bounded examples. It does NOT change importer classification or data.

alias Db.JSONL.CategoryProvenance

args =
  case System.argv() do
    ["--" | rest] -> rest
    rest -> rest
  end

{opts, rest, invalid} =
  OptionParser.parse(args,
    strict: [limit_lines: :integer, sample_per_class: :integer, progress: :integer])

if rest != [] or invalid != [] do
  raise ArgumentError,
    "Usage: mix run scripts/audit_wiktextract_category_provenance.exs " <>
      "[--limit-lines N] [--sample-per-class N] [--progress N]"
end

limit = Keyword.get(opts, :limit_lines, 20_000)
max_samples = Keyword.get(opts, :sample_per_class, 6)
progress = Keyword.get(opts, :progress, 20_000)
path = Db.JSONL.IO.default_path()

if limit < 0 or max_samples < 0 or max_samples > 30 or progress < 0 do
  raise ArgumentError, "Require limit_lines >= 0, sample_per_class 0..30, progress >= 0"
end

unless File.regular?(path), do: raise("Missing source JSONL #{path}")

IO.puts("\n=== Wiktextract category provenance audit ===")
IO.puts("Mode: READ ONLY; no database writes; no importer changes.")
IO.puts("Source: #{path}")
IO.puts("Lines: #{if(limit == 0, do: "entire source", else: "first #{limit}")}")
IO.puts("Classifications are TRIAGE labels, not authoritative semantic labels.")
IO.puts("Even kind=topic or kind=place is not automatically approved for backfill.")

stream = File.stream!(path, [], :line)
stream = if limit == 0, do: stream, else: Stream.take(stream, limit)

initial = %{
  lines: 0,
  parsed_entries: 0,
  source_senses: 0,
  categories: 0,
  parse_errors: 0,
  classes: %{},
  source_and_kind_counts: %{},
  samples: %{}
}

result =
  stream
  |> Stream.with_index(1)
  |> Enum.reduce(initial, fn {line, i}, acc ->
    acc = %{acc | lines: i}

    acc =
      case Jason.decode(line) do
        {:ok, %{"senses" => senses} = entry} when is_list(senses) ->
          acc = %{acc | parsed_entries: acc.parsed_entries + 1}

          Enum.reduce(senses, acc, fn sense, state ->
            state = %{state | source_senses: state.source_senses + 1}

            categories =
              case sense do
                %{"categories" => values} when is_list(values) -> values
                _ -> []
              end

            Enum.reduce(categories, state, fn category, st ->
              class = CategoryProvenance.classify(category)

              {name, source, kind} =
                case category do
                  %{} = m -> {m["name"], m["source"], m["kind"]}
                  s when is_binary(s) -> {s, "legacy_string", "legacy_string"}
                  _ -> {nil, nil, nil}
                end

              example = %{
                word: entry["word"],
                pos: entry["pos"],
                name: name,
                source: source,
                kind: kind
              }

              existing = Map.get(st.samples, class, [])
              samples =
                if length(existing) < max_samples do
                  Map.put(st.samples, class, [example | existing])
                else
                  st.samples
                end

              st
              |> Map.update!(:categories, &(&1 + 1))
              |> Map.update!(:classes, &Map.update(&1, class, 1, fn n -> n + 1 end))
              |> Map.update!(:source_and_kind_counts,
                &Map.update(&1, {source, kind}, 1, fn n -> n + 1 end))
              |> Map.put(:samples, samples)
            end)
          end)

        _ ->
          %{acc | parse_errors: acc.parse_errors + 1}
      end
    end

    if progress > 0 and rem(i, progress) == 0 do
      IO.puts(:stderr, "Scanned #{i} lines; inspected #{acc.categories} category values")
    end

    acc
  end)

IO.inspect(%{
  lines: result.lines,
  parsed_entries: result.parsed_entries,
  source_senses: result.source_senses,
  category_values: result.categories,
  parse_errors_or_missing_senses: result.parse_errors
}, label: "Source totals", pretty: true)

IO.puts("\nCategory classifications (counting VALUES, not senses):")

result.classes
|> Enum.sort_by(fn {_class, n} -> -n end)
|> Enum.each(fn {class, n} -> IO.puts("  #{class}: #{n}") end)

IO.puts("\nBy source + kind (highest frequency first):")

result.source_and_kind_counts
|> Enum.sort_by(fn {_key, n} -> -n end)
|> Enum.take(30)
|> Enum.each(fn {{source, kind}, n} ->
  IO.inspect(%{source: source, kind: kind, category_values: n})
end)

IO.puts("\nBounded examples of each class:")
result.samples
|> Enum.sort_by(fn {class, _} -> to_string(class) end)
|> Enum.each(fn {class, examples} ->
  IO.puts("\n#{class}:")
  examples |> Enum.reverse() |> Enum.each(&IO.inspect(&1, pretty: true))
end)

IO.puts("\nNO DATABASE CHANGES.")
IO.puts("Next: decide which category KINDS/NAMES are acceptable lexical evidence.")
