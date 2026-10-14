# Read-only dictionary evidence-loss census.
#
# Full source scan (English JSONL, streaming, O(max senses per word) memory):
#   mix run scripts/audit_dictionary_evidence_loss.exs
#
# Small trial:
#   mix run scripts/audit_dictionary_evidence_loss.exs --limit-lines 20000
#
# Add an OPTIONAL full-DB aggregate to count stored exact-definition
# duplicates (potentially expensive on millions of rows):
#   mix run scripts/audit_dictionary_evidence_loss.exs --db-duplicates
#
# The source-side counts are opportunities to recover evidence, NOT numbers
# of verified stale DB rows. No UPDATE/INSERT/DELETE or sense activation.

alias Db.JSONL.SenseEvidenceAudit

argv =
  case System.argv() do
    ["--" | rest] -> rest
    other -> other
  end

{opts, rest, invalid} =
  OptionParser.parse(argv,
    strict: [
      limit_lines: :integer,
      sample_limit: :integer,
      progress: :integer,
      db_duplicates: :boolean
    ]
  )

if rest != [] or invalid != [] do
  raise ArgumentError,
    "Allowed options: --limit-lines N --sample-limit N --progress N --db-duplicates"
end

path = Db.JSONL.IO.default_path()
limit = Keyword.get(opts, :limit_lines, 0)
progress = Keyword.get(opts, :progress, 100_000)
sample_limit = Keyword.get(opts, :sample_limit, 8)

unless File.regular?(path) do
  raise "English Wiktextract JSONL file unavailable at #{inspect(path)}"
end

IO.puts("\n=== Wiktextract source evidence-loss census ===")
IO.puts("Mode: READ ONLY")
IO.puts("Source: #{path}")
IO.puts("Lines: #{if(limit > 0, do: "first #{limit}", else: "entire file")}")
IO.puts("First-gloss comparisons group only contiguous normalized-word runs.")
IO.puts("Source statistics are NOT verified counts of stale database cells.")

stream = File.stream!(path, [], :line)
stream = if limit > 0, do: Stream.take(stream, limit), else: stream

{state, bad_json, lines} =
  stream
  |> Stream.with_index(1)
  |> Enum.reduce({SenseEvidenceAudit.new(sample_limit: sample_limit), 0, 0},
    fn {line, number}, {acc, bad, _last_number} ->
      if progress > 0 and rem(number, progress) == 0 do
        IO.puts(:stderr,
          "Scanned #{number} source lines, #{acc.source_senses} senses, " <>
            "#{acc.senses_with_multiple_glosses} with multiple glosses"
        )
      end

      case Jason.decode(line) do
        {:ok, %{} = entry} -> {SenseEvidenceAudit.ingest(acc, entry), bad, number}
        _ -> {acc, bad + 1, number}
      end
    end
  )

summary = SenseEvidenceAudit.finish(state)
samples = Enum.reverse(summary.examples)

IO.puts("\n=== Source-wide counts ===")

IO.inspect(%{
  lines_scanned: lines,
  bad_json_lines: bad_json,
  source_entries: summary.entries,
  source_senses: summary.source_senses,
  source_senses_with_multiple_glosses: summary.senses_with_multiple_glosses,
  source_senses_with_category_maps: summary.senses_with_source_categories,
  source_senses_with_both: summary.senses_with_both,
  individual_source_category_values: summary.source_category_values,
  normalized_word_runs: summary.word_runs,
  repeated_first_gloss_groups: summary.shared_first_gloss_groups,
  senses_in_repeated_first_gloss_groups: summary.shared_first_gloss_senses,
  groups_distinguishable_by_full_gloss: summary.groups_distinguished_by_full_gloss,
  senses_in_groups_distinguishable_by_full_gloss:
    summary.senses_in_distinguishable_groups,
  groups_still_indistinguishable_by_full_gloss:
    summary.groups_not_distinguished_by_full_gloss,
  invalid_or_non_english_entries: summary.invalid_or_non_english_entries
}, pretty: true, width: 120)

IO.puts("\n=== Example groups of ambiguous first glosses ===")

Enum.each(samples, fn sample ->
  IO.inspect(sample, pretty: true, width: 120)
end)

if Keyword.get(opts, :db_duplicates, false) do
  IO.puts("\n=== Stored BrainCell duplicate-definition census ===")
  IO.puts("Read-only aggregate over the full brain_cells table; can be expensive.")

  sql = """
  SELECT COUNT(*)::bigint AS repeated_definition_groups,
         COALESCE(SUM(group_size), 0)::bigint AS brain_cells_in_groups
  FROM (
    SELECT COUNT(*)::bigint AS group_size
    FROM brain_cells
    WHERE definition IS NOT NULL
    GROUP BY norm, pos, definition
    HAVING COUNT(*) > 1
  ) AS duplicates
  """

  case Ecto.Adapters.SQL.query(Db, sql, [], timeout: 120_000) do
    {:ok, %{rows: [[group_count, cell_count]]}} ->
      IO.inspect(%{
        repeated_exact_definition_groups: group_count,
        brain_cells_in_repeated_exact_definition_groups: cell_count
      }, pretty: true)

    {:error, reason} ->
      IO.inspect(reason, label: "Database aggregate failed (no writes performed)")
  end
else
  IO.puts("\nOptional DB duplicate count: rerun with --db-duplicates.")
end

IO.puts("\nNo database records were changed.")
IO.puts("These measurements do not justify an automatic bulk re-import.")
