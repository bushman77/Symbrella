# Full-source, read-only Wiktextract -> PostgreSQL BrainCell fidelity audit.
#
# Trial:
#   env -u DATABASE_URL PGDATABASE=brain_dictionary_test \
#     mix db.dictionary.audit --limit-lines 10000
#
# Complete scan:
#   env -u DATABASE_URL PGDATABASE=brain_dictionary_test mix db.dictionary.audit
#
# JSON reports default to priv/dictionary_fidelity_trial.json and
# priv/dictionary_fidelity_full.json. There are NO database writes.
#
# Existing IDs are positional: a changed English JSONL source version can
# produce false disagreements against a database imported from an older file.

alias Db.JSONL.FullFidelityAudit

argv =
  case System.argv() do
    ["--" | rest] -> rest
    args -> args
  end

{opts, rest, invalid} =
  OptionParser.parse(argv,
    strict: [
      source: :string,
      report: :string,
      expect_db: :string,
      limit_lines: :integer,
      batch: :integer,
      sample_limit: :integer,
      progress: :integer
    ]
  )

if rest != [] or invalid != [] do
  raise ArgumentError, "Unrecognized arguments: #{inspect({rest, invalid})}"
end

# Mix is invoked with --no-start. Only the database infrastructure starts.
Mix.Task.run("app.config")
{:ok, _} = Application.ensure_all_started(:ecto_sql)
{:ok, _} = Application.ensure_all_started(:postgrex)

case Db.start_link() do
  {:ok, _pid} -> :ok
  {:error, {:already_started, _pid}} -> :ok
  {:error, reason} -> raise "Db repo failed to start: #{inspect(reason)}"
end

database =
  case Ecto.Adapters.SQL.query!(Db, "SELECT current_database()", []).rows do
    [[name]] -> name
  end

expected_db = Keyword.get(opts, :expect_db, "brain_dictionary_test")

if database != expected_db do
  raise "Connected to #{inspect(database)} but expected #{inspect(expected_db)}. " <>
          "Use PGDATABASE with DATABASE_URL unset, or set --expect-db explicitly."
end

source = Path.expand(Keyword.get(opts, :source, "priv/english.jsonl"))
unless File.regular?(source), do: raise("Missing source JSONL: #{source}")

limit = Keyword.get(opts, :limit_lines, 0)
report_path =
  Keyword.get(opts, :report,
    if(limit > 0,
      do: "priv/dictionary_fidelity_trial.json",
      else: "priv/dictionary_fidelity_full.json"
    )
  )

IO.puts("Dictionary fidelity audit: READ ONLY")
IO.puts("Database: #{database}")
IO.puts("Source: #{source}")
IO.puts("Scan: #{if(limit == 0, do: "entire source", else: "first #{limit} lines")}")
IO.puts("Source size: #{File.stat!(source).size} bytes")
IO.puts("BrainCell identity is positional; compare against the same source file used for import.")

started = System.monotonic_time(:millisecond)

result =
  FullFidelityAudit.audit(source,
    limit_lines: limit,
    batch: Keyword.get(opts, :batch, 1_000),
    sample_limit: Keyword.get(opts, :sample_limit, 6),
    progress: Keyword.get(opts, :progress, 50_000)
  )

result =
  result
  |> Map.put(:database, database)
  |> Map.put(:source_file_size, File.stat!(source).size)
  |> Map.put(:elapsed_ms, System.monotonic_time(:millisecond) - started)

File.mkdir_p!(Path.dirname(report_path))
File.write!(report_path, Jason.encode!(result, pretty: true) <> "\n")

IO.puts("\n=== Corpus reconciliation ===")
IO.inspect(Map.take(result, [
  :lines, :bad_json, :source_entries, :source_senses, :compared,
  :matching_rows, :absent_ids, :total_field_mismatches,
  :mismatch_fields, :db_total_rows, :row_count_delta,
  :fidelity_pass
]), pretty: true, width: 120)

IO.puts("\n=== Source evidence the current BrainCell projection does not fully preserve ===")
IO.inspect(result.source_evidence, pretty: true, width: 120)
IO.inspect(result.withheld_category_reasons, label: "Withheld category reasons", pretty: true)
IO.inspect(result.unprojected_sense_keys, label: "Unprojected sense keys", pretty: true)
IO.inspect(result.unprojected_entry_keys, label: "Unprojected entry keys", pretty: true)

IO.inspect(result.samples, label: "Bounded mismatch samples", pretty: true, width: 120)
IO.puts("\nReport: #{Path.expand(report_path)}")
IO.puts("No BrainCells were inserted, updated, or deleted.")

if limit == 0 and not result.fidelity_pass do
  IO.puts(:stderr, "Full fidelity audit found gaps; inspect the JSON report.")
  System.halt(1)
end
