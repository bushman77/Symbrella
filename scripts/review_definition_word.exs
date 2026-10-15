# Review ONE normalized word/POS using every occurrence in the FULL source.
#
# READ ONLY — no --apply option and no DB update logic.
#
#   mix run scripts/review_definition_word.exs --word point --pos noun
#   mix run scripts/review_definition_word.exs --word draw --pos verb --show-all
#   mix run scripts/review_definition_word.exs --word cat --pos noun
#
# --show-all prints every existing/proposed definition and source example.
# The fingerprint is a reproducible plan identifier, NOT approval to update.
# Because source IDs are positional, verify sense mapping manually.
#
# This parses the entire source JSONL so uppercase and non-contiguous entries
# cannot silently be omitted. Malformed JSON aborts the review.

alias Db.JSONL.{DefinitionBatchAudit, DefinitionOnlyBackfill, DefinitionWordReview}

argv =
  case System.argv() do
    ["--" | rest] -> rest
    other -> other
  end

{opts, rest, invalid} =
  OptionParser.parse(argv,
    strict: [word: :string, pos: :string, show_all: :boolean, preview: :integer]
  )

if rest != [] or invalid != [] do
  raise ArgumentError,
    "Usage: mix run scripts/review_definition_word.exs " <>
      "--word WORD --pos POS [--show-all] [--preview N]"
end

word = Keyword.get(opts, :word)
pos = Keyword.get(opts, :pos)
show_all? = Keyword.get(opts, :show_all, false)
preview = Keyword.get(opts, :preview, 8)
path = Db.JSONL.IO.default_path()

unless is_binary(word) and String.trim(word) != "" and
         is_binary(pos) and String.trim(pos) != "" and
         preview >= 0 and preview <= 100 do
  raise ArgumentError,
    "--word and canonical --pos are required; --preview must be 0..100"
end

unless File.regular?(path), do: raise("Missing source JSONL: #{path}")

norm = Db.JSONL.Util.normalize_word(word)
pos = String.trim(pos)

IO.puts("\n=== Single word/POS definition review ===")
IO.puts("READ ONLY — no database writes, no --apply option")
IO.puts("Word: #{inspect(norm)} | POS: #{inspect(pos)}")
IO.puts("Parsing FULL source: #{path}")
IO.puts("Source counts include every case variant and noncontiguous occurrence.")

case DefinitionWordReview.collect(File.stream!(path, [], :line), norm) do
  {:error, reason} ->
    IO.inspect(reason, label: "Source scan BLOCKED")
    System.halt(2)

  {:ok, entries, stats} ->
    IO.inspect(stats, label: "Source scan totals", pretty: true)

    {source_cells, _counters} = DefinitionBatchAudit.source_cells(entries, %{})
    stored_cells = Db.brain_cells_for_norm(norm)

    source_pos = Enum.filter(source_cells, &(&1.pos == pos))
    stored_pos = Enum.filter(stored_cells, &(&1.pos == pos))

    source_ids = MapSet.new(Enum.map(source_pos, & &1.id))
    stored_ids = MapSet.new(Enum.map(stored_pos, & &1.id))

    IO.inspect(%{
      source_pos_senses: length(source_pos),
      stored_pos_senses: length(stored_pos),
      source_ids_missing_in_db: MapSet.difference(source_ids, stored_ids) |> MapSet.to_list() |> Enum.sort(),
      db_ids_absent_from_source: MapSet.difference(stored_ids, source_ids) |> MapSet.to_list() |> Enum.sort()
    }, label: "POS identity census", pretty: true, width: 120)

    case DefinitionOnlyBackfill.plan(norm, pos, source_cells, stored_cells) do
      {:error, reason} ->
        IO.inspect(reason, label: "BLOCKED: definition-only plan failed", pretty: true, width: 120)
        IO.puts("No database rows changed.")
        System.halt(2)

      {:ok, plan} ->
        IO.inspect(%{
          word: norm,
          pos: pos,
          total_reviewed_senses: plan.row_count,
          definitions_needing_extension: plan.patch_count,
          unchanged_senses: plan.row_count - plan.patch_count,
          review_fingerprint: plan.fingerprint
        }, label: "Review plan", pretty: true, width: 120)

        rows =
          if show_all? do
            plan.rows
          else
            Enum.take(plan.patches, preview)
          end

        IO.puts("\n=== #{if(show_all?, do: "ALL source and stored senses", else: "Preview only")} ===")

        Enum.each(rows, fn row ->
          IO.inspect(%{
            id: row.id,
            changed?: row.changed?,
            old_definition: row.old_definition,
            proposed_definition: row.new_definition,
            example: row.example
          }, pretty: true, width: 120, limit: :infinity)
        end)

        if not show_all? and plan.patch_count > length(rows) do
          IO.puts("Showing #{length(rows)} of #{plan.patch_count} proposed changes.")
          IO.puts("Run with --show-all before making any repair decision.")
        end

        IO.puts("\nNo database rows changed.")
        IO.puts("Positional sense IDs alone do not establish immutable source identity.")
    end
end
