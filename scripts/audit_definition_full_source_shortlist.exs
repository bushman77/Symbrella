# Full-source, read-only verification of selected word/POS groups.
# Unlike audit_definition_batch_candidates.exs, no contiguous source run
# is ever presumed to be the complete dictionary entry for a word.
#
#   mix run scripts/audit_definition_full_source_shortlist.exs \
#     --targets cat:noun,point:noun,draw:verb
#
#   mix run scripts/audit_definition_full_source_shortlist.exs \
#     --targets cat:noun,point:noun,draw:verb --preview 2
#
# Parses 1,419,337+ source lines ONCE for up to 12 selected normalized words.
# No --apply option. No database modifications. Passing the same strict
# DefinitionOnlyBackfill.plan/4 is a review candidate, not write approval.

alias Db.JSONL.{
  DefinitionBatchAudit,
  DefinitionOnlyBackfill,
  DefinitionShortlistReview,
  Util
}

args = case System.argv() do
  ["--" | rest] -> rest
  rest -> rest
end

{opts, rest, invalid} =
  OptionParser.parse(args, strict: [targets: :string, preview: :integer])

if rest != [] or invalid != [] do
  raise ArgumentError,
    "Usage: mix run scripts/audit_definition_full_source_shortlist.exs " <>
      "--targets cat:noun,point:noun,draw:verb [--preview 0..10]"
end

spec = Keyword.get(opts, :targets)
preview = Keyword.get(opts, :preview, 2)

unless is_binary(spec) and is_integer(preview) and preview >= 0 and preview <= 10 do
  raise ArgumentError, "Require --targets WORD:POS,... and --preview 0..10"
end

targets =
  spec
  |> String.split(",", trim: true)
  |> Enum.map(fn pair ->
    case String.split(pair, ":", parts: 2) do
      [word, pos] ->
        {Util.normalize_word(word), pos |> String.trim() |> String.downcase()}

      _ ->
        {"", ""}
    end
  end)

if targets == [] or length(targets) > 12 or
     Enum.any?(targets, fn {word, pos} -> word == "" or pos == "" end) or
     length(targets) != length(Enum.uniq(targets)) do
  raise ArgumentError, "Require 1..12 unique WORD:POS targets with nonempty names"
end

words = targets |> Enum.map(&elem(&1, 0)) |> Enum.uniq()
path = Db.JSONL.IO.default_path()
unless File.regular?(path), do: raise("Missing source JSONL #{path}")

IO.puts("\n=== Full-source definition shortlist verification ===")
IO.puts("Mode: READ ONLY. No --apply option.")
IO.puts("Source: #{path}")
IO.puts("Targets: #{inspect(targets)}")
IO.puts("All normalized/capitalization variants are collected across the ENTIRE source.")
IO.puts("A successful plan requires exact ID census, original gloss prefix and metadata.")

case DefinitionShortlistReview.collect(File.stream!(path, [], :line), words) do
  {:error, reason} ->
    IO.inspect(reason, label: "Source completeness failure")
    System.halt(2)

  {:ok, by_word, stats} ->
    IO.inspect(stats, label: "Completed full-source scan", pretty: true)

    summaries =
      Enum.map(targets, fn {word, pos} ->
        entries = Map.fetch!(by_word, word)
        variants = Enum.frequencies_by(entries, &Map.get(&1, "word"))
        {source, _} = DefinitionBatchAudit.source_cells(entries, %{})
        stored = Db.brain_cells_for_norm(word)
        source_pos = Enum.filter(source, &(&1.pos == pos))
        stored_pos = Enum.filter(stored, &(&1.pos == pos))

        source_ids = MapSet.new(Enum.map(source_pos, & &1.id))
        stored_ids = MapSet.new(Enum.map(stored_pos, & &1.id))

        census = %{
          source_pos_senses: length(source_pos),
          stored_pos_senses: length(stored_pos),
          source_ids_absent_from_db:
            source_ids |> MapSet.difference(stored_ids) |> MapSet.to_list() |> Enum.sort(),
          db_ids_absent_from_source:
            stored_ids |> MapSet.difference(source_ids) |> MapSet.to_list() |> Enum.sort()
        }

        IO.puts("\n--- #{word}|#{pos} ---")
        IO.inspect(variants, label: "Source capitalization variants")
        IO.inspect(census, label: "Complete word/POS ID census", pretty: true, width: 120)

        case DefinitionOnlyBackfill.plan(word, pos, source, stored) do
          {:ok, plan} ->
            IO.inspect(%{
              verdict: :preflight_pass,
              total_senses: plan.row_count,
              definitions_needing_extension: plan.patch_count,
              unchanged_senses: plan.row_count - plan.patch_count,
              fingerprint_for_separate_review: plan.fingerprint
            }, label: "Result", pretty: true, width: 120)

            Enum.each(Enum.take(plan.patches, preview), fn row ->
              IO.inspect(%{
                id: row.id,
                old: row.old_definition,
                proposed: row.new_definition,
                example: row.example
              }, label: "Preview", pretty: true, limit: :infinity, width: 120)
            end)

            %{word: word, pos: pos, verdict: :preflight_pass,
              senses: plan.row_count, patches: plan.patch_count}

          {:error, reason} ->
            IO.inspect(reason, label: "Result: BLOCKED", pretty: true, limit: 12, width: 120)
            %{word: word, pos: pos, verdict: :blocked, reason: reason}
        end
      end)

    IO.puts("\n=== Full-source shortlist summary ===")
    Enum.each(summaries, &IO.inspect(&1, pretty: true, limit: 12, width: 120))
    IO.puts("\nNO DATABASE ROWS CHANGED.")
    IO.puts("Do not treat fingerprints as authorization for a bulk update.")
end
