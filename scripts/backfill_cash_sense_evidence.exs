# Guarded restoration for the eight Cash proper-noun senses only.
# Dry-run / human review:
#   mix run scripts/backfill_cash_sense_evidence.exs
#
# Explicitly approved update, only AFTER reviewing the full plan:
#   mix run scripts/backfill_cash_sense_evidence.exs \
#     --apply --fingerprint <sha256-from-the-dry-run>
#
# The optional Mix separator form is also accepted:
#   mix run scripts/backfill_cash_sense_evidence.exs -- \
#     --apply --fingerprint <sha256-from-the-dry-run>
#
# The script will NOT renumber sense IDs, update other words/POS, change
# activation, embeddings, status, connections or relations. An unexpected
# original gloss, example, grammar tag, or ID set aborts the entire plan.
# Back up the DB before applying any migrations or manual updates.

alias Db.JSONL.SenseEvidenceBackfill

# With a script filename, Mix can pass a literal leading "--" through
# System.argv/0. Remove only this separator; never discard unknown options.
argv =
  case System.argv() do
    ["--" | args] -> args
    args -> args
  end

{opts, rest, invalid} =
  OptionParser.parse(argv,
    strict: [apply: :boolean, fingerprint: :string]
  )

if invalid != [] or rest != [] do
  raise ArgumentError,
    "Usage: mix run scripts/backfill_cash_sense_evidence.exs [--apply --fingerprint SHA256]"
end

apply? = Keyword.get(opts, :apply, false)
fingerprint = Keyword.get(opts, :fingerprint)

if apply? and (not is_binary(fingerprint) or String.length(fingerprint) != 64) do
  raise ArgumentError, "--apply requires the SHA256 fingerprint from a reviewed dry-run"
end

{:ok, source} = Db.JSONL.populate_structs("Cash")
stored = Db.brain_cells_for_norm("cash")

case SenseEvidenceBackfill.plan("Cash", "proper_noun", source, stored) do
  {:error, reason} ->
    IO.inspect(reason, label: "Backfill BLOCKED: source/store discrepancy")
    System.halt(2)

  {:ok, plan} ->
    # Eight source name senses were independently inspected in the original
    # Wiktextract dump. If the source changes, redo that review first.
    if plan.row_count != 8 or plan.patch_count != 8 do
      IO.inspect({plan.row_count, plan.patch_count},
        label: "Backfill BLOCKED: unexpected Cash proper-noun census"
      )

      System.halt(2)
    end

    IO.puts("\n=== Cash Wiktextract sense evidence backfill ===")
    IO.puts("Target: word='Cash', database POS='proper_noun'")
    IO.puts("STRICT: 8 source senses, exactly 8 proposed row updates")
    IO.puts("Row count: #{plan.row_count}; rows to update: #{plan.patch_count}")
    IO.puts("Only definition and semantic_atoms (+updated_at) may change.")
    IO.puts("IDs, examples, grammar, synonyms, status and learning state remain intact.")

    Enum.each(plan.rows, fn row ->
      IO.inspect(%{
        id: row.id,
        current_definition: row.stored_definition,
        proposed_definition: row.reconstructed_definition,
        added_categories: row.added_categories,
        changed?: row.changed?
      }, label: "Sense review", pretty: true, width: 120, limit: :infinity)
    end)

    IO.puts("\nReview fingerprint (SHA256): #{plan.fingerprint}")

    if apply? do
      case SenseEvidenceBackfill.apply(plan, fingerprint) do
        {:ok, count} ->
          IO.puts("SUCCESS: updated exactly #{count} sense evidence rows.")

        {:error, reason} ->
          IO.inspect(reason, label: "Backfill aborted, transaction rolled back")
          System.halt(2)
      end
    else
      IO.puts("\nDRY RUN: NO DATABASE WRITES PERFORMED.")
      IO.puts("Review each sense and back up your DB before approving a write.")
      IO.puts("Do not run bulk Db.JSONL.upsert_word/import_all for this repair.")
    end
end
