# Guarded, DEFINITION-ONLY Wiktextract backfill pilot for "free" adjectives.
#
# Step 1: read-only plan + complete sense review:
#   mix run scripts/backfill_free_definitions.exs
#
# Step 2 (ONLY after independent review, passing tests and DB backup):
#   mix run scripts/backfill_free_definitions.exs --apply --fingerprint SHA256
#
# The only mutable columns are brain_cells.definition and updated_at.
# Semantic categories, usage labels, other metadata, sense IDs, relations,
# embeddings, activation, connections, learning state and status are untouched.
# Incorrect counts/IDs, gloss prefixes or sense metadata abort without writes.

alias Db.JSONL.DefinitionOnlyBackfill

args =
  case System.argv() do
    ["--" | rest] -> rest
    rest -> rest
  end

{opts, rest, invalid} =
  OptionParser.parse(args, strict: [apply: :boolean, fingerprint: :string])

if rest != [] or invalid != [] do
  raise ArgumentError,
    "Usage: mix run scripts/backfill_free_definitions.exs " <>
      "[--apply --fingerprint SHA256]"
end

apply? = Keyword.get(opts, :apply, false)
approved = Keyword.get(opts, :fingerprint)

if apply? and
     (not is_binary(approved) or not Regex.match?(~r/\A[0-9a-f]{64}\z/, approved)) do
  raise ArgumentError, "--apply requires the reviewed 64-character SHA256 fingerprint"
end

case Db.JSONL.populate_structs("free") do
  {:error, reason} ->
    IO.inspect(reason, label: "No source senses, no database changes")

  {:ok, source} ->
    stored = Db.brain_cells_for_norm("free")

    case DefinitionOnlyBackfill.plan("free", "adjective", source, stored) do
      {:error, reason} ->
        IO.inspect(reason, label: "Backfill BLOCKED — investigate sense provenance")
        IO.puts("No database rows changed.")
        System.halt(2)

      {:ok, plan} ->
        IO.puts("\n=== Definition-only pilot: free|adjective ===")
        IO.puts("Read-only plan by default. No semantic category changes.")
        IO.puts("Source senses: #{plan.row_count}")
        IO.puts("Definitions needing extension: #{plan.patch_count}")
        IO.puts("Positional IDs alone do NOT prove sense ordering is stable.")
        IO.puts("Review every row's original gloss, full gloss and example.")

        Enum.each(plan.rows, fn row ->
          IO.inspect(%{
            id: row.id,
            changed?: row.changed?,
            old_definition: row.old_definition,
            proposed_definition: row.new_definition,
            original_example: row.example
          }, label: "Sense review", pretty: true, limit: :infinity, width: 120)
        end)

        IO.puts("\nReview fingerprint (SHA256): #{plan.fingerprint}")

        cond do
          not apply? ->
            IO.puts("\nDRY RUN: no database writes.")
            IO.puts("Run tests and back up PostgreSQL before approving.")

          plan.patch_count == 0 ->
            IO.puts("Nothing to update; no database writes.")

          true ->
            case DefinitionOnlyBackfill.apply(plan, approved) do
              {:ok, count} ->
                IO.puts("SUCCESS: updated #{count} definition fields.")

              {:error, reason} ->
                IO.inspect(reason, label: "Transaction aborted; no partial updates")
                System.halt(2)
            end
        end
    end
end
