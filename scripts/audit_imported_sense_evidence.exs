# Compare existing BrainCell rows with current Wiktextract JSONL population,
# read-only. The exact source-word case matters: "Cash" is POS "name",
# while "cash" has common noun/verb senses.
#
#   mix run scripts/audit_imported_sense_evidence.exs --word Cash
#   mix run scripts/audit_imported_sense_evidence.exs --word cash
#
# Does not insert/update/delete rows. Source-index changes may have occurred,
# so an ID match here is NOT by itself proof of permanent semantic identity.

defmodule Symbrella.AuditImportedSenseEvidence do
  @moduledoc false

  def run(argv) do
    {opts, _, invalid} =
      OptionParser.parse(argv, strict: [word: :string])

    if invalid != [] do
      raise ArgumentError, "Unknown options: #{inspect(invalid)}"
    end

    word = Keyword.get(opts, :word, "Cash")

    case Db.JSONL.populate_structs(word) do
      {:error, reason} ->
        IO.inspect(reason, label: "Wiktextract source unavailable")

      {:ok, expected} ->
        stored =
          word
          |> Db.brain_cells_for_norm()
          |> Map.new(&{&1.id, &1})

        IO.puts("\n=== BrainCell evidence preservation: #{inspect(word)} ===")
        IO.puts("Mode: READ ONLY; source sense indices are positional.")
        IO.puts("Expected source senses: #{length(expected)}")
        IO.puts("Existing DB senses for norm: #{map_size(stored)}")

        Enum.each(expected, fn regenerated ->
          original = Map.get(stored, regenerated.id)

          if original == nil do
            IO.inspect(%{id: regenerated.id, status: :missing_in_db,
              expected_definition: regenerated.definition},
              label: "Sense", pretty: true, width: 120)
          else
            old_cats = categories(original.semantic_atoms)
            new_cats = categories(regenerated.semantic_atoms)

            if original.definition != regenerated.definition or
                 MapSet.new(old_cats) != MapSet.new(new_cats) do
              IO.inspect(%{
                id: regenerated.id,
                stored_definition: original.definition,
                reconstructed_definition: regenerated.definition,
                stored_categories: old_cats,
                reconstructed_categories: new_cats,
                stored_synonyms: original.synonyms,
                reconstructed_synonyms: regenerated.synonyms
              }, label: "Differing source evidence", pretty: true, width: 120,
                limit: :infinity)
            end
          end
        end)

        IO.puts("\nNo database changes made.")
        IO.puts("Do NOT run a bulk upsert to repair these until source-ID")
        IO.puts("stability and referenced senses have been verified.")
    end
  end

  defp categories(atoms) do
    atoms
    |> List.wrap()
    |> Enum.filter(&(is_binary(&1) and String.starts_with?(&1, "cat:")))
    |> Enum.sort()
  end
end

Symbrella.AuditImportedSenseEvidence.run(System.argv())
