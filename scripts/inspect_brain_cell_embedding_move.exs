# scripts/inspect_brain_cell_embedding_move.exs
#
# Purpose:
#   First safe embedding-pipeline inspection script.
#
# What this does:
#   - verifies BrainCell embedding coverage
#   - fetches one exact BrainCell sense by id
#   - builds the stable sense_text payload
#   - checks whether that exact sense needs an embedding
#   - previews missing embedding candidates for the same norm
#
# What this does NOT do yet:
#   - does not train a model
#   - does not create an Nx tensor
#   - does not write fake embeddings
#   - does not mutate the database
#
# Run:
#   mix run scripts/inspect_brain_cell_embedding_move.exs
#
# Or:
#   mix run scripts/inspect_brain_cell_embedding_move.exs --id "there|noun|0" --limit 10
{:ok, _} = Application.ensure_all_started(:db)

{opts, _argv, invalid} =
  OptionParser.parse(System.argv(),
    switches: [
      id: :string,
      limit: :integer
    ]
  )

if invalid != [] do
  IO.puts("""
  Invalid options:

  #{inspect(invalid, pretty: true)}

  Valid options:
    --id "there|noun|0"
    --limit 10
  """)

  System.halt(1)
end

id = Keyword.get(opts, :id, "there|noun|0")
limit = Keyword.get(opts, :limit, 5)

section = fn title ->
  IO.puts("\n" <> String.duplicate("=", 80))
  IO.puts(title)
  IO.puts(String.duplicate("=", 80))
end

section.("Global BrainCell embedding coverage")

Db.BrainCellEmbeddings.coverage()
|> IO.inspect(label: "coverage", pretty: true)

section.("Exact BrainCell sense lookup")

cell =
  case Db.BrainCellEmbeddings.get_cell(id) do
    nil ->
      IO.puts("No BrainCell found for id: #{inspect(id)}")
      System.halt(1)

    cell ->
      cell
  end

IO.inspect(
  %{
    id: cell.id,
    word: cell.word,
    norm: cell.norm,
    pos: cell.pos,
    status: cell.status,
    embedding_present: not is_nil(cell.embedding),
    definition: cell.definition,
    example: cell.example,
    semantic_atoms: cell.semantic_atoms
  },
  label: "selected_cell",
  pretty: true,
  limit: :infinity
)

section.("Exact sense embedding state")

IO.inspect(
  %{
    id: cell.id,
    embedded?: Db.BrainCellEmbeddings.embedded?(cell.id),
    needs_embedding?: Db.BrainCellEmbeddings.needs_embedding?(cell.id)
  },
  label: "embedding_state",
  pretty: true
)

section.("Stable sense_text payload")

cell
|> Db.BrainCellEmbeddings.sense_text()
|> IO.puts()

section.("Embedding coverage for norm")

Db.BrainCellEmbeddings.coverage_for_norm(cell.norm)
|> IO.inspect(label: "coverage_for_norm: #{cell.norm}", pretty: true)

section.("Missing embedding candidates for same norm")

cell.norm
|> Db.BrainCellEmbeddings.missing_cells_for_norm(limit: limit)
|> Enum.map(fn candidate ->
  %{
    id: candidate.id,
    norm: candidate.norm,
    pos: candidate.pos,
    status: candidate.status,
    embedding_present: not is_nil(candidate.embedding),
    definition: candidate.definition,
    example: candidate.example
  }
end)
|> IO.inspect(label: "missing_for_norm", pretty: true, limit: :infinity)

section.("Result")

IO.puts("""
Embedding inspection completed.

Next conceptual move:
  sense_text
    -> tokenizer / embedding backend
    -> Nx tensor
    -> f32 embedding vector
    -> Db.BrainCellEmbeddings.put_embedding(id, vector)

No database writes were performed by this script.
""")
