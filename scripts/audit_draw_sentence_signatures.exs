# Read-only inspection of existing Core sentence-signature observations.
#
#   mix run scripts/audit_draw_sentence_signatures.exs
#
# Deliberately constructs *minimal synthetic SI tokens* for a small set of
# example sentences and gets POS alternatives from existing DB BrainCells.
# Uses ALL stored dictionary senses as an upper-bound parser capability
# check. The stored status flag is lifecycle, not a lexical eligibility gate.
# This is not a production pipeline trace.
# No changes to Core signatures, LIFG, DB, or learned state.

alias Core.Syntax.Signatures

scenarios = [
  {"They draw a playing card.", ["draw|verb|95", "draw|verb|120"]},
  {"They draw a sword from its sheath.", ["draw|verb|79", "draw|verb|112"]},
  {"They draw water from a well.", ["draw|verb|81", "draw|verb|113"]},
  {"They draw the curtains closed.", ["draw|verb|5", "draw|verb|36"]},
  {"The artist will draw with a pencil.", ["draw|verb|138", "draw|verb|129"]},
  {"They draw a picture.", ["draw|verb|129", "draw|verb|138"]}
]

tokenize = fn sentence ->
  Regex.scan(~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*/u, sentence)
  |> Enum.map(&hd/1)
  |> Enum.with_index()
  |> Enum.map(fn {word, index} ->
    %{phrase: word, index: index, n: 1}
  end)
end

words =
  scenarios
  |> Enum.flat_map(fn {sentence, _} ->
    tokenize.(sentence)
    |> Enum.map(&String.downcase(&1.phrase))
  end)
  |> Enum.uniq()

inventory = Map.new(words, fn word -> {word, Db.brain_cells_for_norm(word)} end)

IO.puts("\n=== draw sentence-signature coverage audit ===")
IO.puts("READ ONLY. No DB writes or production Stage1 calls.")
IO.puts("Constructed tokens and raw DB lexicon are a structural probe,")
IO.puts("NOT a trace of the actual Core.resolve_input pipeline.")
IO.puts("BrainCell.status is a lifecycle field; it is NOT used to filter lexical POS evidence.")

Enum.each(scenarios, fn {sentence, ids} ->
  tokens = tokenize.(sentence)

  all_cells =
    tokens
    |> Enum.flat_map(fn token ->
      Map.get(inventory, String.downcase(token.phrase), [])
    end)
    |> Enum.uniq_by(& &1.id)

  # Source lexical POS alternatives regardless of their lifecycle status.
  # The :active_cells key is a Core API field name, not a status filter.
  IO.puts("\n--- #{inspect(sentence)} ---")
  IO.inspect(Enum.map(tokens, &{&1.index, &1.phrase}), label: "Original token indices")

  IO.inspect(
    all_cells
    |> Enum.filter(&(&1.id in ids))
    |> Enum.map(&%{id: &1.id, status: &1.status, grammar: &1.gram_function})
    |> Enum.sort_by(& &1.id),
    label: "Competing stored senses"
  )

  Enum.each([{:all_stored_pos_evidence, all_cells}], fn {view, cells} ->
    observed = Signatures.detect(%{
      sentence: sentence,
      tokens: tokens,
      active_cells: cells
    })

    frames =
      observed
      |> Enum.map(fn frame ->
        Map.take(frame, [:id, :span, :roles, :evidence])
      end)

    argument_frames =
      Enum.filter(frames, fn frame ->
        frame.id in [:verb_object_clause, :transitive_clause, :intransitive_clause]
      end)

    IO.inspect(%{
      view: view,
      cells_supplied: length(cells),
      observed_frame_count: length(observed),
      argument_structure_frames: argument_frames,
      observed_frames: frames
    }, label: "Core.Syntax.Signatures.detect/1", pretty: true, limit: :infinity, width: 120)
  end)
end)

IO.puts("\nNo database rows changed.")
IO.puts("No inference of direct-object presence from raw adjacent nouns.")
IO.puts("Absent argument frames mean the existing signatures cannot establish")
IO.puts("transitivity here. An argument-structure observer must be separately")
IO.puts("validated before enabling any Stage1 score adjustment.")
