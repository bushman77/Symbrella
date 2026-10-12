# Read-only lexical sense inventory for a grammatical verb-particle construction.
#
#   mix run scripts/inspect_verb_particle_senses.exs
#
# This does not call Core.resolve_input/2 or persist cognition state.
# Retrieves exact entries from the existing dictionary and displays per-sense
# lexical ContextFit evidence. No senses are inserted or modified.

defmodule Symbrella.InspectVerbParticleSenses do
  alias Brain.LIFG.ContextFit

  @phrases ["turn off", "switch off", "power down"]
  @sentences [
    "Turn the lights off.",
    "Turn off the lights.",
    "He lights off the rocket.",
    "Turn off the employees.",
    "Switch the fan off."
  ]

  def run do
    for phrase <- @phrases do
      rows =
        Db.brain_cells_for_norm(phrase)
        |> Enum.filter(&(&1.pos == "verb"))
        |> Enum.sort_by(& &1.id)

      IO.puts("\n=== VERIFIED DICTIONARY: #{phrase} (#{length(rows)} verb senses) ===")

      if rows == [] do
        IO.puts("No matching verb sense exists in the local DB.")
      end

      Enum.each(rows, fn row ->
        IO.inspect(%{
          id: row.id,
          status: row.status,
          activation: row.activation,
          definition: row.definition,
          example: row.example,
          gram_function: row.gram_function,
          synonyms: row.synonyms,
          semantic_atoms: Enum.take(row.semantic_atoms || [], 12)
        }, pretty: true, width: 100)
      end)

      for sentence <- @sentences,
          String.contains?(String.downcase(sentence), hd(String.split(phrase))) do
        IO.puts("\n--- Context: #{inspect(sentence)} ---")

        ranked =
          Enum.map(rows, fn row ->
            fit = ContextFit.explain(sentence, phrase, row)

            %{
              id: row.id,
              score: fit.score,
              context_terms: fit.context_terms,
              matches: fit.matches,
              contributions: fit.contributions,
              definition: row.definition
            }
          end)
          |> Enum.sort_by(fn row -> {-row.score, row.id} end)

        IO.inspect(ranked, pretty: true, width: 100, limit: :infinity)
      end
    end
  end
end

Symbrella.InspectVerbParticleSenses.run()
