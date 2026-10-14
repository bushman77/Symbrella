# Shadow-only, zero-write, dictionary-backed one-hop synonym bridge audit.
#
#   mix run scripts/audit_lexical_bridges.exs
#
# Works with inactive dictionary rows as well as active ones. It does NOT
# change production Core.Relations behavior or Brain.LIFG scoring/selection.

defmodule Symbrella.AuditLexicalBridges do
  alias Brain.LIFG.ContextFit
  alias Brain.LIFG.LexicalBridges

  @scenarios [
    {"I deposited cash at the bank.", "bank"},
    {"The city approved a new housing development.", "development"},
    {"Japan is developing an electromagnetic railgun to launch projectiles.", "railgun"},
    {"What do you know about Japan's railgun development?", "development"}
  ]

  def run do
    IO.puts("\n=== Lexical synonym bridge hypotheses (SHADOW / read-only) ===")
    IO.puts("Sources are dictionary sense rows, including INACTIVE rows.")
    IO.puts("Edges indicate possible relationships, not verified semantic meaning.")
    IO.puts("No LIFG winners, activation, or database records are changed.")

    Enum.each(@scenarios, fn {sentence, target} ->
      source_words =
        ContextFit.explain(sentence, target, %{}).context_terms
        |> Enum.take(12)

      sources =
        source_words
        |> Map.new(fn word ->
          {word, Db.brain_cells_for_norm(word)}
        end)

      report =
        LexicalBridges.analyze(
          sentence, target, Db.brain_cells_for_norm(target), sources
        )

      IO.puts("\n--- #{inspect(sentence)} ---")
      IO.puts("Target #{inspect(target)}: #{report.target_sense_count} dictionary senses")
      IO.inspect(report.context_inventory, label: "Context-word dictionary availability",
        pretty: true, width: 110, limit: :infinity)
      IO.inspect(report.bridges, label: "Candidate sense-to-sense lexical bridges",
        pretty: true, width: 110, limit: :infinity)
      IO.puts("Bridge hypotheses: #{report.bridge_count}; verified: false")
    end)
  end
end

Symbrella.AuditLexicalBridges.run()
