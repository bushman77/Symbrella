# Read-only, shadow-only dictionary-gloss lexical bridge audit.
#
#   mix run scripts/audit_lexical_gloss_bridges.exs
#
# Uses even inactive lexical rows because the primary dictionary contains
# inactive definitions/examples. Nothing is trained, activated or persisted.

defmodule Symbrella.AuditLexicalGlossBridges do
  alias Brain.LIFG.ContextFit
  alias Brain.LIFG.GlossBridges

  @scenarios [
    {"I deposited cash at the bank.", "bank"},
    {"The city approved a new housing development.", "development"},
    {"Japan is developing an electromagnetic railgun to launch projectiles.", "railgun"},
    {"What do you know about Japan's railgun development?", "development"}
  ]

  def run do
    IO.puts("\n=== Dictionary-definition lexical bridge audit (SHADOW) ===")
    IO.puts("Source: exact BrainCell sense definitions, including inactive records.")
    IO.puts("Overlapping gloss words are only bridge hypotheses, NOT correct sense labels.")
    IO.puts("No DB writes, LIFG winner changes, or neural training.\n")

    Enum.each(@scenarios, fn {sentence, target} ->
      terms =
        ContextFit.explain(sentence, target, %{}).context_terms
        |> Enum.take(12)

      source_rows = Map.new(terms, &{&1, Db.brain_cells_for_norm(&1)})
      targets = Db.brain_cells_for_norm(target)
      report = GlossBridges.analyze(sentence, target, targets, source_rows)

      IO.puts("\n--- #{inspect(sentence)} ---")
      IO.inspect(%{
        target: report.target,
        target_senses: report.target_sense_count,
        source_senses: report.source_sense_count,
        hypothesis_count: report.edge_count,
        verified?: report.verified?
      }, label: "Coverage")

      IO.inspect(
        report.edges
        |> Enum.take(12)
        |> Enum.map(fn edge ->
          Map.take(edge, [
            :context_word, :context_sense_id, :context_status,
            :context_sense_count, :target_sense_id,
            :lexical_fit, :evidence, :verified?
          ])
        end),
        label: "Top lexical hypotheses",
        pretty: true, width: 110, limit: :infinity
      )
    end)
  end
end

Symbrella.AuditLexicalGlossBridges.run()
