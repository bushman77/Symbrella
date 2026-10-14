# Read-only lexical evidence audit against Symbrella's existing BrainCell DB.
#
#   mix run scripts/audit_lexical_discrimination.exs
#   LEXICAL_AUDIT_ALL=1 mix run scripts/audit_lexical_discrimination.exs
#
# No Core.resolve_input, LIFG selection, memory updates, or model calls.
# This is an inventory and evidence diagnostic, not a claim of correct meaning.

defmodule Symbrella.AuditLexicalDiscrimination do
  alias Brain.LIFG.LexicalAudit

  @scenarios [
    {
      "What do you know about Japan's railgun development?",
      ["know", "railgun", "development"]
    },
    {
      "Japan is developing an electromagnetic railgun to launch projectiles.",
      ["railgun"]
    },
    {
      "The city approved a new housing development.",
      ["development"]
    },
    {
      "The development of the technology continued.",
      ["development"]
    },
    {
      "I stood on the bank beside the river.",
      ["bank"]
    },
    {
      "I deposited cash at the bank.",
      ["bank"]
    }
  ]

  def run do
    all? = System.get_env("LEXICAL_AUDIT_ALL") == "1"

    IO.puts("\n=== LIFG dictionary sense discriminability audit ===")
    IO.puts("Read-only: reports dictionary evidence, not verified word meanings.")
    IO.puts("The gate shown is the existing Brain.LIFG.SemanticControl gate.")
    IO.puts("Its adjustment eligibility is not a calibrated correctness probability.\n")

    Enum.each(@scenarios, fn {sentence, terms} ->
      IO.puts("\n--- #{inspect(sentence)} ---")

      Enum.each(terms, fn term ->
        senses =
          Db.brain_cells_for_norm(term)
          |> List.wrap()
          |> Enum.filter(&is_map/1)

        report = LexicalAudit.analyze(sentence, term, senses)

        IO.puts("\nWord: #{term} (#{report.candidate_count} dictionary IDs)")

        Enum.each(report.groups, fn group ->
          IO.inspect(Map.take(group, [
            :pos, :sense_count, :top_id, :top_context_fit, :next_context_fit,
            :evidence_gap, :eligible_for_control?, :reason
          ]), label: "Same-POS evidence", pretty: true, width: 110)
        end)

        rows =
          if all?, do: report.senses,
            else: report.senses |> Enum.group_by(& &1.pos) |>
                Enum.flat_map(fn {_pos, grouped} -> Enum.take(grouped, 4) end)

        Enum.each(rows, fn row ->
          matched =
            row.matches
            |> Enum.flat_map(fn {field, terms} ->
              if terms == [], do: [], else: [{field, terms}]
            end)
            |> Enum.sort()

          IO.inspect(%{
            id: row.id,
            status: row.status,
            context_fit: row.context_fit,
            matches: matched,
            definition: compact(row.definition, 180),
            example: compact(row.example, 120)
          }, label: "Sense evidence", pretty: true, width: 110, limit: :infinity)
        end)
      end)
    end)

    IO.puts("\nInterpretation: no eligible winner means no discriminating")
    IO.puts("original-sentence dictionary evidence, NOT that all senses are equivalent.")
    IO.puts("This script does not modify active choices, database senses, or rules.")
  end

  defp compact(value, limit) when is_binary(value) do
    value |> String.trim() |> String.slice(0, limit)
  end

  defp compact(_, _), do: nil
end

Symbrella.AuditLexicalDiscrimination.run()
