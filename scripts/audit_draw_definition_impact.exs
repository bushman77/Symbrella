# Read-only, BEFORE/AFTER *shadow* evidence analysis for draw|verb.
#
# IMPORTANT: run BEFORE applying 138 definition updates to measure the
# database's actual current state against the proposed (in-memory) glosses:
#
#   mix run scripts/audit_draw_definition_impact.exs
#
# Does NOT write to DB, alter a Stage1 winner, run any model, or train.
# The source is the FULL 1.4M-line Wiktextract source, including variants.
# DefinitionOnlyBackfill.plan/4 must accept the complete word/POS census.
# The projection changes ONLY the definition in a COPY of each stored row.
# Other lexical/cognitive state is bit-for-bit unchanged.
#
# This tests deterministic ContextFit and the SemanticControl *eligibility
# diagnostic*, not actual LIFG Stage1 choice accuracy. Sentence hypotheses
# are manually selected examples, not certified gold evaluation labels.

alias Db.JSONL.{
  DefinitionBatchAudit,
  DefinitionOnlyBackfill,
  DefinitionWordReview
}
alias Brain.LIFG.DefinitionImpactAudit

scenarios = [
  {"They draw a picture.", "draw|verb|129"},
  {"They draw a comparison.", "draw|verb|131"},
  {"She will draw a formal document.", "draw|verb|133"},
  {"They draw money from the bank.", "draw|verb|110"},
  {"The circuit will draw power.", "draw|verb|106"},
  {"They draw a conclusion.", "draw|verb|109"},
  {"They draw a playing card.", "draw|verb|95"},
  {"Workers draw metal into wire.", "draw|verb|65"},
  {"The artist will draw with a pencil.", "draw|verb|138"},
  {"They draw a sword from its sheath.", "draw|verb|79"},
  {"They draw water from a well.", "draw|verb|81"},
  {"They draw the curtains closed.", "draw|verb|5"},
  {"They draw the bowstring.", "draw|verb|20"}
]

path = Db.JSONL.IO.default_path()
unless File.regular?(path), do: raise("Missing source JSONL: #{path}")

IO.puts("\n=== draw|verb deterministic definition-evidence shadow ===")
IO.puts("READ ONLY: no database writes or production LIFG selection.")
IO.puts("All other BrainCell fields held constant in shadow projection.")
IO.puts("Sentence labels are hypotheses, NOT ground-truth sense annotations.")

case DefinitionWordReview.collect(File.stream!(path, [], :line), "draw") do
  {:error, reason} ->
    IO.inspect(reason, label: "Full source scan BLOCKED")
    System.halt(2)

  {:ok, entries, stats} ->
    {source, _} = DefinitionBatchAudit.source_cells(entries, %{})
    stored = Db.brain_cells_for_norm("draw")

    case DefinitionOnlyBackfill.plan("draw", "verb", source, stored) do
      {:error, reason} ->
        IO.inspect(reason, label: "BLOCKED: source/stored sense mismatch")
        System.halt(2)

      {:ok, plan} ->
        IO.inspect(%{
          scanned_source_lines: stats.total_lines,
          matching_word_entries: stats.source_entries,
          verb_senses: plan.row_count,
          definitions_pending_extension: plan.patch_count,
          current_review_fingerprint: plan.fingerprint
        }, label: "Exact current plan")

        old_verb = Enum.filter(stored, &(&1.pos == "verb"))
        new_verb = Enum.filter(source, &(&1.pos == "verb"))

        reports =
          Enum.map(scenarios, fn {sentence, expected_id} ->
            case DefinitionImpactAudit.compare(
              sentence, "draw", expected_id, old_verb, new_verb
            ) do
              {:ok, comparison} ->
                IO.puts("\n--- #{inspect(sentence)} ---")
                IO.inspect(%{
                  hypothesized_id: expected_id,
                  before_rank: comparison.before.hypothesized_id_rank,
                  after_rank: comparison.projected.hypothesized_id_rank,
                  before_fit: comparison.before.expected_fit,
                  after_fit: comparison.projected.expected_fit,
                  fit_delta: comparison.expected_fit_delta,
                  before_top: comparison.before.top_id,
                  after_top: comparison.projected.top_id,
                  before_control_eligible?: comparison.before.semantic_control_eligible?,
                  after_control_eligible?: comparison.projected.semantic_control_eligible?,
                  after_unique_definition_cues:
                    comparison.projected.expected_definition_matches
                }, label: "ContextFit shadow comparison", pretty: true, width: 120)
                comparison

              {:error, reason} ->
                IO.inspect({sentence, reason}, label: "Comparison BLOCKED")
                System.halt(2)
            end
          end)

        IO.puts("\n=== Paired shadow summary (NOT accuracy) ===")
        IO.inspect(%{
          evaluated_sentences: length(reports),
          expected_sense_fit_increased:
            Enum.count(reports, &(&1.expected_fit_delta > 0.0)),
          expected_sense_fit_unchanged:
            Enum.count(reports, &(&1.expected_fit_delta == 0.0)),
          expected_sense_fit_decreased:
            Enum.count(reports, &(&1.expected_fit_delta < 0.0)),
          expected_sense_rank_improved:
            Enum.count(reports, fn r ->
              r.projected.hypothesized_id_rank < r.before.hypothesized_id_rank
            end),
          original_context_gate_eligible:
            Enum.count(reports, & &1.before.semantic_control_eligible?),
          projected_context_gate_eligible:
            Enum.count(reports, & &1.projected.semantic_control_eligible?),
          production_stage1_accuracy_measured?: false
        }, pretty: true, width: 120)

        IO.puts("\nNO DATABASE ROWS CHANGED.")
        IO.puts("Definition evidence can change without improving the Stage1 winner.")
        IO.puts("Do not infer correctness or train from hypothesis labels.")
    end
end
