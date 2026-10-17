# Read-only, complete-source before/projected/syntax evidence comparison.
#
#   mix run scripts/audit_draw_verb_argument_impact.exs \
#     | tee tmp/draw_verb_argument_impact.txt
#
# NO DB writes, NO production Stage1 calls, NO BrainCell lifecycle changes.
# Uses the SAME stored sense rows in all three conditions. Only their glosses
# are replaced in the projected copies; then a bounded, positive-only syntax
# hint is added in a separate shadow scoring pass.
# Sentence expectations are human hypotheses, not certified gold labels.

alias Db.JSONL.{
  DefinitionBatchAudit,
  DefinitionOnlyBackfill,
  DefinitionWordReview
}
alias Brain.LIFG.VerbArgumentShadowScoring, as: Score
alias Core.Syntax.VerbArgumentShadow, as: Observe

scenarios = [
  {"They draw a playing card.", 1, "draw|verb|95", "draw|verb|120"},
  {"They draw a sword from its sheath.", 1, "draw|verb|79", "draw|verb|112"},
  {"They draw water from a well.", 1, "draw|verb|81", "draw|verb|113"},
  {"They draw the curtains closed.", 1, "draw|verb|5", "draw|verb|36"},
  {"The artist will draw with a pencil.", 3, "draw|verb|138", "draw|verb|129"},
  {"They draw a picture.", 1, "draw|verb|129", "draw|verb|138"}
]

tokenize = fn sentence ->
  Regex.scan(~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*/u, sentence)
  |> Enum.map(&hd/1)
  |> Enum.with_index()
  |> Enum.map(fn {word, index} -> %{phrase: word, index: index, n: 1} end)
end

words =
  scenarios
  |> Enum.flat_map(fn {sentence, _, _, _} ->
    Enum.map(tokenize.(sentence), &String.downcase(&1.phrase))
  end)
  |> Enum.uniq()

inventory = Map.new(words, &{&1, Db.brain_cells_for_norm(&1)})

path = Db.JSONL.IO.default_path()
unless File.regular?(path), do: raise("Missing source JSONL: #{path}")

IO.puts("\n=== draw grammar + definition impact (SHADOW) ===")
IO.puts("READ ONLY: no DB writes, no lifecycle mutations, no Stage1 winner changes.")
IO.puts("Before: original definitions. Projected: reconstructed definitions.")
IO.puts("Grammar: same projected cells + bounded, positive-only syntax hypothesis.")
IO.puts("Targets are review hypotheses, NOT certified gold labels.")

case DefinitionWordReview.collect(File.stream!(path, [], :line), "draw") do
  {:error, reason} ->
    IO.inspect(reason, label: "BLOCKED: incomplete source scan")
    System.halt(2)

  {:ok, entries, stats} ->
    {source, _} = DefinitionBatchAudit.source_cells(entries, %{})
    stored = Db.brain_cells_for_norm("draw")

    case DefinitionOnlyBackfill.plan("draw", "verb", source, stored) do
      {:error, reason} ->
        IO.inspect(reason, label: "BLOCKED: full source sense mismatch")
        System.halt(2)

      {:ok, plan} ->
        IO.inspect(%{
          source_lines: stats.total_lines,
          matching_source_entries: stats.source_entries,
          verb_senses: plan.row_count,
          pending_recovery: plan.patch_count,
          full_source_fingerprint: plan.fingerprint
        }, label: "Full-source guard")

        old_cells = Enum.filter(stored, &(&1.pos == "verb"))
        source_by_id =
          source
          |> Enum.filter(&(&1.pos == "verb"))
          |> Map.new(&{&1.id, &1.definition})

        projected_cells =
          Enum.map(old_cells, fn cell ->
            %{cell | definition: Map.fetch!(source_by_id, cell.id)}
          end)

        blank = %{status: :abstain, reason: :syntax_not_used}

        rank = fn result, id ->
          case Enum.find_index(result.candidates, &(&1.id == id)) do
            nil -> nil
            zero_based -> zero_based + 1
          end
        end

        find = fn result, id ->
          case Enum.find(result.candidates, &(&1.id == id)) do
            nil -> raise("Missing expected sense ID #{id}")
            row -> row
          end
        end

        pair_win = fn result, expected_id, rival_id ->
          expected = find.(result, expected_id)
          rival = find.(result, rival_id)

          cond do
            expected.combined > rival.combined -> :hypothesized_sense_ahead
            expected.combined < rival.combined -> :rival_ahead
            true -> :tie
          end
        end

        outcomes =
          Enum.map(scenarios, fn {sentence, verb_index, expected_id, rival_id} ->
            tokens = tokenize.(sentence)

            sentence_cells =
              tokens
              |> Enum.flat_map(&Map.get(inventory, String.downcase(&1.phrase), []))
              |> Enum.uniq_by(& &1.id)

            observation = Observe.observe(tokens, verb_index, sentence_cells)
            baseline = Score.score(sentence, "draw", verb_index, old_cells, blank)
            projected = Score.score(sentence, "draw", verb_index, projected_cells, blank)
            grammatical = Score.score(sentence, "draw", verb_index, projected_cells, observation)

            expected_baseline = find.(baseline, expected_id)
            expected_projected = find.(projected, expected_id)
            expected_grammatical = find.(grammatical, expected_id)
            rival_grammatical = find.(grammatical, rival_id)

            outcome = %{
              sentence: sentence,
              hypothesized_id: expected_id,
              competitor_id: rival_id,
              observation: {observation.status, Map.get(observation, :strength)},
              no_intransitive_exclusion?: observation.rejects_intransitive? == false,
              before_rank_all_verb_senses: rank.(baseline, expected_id),
              reconstructed_rank_all_verb_senses: rank.(projected, expected_id),
              grammar_rank_all_verb_senses: rank.(grammatical, expected_id),
              before_expected_fit: expected_baseline.lexical,
              reconstructed_expected_fit: expected_projected.lexical,
              grammar_expected_bonus: expected_grammatical.grammar_bonus,
              grammar_competitor_bonus: rival_grammatical.grammar_bonus,
              before_pair: pair_win.(baseline, expected_id, rival_id),
              reconstructed_pair: pair_win.(projected, expected_id, rival_id),
              grammar_pair: pair_win.(grammatical, expected_id, rival_id),
              reconstructed_top_id: hd(projected.candidates).id,
              grammar_top_id: hd(grammatical.candidates).id,
              adjusted_candidates: grammatical.adjusted_senses
            }

            IO.puts("\n--- #{inspect(sentence)} ---")
            IO.inspect(outcome, label: "Paired lexical + grammar hypothesis",
              pretty: true, width: 120)

            outcome
          end)

        IO.puts("\n=== SHADOW summary (not accuracy) ===")
        IO.inspect(%{
          scenarios: length(outcomes),
          grammar_observed_transitive_hypotheses:
            Enum.count(outcomes, fn o -> elem(o.observation, 0) == :candidate end),
          grammar_abstentions:
            Enum.count(outcomes, fn o -> elem(o.observation, 0) == :abstain end),
          reconstructed_pair_hypothesis_ahead:
            Enum.count(outcomes, &(&1.reconstructed_pair == :hypothesized_sense_ahead)),
          syntax_pair_hypothesis_ahead:
            Enum.count(outcomes, &(&1.grammar_pair == :hypothesized_sense_ahead)),
          grammar_full_slate_rank_improvements:
            Enum.count(outcomes, fn o ->
              o.grammar_rank_all_verb_senses < o.reconstructed_rank_all_verb_senses
            end),
          all_intransitive_senses_preserved?:
            Enum.all?(outcomes, & &1.no_intransitive_exclusion?),
          production_stage1_accuracy_measured?: false
        }, pretty: true, width: 120)

        IO.puts("NO DB ROWS CHANGED. No persistent lifecycle status updated.")
        IO.puts("Positive grammar evidence is a hypothesis, not an assertion of correctness.")
    end
end
