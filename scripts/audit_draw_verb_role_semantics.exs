# READ-ONLY role-aware semantic and negative-control audit.
#
#   mix run scripts/audit_draw_verb_role_semantics.exs \
#     | tee tmp/draw_verb_role_semantics.txt
#
# Exactly the same stored 138 verb senses are scored under three conditions:
#   stored original definitions + existing grammar-only shadow;
#   projected full definitions + existing grammar-only shadow;
#   projected full definitions + grammar + candidate-specific V OBJECT from SOURCE.
# No Core/Stage1 scoring changes, memory writes, status filtering or training.
# Human expected IDs are review hypotheses, not gold labels.

alias Db.JSONL.{DefinitionBatchAudit, DefinitionOnlyBackfill, DefinitionWordReview}
alias Brain.LIFG.VerbArgumentShadowScoring, as: GrammarScore
alias Brain.LIFG.VerbRoleShadowFit, as: RoleScore
alias Core.Syntax.VerbArgumentShadow
alias Core.Syntax.VerbRoleShadow

cases = [
  {"They draw water from a well.", 1, "draw|verb|81", "draw|verb|17"},
  {"They draw water out of a well.", 1, "draw|verb|81", "draw|verb|17"},
  {"They draw a sword from its sheath.", 1, "draw|verb|79", "draw|verb|112"},
  {"They draw a playing card.", 1, "draw|verb|95", "draw|verb|120"},
  {"They draw the curtains closed.", 1, "draw|verb|5", "draw|verb|36"},
  {"The artist will draw with a pencil.", 3, "draw|verb|138", "draw|verb|129"},
  {"They draw a picture.", 1, "draw|verb|129", "draw|verb|138"},
  {"They draw water with a well.", 1, "draw|verb|81", "draw|verb|17"},
  {"They draw water. From a well.", 1, "draw|verb|81", "draw|verb|17"},
  {"They draw water (from a well).", 1, "draw|verb|81", "draw|verb|17"},
  {"They draw water — from a well.", 1, "draw|verb|81", "draw|verb|17"},
  {"They draw water from a mysteryword.", 1, "draw|verb|81", "draw|verb|17"}
]

# PRESERVE punctuation and token indices: no fabricated clause adjacency.
tokenize = fn sentence ->
  Regex.scan(~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*|[\p{P}\p{S}]/u, sentence)
  |> Enum.map(&hd/1)
  |> Enum.with_index()
  |> Enum.map(fn {phrase, index} -> %{phrase: phrase, index: index, n: 1} end)
end

words =
  cases
  |> Enum.flat_map(fn {sentence, _, _, _} ->
    tokenize.(sentence)
    |> Enum.map(&String.downcase(&1.phrase))
    |> Enum.filter(&Regex.match?(~r/^[\p{L}\p{N}]/u, &1))
  end)
  |> Enum.uniq()

inventory = Map.new(words, &{&1, Db.brain_cells_for_norm(&1)})

path = Db.JSONL.IO.default_path()
unless File.regular?(path), do: raise("Missing source JSONL: #{path}")

IO.puts("\n=== draw verb: structural role -> exact source-gloss SHADOW ===")
IO.puts("READ ONLY. No production Stage1, lifecycle or database changes.")
IO.puts("Human hypothesized sense IDs are NOT verified truth labels.")
IO.puts("Punctuation is retained to block synthetic cross-clause adjacency.")

case DefinitionWordReview.collect(File.stream!(path, [], :line), "draw") do
  {:error, reason} ->
    IO.inspect(reason, label: "BLOCKED: source scan")
    System.halt(2)

  {:ok, entries, stats} ->
    {source_cells, _} = DefinitionBatchAudit.source_cells(entries, %{})
    stored = Db.brain_cells_for_norm("draw")

    case DefinitionOnlyBackfill.plan("draw", "verb", source_cells, stored) do
      {:error, reason} ->
        IO.inspect(reason, label: "BLOCKED: exact source census")
        System.halt(2)

      {:ok, plan} ->
        IO.inspect(%{
          lines: stats.total_lines,
          entries: stats.source_entries,
          reviewed_senses: plan.row_count,
          pending_repair: plan.patch_count,
          fingerprint: plan.fingerprint
        }, label: "Full-source safety gate")

        old = Enum.filter(stored, &(&1.pos == "verb"))

        reconstructed_by_id =
          source_cells
          |> Enum.filter(&(&1.pos == "verb"))
          |> Map.new(&{&1.id, &1.definition})

        projected =
          Enum.map(old, fn cell ->
            %{cell | definition: Map.fetch!(reconstructed_by_id, cell.id)}
          end)

        find = fn report, id ->
          Enum.find(report.candidates, &(&1.id == id)) ||
            raise("Missing hypothesis sense #{id}")
        end

        rank = fn report, id ->
          index = Enum.find_index(report.candidates, &(&1.id == id))
          if is_nil(index), do: nil, else: index + 1
        end

        output =
          Enum.map(cases, fn {sentence, verb_index, expected_id, rival_id} ->
            tokens = tokenize.(sentence)

            sentence_cells =
              tokens
              |> Enum.flat_map(fn token ->
                Map.get(inventory, String.downcase(token.phrase), [])
              end)
              |> Enum.uniq_by(& &1.id)

            argument = VerbArgumentShadow.observe(tokens, verb_index, sentence_cells)
            roles = VerbRoleShadow.observe(tokens, verb_index, sentence_cells)

            original = GrammarScore.score(sentence, "draw", verb_index, old, argument)
            glossary = GrammarScore.score(sentence, "draw", verb_index, projected, argument)
            enriched = RoleScore.score(sentence, "draw", verb_index, projected, argument, roles)

            old_expected = find.(original, expected_id)
            gloss_expected = find.(glossary, expected_id)
            enriched_expected = find.(enriched, expected_id)
            enriched_rival = find.(enriched, rival_id)

            result = %{
              sentence: sentence,
              expected_id: expected_id,
              rival_id: rival_id,
              argument_observation: {argument.status, Map.get(argument, :strength)},
              source_observation: {roles.status, Map.get(roles, :reason)},
              source_words: Map.get(roles, :words),
              role_observation_accepted?: enriched.role_observation_accepted?,
              original_rank: rank.(original, expected_id),
              projected_rank: rank.(glossary, expected_id),
              role_aware_rank: rank.(enriched, expected_id),
              projected_top: hd(glossary.candidates).id,
              role_aware_top: hd(enriched.candidates).id,
              original_target_score: old_expected.combined,
              projected_target_score: gloss_expected.combined,
              role_target_score: enriched_expected.combined,
              role_target_bonus: enriched_expected.role_bonus,
              role_gloss_match: Map.get(enriched_expected, :role_match),
              rival_role_bonus: enriched_rival.role_bonus,
              role_adjusted_senses: enriched.role_adjusted_senses
            }

            IO.puts("\n--- #{inspect(sentence)} ---")
            IO.inspect(result, label: "Role-aware evidence impact", pretty: true, width: 120)
            result
          end)

        IO.puts("\n=== SHADOW summary (not production accuracy) ===")
        IO.inspect(%{
          scenarios: length(output),
          accepted_source_role_observations:
            Enum.count(output, & &1.role_observation_accepted?),
          sentences_with_exact_role_gloss_matches:
            Enum.count(output, &(&1.role_adjusted_senses > 0)),
          expected_rank_improvements_over_grammar_only:
            Enum.count(output, fn row -> row.role_aware_rank < row.projected_rank end),
          false_source_bonus_on_negative_controls?:
            Enum.any?(Enum.drop(output, 7), &(&1.role_adjusted_senses > 0)),
          production_stage1_called?: false,
          brain_cell_lifecycle_updated?: false,
          db_writes?: false
        }, pretty: true, width: 120)

        IO.puts("No database records modified. No BrainCell lifecycle changes.")
        IO.puts("Source role match is a hypothesis; lexical association alone is not meaning.")
    end
end
