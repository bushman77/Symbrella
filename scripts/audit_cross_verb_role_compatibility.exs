# Read-only, synthetic-fixture Core -> Brain semantic-role evaluation.
#
#   mix run scripts/audit_cross_verb_role_compatibility.exs \
#     | tee tmp/cross_verb_role_compatibility.txt
#
# Unlike the full-source draw audit, this is an isolated cross-verb regression
# probe. The provided lexical rows and definitions are human-curated TEST
# fixtures, NOT real stored dictionary senses or correctness ground truth.
# No Repo writes, training, memory updates or production Stage1 calls.

alias Core.Syntax.VerbArgumentShadow
alias Core.Syntax.VerbRoleShadow
alias Brain.LIFG.VerbArgumentShadowScoring
alias Brain.LIFG.VerbRoleShadowFit

lexicon =
  [
    {"draw", "verb"}, {"pull", "verb"}, {"remove", "verb"},
    {"withdraw", "verb"}, {"water", "noun"}, {"well", "noun"},
    {"sword", "noun"}, {"sheath", "noun"}, {"money", "noun"},
    {"bank", "noun"}
  ]
  |> Enum.map(fn {norm, pos} ->
    %{norm: norm, pos: pos, status: "inactive"}
  end)

cases = [
  {"They draw water from a well.", "draw", "To take (water) out of a well.", true},
  {"They draw water out of a well.", "draw", "To take water from a well.", true},
  {"They pull a sword out of its sheath.", "pull", "To remove a sword from its sheath.", true},
  {"They remove a sword from its sheath.", "remove", "To pull a sword out of its sheath.", true},
  {"They withdraw money from a bank.", "withdraw", "To take money out of the bank.", true},
  {"They withdraw money into a bank.", "withdraw", "To take money from a bank.", false},
  {"They draw water. From a well.", "draw", "To take water from a well.", false},
  {"They draw water (from a well).", "draw", "To take water from a well.", false},
  {"They pull a sword out, of its sheath.", "pull", "To remove a sword from its sheath.", false}
]

tokenize = fn sentence ->
  Regex.scan(~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*|[\p{P}\p{S}]/u, sentence)
  |> Enum.map(&hd/1)
  |> Enum.with_index()
  |> Enum.map(fn {phrase, index} -> %{phrase: phrase, index: index, n: 1} end)
end

results =
  Enum.map(cases, fn {sentence, verb, positive_gloss, expect_role_bonus?} ->
    tokens = tokenize.(sentence)
    verb_index = 1
    argument = VerbArgumentShadow.observe(tokens, verb_index, lexicon)
    roles = VerbRoleShadow.observe(tokens, verb_index, lexicon)

    cells = [
      %{
        id: "#{verb}|verb|1", pos: "verb", norm: verb, status: "inactive",
        gram_function: ["transitive"], definition: positive_gloss,
        example: nil, synonyms: [], semantic_atoms: []
      },
      %{
        id: "#{verb}|verb|2", pos: "verb", norm: verb, status: "inactive",
        gram_function: ["intransitive"],
        definition: "An unrelated meaning concerning water or vessels.",
        example: nil, synonyms: [], semantic_atoms: []
      },
      %{
        id: "#{verb}|verb|3", pos: "verb", norm: verb, status: "inactive",
        gram_function: ["transitive"],
        definition: "Another meaning about an object without its source.",
        example: nil, synonyms: [], semantic_atoms: []
      }
    ]

    grammar = VerbArgumentShadowScoring.score(
      sentence, verb, verb_index, cells, argument)
    semantic = VerbRoleShadowFit.score(
      sentence, verb, verb_index, cells, argument, roles)

    target = Enum.find(semantic.candidates, &(&1.id == "#{verb}|verb|1"))
    bonus = target.role_bonus
    observed? = bonus > 0.0

    report = %{
      sentence: sentence,
      grammatical_argument: {argument.status, Map.get(argument, :strength)},
      source_relation: {roles.status, Map.get(roles, :words)},
      semantic_role_matched?: observed?,
      expected_role_match?: expect_role_bonus?,
      grammar_only_top: hd(grammar.candidates).id,
      role_aware_top: hd(semantic.candidates).id,
      candidate_specific_bonus: bonus,
      total_role_adjusted_senses: semantic.role_adjusted_senses,
      no_lifecycle_changes?: Enum.all?(cells, &(&1.status == "inactive")),
      agreed?: observed? == expect_role_bonus? and
        semantic.role_adjusted_senses == if(expect_role_bonus?, do: 1, else: 0)
    }
    IO.puts("\n--- #{inspect(sentence)} ---")
    IO.inspect(report, pretty: true, width: 120)
    report
  end)

IO.puts("\n=== Cross-verb fixture summary (NOT real-sense accuracy) ===")
IO.inspect(%{
  cases: length(results),
  expected_positive: Enum.count(results, & &1.expected_role_match?),
  observed_positive: Enum.count(results, & &1.semantic_role_matched?),
  agreements: Enum.count(results, & &1.agreed?),
  all_lifecycles_unchanged?: Enum.all?(results, & &1.no_lifecycle_changes?),
  db_writes?: false,
  production_stage1_called?: false
}, pretty: true)

unless Enum.all?(results, & &1.agreed?) do
  IO.puts("FAILED: one or more cross-verb fixture predictions diverged.")
  System.halt(1)
end
