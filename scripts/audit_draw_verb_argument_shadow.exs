# READ-ONLY DB-backed coverage check for the isolated argument observer.
#
#   mix run scripts/audit_draw_verb_argument_shadow.exs
#
# This uses ALL stored BrainCell lexical senses, irrespective of "active" /
# "inactive" lifecycle status. It constructs tokens as a controlled structural
# probe and DOES NOT call Core.resolve_input or change production LIFG scoring.
# Emitted :candidate is an unverified hypothesis, NOT syntactic ground truth.

alias Core.Syntax.VerbArgumentShadow

scenarios = [
  {"They draw a playing card.", 1, {:candidate, :strong}},
  {"They draw a sword from its sheath.", 1, {:candidate, :strong}},
  {"They draw water from a well.", 1, {:candidate, :weak}},
  {"They draw the curtains closed.", 1, {:candidate, :strong}},
  {"The artist will draw with a pencil.", 3, {:abstain, nil}},
  {"They draw a picture.", 1, {:candidate, :strong}},
  {"They draw from a well.", 1, {:abstain, nil}},
  {"They draw.", 1, {:abstain, nil}}
]

tokenize = fn sentence ->
  Regex.scan(~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*/u, sentence)
  |> Enum.map(&hd/1)
  |> Enum.with_index()
  |> Enum.map(fn {word, index} -> %{phrase: word, index: index, n: 1} end)
end

words =
  scenarios
  |> Enum.flat_map(fn {sentence, _verb_index, _expected} ->
    Enum.map(tokenize.(sentence), &String.downcase(&1.phrase))
  end)
  |> Enum.uniq()

inventory = Map.new(words, &{&1, Db.brain_cells_for_norm(&1)})

IO.puts("\n=== draw verb argument observer — DB-backed SHADOW ===")
IO.puts("READ ONLY. Existing BrainCell lifecycle values are not filtered or modified.")
IO.puts("Constructed tokens are NOT an actual Core pipeline trace.")
IO.puts("The intended outcomes are hypotheses, NOT gold semantic labels.")

results =
  Enum.map(scenarios, fn {sentence, verb_index, expected} ->
    tokens = tokenize.(sentence)

    cells =
      tokens
      |> Enum.flat_map(&Map.get(inventory, String.downcase(&1.phrase), []))
      |> Enum.uniq_by(& &1.id)

    result = VerbArgumentShadow.observe(tokens, verb_index, cells)
    actual = {result.status, Map.get(result, :strength)}
    matched? = actual == expected

    IO.puts("\n--- #{inspect(sentence)} ---")
    IO.inspect(%{
      expected: expected,
      observed: actual,
      agreement?: matched?,
      reason: Map.get(result, :reason),
      roles: Map.get(result, :roles),
      source_lexical_cells: length(cells),
      lifecycle_statuses: Enum.frequencies_by(cells, & &1.status),
      verified?: result.verified?,
      rejects_intransitive?: result.rejects_intransitive?
    }, label: "Unverified argument observation", pretty: true, width: 120)

    %{sentence: sentence, expected: expected, observed: actual, agreement?: matched?}
  end)

IO.puts("\n=== Structural-probe summary ===")
IO.inspect(%{
  scenarios: length(results),
  agreed_with_expected_hypothesis: Enum.count(results, & &1.agreement?),
  different_from_expected_hypothesis: Enum.count(results, &(not &1.agreement?)),
  production_stage1_called?: false,
  database_updated?: false
}, pretty: true)

IO.puts("\nNo persistent lifecycle values were modified.")
IO.puts("A successful probe is still not proof of general grammatical reliability.")
