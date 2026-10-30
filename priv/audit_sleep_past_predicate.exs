# Run from the umbrella root:
#   mix run priv/audit_sleep_past_predicate.exs
#
# Read-only PostgreSQL dictionary audit. This does not authorize "sleep" in
# finite-past composition or change dictionary/LIFG/model state.

alias Core.Semantic.Composition.LemmaGroundedPredicate

surface_rows = Db.brain_cells_for_norm("slept")
lemma_rows = Db.brain_cells_for_norm("sleep")

form_rows =
  Enum.filter(surface_rows, fn row ->
    Map.get(row, :pos) == "verb" and
      is_binary(Map.get(row, :definition)) and
      String.contains?(String.downcase(row.definition), "past") and
      String.contains?(String.downcase(row.definition), "sleep")
  end)

# Construct the minimal current-turn evidence required by discovery, using
# actual returned rows and explicit positive scores. This audits dictionary
# compatibility, NOT real LIFG scoring or grammatical admission.
observed =
  Enum.map(form_rows, fn row ->
    Map.put(row, :token_index, 0)
  end)

scores =
  observed
  |> Enum.map(fn row -> {row.id, 1.0} end)
  |> Map.new()

probe = %{
  tokens: [%{index: 0, n: 1, phrase: "slept"}],
  active_cells: observed,
  lifg_choices: [%{
    token_index: 0,
    id: case observed do
      [%{id: id} | _] -> id
      _ -> "slept|unknown|0"
    end,
    candidate_probs: scores
  }]
}

discovery = LemmaGroundedPredicate.discover(probe, 0)
grounded = LemmaGroundedPredicate.ground(discovery, lemma_rows)

IO.inspect(%{
  surface: "slept",
  lemma: "sleep",
  surface_dictionary_rows: length(surface_rows),
  surface_verb_rows: Enum.count(surface_rows, &(Map.get(&1, :pos) == "verb")),
  attested_past_form_rows: Enum.map(form_rows,
    &Map.take(&1, [:id, :definition, :semantic_atoms])),
  lemma_dictionary_rows: length(lemma_rows),
  lemma_verb_definitions: lemma_rows
    |> Enum.filter(&(Map.get(&1, :pos) == "verb"))
    |> Enum.map(&Map.take(&1, [:id, :definition])),
  morphological_discovery_status: discovery.status,
  requested_lemmas: Map.get(discovery, :requested_lemmas, []),
  grounded_status: grounded.status,
  candidate_features: Map.get(grounded, :feature_hypotheses, [])
    |> Enum.map(&Map.take(&1, [:feature, :source_lemma, :source_lemma_sense_id,
                                 :supporting_text]))
}, label: "Sleep/slept PostgreSQL evidence audit", limit: :infinity, printable_limit: :infinity)
