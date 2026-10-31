# Run from the Symbrella umbrella root:
#   mix run priv/verify_sleep_past_default.exs
#
# Uses normal production defaults against real PostgreSQL dictionary evidence.
# The existing Core.resolve_input pipeline may perform its usual runtime writes.

alias Core.Semantic.Composition.{FinitePastLocative, LemmaGroundedPredicate}

sentence = "The bird slept on the fence."
si = Core.resolve_input(sentence)
proof = si.finite_past_locative_derivation
discovered = LemmaGroundedPredicate.discover(si, 2)

selected =
  si.lifg_choices
  |> List.wrap()
  |> Enum.find(&(Map.get(&1, :token_index) == 2))

report = %{
  sentence: sentence,
  status: if(is_map(proof), do: Map.get(proof, :status), else: nil),
  proof_valid?: is_map(proof) and FinitePastLocative.verify(proof) == :ok,
  predicate: if(is_map(proof), do: get_in(proof, [:event_semantics, :predicate]), else: nil),
  event_existence:
    if(is_map(proof), do: get_in(proof, [:event_semantics, :event_existence]), else: nil),
  factual_authority?: if(is_map(proof), do: Map.get(proof, :factual_authority?), else: nil),
  selected_lifg_id: if(is_map(selected), do: Map.get(selected, :id), else: nil),
  form_discovery: discovered.status,
  requested_lemmas: Map.get(discovered, :requested_lemmas, []),
  original_composition_kind:
    if(is_map(si.compositional_derivation),
      do: Map.get(si.compositional_derivation, :kind),
      else: nil)
}

IO.inspect(report, label: "Sleep/slept default-on production verification")

unless report.status == :derived and report.proof_valid? and
         report.predicate == :sleep and report.event_existence == :not_asserted and
         report.factual_authority? == false and
         report.requested_lemmas == ["sleep"] do
  raise "Sleep/slept default-on production verification failed; inspect report"
end
