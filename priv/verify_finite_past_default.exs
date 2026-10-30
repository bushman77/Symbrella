# From the Symbrella umbrella root:
#   mix run priv/verify_finite_past_default.exs
#
# Runs the normal production pipeline and checks its default-on,
# independently verified finite-past derivation.

alias Core.Semantic.Composition.FinitePastLocative

si = Core.resolve_input("The bird sat on the fence.")
proof = si.finite_past_locative_derivation

predicate_choice =
  si.lifg_choices
  |> List.wrap()
  |> Enum.find(&(Map.get(&1, :token_index) == 2))

result = %{
  status: if(is_map(proof), do: Map.get(proof, :status), else: nil),
  proof_valid?: is_map(proof) and FinitePastLocative.verify(proof) == :ok,
  original_lifg_choice: if(predicate_choice, do: Map.get(predicate_choice, :id), else: nil),
  original_composition_kind:
    if(is_map(si.compositional_derivation),
      do: Map.get(si.compositional_derivation, :kind),
      else: nil)
}

IO.inspect(result, label: "Finite-past default-on production verification")

expected? =
  result.status == :derived and
    result.proof_valid? and
    result.original_lifg_choice == "sat|noun|1"

if not expected? do
  raise "Finite-past default-on production verification failed"
end
