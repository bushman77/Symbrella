# Run at umbrella root: mix run priv/finite_past_locative_live_probe.exs
#
# Observational only: the integration adds no writes or pipeline attachments.
# Core.resolve_input/2 itself is the usual production pipeline and may have
# its existing runtime side effects (memory, telemetry, etc.).
# Does not modify dictionary rows, LIFG winners, or source files.

alias Core.Semantic.Composition.{
  FinitePastLocative,
  FinitePastLocativeIntegration,
  LemmaGroundedPredicate,
  RoleWitness
}

sentence = "The bird sat on the fence."

si =
  Core.resolve_input(sentence,
    fuzzy: false,
    neuron_activation?: false,
    notify_brain?: false,
    lifg_opts: [
      scores: :all,
      synapse_eval_full_scores?: true,
      spatial_pp_senses?: false
    ]
  )

signatures = List.wrap(Map.get(si, :sentence_signatures))
predicate = LemmaGroundedPredicate.discover(si, 2)

rows =
  predicate
  |> Map.get(:requested_lemmas, [])
  |> Enum.take(4)
  |> Enum.flat_map(&Db.brain_cells_for_norm/1)

grounded = LemmaGroundedPredicate.ground(predicate, rows)
derivation = FinitePastLocativeIntegration.derive(si)
result =
  case derivation do
    {:ok, proof} ->
      %{status: :derived, proof_verified?: FinitePastLocative.verify(proof) == :ok,
        event_semantics: proof.event_semantics,
        selected_lifg_id: proof.premises.selected_lifg_id,
        factual_authority?: proof.factual_authority?}

    :not_applicable ->
      %{status: :not_applicable}
  end

IO.inspect(%{
  sentence: sentence,
  token_surfaces: si |> Map.get(:tokens, []) |> List.wrap() |>
    Enum.filter(&(is_map(&1) and Map.get(&1, :n, 1) == 1)) |>
    Enum.sort_by(&Map.get(&1, :index)) |>
    Enum.map(&Map.take(&1, [:index, :phrase])),
  finite_past_signatures: Enum.filter(signatures,
    &(&1.id == :finite_past_locative_candidate)),
  lifg_predicate_choice: Enum.find(List.wrap(Map.get(si, :lifg_choices)),
    &(is_map(&1) and Map.get(&1, :token_index) == 2)) |>
    case do
      nil -> nil
      choice -> Map.take(choice, [:id, :token_index])
    end,
  morphology_status: Map.get(predicate, :status),
  requested_lemmas: Map.get(predicate, :requested_lemmas, []),
  form_witness_count: length(Map.get(predicate, :forms, [])),
  lemma_dictionary_rows: length(rows),
  grounding_status: Map.get(grounded, :status),
  seated_posture_candidates: grounded |> Map.get(:feature_hypotheses, []) |>
    Enum.count(&(Map.get(&1, :feature) == :seated_posture)),
  integration: result
}, label: "Finite-past locative — real pipeline diagnostic", limit: :infinity)

# Explicitly expose every abstention gate, without overriding a witness.
roles = [
  {:subject_determiner, 0, "determiner"},
  {:subject_head, 1, "noun"},
  {:preposition, 3, "preposition"},
  {:location_determiner, 4, "determiner"},
  {:location_head, 5, "noun"}
]

role_checks =
  Enum.map(roles, fn {role, index, pos} ->
    {role, case RoleWitness.resolve(si, role, index, [pos]) do
      {:ok, witness} -> %{status: :admitted,
        selection_basis: Map.get(witness, :selection_basis)}
      :unsupported -> RoleWitness.audit(si, role, index, [pos])
    end}
  end)
  |> Map.new()

signature =
  Enum.find(signatures, &(&1.id == :finite_past_locative_candidate and
                           &1.span == {0, 5}))

direct_composition =
  if signature do
    case FinitePastLocative.compose(%{
      signature: signature, grounding: grounded, predicate_surface: "sat",
      actor: "bird", preposition: "on", landmark: "fence"
    }) do
      {:ok, proof} ->
        %{status: :derived, verified?: FinitePastLocative.verify(proof) == :ok}
      :unsupported -> %{status: :unsupported}
    end
  else
    %{status: :missing_signature}
  end

IO.inspect(%{
  role_checks: role_checks,
  saved_signature_matches_detection?:
    signature in List.wrap(Map.get(si, :sentence_signatures)),
  direct_composition: direct_composition,
  integration: result
}, label: "Finite-past locative — remaining admission gates", limit: :infinity)
