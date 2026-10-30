# Run from the umbrella root:
#   mix run priv/lambda_sense_sensitivity.exs
#
# Counterfactual, observation-only probe. Resolves one real sentence, then
# changes only the LIFG choice ID for "on" in ephemeral SI copies.
# This DOES NOT rerun LIFG or validate the hypothetical rankings.
# It measures whether the downstream lambda composition is sensitive
# to an otherwise-attested sense change.
#
# Output only: priv/lambda_sense_sensitivity_results.jsonl

alias Core.Semantic.Composition
alias Core.Syntax.StructuralCompetition

sentence = "The bird is sitting on the fence."

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

tokens = List.wrap(Map.get(si, :tokens))
choices = List.wrap(Map.get(si, :lifg_choices))
cells = List.wrap(Map.get(si, :active_cells))

on_token =
  Enum.find(tokens, fn token ->
    Map.get(token, :n, 1) == 1 and
      String.downcase(Map.get(token, :phrase, "")) == "on"
  end)

unless is_map(on_token) do
  raise "Expected the original sentence to contain a unigram 'on' token"
end

index = on_token.index

selected =
  Enum.find(choices, &(Map.get(&1, :token_index) == index))

unless is_map(selected) do
  raise "Expected the original LIFG choice at the 'on' token"
end

cell_by_id =
  cells
  |> Enum.filter(&(is_map(&1) and is_binary(Map.get(&1, :id))))
  |> Map.new(&{&1.id, &1})

on_candidates =
  selected
  |> Map.get(:candidate_probs, %{})
  |> case do
    %{} = scored -> Map.keys(scored)
    _ -> []
  end
  |> Enum.filter(&String.starts_with?(&1, "on|preposition|"))
  |> Enum.filter(&Map.has_key?(cell_by_id, &1))

if on_candidates == [] do
  raise "No attested, scored 'on' preposition candidates; check full score opt-in"
end

# This samples contrasts; no particular sense is treated as a gold label.
targets =
  [selected.id, "on|preposition|1", "on|preposition|12",
   "on|preposition|19", "on|preposition|39"]
  |> Enum.uniq()

variants = [{:original, selected.id} | Enum.map(targets, &{:substituted, &1})]

inspect_variant = fn {mode, sense_id} ->
  sense = Map.get(cell_by_id, sense_id)

  if is_nil(sense) or sense_id not in on_candidates do
    %{
      mode: mode, requested_id: sense_id, status: :missing_attested_scored_sense
    }
  else
    changed_choices =
      Enum.map(choices, fn choice ->
        if Map.get(choice, :token_index) == index and mode == :substituted do
          choice
          |> Map.put(:id, sense_id)
          |> Map.put(:pos, sense.pos)
          |> Map.put(:definition, Map.get(sense, :definition))
        else
          choice
        end
      end)

    # The comparison regenerates structural and lambda results from the
    # modified SI. All substitutions remain in memory only.
    adjusted =
      si
      |> Map.put(:lifg_choices, changed_choices)
      |> StructuralCompetition.attach()
      |> Composition.attach()

    verification =
      case Composition.verified(adjusted) do
        {:ok, proof} ->
          witness =
            proof
            |> Map.get(:premises, %{})
            |> Map.get(:lexical_witnesses, [])
            |> Enum.find(&(Map.get(&1, :role) == :preposition))

          %{
            lambda_status: :verified,
            proof_steps: length(proof.steps),
            result_type: inspect(proof.result_type),
            relation: get_in(proof, [:event_semantics, :spatial, :relation]),
            witness_sense_id: if(is_map(witness), do: Map.get(witness, :sense_id), else: nil),
            witness_basis: if(is_map(witness), do: Map.get(witness, :selection_basis), else: nil),
            lambda_term: proof.initial,
            sense_alignment: Core.Semantic.Composition.SenseAlignment.audit(proof, sense)
          }

        :not_applicable ->
          %{lambda_status: :not_applicable}
      end

    %{
      mode: mode,
      requested_id: sense_id,
      definition: Map.get(sense, :definition),
      structural_status: Map.get(Map.get(adjusted, :structural_competition) || %{}, :status),
      proof: verification
    }
  end
end

results = Enum.map(variants, inspect_variant)

reference =
  Enum.find(results, fn result ->
    result.mode == :original and
      get_in(result, [:proof, :lambda_status]) == :verified
  end)

report =
  Enum.map(results, fn result ->
    term = get_in(result, [:proof, :lambda_term])

    result
    |> update_in([:proof], fn
      %{lambda_term: _} = proof -> Map.delete(proof, :lambda_term)
      proof -> proof
    end)
    |> Map.put(:same_lambda_term_as_original?,
      if(reference && term,
        do: term == get_in(reference, [:proof, :lambda_term]),
        else: nil))
  end)

file = "priv/lambda_sense_sensitivity_results.jsonl"
File.write!(file, Enum.map_join(report, "", &(Jason.encode!(&1) <> "\n")))

IO.inspect(%{
  original_selected: selected.id,
  original_definition: Map.get(cell_by_id[selected.id], :definition),
  scored_on_senses: length(on_candidates),
  rows: Enum.map(report, fn result ->
    Map.take(result, [:mode, :requested_id, :definition,
      :structural_status, :proof, :same_lambda_term_as_original?])
  end),
  saved_to: file
}, label: "Lambda sense-dependency counterfactual", limit: :infinity)
