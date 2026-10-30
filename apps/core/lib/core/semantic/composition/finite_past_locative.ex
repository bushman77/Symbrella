defmodule Core.Semantic.Composition.FinitePastLocative do
  @moduledoc """
  Pure, bounded finite-past locative property composer.

  Accepts an externally prepared grammatical signature and lemma-grounding
  report. Verifies their structural correspondence, but does not attest that
  the reports originated from a current SemanticInput. A later integration
  boundary must independently revalidate the evidence against that input.

  Success proves a typed *possible-event property*, not the truth of an
  utterance, the intended dictionary sense, or a resolved referent.
  """

  alias Core.Semantic.Composition.{Expression, Lexical, Proof, Reducer}

  @required_evidence [:contiguous_surface, :attested_nominal_subject,
    :attested_simple_past_verb, :attested_preposition, :attested_nominal_location]
  @static_relations ~w(on in under beside above below)
  @event_property {:arrow, :event, :truth}
  # Explicit lemma-to-feature admission policy. Extend only with independently
  # validated predicate evidence; a morphological form alone is insufficient.
  @predicate_features %{"sit" => :seated_posture}

  @spec compose(map()) :: {:ok, map()} | :unsupported
  def compose(%{signature: signature, grounding: grounding, actor: actor,
                preposition: prep, landmark: landmark} = input) do
    with true <- valid_signature?(signature),
         {:ok, lemma, form, feature} <- morphological_support(grounding, input),
         {:ok, %{term: action_term, symbol: action_symbol}} <- Lexical.canonical_action(lemma),
         {:ok, %{term: actor_term, symbol: actor_symbol}} <- Lexical.actor(actor),
         {:ok, %{term: past_operator}} <- Lexical.past_event(),
         {:ok, %{term: actor_operator}} <- Lexical.actor_modifier(),
         true <- prep in @static_relations,
         {:ok, %{term: relation_term, symbol: relation}} <- Lexical.spatial_modifier(prep),
         {:ok, %{term: landmark_term, symbol: landmark_symbol}} <- Lexical.landmark(landmark) do
      initial =
        {:apply, {:apply, relation_term, landmark_term},
         {:apply, {:apply, actor_operator, actor_term},
          {:apply, past_operator, action_term}}}

      with {:ok, @event_property} <- Expression.type_of(initial),
           {:ok, result, steps} <- Reducer.reduce(initial),
           :ok <- Proof.verify(%{initial: initial, result: result,
                                 steps: steps, result_type: @event_property}),
           true <- normal_form?(result, action_symbol, actor_symbol, relation, landmark_symbol) do
        {:ok, %{
          kind: :finite_past_locative_property,
          status: :derived,
          construction: :finite_past_locative_candidate,
          initial: initial,
          steps: steps,
          result: result,
          result_type: @event_property,
          premises: %{signature: signature, form_witness: form,
                      lemma_feature_candidate: feature,
                      selected_lifg_id: grounding.selected_lifg_id},
          event_semantics: %{
            tense: :past,
            predicate: action_symbol,
            actor: %{kind: actor_symbol, referent: :unresolved},
            spatial: %{relation: relation, landmark_kind: landmark_symbol,
                       arrival: :not_established},
            event_existence: :not_asserted
          },
          epistemic_status: :conditional_on_supplied_evidence,
          semantic_compatibility_proven?: false,
          factual_authority?: false
        }}
      else
        _ -> :unsupported
      end
    else
      _ -> :unsupported
    end
  end

  def compose(_), do: :unsupported

  @doc "Replay the mathematical proof and recompute the expected normal form."
  @spec verify(map()) :: :ok | {:error, :invalid_derivation}
  def verify(%{kind: :finite_past_locative_property, premises: premises} = proof) do
    input = %{
      signature: Map.get(premises, :signature),
      grounding: %{
        status: :predicate_feature_candidates,
        surface: get_in(premises, [:form_witness, :surface]),
        selected_lifg_id: Map.get(premises, :selected_lifg_id),
        forms: [Map.get(premises, :form_witness)],
        feature_hypotheses: [Map.get(premises, :lemma_feature_candidate)]
      },
      predicate_surface: get_in(premises, [:form_witness, :surface]),
      actor: get_in(proof, [:event_semantics, :actor, :kind]) |> symbol_word(),
      preposition: get_in(proof, [:event_semantics, :spatial, :relation]) |> symbol_word(),
      landmark: get_in(proof, [:event_semantics, :spatial, :landmark_kind]) |> symbol_word()
    }

    case compose(input) do
      {:ok, recomputed} ->
        if proof == recomputed, do: :ok, else: {:error, :invalid_derivation}
      _ -> {:error, :invalid_derivation}
    end
  end

  def verify(_), do: {:error, :invalid_derivation}

  defp valid_signature?(%{id: :finite_past_locative_candidate,
                           source: :core_syntax, span: {0, 5},
                           roles: roles, evidence: evidence})
       when is_map(roles) and is_list(evidence) do
    roles == %{subject_determiner: 0, subject_head: 1, predicate: 2,
               preposition: 3, location_determiner: 4, location_head: 5} and
      Enum.all?(@required_evidence, &(&1 in evidence))
  end

  defp valid_signature?(_), do: false

  defp morphological_support(
         %{status: :predicate_feature_candidates, surface: surface,
           selected_lifg_id: selected_id, forms: forms,
           feature_hypotheses: features},
         %{predicate_surface: surface})
       when is_binary(surface) and is_binary(selected_id) and
            is_list(forms) and is_list(features) do
    valid_forms = Enum.filter(forms, fn
      %{surface: ^surface, lemma: lemma, inflection: inflection,
        source: :attested_scored_form_of_dictionary_sense,
        morphological_only?: true, inflected_sense_id: id, score: score}
        when is_binary(lemma) and
             inflection in ["simple past", "simple past and past participle"] and
             is_binary(id) and is_number(score) and score >= 0 ->
        Map.has_key?(@predicate_features, lemma) and
          String.starts_with?(id, surface <> "|verb|")
      _ -> false
    end)

    matching_features = Enum.filter(features, fn
      %{feature: feature, source_lemma: lemma,
        source: :lemma_dictionary_definition,
        epistemic_status: :candidate_only, source_lemma_sense_id: id}
        when is_binary(lemma) and is_binary(id) ->
        Map.get(@predicate_features, lemma) == feature and
          String.starts_with?(id, lemma <> "|verb|")
      _ -> false
    end)

    case {valid_forms, matching_features} do
      {[form], [_ | _] = candidates} ->
        matching = Enum.filter(candidates, &(&1.source_lemma == form.lemma))
        case matching do
          [] -> :unsupported
          _ ->
            feature = Enum.min_by(matching, & &1.source_lemma_sense_id)
            {:ok, form.lemma, form, feature}
        end
      _ -> :unsupported
    end
  end

  defp morphological_support(_, _), do: :unsupported

  defp normal_form?(
         {:lambda, event, :event,
          {:and,
           {:and,
            {:and,
             {:event_action, {:action_kind, action}, {:var, e1}},
             {:event_tense, :past, {:var, e2}}},
            {:event_actor_kind, {:actor_kind, actor}, {:var, e3}}},
           {:event_spatial, relation, {:landmark_kind, landmark}, {:var, e4}}}},
         action, actor, relation, landmark)
       when is_binary(event) and event == e1 and event == e2 and
            event == e3 and event == e4, do: true

  defp normal_form?(_, _, _, _, _), do: false

  defp symbol_word(value) when is_atom(value), do: Atom.to_string(value)
  defp symbol_word(_), do: nil
end
