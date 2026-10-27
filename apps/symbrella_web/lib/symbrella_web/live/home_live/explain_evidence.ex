defmodule SymbrellaWeb.HomeLive.ExplainEvidence do
  @moduledoc """
  Read-only projection of the exact Core.SemanticInput returned by
  Core.resolve_input/2 for one conversation turn.

  The UI must not infer a phrasal sense from word-level LIFG choices, promote
  a syntax-only MWE hypothesis into dictionary fact, or relabel word senses
  when the canonical semantic cover contains a different MWE choice.

  No DB access, extra inference or response generation takes place here.
  This data is safe to persist with a chat message's Explain payload.
  """

  @spec from_si(map() | term()) :: map()
  def from_si(si) when is_map(si) do
    # Fast-path memory, identity and fallback responses never went through the
    # Core lexical pipeline; showing their stub :intent/:confidence as a Core
    # runtime decision would be a misleading attribution.
    if not substantive_cognition?(si) do
      %{}
    else
      build(si)
    end
  end

  def from_si(_), do: %{}

  defp build(si) do
    comprehension = get(si, :comprehension)

    result = %{
      runtime: runtime(si),
      provisional_interpretation: get(si, :provisional_interpretation),
      discourse_reference: get(si, :discourse_reference),
      verified_phrasal_meanings: verified_meanings(si),
      semantic_winners:
        rows(si, :semantic_choices,
          [:id, :token_index, :token_span, :kind, :lemma, :pos, :definition, :score, :source]
        ),
      lexical_winners:
        rows(si, :lifg_choices,
          [:id, :token_index, :lemma, :pos, :definition, :score, :source,
           :selection_scope, :context_score]
        ),
      comprehension: comprehension(comprehension),
      semantic_sense_shadow: semantic_sense_shadow(si),
      semantic_cohort: semantic_cohort(si),
      sentence_competition: sentence_competition(get(si, :sentence_competition)),
      question_signatures:
        si
        |> get(:sentence_signatures, [])
        |> list()
        |> Enum.filter(&(get(&1, :id) == :wh_aux_topic_question))
        |> Enum.map(&pick(&1, [:id, :span, :roles, :evidence])),
      verb_particle_signatures:
        si
        |> get(:sentence_signatures, [])
        |> list()
        |> Enum.filter(&(get(&1, :id) == :verb_particle_construction))
        |> Enum.map(&pick(&1, [:id, :span, :roles, :evidence])),
      atl_verb_particles:
        si
        |> get(:atl_slate, %{})
        |> get(:constituents, [])
        |> list()
        |> Enum.filter(&(get(&1, :id) == :verb_particle_construction))
        |> Enum.map(&pick(&1, [:id, :span, :status, :roles, :unresolved_roles])),
      unverified_phrasal_hypotheses:
        si
        |> get(:mwe_hypotheses, [])
        |> list()
        |> Enum.filter(&(get(&1, :category) == :phrasal_verb))
        |> Enum.map(&pick(&1, [:phrase, :span, :constituent_indices,
                                :particle_position, :dictionary_check, :verified?]))
    }

    if empty?(result), do: %{}, else: result
  end

  defp substantive_cognition?(si) do
    Enum.any?([:lifg_choices, :semantic_choices, :lexical_constructions,
               :sentence_signatures], fn key ->
      case get(si, key) do
        values when is_list(values) -> values != []
        _ -> false
      end
    end) or is_map(get(si, :comprehension))
  end

  defp runtime(si) do
    context = get(si, :context_frame)

    %{
      intent: get(si, :intent),
      confidence: get(si, :confidence),
      conversation_act: get(si, :conversation_act),
      context_intent: get(context, :intent),
      selected_action: get(si, :selected_action)
    }
    |> Enum.reject(fn {_key, value} -> is_nil(value) end)
    |> Map.new()
  end

  defp verified_meanings(si) do
    si
    |> get(:lexical_constructions, [])
    |> list()
    |> Enum.filter(fn meaning ->
      get(meaning, :dictionary_verified?) == true and
        is_binary(get(meaning, :id)) and
        is_binary(get(meaning, :definition))
    end)
    |> Enum.map(&pick(&1, [:id, :phrase, :definition, :source, :context_score,
                           :span, :object_index, :constituent_indices]))
  end

  # Only the actual Core trace may populate this section. Never derive a
  # supposed semantic decision from LIFG words, assistant text or noun glosses.
  defp semantic_sense_shadow(si) do
    si
    |> get(:trace, [])
    |> list()
    |> Enum.find(&(get(&1, :stage) == :semantic_sense_shadow or
                    get(&1, :stage) == "semantic_sense_shadow"))
    |> case do
      %{} = event ->
        meta = get(event, :meta)

        if is_map(meta) and get(meta, :mode) in [:shadow_only, "shadow_only"] do
          %{
            decision: get(event, :decision),
            reason: get(event, :reason),
            metadata: pick(meta, [
              :mode, :status, :skip_reason, :original_surface_id,
              :attested_base_lexeme, :sense_count, :excluded_count,
              :baseline_id, :proposed_id, :baseline_gap, :evaluated_gap,
              :role_expansion_enabled?, :role_expansion_status, :role_sources,
              :top_candidates, :exclusions, :semantic_cohort, :sense_verified?,
              :gold_label_provided?, :applied_to_lifg?,
              :brain_cells_stimulated?, :synaptic_weight_transferred?,
              :response_changed?
            ])
          }
        end

      _ -> nil
    end
  end

  # Cohort evidence comes only from the verified semantic-sense shadow
  # trace. There is no fallback to word definitions, an LLM reply, or
  # a guessed sense group. This also supports saved Explain reconstructions.
  defp semantic_cohort(si) do
    si
    |> semantic_sense_shadow()
    |> case do
      %{metadata: meta} when is_map(meta) ->
        case get(meta, :semantic_cohort) do
          %{status: status} = cohort when is_atom(status) -> cohort
          %{"status" => _} = cohort -> cohort
          _ -> nil
        end

      _ -> nil
    end
  end

  defp sentence_competition(value) when is_map(value) do
    pick(value, [:version, :competing_frames, :rejected_spans, :comparisons])
  end

  defp sentence_competition(_), do: nil

  defp comprehension(value) when is_map(value) do
    pick(value, [:intent, :confidence, :recognized_topics,
                 :selected_lexical_candidates, :understood, :uncertain, :unresolved,
                 :degraded?, :reasons, :lexical_meanings])
  end

  defp comprehension(_), do: nil

  defp rows(si, key, fields) do
    si
    |> get(key, [])
    |> list()
    |> Enum.filter(&is_map/1)
    |> Enum.map(&pick(&1, fields))
  end

  defp pick(value, fields) when is_map(value) do
    fields
    |> Enum.reduce(%{}, fn key, acc ->
      case get(value, key) do
        nil -> acc
        v -> Map.put(acc, key, v)
      end
    end)
  end

  defp pick(_, _), do: %{}

  defp empty?(result) do
    result.runtime == %{} and result.verified_phrasal_meanings == [] and
      is_nil(result.provisional_interpretation) and is_nil(result.discourse_reference) and
      result.semantic_winners == [] and result.lexical_winners == [] and
      result.comprehension == nil and result.semantic_sense_shadow == nil and
      result.semantic_cohort == nil and
      result.sentence_competition == nil and
      result.question_signatures == [] and result.verb_particle_signatures == [] and
      result.atl_verb_particles == [] and result.unverified_phrasal_hypotheses == []
  end

  defp get(map, key, default \\ nil)

  defp get(map, key, default) when is_map(map) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp get(_, _, default), do: default

  defp list(value) when is_list(value), do: value
  defp list(_), do: []
end
