defmodule SymbrellaWeb.HomeLive.ExplainEvidence do
  @moduledoc """
  Read-only, bounded projection of the exact Core.SemanticInput returned by
  Core.resolve_input/2 for one conversation turn.

  The UI must not infer a phrasal sense from word-level LIFG choices, promote
  a syntax-only MWE hypothesis into dictionary fact, or relabel word senses
  when the canonical semantic cover contains a different MWE choice.

  No DB access, extra inference or response generation takes place here.
  This data is safe to persist with a chat message's Explain payload.
  """

  @limit 24

  @spec from_si(map() | term()) :: map()
  def from_si(si) when is_map(si) do
    comprehension = get(si, :comprehension)

    result = %{
      runtime: runtime(si),
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
      verb_particle_signatures:
        si
        |> get(:sentence_signatures, [])
        |> list()
        |> Enum.filter(&(get(&1, :id) == :verb_particle_construction))
        |> Enum.take(@limit)
        |> Enum.map(&pick(&1, [:id, :span, :roles, :evidence])),
      atl_verb_particles:
        si
        |> get(:atl_slate, %{})
        |> get(:constituents, [])
        |> list()
        |> Enum.filter(&(get(&1, :id) == :verb_particle_construction))
        |> Enum.take(@limit)
        |> Enum.map(&pick(&1, [:id, :span, :status, :roles, :unresolved_roles])),
      unverified_phrasal_hypotheses:
        si
        |> get(:mwe_hypotheses, [])
        |> list()
        |> Enum.filter(&(get(&1, :category) == :phrasal_verb))
        |> Enum.take(@limit)
        |> Enum.map(&pick(&1, [:phrase, :span, :constituent_indices,
                                :particle_position, :dictionary_check, :verified?]))
    }

    if empty?(result), do: %{}, else: result
  end

  def from_si(_), do: %{}

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
    |> Enum.take(@limit)
    |> Enum.map(&pick(&1, [:id, :phrase, :definition, :source, :context_score,
                           :span, :object_index, :constituent_indices]))
  end

  defp comprehension(value) when is_map(value) do
    pick(value, [:intent, :confidence, :understood, :uncertain, :unresolved,
                 :degraded?, :reasons, :lexical_meanings])
  end

  defp comprehension(_), do: nil

  defp rows(si, key, fields) do
    si
    |> get(key, [])
    |> list()
    |> Enum.filter(&is_map/1)
    |> Enum.take(@limit)
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
      result.semantic_winners == [] and result.lexical_winners == [] and
      result.comprehension == nil and result.verb_particle_signatures == [] and
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
