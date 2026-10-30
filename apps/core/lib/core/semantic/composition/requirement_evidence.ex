defmodule Core.Semantic.Composition.RequirementEvidence do
  @moduledoc """
  Observation-only evidence check for provisional semantic requirements.

  Reads *current dictionary evidence* for the PP object and predicate.
  Recognition of a gloss cue means only that a requirement has a candidate
  witness; it does not prove the intended sense, the sentence's truth, or
  incompatibility of readings that lack a matching cue.

  All non-witnessed requirements remain :unresolved (never :incompatible).
  LIFG scores, selected choices, and typed composition are not modified.
  """

  # A limited, reviewable set of noun-definition indications. Deliberately
  # conservative: e.g. a "fence" barrier does not automatically prove
  # "supporting_surface" without lexical/physical-world evidence.
  @noun_rules %{
    geographic_landmark: ~r/\b(?:river|lake|watercourse|shore|coast|geographical|geographic)\b/iu,
    transport_landmark: ~r/\b(?:vehicle|car|bus|train|ship|boat|aircraft|ferry|motorcycle)\b/iu,
    supporting_surface: ~r/\b(?:surface|platform|floor|tabletop|supporting ledge)\b/iu,
    physical_surface: ~r/\b(?:surface|outer wall|exterior)\b/iu,
    group_landmark: ~r/\b(?:committee|council|group of people|organization|organisation|team)\b/iu,
    date_landmark: ~r/\b(?:calendar date|day of the week|day of the month)\b/iu,
    time_landmark: ~r/\b(?:time of day|instant in time|moment in time)\b/iu
  }

  @form_of ~r/\b(?:present participle and gerund|simple past and past participle|past participle|present participle)\s+of\s+([\p{L}-]+)\b/iu

  @spec audit(map(), map()) :: map()
  def audit(%{} = context, %{} = hypothesis) do
    requirements = get(hypothesis, :required_context)
    noun = selected_dictionary_cell(get(context, :object))
    verb = candidate_verb_cell(get(context, :predicate))

    checks =
      if is_list(requirements) do
        Enum.map(requirements, fn requirement ->
          key =
            case requirement do
              r when is_atom(r) -> r
              r when is_binary(r) ->
                # No String.to_atom: only translate known declared requirements.
                Enum.find(Map.keys(@noun_rules),
                  &(Atom.to_string(&1) == r))
              _ -> nil
            end

          pattern = if key, do: Map.get(@noun_rules, key), else: nil
          definition = if noun, do: get(noun, :definition), else: nil

          matched =
            if is_binary(definition) and is_struct(pattern, Regex) do
              Regex.run(pattern, definition)
            else
              nil
            end

          %{
            requirement: requirement,
            status: if(is_list(matched), do: :candidate_witness, else: :unresolved),
            source_role: if(is_list(matched), do: :pp_object, else: nil),
            source_sense_id: if(is_list(matched), do: get(noun, :id), else: nil),
            supporting_text: if(is_list(matched), do: hd(matched), else: nil)
          }
        end)
      else
        []
      end

    form_of =
      case verb do
        nil -> nil
        cell ->
          case get(cell, :definition) do
            definition when is_binary(definition) ->
              case Regex.run(@form_of, definition) do
                [_matched, lemma] ->
                  %{attested_form_of_lemma: String.downcase(lemma),
                    verb_sense_id: get(cell, :id),
                    morphological_only?: true}
                _ -> nil
              end
            _ -> nil
          end
      end

    %{
      status:
        cond do
          checks == [] -> :no_requirements
          Enum.all?(checks, &(&1.status == :candidate_witness)) ->
            :all_requirements_have_candidate_witnesses
          Enum.any?(checks, &(&1.status == :candidate_witness)) ->
            :some_requirements_have_candidate_witnesses
          true -> :unresolved
        end,
      requirement_checks: checks,
      predicate_form_of: form_of,
      object_selected_sense_id: if(noun, do: get(noun, :id), else: nil),
      predicate_selected_sense_id: get(get(context, :predicate) || %{}, :chosen_id),
      semantic_compatibility_proven?: false,
      any_incompatibility_proven?: false,
      modifies_lifg?: false
    }
  end

  def audit(_, _), do: %{status: :invalid_input, semantic_compatibility_proven?: false,
                          any_incompatibility_proven?: false, modifies_lifg?: false}

  defp selected_dictionary_cell(%{} = role) do
    candidate_id = get(role, :chosen_id)

    get(role, :dictionary_candidates)
    |> List.wrap()
    |> Enum.find(fn cell ->
      is_map(cell) and get(cell, :id) == candidate_id and
        is_binary(get(cell, :definition))
    end)
  end

  defp selected_dictionary_cell(_), do: nil

  defp candidate_verb_cell(%{} = role) do
    # This is an attested, scored VERB alternative, not a corrected POS winner.
    candidates = get(role, :dictionary_candidates) |> List.wrap()

    Enum.find(candidates, fn
      %{} = cell ->
        id = get(cell, :id)
        is_binary(id) and String.contains?(id, "|verb|") and
          is_binary(get(cell, :definition))
      _ -> false
    end)
  end

  defp candidate_verb_cell(_), do: nil

  defp get(map, key) when is_map(map),
    do: Map.get(map, key) || Map.get(map, Atom.to_string(key))
  defp get(_, _), do: nil
end
