defmodule Core.Brain.RoleSemanticContext do
  @moduledoc """
  A read-only, one-hop expansion from already-selected SVO actor/patient
  *noun senses*. The only source is each exact selected noun BrainCell's
  dictionary gloss, with a matching LIFG choice and nonambiguous noun
  probability slate. No other noun senses are fetched or inferred.

  The expansion is WEAK contextual evidence. It never proves the selected
  noun sense, a verb sense, a taxonomic edge, or factual truth. It is not
  allowed to satisfy the validator's explicit specialist-domain gates.
  """

  alias Brain.LIFG.ContextFit
  alias Core.Brain.SynapticExperience
  alias Core.Semantic.SVOEvent

  @max_terms 12
  @max_gloss_bytes 2000
  @noun_margin 0.08
  @patient_weight 0.35
  @actor_weight 0.15
  @patient_cap 0.105
  @actor_cap 0.045
  @total_cap 0.13

  @uninformative MapSet.new(~w(
    a an the this that some any one to from for with of on in by as
    or and is are be been being when which who what whose where
    something someone thing things object objects person persons
    type kind form way means act state process often usually typically
    used use using other certain relating referring comprising
    containing consisting example especially also generally often
    item instance relating usually part parts
    noun verb countable uncountable plural singular informal formal
    figurative slang transitive intransitive ambitransitive
  ))

  @spec extract(map(), keyword()) :: map()
  def extract(si, opts \\ [])

  def extract(%{} = si, opts) when is_list(opts) do
    with {:ok, _} <- SynapticExperience.extract(si),
         %{roles: %{subject_head: actor_i, object_head: patient_i}} <-
           SVOEvent.complete_signature(si),
         true <- is_integer(actor_i) and is_integer(patient_i) do
      contexts =
        [{:actor, actor_i}, {:patient, patient_i}]
        |> Enum.map(fn {role, index} ->
          from_selected(si, role, index, opts)
        end)

      active = Enum.filter(contexts, &(&1.status == :grounded_one_hop))
      %{
        status: if(active == [], do: :no_grounded_role_gloss, else: :grounded),
        contexts: contexts,
        active_count: length(active),
        inferred_noun_meanings_verified?: false,
        applied_to_lifg?: false,
        synapses_modified?: false
      }
    else
      _ ->
        %{status: :ineligible, contexts: [], active_count: 0,
          inferred_noun_meanings_verified?: false, applied_to_lifg?: false,
          synapses_modified?: false}
    end
  end

  def extract(_, _), do: %{status: :invalid_input, contexts: [], active_count: 0}

  @doc """
  Return distinct actor/patient evidence for a REAL dictionary verb sense.
  Direct lexical evidence is not included. Maximum total contribution
  is 0.13; actor evidence alone cannot later license a sense hypothesis.
  """
  @spec support(map(), map()) :: map()
  def support(%{contexts: contexts}, candidate) when is_list(contexts) and is_map(candidate) do
    components =
      contexts
      |> Enum.filter(&(&1.status == :grounded_one_hop))
      |> Enum.map(fn ctx ->
        score = ContextFit.explain(nil, nil, candidate,
          extra_context: [Enum.join(ctx.terms, " ")])
        matched =
          score.unique_matches
          |> Map.values()
          |> List.flatten()
          |> Enum.uniq()
          |> Enum.sort()

        factor = if(ctx.role == :patient, do: @patient_weight, else: @actor_weight)
        cap = if(ctx.role == :patient, do: @patient_cap, else: @actor_cap)

        # A vague "something" or target inflection cannot score because
        # they were filtered from the source gloss before comparison.
        contribution =
          if matched == [], do: 0.0,
            else: min(cap, score.score * factor)

        %{role: ctx.role, source_noun_sense_id: ctx.noun_sense_id,
          selected_noun_probability: ctx.selected_probability,
          context_fit: score.score,
          matched: matched, contribution: Float.round(contribution, 6),
          provenance: :exact_selected_noun_definition}
      end)

    actor = components
      |> Enum.filter(&(&1.role == :actor))
      |> Enum.reduce(0.0, fn c, acc -> acc + c.contribution end)

    patient = components
      |> Enum.filter(&(&1.role == :patient))
      |> Enum.reduce(0.0, fn c, acc -> acc + c.contribution end)

    %{
      signal: Float.round(min(@total_cap, actor + patient), 6),
      patient_signal: Float.round(min(patient, @patient_cap), 6),
      actor_signal: Float.round(min(actor, @actor_cap), 6),
      witnesses: Enum.filter(components, &(&1.contribution > 0.0)),
      one_hop_only?: true,
      verified_sense_equivalence?: false
    }
  end

  def support(_, _), do: %{signal: 0.0, patient_signal: 0.0,
    actor_signal: 0.0, witnesses: [], one_hop_only?: true,
    verified_sense_equivalence?: false}

  defp from_selected(si, role, index, _opts) do
    with [choice] <- Enum.filter(List.wrap(Map.get(si, :lifg_choices)),
           &(is_map(&1) and Map.get(&1, :token_index) == index)),
         %{id: noun_id, definition: definition} <- choice,
         true <- is_binary(noun_id) and byte_size(noun_id) <= 128,
         [norm, "noun", suffix] <- String.split(noun_id, "|", parts: 3),
         true <- suffix != "" and suffix != "fallback",
         true <- is_binary(definition) and definition != "" and
           byte_size(definition) <= @max_gloss_bytes,
         [cell] <- Enum.filter(List.wrap(Map.get(si, :active_cells)),
           &(is_map(&1) and Map.get(&1, :id) == noun_id)),
         true <- Map.get(cell, :pos) == "noun" and
           Map.get(cell, :norm) == norm and
           Map.get(cell, :definition) == definition,
         {:ok, selected_p} <- noun_confidence(choice, norm, noun_id),
         terms when is_list(terms) <- gloss_terms(definition, norm, si),
         true <- terms != [] do
      %{
        status: :grounded_one_hop,
        role: role,
        noun_sense_id: noun_id,
        selected_probability: selected_p,
        terms: terms,
        source: :selected_brain_cell_definition,
        noun_sense_independently_verified?: false
      }
    else
      _ ->
        %{status: :unavailable_or_ambiguous, role: role,
          noun_sense_independently_verified?: false}
    end
  end

  defp noun_confidence(choice, norm, id) do
    scores = Map.get(choice, :candidate_probs)

    with %{} = scores <- scores,
         true <- map_size(scores) > 0 and map_size(scores) <= 256,
         true <- Enum.all?(scores, fn {candidate_id, value} ->
           is_binary(candidate_id) and is_number(value) and value == value and
             value >= 0.0 and value <= 1.0
         end),
         total <- Enum.reduce(scores, 0.0, fn {_, p}, acc -> acc + p end),
         true <- total >= 0.98 and total <= 1.02,
         value when is_number(value) and value > 0.0 <- Map.get(scores, id) do
      competitors =
        scores
        |> Enum.filter(fn {key, _} ->
          case String.split(key, "|", parts: 3) do
            [^norm, "noun", _] -> key != id
            _ -> false
          end
        end)
        |> Enum.map(&elem(&1, 1))

      if competitors == [] or value - Enum.max(competitors) >= @noun_margin do
        {:ok, value}
      else
        :uncertain_noun_selection
      end
    else
      _ -> :no_scored_noun_selection
    end
  end

  defp gloss_terms(definition, norm, si) do
    verb =
      case SVOEvent.complete_signature(si) do
        %{roles: %{predicate: i}} ->
          case Enum.filter(List.wrap(Map.get(si, :lifg_choices)),
                 &(Map.get(&1, :token_index) == i)) do
            [%{id: id}] -> id |> String.split("|") |> hd()
            _ -> ""
          end
        _ -> ""
      end

    cleaned =
      definition
      |> String.replace(~r/\A(?:\([^)]{1,120}\)\s*)+/u, "")
      |> String.split(" — ")
      |> List.last()
      |> String.split(~r/[.;:]/u)
      |> hd()
      |> String.downcase()

    cleaned
    |> then(&Regex.scan(~r/[\p{L}]{3,26}/u, &1))
    |> List.flatten()
    |> Enum.reject(&MapSet.member?(@uninformative, &1))
    |> Enum.reject(&(&1 in [norm, verb]))
    |> Enum.uniq()
    |> Enum.take(@max_terms)
  end
end
