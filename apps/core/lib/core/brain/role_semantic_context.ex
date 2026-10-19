defmodule Core.Brain.RoleSemanticContext do
  @moduledoc """
  A read-only, one-hop expansion from already-selected SVO actor/patient
  *noun senses*. A uniquely admitted noun uses its exact selected
  BrainCell gloss. A tied same-surface noun slate may contribute only
  lexical terms shared by EVERY scored tied noun sense, after verifying
  all IDs against actual dictionary cells; it does not choose a sense.
  The optional tie-consensus lookup is read-only and bounded.

  The expansion is WEAK contextual evidence. It never proves the selected
  noun sense, a verb sense, a taxonomic edge, or factual truth. It is not
  allowed to satisfy the validator's explicit specialist-domain gates.
  """

  alias Brain.LIFG.ContextFit
  alias Core.Brain.SynapticExperience
  alias Core.Semantic.SVOEvent

  @max_terms 12
  @max_tied_nouns 16
  @tie_epsilon 1.0e-6
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

      active = Enum.filter(contexts, &(&1.status in
        [:grounded_one_hop, :grounded_tie_consensus]))
      %{
        status:
          cond do
            active == [] -> :no_grounded_role_gloss
            Enum.any?(active, &(&1.status == :grounded_one_hop)) -> :grounded
            true -> :tie_consensus_only
          end,
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
      |> Enum.filter(&(&1.status in
        [:grounded_one_hop, :grounded_tie_consensus]))
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

        %{
          role: ctx.role,
          source_noun_sense_id: Map.get(ctx, :noun_sense_id),
          source_noun_sense_ids: Map.get(ctx, :noun_sense_ids, []),
          selected_noun_probability: ctx.selected_probability,
          context_fit: score.score,
          matched: matched,
          contribution: Float.round(contribution, 6),
          provenance: ctx.source,
          noun_sense_identity_verified?: false
        }
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

  defp from_selected(si, role, index, opts) do
    choices =
      Enum.filter(List.wrap(Map.get(si, :lifg_choices)),
        &(is_map(&1) and Map.get(&1, :token_index) == index))

    case choices do
      [choice] -> from_choice(si, choice, role, opts)
      _ -> unavailable(role, :missing_or_duplicate_noun_choice)
    end
  end

  defp from_choice(si, choice, role, opts) do
    with %{id: noun_id, definition: definition} <- choice,
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
      {:error, :ambiguous_same_surface_noun_senses} ->
        tied_noun_consensus(si, choice, role, opts)

      {:error, reason} ->
        unavailable(role, reason)

      _ ->
        unavailable(role, :missing_or_inconsistent_selected_noun_definition)
    end
  end

  # Ties are not confidence: no single noun sense is selected. We use
  # only the intersection of independent real dictionary glosses across
  # *all* equally top-scored noun senses. A missing row, conflicting
  # definition or oversized tie fails closed. No one sense is promoted.
  defp tied_noun_consensus(si, choice, role, opts) do
    noun_id = Map.get(choice, :id)
    [norm, "noun", _] = String.split(noun_id, "|", parts: 3)
    scores = Map.get(choice, :candidate_probs, %{})
    selected_p = Map.get(scores, noun_id)

    tied =
      scores
      |> Enum.filter(fn {id, p} ->
        case String.split(id, "|", parts: 3) do
          [^norm, "noun", suffix] when suffix not in ["", "fallback"] ->
            abs(p - selected_p) <= @tie_epsilon
          _ -> false
        end
      end)
      |> Enum.map(&elem(&1, 0))
      |> Enum.sort()

    stronger_competitor? =
      Enum.any?(scores, fn {id, p} ->
        case String.split(id, "|", parts: 3) do
          [^norm, "noun", _] -> p > selected_p + @tie_epsilon
          _ -> false
        end
      end)

    cond do
      stronger_competitor? ->
        unavailable(role, :selected_noun_not_highest_scored)

      length(tied) < 2 ->
        unavailable(role, :no_exact_tie_to_consense)

      length(tied) > @max_tied_nouns ->
        unavailable(role, :too_many_tied_noun_senses)

      noun_id not in tied ->
        unavailable(role, :selected_noun_not_in_top_tie)

      true ->
        with {:ok, rows} <- fetch_noun_cells(norm, opts),
             {:ok, verified} <- exact_tied_cells(rows, tied, norm, noun_id,
               Map.get(choice, :definition)),
             term_lists <- Enum.map(verified, fn cell ->
               gloss_terms(Map.get(cell, :definition), norm, si)
             end),
             true <- Enum.all?(term_lists, &(&1 != [])),
             common <- unanimous_terms(term_lists),
             true <- common != [] do
          %{
            status: :grounded_tie_consensus,
            role: role,
            reason: :identical_score_gloss_intersection,
            noun_sense_id: nil,
            noun_sense_ids: tied,
            competing_sense_count: length(tied),
            selected_probability: selected_p,
            terms: common,
            source: :unanimous_tied_dictionary_glosses,
            noun_sense_independently_verified?: false
          }
        else
          {:error, reason} -> unavailable(role, reason)
          _ -> unavailable(role, :no_unanimous_noun_gloss_terms)
        end
    end
  end

  defp fetch_noun_cells(norm, opts) do
    loader = Keyword.get(opts, :fetch_noun_senses, &default_noun_fetch/1)

    if is_function(loader, 1) do
      try do
        case loader.(norm) do
          cells when is_list(cells) and length(cells) <= 256 ->
            {:ok, cells}
          _ -> {:error, :noun_dictionary_inventory_unavailable}
        end
      rescue
        _ -> {:error, :noun_dictionary_inventory_unavailable}
      catch
        _, _ -> {:error, :noun_dictionary_inventory_unavailable}
      end
    else
      {:error, :noun_dictionary_inventory_unavailable}
    end
  end

  defp default_noun_fetch(norm) do
    if Code.ensure_loaded?(Db) and function_exported?(Db, :brain_cells_for_norm, 1),
      do: Db.brain_cells_for_norm(norm),
      else: nil
  end

  defp exact_tied_cells(rows, ids, norm, selected_id, selected_definition) do
    noun_rows = Enum.filter(rows, fn row ->
      is_map(row) and Map.get(row, :pos) == "noun" and
        Map.get(row, :norm) == norm and Map.get(row, :id) in ids
    end)

    actual_ids = Enum.map(noun_rows, &Map.get(&1, :id))

    cond do
      length(noun_rows) != length(ids) or
        Enum.sort(actual_ids) != ids ->
        {:error, :incomplete_tied_noun_dictionary_evidence}

      not Enum.all?(noun_rows, fn row ->
        gloss = Map.get(row, :definition)
        is_binary(gloss) and gloss != "" and
          byte_size(gloss) <= @max_gloss_bytes
      end) ->
        {:error, :missing_tied_noun_definitions}

      not Enum.any?(noun_rows, fn row ->
        Map.get(row, :id) == selected_id and
          Map.get(row, :definition) == selected_definition
      end) ->
        {:error, :selected_noun_definition_differs_from_dictionary}

      true ->
        {:ok, noun_rows}
    end
  end

  defp unanimous_terms([first | rest]) do
    rest
    |> Enum.reduce(MapSet.new(first), fn terms, acc ->
      MapSet.intersection(acc, MapSet.new(terms))
    end)
    |> MapSet.to_list()
    |> Enum.sort()
    |> Enum.take(@max_terms)
  end

  defp unanimous_terms(_), do: []

  defp unavailable(role, reason), do: %{
    status: :unavailable_or_ambiguous,
    role: role,
    reason: reason,
    noun_sense_independently_verified?: false
  }

  defp noun_confidence(choice, norm, id) do
    scores = Map.get(choice, :candidate_probs)

    cond do
      not is_map(scores) ->
        {:error, :missing_noun_probability_slate}

      map_size(scores) == 0 or map_size(scores) > 256 ->
        {:error, :invalid_noun_probability_slate_size}

      not Enum.all?(scores, fn {candidate_id, value} ->
        is_binary(candidate_id) and is_number(value) and value == value and
          value >= 0.0 and value <= 1.0
      end) ->
        {:error, :invalid_noun_probability_values}

      true ->
        noun_confidence_from_scores(scores, norm, id)
    end
  end

  defp noun_confidence_from_scores(scores, norm, id) do
    total = Enum.reduce(scores, 0.0, fn {_, p}, acc -> acc + p end)
    value = Map.get(scores, id)

    cond do
      total < 0.98 or total > 1.02 ->
        {:error, :incomplete_noun_probability_slate}

      not is_number(value) or value <= 0.0 ->
        {:error, :selected_noun_not_scored}

      true ->
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
          {:error, :ambiguous_same_surface_noun_senses}
        end
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
