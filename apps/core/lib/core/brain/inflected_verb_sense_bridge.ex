defmodule Core.Brain.InflectedVerbSenseBridge do
  @moduledoc """
  Read-only bridge from an attested finite-present surface verb to the
  dictionary sense inventory of its base verb.

  Proof of morphology ("draws" is a form of "draw") permits *retrieval*
  of draw|verb|N, not inference that any N is the correct meaning. Each
  existing base sense is given a separate deterministic lexical-context
  score. Score gaps are hypotheses, never verified sense equivalences.

  This never updates SemanticInput, LIFG, ATL, synapses or BrainCells; the
  production dictionary is queried read-only and an injected sense fetcher
  allows isolated unit testing.
  """

  alias Brain.LIFG.ContextFit
  alias Core.Brain.SynapticExperience
  alias Core.Semantic.{SVOEvent, VerbLemma}

  @max_base_senses 256
  @max_reported 12
  @min_gap 0.10

  @spec project(map(), keyword()) :: map()
  def project(si, opts \\ [])

  def project(%{} = si, opts) when is_list(opts) do
    with {:ok, %{edges: edges}} <- SynapticExperience.extract(si),
         %{roles: %{predicate: predicate_index}} <- SVOEvent.complete_signature(si),
         true <- is_integer(predicate_index),
         [{surface_id, _patient, :patient}] <-
           Enum.filter(edges, fn {_, _, role} -> role == :patient end),
         [%{id: ^surface_id} = selected] <-
           Enum.filter(List.wrap(Map.get(si, :lifg_choices)), fn
             %{token_index: ^predicate_index} -> true
             _ -> false
           end),
         [surface, "verb", _] <- String.split(surface_id, "|", parts: 3),
         {:ok, base} <- VerbLemma.canonical(si, selected),
         true <- base != surface,
         {:ok, candidates} <- fetch_base_senses(base, opts),
         {:ok, rows} <- validate_base_senses(candidates, base) do
      evidence =
        Enum.map(rows, fn candidate ->
          id = Map.fetch!(candidate, :id)

          fit = ContextFit.explain(
            Map.get(si, :sentence), surface, candidate,
            related_morphology?: false
          )

          %{
            id: id,
            context_fit: fit.score,
            matched_context_terms:
              fit.unique_matches
              |> Map.values()
              |> List.flatten()
              |> Enum.uniq()
              |> Enum.sort(),
            evidence_components: fit.contributions,
            dictionary_definition: short(Map.get(candidate, :definition)),
            dictionary_example: short(Map.get(candidate, :example)),
            grammatical_functions: Map.get(candidate, :gram_function, []),
            dictionary_sense_verified?: true,
            meaning_in_this_sentence_verified?: false,
            synaptic_weight_transferred?: false
          }
        end)
        |> Enum.sort_by(fn row -> {-row.context_fit, row.id} end)

      [first | rest] = evidence
      second = List.first(rest)
      gap = if(second, do: first.context_fit - second.context_fit, else: 0.0)

      %{
        status: :read_only_sense_inventory,
        original_surface_id: surface_id,
        predicate_index: predicate_index,
        attested_base_lexeme: base,
        attestation: :dictionary_form_of_gloss_and_morphology_tags,
        base_sense_count: length(evidence),
        candidates_with_context_evidence:
          Enum.count(evidence, &(&1.context_fit > 0.0)),
        highest_context_fit: first.context_fit,
        top_two_gap: gap,
        lexical_hypothesis:
          if(first.context_fit > 0.0 and gap >= @min_gap,
             do: %{candidate_id: first.id, source: :dictionary_context_overlap},
             else: nil),
        all_sense_ids: Enum.map(evidence, & &1.id),
        ranked: Enum.take(evidence, @max_reported),
        complete_inventory_considered?: true,
        surface_to_base_form_attested?: true,
        surface_to_specific_base_sense_verified?: false,
        gold_sense_known?: false,
        selected_new_lifg_sense?: false,
        synaptic_weight_transferred?: false,
        applied_to_lifg?: false,
        brain_cells_stimulated?: false
      }
    else
      {:error, reason} -> skipped(reason)
      _ -> skipped(:ineligible_or_unattested_form)
    end
  end

  def project(_, _), do: skipped(:invalid_input)

  defp skipped(reason) do
    %{status: :skipped, reason: reason, applied_to_lifg?: false,
      synaptic_weight_transferred?: false, selected_new_lifg_sense?: false}
  end

  defp fetch_base_senses(base, opts) do
    loader = Keyword.get(opts, :fetch_base_senses, &default_fetch/1)

    if is_function(loader, 1) do
      try do
        case loader.(base) do
          list when is_list(list) -> {:ok, list}
          _ -> {:error, :base_dictionary_unavailable}
        end
      rescue
        _ -> {:error, :base_dictionary_unavailable}
      catch
        :exit, _ -> {:error, :base_dictionary_unavailable}
      end
    else
      {:error, :base_dictionary_unavailable}
    end
  end

  defp default_fetch(base) do
    if Code.ensure_loaded?(Db) and function_exported?(Db, :brain_cells_for_norm, 1),
      do: Db.brain_cells_for_norm(base),
      else: nil
  end

  defp validate_base_senses(rows, base) when is_list(rows) do
    base_verb_rows =
      rows
      |> Enum.filter(&verb_sense?(&1, base))
      |> Enum.map(&candidate_map/1)
      |> Enum.sort_by(& &1.id)

    ids = Enum.map(base_verb_rows, & &1.id)

    cond do
      length(base_verb_rows) < 2 ->
        {:error, :insufficient_base_verb_senses}

      length(base_verb_rows) > @max_base_senses ->
        {:error, :oversized_base_verb_slate}

      length(Enum.uniq(ids)) != length(ids) ->
        {:error, :duplicate_dictionary_senses}

      true ->
        {:ok, base_verb_rows}
    end
  end

  defp verb_sense?(%{} = candidate, base) do
    id = field(candidate, :id)
    pos = field(candidate, :pos)
    norm = field(candidate, :norm)
    definition = field(candidate, :definition)

    case id do
      id when is_binary(id) ->
        case String.split(id, "|") do
          [^base, "verb", sense] ->
            sense != "" and sense != "fallback" and
              pos == "verb" and norm == base and
              is_binary(definition) and String.trim(definition) != ""

          _ -> false
        end

      _ -> false
    end
  end

  defp verb_sense?(_, _), do: false

  defp candidate_map(candidate) do
    [:id, :pos, :norm, :definition, :example, :synonyms,
     :semantic_atoms, :gram_function, :antonyms]
    |> Enum.map(fn key -> {key, field(candidate, key)} end)
    |> Map.new()
  end

  defp short(value) when is_binary(value), do: String.slice(value, 0, 180)
  defp short(_), do: nil

  defp field(%{} = map, key), do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
  defp field(_, _), do: nil
end
