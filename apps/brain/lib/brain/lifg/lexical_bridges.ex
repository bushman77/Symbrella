defmodule Brain.LIFG.LexicalBridges do
  @moduledoc """
  Read-only, sense-provenanced synonym-to-definition bridge candidates.

  Given a sentence, a target word's *existing* dictionary senses and dictionary
  rows for surrounding context words, report one-hop lexical connections.

  A bridge is an exploration hypothesis, NOT verified sense selection.
  In particular, a polysemous context word can contribute contradictory
  synonyms from different senses. The report must never be fed directly to
  LIFG scores without independent sense verification and regression tests.

  This module never reads or writes a DB, creates senses or updates state.
  """

  alias Brain.LIFG.ContextFit

  @max_context_terms 12
  @max_source_senses 40
  @max_target_senses 60
  @max_synonyms_per_sense 40
  @max_edges 120

  @spec analyze(String.t(), String.t(), [map()], map()) :: map()
  def analyze(sentence, target, target_senses, source_rows)
      when is_binary(sentence) and is_binary(target) and is_list(target_senses) and
             is_map(source_rows) do
    context_terms =
      ContextFit.explain(sentence, target, %{}).context_terms
      |> Enum.take(@max_context_terms)

    senses =
      target_senses
      |> Enum.filter(&valid_sense?/1)
      |> Enum.take(@max_target_senses)

    {inventory_entries, bridge_edges} =
      Enum.map_reduce(context_terms, [], fn context_word, all_edges ->
        source =
          source_rows
          |> Map.get(context_word, [])
          |> List.wrap()
          |> Enum.filter(&valid_source?(&1, context_word))
          |> Enum.take(@max_source_senses)

        source_count = length(source)

        candidate_edges =
          for context_sense <- source,
              synonym <- synonyms(context_sense),
              sense <- senses,
              MapSet.member?(definition_terms(sense), synonym) do
            %{
              context_word: context_word,
              context_sense_id: Map.get(context_sense, :id),
              context_pos: Map.get(context_sense, :pos),
              context_status: Map.get(context_sense, :status),
              context_sense_count: source_count,
              synonym: synonym,
              target_sense_id: Map.get(sense, :id),
              source: :source_sense_synonym_matches_target_definition,
              verified?: false
            }
          end

        inventory = %{
          word: context_word,
          available_senses: source_count,
          senses_with_synonyms: Enum.count(source, &(synonyms(&1) != []))
        }

        {inventory, candidate_edges ++ all_edges}
      end)

    sorted =
      bridge_edges
      |> Enum.uniq_by(&{&1.context_word, &1.context_sense_id, &1.synonym, &1.target_sense_id})
      |> Enum.sort_by(&{&1.target_sense_id, &1.context_word, &1.synonym, &1.context_sense_id})
      |> Enum.take(@max_edges)

    %{
      target: target,
      context_terms: context_terms,
      target_sense_count: length(senses),
      context_inventory: Enum.sort_by(inventory_entries, & &1.word),
      bridge_count: length(sorted),
      bridges: sorted,
      verified?: false
    }
  end

  def analyze(_, target, _, _),
    do: %{target: target, context_terms: [], target_sense_count: 0,
          context_inventory: [], bridge_count: 0, bridges: [], verified?: false}

  defp valid_sense?(%{id: id}) when is_binary(id), do: String.contains?(id, "|")
  defp valid_sense?(_), do: false

  defp valid_source?(%{id: id, norm: norm}, term)
       when is_binary(id) and is_binary(norm) do
    String.downcase(String.trim(norm)) == term
  end

  defp valid_source?(_, _), do: false

  defp synonyms(sense) do
    sense
    |> Map.get(:synonyms, [])
    |> List.wrap()
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.downcase(String.trim(&1)))
    |> Enum.filter(&Regex.match?(~r/^[\p{L}\p{N}]+$/u, &1))
    |> Enum.uniq()
    |> Enum.take(@max_synonyms_per_sense)
  end

  defp definition_terms(sense) do
    case Map.get(sense, :definition) do
      value when is_binary(value) ->
        value
        |> String.downcase()
        |> then(&Regex.scan(~r/[\p{L}\p{N}]+/u, &1))
        |> List.flatten()
        |> MapSet.new()

      _ ->
        MapSet.new()
    end
  end
end
