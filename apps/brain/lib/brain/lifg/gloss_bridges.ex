defmodule Brain.LIFG.GlossBridges do
  @moduledoc """
  Read-only, one-hop dictionary-gloss bridge audit.

  Compares dictionary definitions for actual context-word senses with the
  exact definitions of candidate senses of a target word. This can surface
  candidate links when synonym arrays are empty (cash -> money -> bank).

  A source word may have many unrelated senses; neither its definition nor a
  shared term proves the intended sense. Every output is unverified and must
  not be fed into Stage1 as a score or treated as a training gold label.

  Pure, deterministic, no DB operations or model calls.
  """

  alias Brain.LIFG.ContextFit

  @max_context_words 12
  @max_source_senses 40
  @max_target_senses 60
  @max_gloss_length 320
  @max_edges 60

  @spec analyze(String.t(), String.t(), [map()], map()) :: map()
  def analyze(sentence, target, target_senses, context_rows)
      when is_binary(sentence) and is_binary(target) and
             is_list(target_senses) and is_map(context_rows) do
    context =
      ContextFit.explain(sentence, target, %{}).context_terms
      |> Enum.take(@max_context_words)

    targets =
      target_senses
      |> Enum.filter(&valid_target?/1)
      |> Enum.sort_by(&Map.fetch!(&1, :id))
      |> Enum.take(@max_target_senses)

    sources =
      context
      |> Enum.flat_map(fn word ->
        rows =
          context_rows
          |> Map.get(word, [])
          |> List.wrap()
          |> Enum.filter(&valid_source?(&1, word))
          |> Enum.sort_by(&Map.fetch!(&1, :id))
          |> Enum.take(@max_source_senses)

        Enum.map(rows, fn row -> {word, row, length(rows)} end)
      end)

    edges =
      for {word, source, source_count} <- sources,
          target_sense <- targets,
          gloss = String.slice(source.definition, 0, @max_gloss_length),
          report = ContextFit.explain(gloss, target, target_sense),
          report.score > 0.0 do
        matches =
          report.unique_matches
          |> Enum.flat_map(fn {field, terms} ->
            Enum.map(terms, &%{field: field, term: &1})
          end)
          |> Enum.sort_by(&{&1.term, &1.field})

        %{
          context_word: word,
          context_sense_id: source.id,
          context_status: Map.get(source, :status),
          context_sense_count: source_count,
          context_definition: gloss,
          target_sense_id: target_sense.id,
          target_definition: Map.get(target_sense, :definition),
          lexical_fit: report.score,
          evidence: matches,
          method: :source_gloss_to_target_definition,
          verified?: false
        }
      end
      |> Enum.sort_by(&{-&1.lexical_fit, &1.target_sense_id, &1.context_sense_id})
      |> Enum.take(@max_edges)

    %{
      target: target,
      target_sense_count: length(targets),
      context_words: context,
      source_sense_count: length(sources),
      edge_count: length(edges),
      edges: edges,
      verified?: false
    }
  end

  def analyze(_, target, _, _),
    do: %{target: target, target_sense_count: 0, context_words: [],
          source_sense_count: 0, edge_count: 0, edges: [], verified?: false}

  defp valid_target?(%{id: id, definition: definition})
       when is_binary(id) and is_binary(definition) do
    String.contains?(id, "|") and String.trim(definition) != ""
  end

  defp valid_target?(_), do: false

  defp valid_source?(%{id: id, norm: norm, definition: definition}, word)
       when is_binary(id) and is_binary(norm) and is_binary(definition) do
    String.contains?(id, "|") and
      String.downcase(String.trim(norm)) == word and
      String.trim(definition) != ""
  end

  defp valid_source?(_, _), do: false
end
