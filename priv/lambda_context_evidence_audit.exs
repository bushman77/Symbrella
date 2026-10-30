# Run from the Symbrella umbrella root:
#   mix run priv/lambda_context_evidence_audit.exs
#
# Observational evidence inventory: one supported progressive sentence and one
# finite-past sentence previously outside the closed lambda vocabulary.
# Reads actual syntax signatures and dictionary witnesses, not fixture labels.
# This script DOES NOT claim that a dictionary gloss proves a lexical sense,
# infer context requirements, modify LIFG rankings, or train the model.
# Core.resolve_input invokes the ordinary pipeline (which can have its usual
# side effects). This script itself only writes under priv/.

alias Core.Semantic.Composition
alias Core.Semantic.Composition.SenseDenotations

samples = [
  %{name: :known_progressive,
    sentence: "The bird is sitting on the fence."},
  %{name: :novel_past_locative,
    sentence: "The fisherman sat on the bank of the river."}
]

summaries =
  Enum.map(samples, fn %{name: sample_name, sentence: sentence} ->
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
    cells = List.wrap(Map.get(si, :active_cells))
    choices = List.wrap(Map.get(si, :lifg_choices))
    signatures = List.wrap(Map.get(si, :sentence_signatures))

    words =
      tokens
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :n, 1) == 1))
      |> Map.new(fn token ->
        {Map.get(token, :index), String.downcase(Map.get(token, :phrase, ""))}
      end)

    on_index =
      Enum.find_value(words, fn
        {index, "on"} -> index
        _ -> nil
      end)

    unless is_integer(on_index) do
      raise "No 'on' lexical token in #{inspect(sample_name)}"
    end

    # Compare complete attested NP complements first. A bare PP may mistake
    # the article 'the' for its object because a noun alternative is attested.
    pp =
      signatures
      |> Enum.filter(fn
        %{id: :prepositional_phrase, source: :core_syntax,
          roles: %{preposition: index, object: object},
          evidence: evidence}
        when index == on_index and is_integer(object) and is_list(evidence) ->
          :preposition in evidence and object > index

        _ -> false
      end)
      |> Enum.sort_by(fn
        %{span: {first, last}, evidence: evidence} ->
          {if(:noun_phrase_complement in evidence, do: 0, else: 1),
           -(last - first), last}

        _ -> {2, 0, 0}
      end)
      |> List.first()

    object_index =
      if is_map(pp), do: get_in(pp, [:roles, :object]), else: nil

    predicate_index =
      if is_integer(on_index) and on_index > 0,
        do: on_index - 1, else: nil

    role_report = fn label, index, expected_pos ->
      word = if is_integer(index), do: Map.get(words, index), else: nil

      choice =
        Enum.find(choices, fn c ->
          is_map(c) and Map.get(c, :token_index) == index
        end)

      probabilities =
        if is_map(choice) do
          case Map.get(choice, :candidate_probs) do
            %{} = scores -> scores
            _ -> %{}
          end
        else
          %{}
        end

      witnesses =
        Enum.filter(cells, fn cell ->
          is_map(cell) and
            Map.get(cell, :norm) == word and
            to_string(Map.get(cell, :pos)) == expected_pos and
            Map.get(cell, :token_index) == index
        end)

      # Include the selected dictionary cell in addition to scored alternatives.
      by_id = Map.new(witnesses, &{Map.get(&1, :id), &1})

      top_ids =
        probabilities
        |> Enum.filter(fn {id, probability} ->
          is_binary(id) and is_number(probability) and Map.has_key?(by_id, id)
        end)
        |> Enum.sort_by(fn {id, probability} -> {-probability, id} end)
        |> Enum.map(&elem(&1, 0))
        |> Enum.take(10)

      selected_id = if(is_map(choice), do: Map.get(choice, :id), else: nil)

      ids =
        [selected_id | top_ids]
        |> Enum.filter(&is_binary/1)
        |> Enum.uniq()

      %{
        role: label,
        token_index: index,
        token: word,
        chosen_id: selected_id,
        total_attested_dictionary_senses: length(witnesses),
        scored_candidate_count: map_size(probabilities),
        dictionary_candidates:
          Enum.flat_map(ids, fn id ->
            case Map.get(by_id, id) do
              nil -> []
              cell -> [
                %{
                  id: id,
                  probability: Map.get(probabilities, id),
                  definition: Map.get(cell, :definition),
                  semantic_atoms: Map.get(cell, :semantic_atoms),
                  selected?: id == selected_id
                }
              ]
            end
          end)
      }
    end

    predicate = role_report.(:predicate, predicate_index, "verb")
    object = role_report.(:pp_object, object_index, "noun")
    preposition = role_report.(:preposition, on_index, "preposition")

    # Aggregate over all same-token preposition dictionary candidates.
    # No candidate receives a compatibility verdict or score adjustment.
    preposition_denotations =
      cells
      |> Enum.filter(fn cell ->
        is_map(cell) and
          Map.get(cell, :token_index) == on_index and
          Map.get(cell, :norm) == "on" and
          to_string(Map.get(cell, :pos)) == "preposition"
      end)
      |> Enum.uniq_by(&Map.get(&1, :id))
      |> Enum.map(fn cell ->
        report = SenseDenotations.propose(cell)
        %{
          id: Map.get(cell, :id),
          candidate_count: report.candidate_count,
          requirements: Enum.flat_map(report.candidates, fn candidate ->
            [%{
              subtype: candidate.subtype,
              relation_family: candidate.relation_family,
              required_context: candidate.required_context,
              source_span: candidate.matched_text
            }]
          end)
        }
      end)
      |> Enum.sort_by(& &1.id)

    composition =
      case Composition.verified(si) do
        {:ok, proof} ->
          %{status: :formally_verified, kind: proof.kind,
            proof_steps: length(proof.steps),
            epistemic_status: Map.get(proof, :epistemic_status)}
        :not_applicable ->
          %{status: :not_applicable}
      end

    %{
      name: sample_name,
      sentence: sentence,
      pp_signature: pp,
      predicate: predicate,
      object: object,
      preposition: preposition,
      composition: composition,
      preposition_denotations: preposition_denotations
    }
  end)

output = "priv/lambda_context_evidence_results.jsonl"
File.write!(output, Enum.map_join(summaries, "", &(Jason.encode!(&1) <> "\n")))

IO.inspect(%{
  saved_to: output,
  samples: Enum.map(summaries, fn record ->
    %{
      name: record.name,
      pp_signature: if(is_map(record.pp_signature),
        do: Map.take(record.pp_signature, [:id, :span, :roles, :evidence]), else: nil),
      predicate: Map.take(record.predicate,
        [:token_index, :token, :chosen_id, :total_attested_dictionary_senses,
         :scored_candidate_count, :dictionary_candidates])
        |> Map.update!(:dictionary_candidates, &Enum.take(&1, 4)),
      object: Map.take(record.object,
        [:token_index, :token, :chosen_id, :total_attested_dictionary_senses,
         :scored_candidate_count, :dictionary_candidates])
        |> Map.update!(:dictionary_candidates, &Enum.take(&1, 4)),
      preposition_selected: record.preposition.chosen_id,
      preposition_candidate_denotations:
        Enum.count(record.preposition_denotations, &(&1.candidate_count > 0)),
      composition: record.composition
    }
  end)
}, label: "Lexical and syntactic context witnesses",
  limit: :infinity, printable_limit: 1200)
