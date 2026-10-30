# Run from umbrella root:
#   mix run priv/lambda_full_slate_audit.exs
#
# Read-only scoring/lexicon coverage probe for the original "on" ambiguity.
# Does not change sense scores, proofs, stored weights, or selected winners.
# Runs ordinary Core.resolve_input once (normal pipeline may have its usual
# side effects). Writes only priv/lambda_full_slate_audit_results.jsonl.
#
# The dictionary cue flags are evidence for manual study, not gold labels or
# mutually exclusive semantic types. No automatic correctness is inferred.

sentence = "The bird is sitting on the fence."

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

token =
  Enum.find(tokens, fn
    %{n: 1, phrase: phrase} when is_binary(phrase) ->
      String.downcase(phrase) == "on"
    _ -> false
  end)

unless is_map(token), do: raise("No unigram 'on' token found")
index = Map.fetch!(token, :index)

choice = Enum.find(choices, &(Map.get(&1, :token_index) == index))
unless is_map(choice), do: raise("No LIFG choice for 'on' found")

scores =
  case Map.get(choice, :candidate_probs) do
    %{} = values when map_size(values) > 0 -> values
    _ -> raise("No full LIFG candidate probability map; enable full scores")
  end

bucket =
  case Map.get(si, :sense_candidates, %{}) do
    %{} = indexed -> Map.get(indexed, index, [])
    _ -> []
  end

bucket_ids =
  bucket
  |> List.wrap()
  |> Enum.filter(&is_map/1)
  |> Enum.map(&Map.get(&1, :id))
  |> MapSet.new()

# Query all active witnesses, not a flattened Map.new(id => cell).
# Retaining every occurrence prevents duplicate token indices being hidden.
cues = [
  {:positional_definition, ~r/\AIndicating position or location\./iu},
  {:physical_contact_definition, ~r/\ADenoting physical contact or interaction\b/iu},
  {:topic_definition, ~r/\ADealing with the subject of\b/iu},
  {:explicit_opposition, ~r/\bAgainst;\s*in opposition to\./iu},
  {:resting_upon, ~r/\bresting upon\b/iu},
  {:surface_position, ~r/\b(?:positioned|resting)\s+(?:at|on|against)\s+the\s+(?:upper|outer)?\s*surface\b/iu},
  {:force_cue, ~r/\b(?:impact|force|hitting|pressing|pushing|pulling|rubbing)\b/iu},
  {:transport_cue, ~r/\b(?:transport|aboard|vehicle)\b/iu},
  {:sequence_cue, ~r/\b(?:position within a sequence|sequence)\b/iu}
]

records =
  scores
  |> Enum.filter(fn {id, score} ->
    is_binary(id) and is_number(score) and
      String.starts_with?(id, "on|preposition|")
  end)
  |> Enum.sort_by(fn {id, score} -> {-score, id} end)
  |> Enum.map(fn {id, score} ->
    witnesses =
      Enum.filter(cells, fn
        %{} = cell -> Map.get(cell, :id) == id
        _ -> false
      end)

    exact =
      Enum.filter(witnesses, fn cell ->
        Map.get(cell, :norm) == "on" and
          to_string(Map.get(cell, :pos)) == "preposition"
      end)

    indexed = Enum.filter(exact, &(Map.get(&1, :token_index) == index))

    definition =
      (Enum.find(indexed, &(is_binary(Map.get(&1, :definition)))) ||
         Enum.find(exact, &(is_binary(Map.get(&1, :definition)))) ||
         Enum.find(witnesses, &(is_binary(Map.get(&1, :definition)))))

    text = if(is_map(definition), do: Map.get(definition, :definition), else: nil)

    matched =
      if is_binary(text) do
        for {cue, pattern} <- cues, Regex.match?(pattern, text), do: cue
      else
        []
      end

    %{
      id: id,
      probability: score,
      selected?: Map.get(choice, :id) == id,
      in_active_cells?: witnesses != [],
      exact_word_pos_witness?: exact != [],
      exact_token_index_witness?: indexed != [],
      active_cell_count: length(witnesses),
      in_sense_candidates_bucket?: MapSet.member?(bucket_ids, id),
      definition: text,
      matched_gloss_cues: matched,
      has_multiple_gloss_cues?: length(matched) > 1
    }
  end)

all_scored = map_size(scores)
on_scored = length(records)
active = Enum.count(records, & &1.in_active_cells?)
exact = Enum.count(records, & &1.exact_word_pos_witness?)
indexed = Enum.count(records, & &1.exact_token_index_witness?)
with_def = Enum.count(records, &is_binary(&1.definition))
without_def = Enum.reject(records, &is_binary(&1.definition))
multi = Enum.filter(records, & &1.has_multiple_gloss_cues?)
selected = Enum.find(records, & &1.selected?)

file = "priv/lambda_full_slate_audit_results.jsonl"
File.write!(file, Enum.map_join(records, "", &(Jason.encode!(&1) <> "\n")))

IO.inspect(
  %{
    sentence: sentence,
    selected: if(selected, do: Map.take(selected,
      [:id, :definition, :matched_gloss_cues]), else: nil),
    scored_total_all_pos: all_scored,
    scored_on_preposition_senses: on_scored,
    scored_with_active_cell: active,
    scored_with_exact_word_pos: exact,
    scored_with_exact_token_index: indexed,
    scored_with_definition: with_def,
    absent_active_cell_sample:
      records
      |> Enum.reject(& &1.in_active_cells?)
      |> Enum.map(& &1.id)
      |> Enum.take(20),
    missing_definition_sample: Enum.take(Enum.map(without_def, & &1.id), 20),
    multiple_cue_examples:
      multi
      |> Enum.take(12)
      |> Enum.map(&Map.take(&1, [:id, :matched_gloss_cues, :definition])),
    complete_record_count: length(records),
    saved_to: file
  },
  label: "Full preposition-sense evidence inventory",
  limit: :infinity,
  printable_limit: :infinity
)
