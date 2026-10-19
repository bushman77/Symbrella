# Read-only production pipeline trace for participant and spatial-role binding.
#
# mix run scripts/trace_participant_spatial.exs
# mix run scripts/trace_participant_spatial.exs "The cat is resting under the table."
#
# This does not generate a response or write an event.
sentence =
  case System.argv() do
    [] -> "A bird is sitting on the fence."
    words -> Enum.join(words, " ")
  end

si =
  Core.resolve_input(sentence,
    mode: :prod,
    response: :off,
    persist_episodes: false
  )

get = fn item, key ->
  if is_map(item), do: Map.get(item, key, Map.get(item, Atom.to_string(key))), else: nil
end

base_tokens =
  si
  |> Map.get(:tokens, [])
  |> List.wrap()
  |> Enum.filter(fn token ->
    n = get.(token, :n)
    kind = get.(token, :kind)
    (is_nil(n) or n == 1) and kind not in [:punct, "punct"]
  end)
  |> Enum.map(&Map.take(&1, [:index, :phrase, :n]))

participles =
  si
  |> Map.get(:active_cells, [])
  |> List.wrap()
  |> Enum.filter(fn cell ->
    atoms = get.(cell, :semantic_atoms)
    id = get.(cell, :id)
    is_binary(id) and String.contains?(id, "|verb|") and
      is_list(atoms) and
      Enum.all?(["tag:form-of", "tag:present", "tag:participle"], &(&1 in atoms))
  end)
  |> Enum.map(&Map.take(&1, [:id, :definition, :semantic_atoms]))

choices =
  si
  |> Map.get(:lifg_choices, [])
  |> List.wrap()
  |> Enum.map(&Map.take(&1, [:token_index, :id, :chosen_id, :lemma, :score]))

winners =
  si
  |> Map.get(:atl_slate, %{})
  |> case do
    %{} = slate -> get.(slate, :winners) || []
    _ -> []
  end
  |> List.wrap()
  |> Enum.map(&Map.take(&1, [:token_index, :id, :chosen_id, :lemma]))

# Track the dictionary evidence underneath covered (MWE) word positions.
# Full word-level LIFG winners are not required when complete syntax and exact
# dictionary POS support the location-role binding.
locative_role_evidence =
  for {index, pos} <- [{4, "preposition"}, {6, "noun"}] do
    surface =
      base_tokens
      |> Enum.find(&(Map.get(&1, :index) == index))
      |> case do
        %{phrase: phrase} -> phrase |> String.downcase() |> String.trim_trailing(".")
        _ -> ""
      end

    dictionary_ids =
      si
      |> Map.get(:active_cells, [])
      |> List.wrap()
      |> Enum.filter(fn cell ->
        get.(cell, :norm) == surface and get.(cell, :pos) == pos
      end)
      |> Enum.map(&get.(&1, :id))
      |> Enum.take(6)

    %{index: index, surface: surface, required_pos: pos,
      dictionary_ids: dictionary_ids,
      lifg_selected?: Enum.any?(choices, &(Map.get(&1, :token_index) == index)),
      atl_selected?: Enum.any?(winners, &(Map.get(&1, :token_index) == index))}
  end

mwe_units =
  si
  |> Map.get(:mwe_lexicon, [])
  |> List.wrap()
  |> Enum.map(&Map.take(&1, [:span, :phrase, :lexical_pos]))
  |> Enum.take(12)

proposed = Core.Semantic.ParticipantSpatial.project(si, sentence)

IO.inspect(%{
  sentence: sentence,
  intent: Map.get(si, :intent),
  base_tokens: base_tokens,
  progressive_signatures:
    si
    |> Map.get(:sentence_signatures, [])
    |> List.wrap()
    |> Enum.filter(&(get.(&1, :id) == :progressive_locative_candidate)),
  participle_dictionary_evidence: participles,
  locative_role_evidence: locative_role_evidence,
  mwe_lexical_units: mwe_units,
  sentence_competition: Map.get(si, :sentence_competition),
  lifg_choices: choices,
  atl_winners: winners,
  proposed_participant_events: proposed,
  admitted_candidates: Map.get(si, :episodic_candidates)
}, pretty: true, limit: :infinity, label: "Participant spatial diagnostic")

case proposed do
  [%{entity_key: key, relation: relation}] ->
    owner = Application.get_env(:symbrella, :memory_owner_id, "local-primary")
    IO.inspect(Brain.Hippocampus.user_event(owner, key, relation),
      pretty: true, label: "Already-persisted event (read-only)")

  _ ->
    IO.puts("No uniquely supported participant event; no event was written.")
end
