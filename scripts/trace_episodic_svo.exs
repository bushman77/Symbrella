# Live pipeline diagnostic for declarative subject-verb-object episode admission.
# This run disables response generation and normal episode persistence.
#
# mix run scripts/trace_episodic_svo.exs
# mix run scripts/trace_episodic_svo.exs "The artist draws a picture."
#
# Observed decisions are printed; this script does not insert a typed event.
sentence =
  case System.argv() do
    [] -> "The artist draws a picture."
    words -> Enum.join(words, " ")
  end

si =
  Core.resolve_input(sentence,
    mode: :prod,
    response: :off,
    persist_episodes: false,
    fuzzy: false
  )

get = fn item, key ->
  if is_map(item), do: Map.get(item, key, Map.get(item, Atom.to_string(key))), else: nil
end

signature_count =
  si
  |> Map.get(:sentence_signatures, [])
  |> List.wrap()
  |> Enum.filter(&(get.(&1, :id) == :finite_present_svo_candidate))

choices =
  si
  |> Map.get(:lifg_choices, [])
  |> List.wrap()
  |> Enum.map(&Map.take(&1, [:token_index, :chosen_id, :id, :lemma, :score, :margin]))

winners =
  si
  |> Map.get(:atl_slate, %{})
  |> case do
    %{} = slate -> get.(slate, :winners) || []
    _ -> []
  end
  |> List.wrap()
  |> Enum.map(&Map.take(&1, [:token_index, :id, :chosen_id, :lemma, :norm, :score]))

base_tokens =
  si
  |> Map.get(:tokens, [])
  |> List.wrap()
  |> Enum.filter(fn token ->
    n = get.(token, :n)
    kind = get.(token, :kind)
    (is_nil(n) or n == 1) and kind not in [:punct, "punct"]
  end)
  |> Enum.map(&Map.take(&1, [:index, :phrase, :n, :kind]))

IO.inspect(%{
  sentence: sentence,
  intent: Map.get(si, :intent),
  conversation_act: Map.get(si, :conversation_act),
  source: Map.get(si, :source),
  base_tokens: base_tokens,
  svo_signatures: signature_count,
  lifg_choices: choices,
  atl_winners: winners,
  proposed_events: Core.Semantic.SVOEvent.project(si, sentence),
  admitted_candidates: Map.get(si, :episodic_candidates)
}, pretty: true, limit: :infinity, label: "Semantic SVO diagnostic")

owner = Application.get_env(:symbrella, :memory_owner_id, "local-primary")

IO.inspect(
  Brain.Hippocampus.user_event(owner, "artist", "verb_draw"),
  label: "Already-persisted artist/draw event",
  pretty: true
)
