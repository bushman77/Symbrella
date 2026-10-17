# Read-only production-equivalent SVO diagnostic.
#
#   mix run scripts/trace_action_participants.exs
#
# The same lexical/LIFG options as Phoenix HomeLive, but response planning
# and episode persistence are disabled. Never mistake a fixture test for an
# actual production lexical proof.
sentence = "The artist draws a picture."

si =
  Core.resolve_input(sentence,
    mode: :prod,
    enrich_lexicon?: true,
    lexicon_stage?: true,
    lifg_opts: [grammar_weight: 1.0],
    response: :off,
    persist_episodes: false
  )

choice_rows =
  si
  |> Map.get(:lifg_choices, [])
  |> List.wrap()
  |> Enum.map(&Map.take(&1, [:token_index, :id, :chosen_id, :lemma]))

winners =
  si
  |> Map.get(:atl_slate, %{})
  |> then(fn slate -> if is_map(slate), do: Map.get(slate, :winners, []), else: [] end)
  |> List.wrap()
  |> Enum.map(&Map.take(&1, [:token_index, :id, :chosen_id, :lemma]))

signatures =
  si
  |> Map.get(:sentence_signatures, [])
  |> List.wrap()
  |> Enum.filter(&(Map.get(&1, :id) == :finite_present_svo_candidate))

candidates =
  si
  |> Map.get(:episodic_candidates, [])
  |> List.wrap()

revalidated =
  Enum.map(candidates, fn event ->
    %{
      relation: event.relation,
      result: Core.Semantic.EpisodicProjection.validate_candidate(si, event)
    }
  end)

IO.inspect(%{
  sentence: sentence,
  source: si.source,
  intent: si.intent,
  base_tokens:
    si.tokens
    |> List.wrap()
    |> Enum.filter(&(Map.get(&1, :n, 1) == 1))
    |> Enum.map(&Map.take(&1, [:index, :phrase])),
  finite_present_svo_signatures: signatures,
  lifg_choices: choice_rows,
  atl_winners: winners,
  projected_actions: candidates,
  validation: revalidated
}, label: "Read-only action participant diagnostic", pretty: true, limit: :infinity)

if candidates == [] do
  IO.puts("No complete action event admitted: inspect morphological evidence and LIFG/ATL agreement.")
else
  IO.puts("Action candidates were projected. No episode was written.")
end
