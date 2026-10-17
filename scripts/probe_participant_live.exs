# Run the same semantic options as the LiveView chat without invoking the LLM.
# By default this is READ-ONLY. Pass --persist to test the validated
# Memory.acknowledge_projected -> Hippocampus database handoff explicitly.
#
#   mix run scripts/probe_participant_live.exs
#   mix run scripts/probe_participant_live.exs --persist
#
# This probe does not test the already-running Phoenix worker's code version.
sentence = "A bird is sitting on the fence."
persist? = "--persist" in System.argv()
session_id = "participant-live-probe-#{System.unique_integer([:positive])}"

si =
  Core.resolve_input(sentence,
    mode: :prod,
    enrich_lexicon?: true,
    lexicon_stage?: true,
    lifg_opts: [grammar_weight: 1.0],
    session_id: session_id,
    response: :off,
    persist_episodes: false
  )

candidates = Map.get(si, :episodic_candidates, []) || []
proofs =
  Enum.map(candidates, fn candidate ->
    %{relation: Map.get(candidate, :relation),
      entity_key: Map.get(candidate, :entity_key),
      validation: Core.Semantic.EpisodicProjection.validate_candidate(si, candidate)}
  end)

IO.inspect(%{
  mode: :liveview_equivalent,
  source: si.source,
  intent: si.intent,
  sentence: si.sentence,
  selected_roles:
    si.lifg_choices
    |> List.wrap()
    |> Enum.map(&Map.take(&1, [:id, :chosen_id, :token_index])),
  grammar:
    si.sentence_signatures
    |> List.wrap()
    |> Enum.filter(&(Map.get(&1, :id) == :progressive_locative_candidate)),
  episodic_candidates: candidates,
  revalidation: proofs,
  response_planning_suppressed?: true,
  persistent_write_requested?: persist?
}, label: "LiveView-equivalent participant projection", pretty: true, limit: :infinity)

owner = Application.get_env(:symbrella, :memory_owner_id, "local-primary")

if persist? do
  result =
    Core.Response.Memory.acknowledge_projected(
      Map.from_struct(si), session_id, sentence, si.intent, si.confidence || 0.0
    )

  IO.inspect(result, label: "Validated memory acknowledgement", pretty: true)
  IO.inspect(
    Brain.Hippocampus.user_event(owner, "bird", "spatial_state"),
    label: "Persisted participant episode",
    pretty: true
  )
else
  IO.puts("No event was written. Run with --persist to test the database write.")
end
