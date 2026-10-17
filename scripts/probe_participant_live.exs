# Run the same semantic options as the LiveView chat without invoking the LLM.
# By default this is READ-ONLY. Pass --persist to test the validated
# Memory.acknowledge_projected -> Hippocampus database handoff explicitly,
# using an isolated temporary owner that is cleaned up after retrieval.
#
#   mix run scripts/probe_participant_live.exs
#   mix run scripts/probe_participant_live.exs --persist
#
# This probe does not test the already-running Phoenix worker's code version.
import Ecto.Query

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

if persist? do
  # Isolate the test from the normal live-chat memory owner. A successful
  # probe must not make a subsequent chat run appear to have persisted.
  owner_key = :memory_owner_id
  original_owner = Application.get_env(:symbrella, owner_key)
  probe_owner = "participant-probe-#{System.unique_integer([:positive, :monotonic])}"
  Application.put_env(:symbrella, owner_key, probe_owner)

  try do
    acknowledgement =
      Core.Response.Memory.acknowledge_projected(
        Map.from_struct(si), session_id, sentence, si.intent, si.confidence || 0.0
      )

    persisted =
      Brain.Hippocampus.user_event(probe_owner, "bird", "spatial_state")

    IO.inspect(acknowledgement, label: "Validated memory acknowledgement", pretty: true)
    IO.inspect(persisted, label: "Isolated PostgreSQL participant event", pretty: true)

    for question <- [
          "Where is the bird?",
          "What is sitting on the fence?",
          "What is the bird doing?",
          "What is the bird sitting on?"
        ] do
      IO.inspect(Core.Response.EpisodicMemory.reply(question),
        label: "Independent symbolic recall: #{question}",
        pretty: true
      )
    end

    cond do
      not match?({_, _, %{persistence_status: :persisted}}, acknowledgement) ->
        IO.puts("Persistence check FAILED: no confirmed memory acknowledgement.")

      not match?(%{status: "asserted", value: "the fence"}, persisted) ->
        IO.puts("Persistence check FAILED: PostgreSQL readback missing.")

      true ->
        IO.puts("Persistence and readback confirmed under isolated probe owner.")
    end
  after
    try do
      {deleted, _} =
        Db.delete_all(
          from e in Db.Episode,
            where: fragment("? ->> 'owner' = ?", e.meta, ^probe_owner)
        )

      IO.puts("Isolated probe cleanup deleted #{deleted} row(s).")
    after
      if is_nil(original_owner),
        do: Application.delete_env(:symbrella, owner_key),
        else: Application.put_env(:symbrella, owner_key, original_owner)
    end
  end
else
  IO.puts("No event was written. Run with --persist for an isolated write/read test.")
end
