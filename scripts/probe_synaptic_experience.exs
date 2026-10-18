# Shadow synaptic plasticity with real Core comprehension and the existing
# production dictionary. No Axon model, DB connection updates, or LIFG rewiring.
# Run: mix run scripts/probe_synaptic_experience.exs
#
# This is a deliberately repeated claim, NOT independent corroboration.
# The second observation MUST NOT increase the provisional weight.

alias Brain.Synapses.Shadow

opts = [
  mode: :prod,
  experience_origin: :user_turn,
  session_id: "synapse-shadow-probe-#{System.unique_integer([:positive, :monotonic])}",
  response: :off,
  episodes: false,
  persist_episodes: false,
  hippocampus_encode?: false,
  self_model: :off,
  comprehension_wm?: false,
  focus_prompt_topics?: false,
  prefrontal_control?: false,
  execute_agency_commands?: false,
  lifg_opts: [grammar_weight: 1.0,
              cerebellum_learn?: false, cerebellum_snapshot_path: nil]
]

sentence = "The artist draws a picture."

before = Shadow.snapshot()
first = Core.resolve_input(sentence, opts)
after_first = Shadow.snapshot()
second = Core.resolve_input(sentence, Keyword.put(opts, :session_id, opts[:session_id] <> "-again"))
after_second = Shadow.snapshot()

status = fn si ->
  si.trace
  |> Enum.find(fn
    %{stage: :synaptic_experience} -> true
    _ -> false
  end)
  |> case do
    %{decision: decision} -> decision
    _ -> :missing
  end
end

IO.puts("\n=== Experience-driven synaptic learning (SHADOW ONLY) ===")

IO.inspect(%{
  first_sentence_status: first.sentence_meaning.status,
  first_experience: status.(first),
  replay_experience: status.(second),
  before: before,
  after_first: after_first,
  after_replay: after_second,
  predicate_patient_edge: Shadow.edge("draws|verb|1", "picture|noun|1", :patient)
}, pretty: true, limit: :infinity)

unless first.sentence_meaning.status == :structurally_interpreted and
       status.(first) == :recorded and status.(second) == :duplicate and
       after_first.experiences == before.experiences + 1 and
       after_second.experiences == after_first.experiences and
       after_second.connections == after_first.connections and
       match?(%{lexical_sense_verified?: false, deployment: :shadow_only},
         Shadow.edge("draws|verb|1", "picture|noun|1", :patient)) do
  raise "Synaptic shadow experience gating did not pass."
end

IO.puts("PASS: one experience updated provisional edges; replay did not.")
