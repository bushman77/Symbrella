# End-to-end production-dictionary contradiction *proposal* probe.
#
#   mix run scripts/probe_synaptic_contradiction.exs
#
# A correctly parsed explicit correction may suggest a review, never alter the
# receipt or its weight. The two reconstructed assertions are separately
# interpreted by real Core with feedback, persistence, LLM and learning off.

alias Brain.Synapses.Shadow

opts = [
  mode: :prod,
  experience_origin: :user_turn,
  session_id: "contradiction-probe-#{System.unique_integer([:positive, :monotonic])}",
  fuzzy: false,
  response: :off,
  episodes: false,
  persist_episodes: false,
  hippocampus_encode?: false,
  self_model: :off,
  comprehension_wm?: false,
  focus_prompt_topics?: false,
  prefrontal_control?: false,
  execute_agency_commands?: false,
  notify_brain?: false,
  neuron_activation?: false,
  lifg_opts: [grammar_weight: 1.0, cerebellum_learn?: false,
              cerebellum_snapshot_path: nil]
]

first = Core.resolve_input("The artist draws a picture.", opts)
before = Shadow.snapshot()
original = Shadow.edge("draws|verb|1", "picture|noun|1", :patient)

correction =
  Core.resolve_input("Actually, the artist draws a diagram, not a picture.", opts)

after_correction = Shadow.snapshot()
unchanged = Shadow.edge("draws|verb|1", "picture|noun|1", :patient)

event = fn si, stage ->
  Enum.find(si.trace, fn
    %{stage: ^stage} -> true
    _ -> false
  end)
end

proposal = event.(correction, :synaptic_correction)
first_experience = event.(first, :synaptic_experience)

IO.puts("\n=== Explicit contradiction detection (READ-ONLY) ===")
IO.inspect(%{
  first_sentence_status: first.sentence_meaning.status,
  original_experience: first_experience && first_experience.decision,
  old_patient_edge: original,
  proposal: proposal && %{
    decision: proposal.decision,
    details: proposal.meta
  },
  shadow_before: before,
  shadow_after: after_correction,
  old_patient_edge_after: unchanged,
  proposed_new_patient_edge: Shadow.edge("draws|verb|1", "diagram|noun|1", :patient)
}, pretty: true, limit: :infinity)

unless match?(%{decision: :recorded}, first_experience) and
       match?(%{decision: :review_proposed,
                meta: %{applied?: false, externally_verified?: false}},
         proposal) and
       Shadow.snapshot() == before and original == unchanged and
       Shadow.edge("draws|verb|1", "diagram|noun|1", :patient) == nil do
  raise "Explicit correction did not produce a non-mutating review proposal."
end

IO.puts("PASS: explicit patient correction proposed a review without modifying synapses.")
