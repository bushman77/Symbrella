# One-shot production dictionary probe for durable synaptic consolidation.
# Validates the actual Core semantic frame, then uses a dedicated private
# durable shadow instance to prove reconstruction after a GenServer restart.
#
#   mix run scripts/probe_synaptic_consolidation.exs
#
# Isolated proof: no live LIFG influence, no BrainCell DB writes, no changes
# to the chat's supervised synapse owner. Local temporary probe files removed
# after successful or failed validation.

alias Brain.Synapses.Shadow
alias Core.Brain.SynapticExperience

opts = [
  mode: :prod,
  experience_origin: :user_turn,
  session_id: "durable-probe-#{System.unique_integer([:positive, :monotonic])}",
  response: :off,
  episodes: false,
  persist_episodes: false,
  hippocampus_encode?: false,
  self_model: :off,
  comprehension_wm?: false,
  focus_prompt_topics?: false,
  prefrontal_control?: false,
  execute_agency_commands?: false,
  record_agency_commands?: false,
  notify_brain?: false,
  neuron_activation?: false,
  synapse_shadow?: false,
  synapse_correction?: false,
  lifg_opts: [grammar_weight: 1.0, cerebellum_learn?: false,
              cerebellum_snapshot_path: nil]
]

si = Core.resolve_input("The artist draws a picture.", opts)

case SynapticExperience.extract(si) do
  {:ok, observation} ->
    root = Path.join("tmp", "synapses")
    dir = Path.join(root, "consolidation-probe-#{System.unique_integer([:positive, :monotonic])}")

    try do
      {:ok, a} = Shadow.start_link(name: nil, durable?: true, dir: dir)
      assert_result = fn actual, expected ->
        unless actual == expected, do: raise("Expected #{inspect(expected)}, got #{inspect(actual)}")
      end

      assert_result.(Shadow.observe(observation, a), {:ok, :recorded})

      initial = Shadow.edge("draws|verb|1", "picture|noun|1", :patient, a)
      review = %{evidence: observation.evidence, provenance: :human_review,
                 reviewer: "developer", decision: :reject}

      assert_result.(Shadow.review(review, a), {:ok, :rejected})
      GenServer.stop(a)

      {:ok, b} = Shadow.start_link(name: nil, durable?: true, dir: dir)
      snapshot = Shadow.snapshot(b)
      assert_result.(Shadow.observe(observation, b), {:ok, :duplicate})
      removed = Shadow.edge("draws|verb|1", "picture|noun|1", :patient, b)
      assert_result.(removed, nil)

      reconfirm = %{review | decision: :confirm}
      assert_result.(Shadow.review(reconfirm, b), {:ok, :confirmed})
      restored = Shadow.edge("draws|verb|1", "picture|noun|1", :patient, b)
      GenServer.stop(b)

      {:ok, c} = Shadow.start_link(name: nil, durable?: true, dir: dir)
      reloaded = Shadow.edge("draws|verb|1", "picture|noun|1", :patient, c)
      final_status = Shadow.snapshot(c)
      GenServer.stop(c)

      IO.inspect(%{
        comprehension: si.sentence_meaning.status,
        before_review: initial,
        after_restart: snapshot,
        rejected_edge: removed,
        after_confirm: restored,
        reconstructed_again: reloaded,
        final_status: final_status
      }, label: "Synaptic consolidation", pretty: true, limit: :infinity)

      unless snapshot.persistence_status == :ready and
             snapshot.rejected_experiences == 1 and
             match?(%{weight: weight} when weight >= 0.029 and weight <= 0.03,
               reloaded) and
             reloaded == restored and
             final_status.confirmed_experiences == 1 and
             final_status.live_priming? == false do
        raise "Synaptic receipt recovery invariant failed"
      end

      IO.puts("PASS: durable receipt restored, rejected replay blocked, reviewed weights reconstructed.")
    after
      File.rm_rf(dir)
    end

  _ ->
    raise "Production dictionary did not produce an independently validated SVO experience."
end
