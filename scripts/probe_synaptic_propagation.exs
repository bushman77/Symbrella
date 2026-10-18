# Real-dictionary, role-conditioned synaptic A/B without live LIFG mutation.
#
#   mix run scripts/probe_synaptic_propagation.exs
#
# Uses an isolated in-memory shadow, and independently processed sentences.
# One original experience can provide future exact-sense *shadow support*
# for a second subject and the same patient, but not an unrelated patient.
# This is a diagnostic signal, NOT demonstrated LIFG accuracy improvement.

alias Brain.Synapses.Shadow
alias Core.Brain.{SynapticExperience, SynapticPropagation}

opts = [
  mode: :prod,
  experience_origin: :user_turn,
  session_id: "propagation-probe-#{System.unique_integer([:positive, :monotonic])}",
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
  neuron_activation?: false,
  notify_brain?: false,
  synapse_shadow?: false,
  synapse_propagation?: false,
  synapse_correction?: false,
  lifg_opts: [grammar_weight: 1.0,
              cerebellum_learn?: false, cerebellum_snapshot_path: nil]
]

source = Core.resolve_input("The artist draws a picture.", opts)
matching = Core.resolve_input("The painter draws a picture.",
  Keyword.put(opts, :session_id, opts[:session_id] <> "-held-out"))
other_patient = Core.resolve_input("The painter draws a diagram.",
  Keyword.put(opts, :session_id, opts[:session_id] <> "-other-patient"))

{:ok, server} = Shadow.start_link(name: nil, durable?: false)

case SynapticExperience.extract(source) do
  {:ok, admitted} ->
    before = SynapticPropagation.compare(matching, synapse_shadow_server: server)
    unless before.status == :shadow_comparison do
      raise "Baseline cannot inspect the real verb candidate slate: #{inspect(before.status)}"
    end

    unless Shadow.observe(admitted, server) == {:ok, :recorded} do
      raise "Could not seed independent prior experience"
    end

    after_learning = SynapticPropagation.compare(matching, synapse_shadow_server: server)
    unrelated = SynapticPropagation.compare(other_patient, synapse_shadow_server: server)

    IO.puts("\n=== Learned synaptic role context: shadow A/B ===")

    describe = fn report ->
      %{
        status: report.status,
        candidates_checked: Map.get(report, :candidates_checked),
        selected_predicate_id: Map.get(report, :selected_predicate_id),
        support_on: get_in(report, [:propagation_on, :supported_count]),
        support_off: get_in(report, [:propagation_off, :supported_count]),
        positive: get_in(report, [:propagation_on, :candidates])
          |> List.wrap()
          |> Enum.filter(&(&1.shadow_signal > 0))
          |> Enum.take(8)
          |> Enum.map(&Map.take(&1, [:id, :shadow_signal,
                                     :provisional_signal, :reviewed_signal])),
        applied_to_lifg?: report.applied_to_lifg?,
        brain_cells_stimulated?: report.brain_cells_stimulated?
      }
    end

    IO.inspect(%{
      original_comprehension: source.sentence_meaning.status,
      matching_comprehension: matching.sentence_meaning.status,
      baseline: describe.(before),
      after_previous_experience: describe.(after_learning),
      different_patient: describe.(unrelated),
      shadow_receipts: Shadow.snapshot(server)
    }, pretty: true, limit: :infinity)

    assert_condition = fn ok?, why ->
      unless ok?, do: raise("FAIL: " <> why)
    end

    assert_condition.(before.propagation_on.supported_count == 0,
      "Untrained baseline must have no signal")
    assert_condition.(after_learning.status == :shadow_comparison,
      "Matching context must have an inspectable existing sense slate")
    assert_condition.(after_learning.propagation_on.supported_count > 0,
      "Matching patient must induce a weak prior-experience signal")
    assert_condition.(after_learning.propagation_off.supported_count == 0,
      "Disabled propagation must have no signal")
    assert_condition.(unrelated.status == :shadow_comparison and
      unrelated.propagation_on.supported_count == 0,
      "Unrelated patient must not inherit patient-conditioned signal")
    assert_condition.(Shadow.snapshot(server).experiences == 1 and
      Shadow.snapshot(server).connections == 2,
      "Read-only future comparisons must not alter the ledger")

    IO.puts("PASS: prior experience supplied bounded exact-sense support only for matching patient.")
    IO.puts("NOTE: LIFG decisions are unchanged; comprehension improvement has not been demonstrated.")

  _ ->
    raise "Source sentence lacked an independently validated SVO experience."
end

GenServer.stop(server)
