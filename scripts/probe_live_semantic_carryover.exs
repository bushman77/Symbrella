# Exercise genuine Core -> Brain -> Core neural carryover using the imported
# production dictionary. Unlike compare_neuron_comprehension.exs, NO synthetic
# neuron or semantic probe snapshot is installed. This intentionally stimulates
# the real Brain coordinator, but disables episode/user memory persistence,
# neural calibration learning, and LLM replies.
#
#   mix run scripts/probe_live_semantic_carryover.exs
#
# Requires production lexical records for:
#   "The artist draws a picture." / "what did the artist draw?"
#
# Not a clean-room causality experiment: other cognitive singletons can
# change transient state, and direct memory fast-path replies are not tested.

alias Core.Brain.SemanticPropagation

session = "live-pulse-probe-#{System.unique_integer([:positive, :monotonic])}"
other_session = session <> "-other"

opts = [
  mode: :prod,
  session_id: session,
  response: :off,
  episodes: false,
  persist_episodes: false,
  hippocampus_encode?: false,
  persist_self_model: false,
  write_self_monitor_memory: false,
  comprehension_wm?: false,
  focus_prompt_topics?: false,
  lifg_opts: [cerebellum_learn?: false, cerebellum_snapshot_path: nil],
  neuron_activation?: true
]

feedback = fn si ->
  Enum.find(si.trace, fn
    %{stage: :neuron_feedback} -> true
    _ -> false
  end)
end

primed_count = fn si ->
  si
  |> Map.get(:active_cells, [])
  |> Enum.count(fn
    %{id: "draw|verb|" <> suffix, prior_semantic_activation: value}
    when suffix != "" and suffix != "fallback" and is_number(value) and value > 0 ->
      true

    _ ->
      false
  end)
end

IO.puts("\n=== Live Core / Brain semantic carryover (no synthetic snapshots) ===")
IO.puts("Session: #{session}")

first =
  Core.resolve_input("The artist draws a picture.",
    Keyword.put(opts, :notify_brain?, true))

source = SemanticPropagation.lemma_pulse(first)
accepted? = match?(%{meta: %{lemma_pulse_submitted?: true}}, feedback.(first))
stored = Brain.semantic_snapshot(session)

second =
  Core.resolve_input("what did the artist draw?",
    Keyword.put(opts, :notify_brain?, false))

off =
  Core.resolve_input("what did the artist draw?",
    opts
    |> Keyword.put(:notify_brain?, false)
    |> Keyword.put(:semantic_propagation?, false))

other = Brain.semantic_snapshot(other_session)

IO.inspect(%{
  complete_svo: case Core.Semantic.SVOEvent.complete_signature(first) do
    %{span: span} -> span
    _ -> nil
  end,
  first_structural_status: get_in(first, [:sentence_meaning, :status]),
  first_episode_count: length(List.wrap(first.episodic_candidates)),
  pulse_source: source,
  pulse_submitted?: accepted?,
  brain_stored_pulse: stored,
  second_live_pulse: second.semantic_pulses,
  second_primed_draw_verbs: primed_count.(second),
  second_structural_status: get_in(second, [:sentence_meaning, :status]),
  propagation_off_pulse: off.semantic_pulses,
  propagation_off_primed_draw_verbs: primed_count.(off),
  other_session_pulse: other
}, label: "Live two-turn result", pretty: true, limit: :infinity)

unless match?(%{lemma: "draw", source_id: "draws|verb|1"}, source) and
         accepted? and
         (is_number(Map.get(stored, "draw")) and Map.get(stored, "draw") > 0) and
         (is_number(Map.get(second.semantic_pulses, "draw")) and
            Map.get(second.semantic_pulses, "draw") > 0) and
         primed_count.(second) > 0 and primed_count.(off) == 0 and
         off.semantic_pulses == %{} and other == %{} do
  raise "Live semantic carryover not established; review diagnostic above."
end

IO.puts("PASS: real Brain session pulse reached next-turn lexical candidates.")
