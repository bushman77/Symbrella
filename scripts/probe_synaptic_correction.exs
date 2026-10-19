# Verify correction-sensitive shadow plasticity in the real production pipeline.
# This invokes an explicit developer review, not a natural language command.
#
#   mix run scripts/probe_synaptic_correction.exs
#
# No BrainCell DB writes, no Axon training, no live LIFG adjustment.

alias Brain.Synapses.Shadow
alias Core.Brain.SynapticExperience

opts = [
  mode: :prod,
  experience_origin: :user_turn,
  session_id: "correction-probe-#{System.unique_integer([:positive, :monotonic])}",
  response: :off,
  episodes: false,
  persist_episodes: false,
  hippocampus_encode?: false,
  self_model: :off,
  comprehension_wm?: false,
  focus_prompt_topics?: false,
  prefrontal_control?: false,
  execute_agency_commands?: false,
  lifg_opts: [grammar_weight: 1.0, cerebellum_learn?: false,
              cerebellum_snapshot_path: nil]
]

si = Core.resolve_input("The artist draws a picture.", opts)
patient = fn -> Shadow.edge("draws|verb|1", "picture|noun|1", :patient) end

recorded = patient.()
rejected = SynapticExperience.review(si, :reject, "local-developer")
after_reject = patient.()

replay = Core.resolve_input("The artist draws a picture.",
  Keyword.put(opts, :session_id, opts[:session_id] <> "-replay"))
after_replay = patient.()

confirmed = SynapticExperience.review(si, :confirm, "local-developer")
after_confirm = patient.()

replay_event =
  Enum.find(replay.trace, fn
    %{stage: :synaptic_experience} -> true
    _ -> false
  end)

IO.inspect(%{
  before_review: recorded,
  rejected: rejected,
  after_reject: after_reject,
  replay: replay_event && replay_event.decision,
  after_replay: after_replay,
  confirmed: confirmed,
  after_confirm: after_confirm,
  metrics: Shadow.snapshot(),
  preview: SynapticExperience.preview(si, ["draws|verb|1"])
}, label: "Synaptic corrections", pretty: true, limit: :infinity)

unless match?(%{weight: 0.01}, recorded) and
       rejected == {:ok, :rejected} and after_reject == nil and
       match?(%{decision: :duplicate}, replay_event) and after_replay == nil and
       confirmed == {:ok, :confirmed} and
       match?(%{weight: value} when value >= 0.029 and value <= 0.03, after_confirm) do
  raise "Failed correction-sensitive synaptic shadow probe."
end

IO.puts("PASS: review revoked the weight, replay could not restore it, review restored it.")
