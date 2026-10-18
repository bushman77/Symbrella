# The observed LLM hallucination, tested against live read-only
# Core comprehension. Does not call the LLM, emit a response, or write
# hippocampal/synaptic records.
#
# mix run scripts/probe_semantic_role_fidelity.exs

alias Core.Response.{ProjectedExpression, SemanticRoleFidelity}

opts = [
  mode: :prod, experience_origin: :user_turn,
  session_id: "semantic-role-fidelity-diagnostic",
  fuzzy: true, enrich_lexicon?: true, lexicon_stage?: true,
  response: :off,
  episodes: false, persist_episodes: false,
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
  lifg_opts: [
    grammar_weight: 1.0, scores: :all,
    synapse_eval_full_scores?: true,
    cerebellum_learn?: false,
    cerebellum_snapshot_path: nil
  ]
]

cases = [
  {"The gardener plants a tree.",
   "The gardener is also cooking a meal."},
  {"The baker cooks a meal.",
   "The baker is also planting a tree, which is a tree in a garden."},
  {"The child throws a ball.",
   "A ball throws the child."},
  {"The captain steers a ship.",
   "The captain steers a ship."}
]

IO.puts("\n=== Response semantic role fidelity (READ ONLY) ===")

for {{sentence, draft}, index} <- Enum.with_index(cases, 1) do
  si = Core.resolve_input(sentence,
    Keyword.put(opts, :session_id, "role-fidelity-#{index}"))

  frame = ProjectedExpression.from_si(si)
  audit = SemanticRoleFidelity.audit(sentence, draft, frame)
  response_review = ProjectedExpression.review_draft(sentence, draft, frame)

  IO.inspect(%{
    case: index,
    user_statement: sentence,
    simulated_model_draft: draft,
    source_frame_status: if(is_map(frame), do: frame.evidence_level,
      else: :unavailable),
    source_actor: if(is_map(frame), do: frame.actor, else: nil),
    source_predicate: if(is_map(frame),
      do: frame.predicate_lemma, else: nil),
    source_patient: if(is_map(frame), do: frame.patient, else: nil),
    semantic_role_fidelity: audit,
    would_review_draft_as: response_review,
    independent_event_truth_verified?: false,
    new_memory_writes?: false,
    altered_lifg?: false,
    altered_synapses?: false
  }, label: "Role fidelity case #{index}", pretty: true, limit: :infinity)
end

IO.puts("""
A role mismatch audit is an inspectable *limited grammatical comparison*,
not dictionary entailment. Complex or ambiguous responses abstain.
No LLM was called; normal chat runs the guard after model reflection.
""")
