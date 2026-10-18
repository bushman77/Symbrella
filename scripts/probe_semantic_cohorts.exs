# Real-dictionary, read-only comparison of exact-sense shadow decisions
# and sentence-conditioned semantic cohorts.
#
#   mix run scripts/probe_semantic_cohorts.exs
#
# No generated answer, live sense choice, synaptic update, event persistence,
# automatically accepted labels or cohort-to-LIFG feedback.

alias Core.Brain.{SemanticCohort, SemanticSenseValidator}

opts = [
  mode: :prod,
  experience_origin: :user_turn,
  session_id: "semantic-cohort-probe",
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
  record_agency_commands?: false,
  neuron_activation?: false,
  notify_brain?: false,
  synapse_shadow?: false,
  semantic_sense_shadow?: false,
  synapse_propagation?: false,
  synapse_correction?: false,
  lifg_opts: [
    grammar_weight: 1.0,
    scores: :all,
    synapse_eval_full_scores?: true,
    cerebellum_learn?: false,
    cerebellum_snapshot_path: nil
  ]
]

sentences = [
  "The painter draws a picture.",
  "The baker cooks a meal.",
  "The gardener plants a tree.",
  "The author writes a letter.",
  "The student paints a portrait.",
  "The worker builds a house.",
  "The child throws a ball.",
  "The captain steers a ship."
]

IO.puts("\n=== Semantic cohorts / exact sense validation (read-only) ===")

reports =
  sentences
  |> Enum.with_index(1)
  |> Enum.map(fn {sentence, i} ->
    si = Core.resolve_input(sentence,
      Keyword.put(opts, :session_id, "cohort-probe-#{i}"))
    result = SemanticSenseValidator.evaluate(si,
      role_context?: true, cohort_evidence?: true)
    cohort = SemanticCohort.evaluate(si, result)

    groups =
      cohort
      |> Map.get(:cohorts, [])
      |> Enum.map(fn c ->
        Map.take(c, [:anchor_term, :evidence_origin,
          :member_count, :members, :shared_definition_terms,
          :minimum_member_support, :exact_sense_unresolved?,
          :shared_meaning_verified?])
      end)

    IO.inspect(%{
      case: i,
      sentence: sentence,
      verb_base: Map.get(result, :attested_base_lexeme),
      original_sense_hypothesis: Map.get(result, :baseline_id),
      validator_status: result.status,
      validator_proposed_id: Map.get(result, :proposed_id),
      validator_reason: Map.get(result, :reason),
      cohort_status: cohort.status,
      cohort_reason: Map.get(cohort, :reason),
      eligible_senses: Map.get(cohort, :eligible_sense_count, 0),
      group_count: Map.get(cohort, :cohort_count, 0),
      groups: groups,
      selected_specific_meaning?: false,
      modified_live_lifg?: false,
      changed_synapses?: false
    }, label: "Cohort case #{i}", limit: :infinity, pretty: true)

    %{validator: result, cohort: cohort}
  end)

IO.inspect(%{
  sentences: length(reports),
  checked: Enum.count(reports, &(&1.validator.status in
    [:abstained, :provisional_hypothesis])),
  with_supported_cohorts: Enum.count(reports,
    &(&1.cohort.status == :candidate_cohorts)),
  total_reported_cohorts: Enum.reduce(reports, 0,
    &(&1.cohort.cohort_count + &2)),
  exact_sense_proposals: Enum.count(reports,
    &(&1.validator.status == :provisional_hypothesis)),
  independent_gold_sense_labels?: false,
  cohort_promoted_to_lifg?: false,
  synaptic_learning?: false
}, label: "Semantic cohort shadow summary", pretty: true)

IO.puts("""
A cohort is an anchor-linked compatibility hypothesis, NOT proof that
dictionary senses are equivalent. Overlapping cohorts do not merge by
transitivity. Exact-sense proposals remain the unchanged validator output.
""")
