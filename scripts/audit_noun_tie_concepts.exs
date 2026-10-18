# Read-only audit: exact equal LIFG noun-sense scores != common meaning.
#
#   mix run scripts/audit_noun_tie_concepts.exs
#
# Full dictionary records are checked for every tied noun sense.
# Only recurrent gloss TERMS are reported; no sense is selected, no
# semantic scores change, and no user memory/synaptic writes are made.

alias Core.Brain.NounTieConceptAudit

opts = [
  mode: :prod,
  experience_origin: :user_turn,
  session_id: "noun-tie-concept-audit",
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

IO.puts("\n=== Exact noun ties vs shared dictionary gloss concepts (READ ONLY) ===")

audits =
  sentences
  |> Enum.with_index(1)
  |> Enum.map(fn {sentence, i} ->
    si = Core.resolve_input(sentence,
      Keyword.put(opts, :session_id, "noun-tie-concept-audit-#{i}"))
    audit = NounTieConceptAudit.inspect(si)

    role_rows = Enum.map(Map.get(audit, :roles, []), fn role ->
      Map.take(role, [
        :role, :status, :reason, :noun_norm, :tied_noun_sense_ids,
        :tied_count, :unanimous_gloss_tokens,
        :recurrent_but_not_unanimous_gloss_tokens,
        :score_tie_proves_shared_concept?,
        :exact_noun_sense_verified?, :promoted_sense?
      ])
    end)

    IO.inspect(%{case: i, sentence: sentence,
      status: audit.status, roles: role_rows},
      label: "Noun tie audit #{i}", pretty: true, limit: :infinity)

    audit
  end)

roles =
  audits
  |> Enum.flat_map(&Map.get(&1, :roles, []))

IO.inspect(%{
  sentences: length(sentences),
  auditable_exact_ties: Enum.count(roles,
    &(Map.get(&1, :status) in [
      :ambiguous_with_unanimous_terms,
      :ambiguous_with_no_unanimous_terms
    ])),
  ties_with_unanimous_gloss_tokens: Enum.count(roles,
    &(Map.get(&1, :status) == :ambiguous_with_unanimous_terms)),
  ties_without_unanimous_gloss_tokens: Enum.count(roles,
    &(Map.get(&1, :status) == :ambiguous_with_no_unanimous_terms)),
  changed_lifg?: false,
  changed_synapses?: false,
  verified_meanings?: false
}, label: "Tie/gloss summary")

IO.puts("""
Reminder: identical LIFG scores only establish unresolved competition.
Repeated dictionary gloss words are lexical evidence of potential shared
properties, not verified concepts or a uniquely selected meaning.
""")
