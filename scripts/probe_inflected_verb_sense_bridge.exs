# Read-only production-dictionary inflection -> base verb sense projection.
#
#   mix run scripts/probe_inflected_verb_sense_bridge.exs
#
# Uses independently verified finite-present SVO structure and Wiktextract
# morphology to LOOK UP base verb senses. It does NOT transfer any
# experience weight from an inflected surface to a specific base meaning.

alias Core.Brain.InflectedVerbSenseBridge

opts = [
  mode: :prod,
  experience_origin: :user_turn,
  session_id: "inflection-sense-bridge-probe",
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
  synapse_propagation?: false,
  synapse_correction?: false,
  lifg_opts: [grammar_weight: 1.0, scores: :all,
              synapse_eval_full_scores?: true,
              cerebellum_learn?: false, cerebellum_snapshot_path: nil]
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

IO.puts("\n=== Attested inflection -> base sense candidates (READ ONLY) ===")

reports =
  Enum.with_index(sentences, 1)
  |> Enum.map(fn {sentence, n} ->
    si = Core.resolve_input(sentence,
      Keyword.put(opts, :session_id, "bridge-eval-#{n}"))

    bridge = InflectedVerbSenseBridge.project(si)

    compact =
      case bridge do
        %{status: :read_only_sense_inventory} ->
          %{
            sentence: sentence,
            base_lexeme: bridge.attested_base_lexeme,
            selected_surface: bridge.original_surface_id,
            base_verb_senses: bridge.base_sense_count,
            evidence_supported_candidates: bridge.candidates_with_context_evidence,
            lexical_hypothesis: bridge.lexical_hypothesis,
            top_gap: bridge.top_two_gap,
            top_candidates: Enum.take(bridge.ranked, 4)
              |> Enum.map(fn item ->
                %{id: item.id,
                  score: item.context_fit,
                  matched: item.matched_context_terms,
                  gloss: item.dictionary_definition}
              end),
            mapped_to_specific_sense?: bridge.surface_to_specific_base_sense_verified?,
            lifg_winner_revised?: bridge.applied_to_lifg?
          }

        other ->
          %{sentence: sentence, result: other}
      end

    IO.inspect(compact, label: "Sense bridge #{n}", pretty: true, limit: :infinity)
    bridge
  end)

complete = Enum.filter(reports, &(&1.status == :read_only_sense_inventory))
multi_sense = Enum.count(complete, &(&1.base_sense_count >= 2))
supported = Enum.count(complete, &(&1.candidates_with_context_evidence > 0))
hypotheses = Enum.count(complete, &(not is_nil(&1.lexical_hypothesis)))

IO.inspect(%{
  total: length(reports),
  base_sense_slates: length(complete),
  multi_sense_slates: multi_sense,
  context_supported: supported,
  lexical_hypotheses_only: hypotheses,
  verified_specific_sense_mappings: 0,
  synaptic_weights_transferred: 0,
  live_lifg_revisions: 0
}, label: "Base sense projection summary")

if multi_sense == 0 do
  IO.puts("INSUFFICIENT_BASE_SENSE_EVIDENCE: no usable dictionary competition was projected.")
else
  IO.puts("PASS: genuine base-verb dictionary senses were projected read-only.")
  IO.puts("NOTE: contextual lexical overlap is a hypothesis, not a verified sense identity.")
end
