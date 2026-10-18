# Exports UNLABELED full dictionary sense candidates for independent review.
#
#   mix run scripts/export_semantic_sense_review.exs
#
# This file contains no projected scores, no predictions and NO gold labels.
# Reviewers must determine gold from dictionary meanings independently.
# It writes only tmp/eval/semantic_sense_review.jsonl; never BrainCells,
# hippocampal memories, synapses, LIFG choices, or the gold label file.

alias Core.Brain.InflectedVerbSenseBridge

opts = [
  mode: :prod,
  experience_origin: :user_turn,
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

rows =
  sentences
  |> Enum.with_index(1)
  |> Enum.map(fn {sentence, index} ->
    si = Core.resolve_input(sentence,
      Keyword.put(opts, :session_id, "independent-sense-review-#{index}"))

    bridge = InflectedVerbSenseBridge.project(si, validation_evidence?: true)

    case bridge do
      %{status: :read_only_sense_inventory, validation_rows: rows}
          when is_list(rows) ->
        %{
          example: index,
          sentence: sentence,
          base_lexeme: bridge.attested_base_lexeme,
          status: "review_candidates",
          predictions_included: false,
          gold_label_included: false,
          candidate_count: length(rows),
          candidates:
            rows
            |> Enum.sort_by(& &1.id)
            |> Enum.map(fn candidate ->
              ev = candidate.validation_evidence
              %{
                id: candidate.id,
                definition: Map.get(ev, :definition),
                example: Map.get(ev, :example),
                gram_function: Map.get(ev, :gram_function),
                semantic_atoms: Map.get(ev, :semantic_atoms)
              }
            end)
        }

      %{status: :skipped, reason: reason} ->
        %{example: index, sentence: sentence,
          status: "unavailable", reason: inspect(reason),
          candidates: []}

      _ ->
        %{example: index, sentence: sentence,
          status: "unavailable", candidates: []}
    end
  end)

path = "tmp/eval/semantic_sense_review.jsonl"
File.mkdir_p!(Path.dirname(path))

body =
  rows
  |> Enum.map(&(Jason.encode!(&1) <> "\n"))
  |> IO.iodata_to_binary()

File.write!(path, body)

IO.inspect(%{
  output: path,
  examples: length(rows),
  reviewable: Enum.count(rows, &(&1.status == "review_candidates")),
  candidates_total: Enum.reduce(rows, 0, &(length(&1.candidates) + &2)),
  contains_predictions?: false,
  contains_gold_labels?: false,
  changed_cognitive_state?: false
}, label: "Unlabeled, reviewer-only dictionary export")

IO.puts("""
Gold annotations belong in the separate tmp/eval/semantic_sense_gold.jsonl.
Review the complete sense inventory independently before annotating.
Do not use the model's projected rankings as the annotation answer.
""")
