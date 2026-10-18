# Inspects real selected noun-sense admission without durable writes.
#
#   mix run scripts/probe_live_noun_admission.exs
#
# No LIFG, BrainCell, synapse or response modification.
alias Core.Brain.RoleSemanticContext
alias Core.Semantic.SVOEvent

opts = [
  mode: :prod,
  experience_origin: :user_turn,
  session_id: "noun-admission-read-only",
  # Mirror the HomeLive lexical admission path; disable only responses/writes.
  fuzzy: true,
  enrich_lexicon?: true,
  lexicon_stage?: true,
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
  lifg_opts: [grammar_weight: 1.0, scores: :all,
              synapse_eval_full_scores?: true,
              cerebellum_learn?: false, cerebellum_snapshot_path: nil]
]

si = Core.resolve_input("The baker cooks a meal.", opts)
audit = RoleSemanticContext.extract(si)

IO.puts("\n=== Selected noun-role admission (read only) ===")
IO.inspect(%{
  intent: si.intent,
  speech_act: Map.get(si, :conversation_act),
  episodic_candidate_count: length(List.wrap(si.episodic_candidates)),
  finite_present_svo_signatures:
    si
    |> Map.get(:sentence_signatures)
    |> List.wrap()
    |> Enum.count(&(Map.get(&1, :id) == :finite_present_svo_candidate)),
  sentence_meaning: Map.take(si.sentence_meaning || %{},
    [:kind, :status, :speech_act, :source_frame]),
  admission_status: audit.status,
  roles: audit.contexts
}, label: "Role gate", pretty: true, limit: :infinity)

case SVOEvent.complete_signature(si) do
  %{roles: %{subject_head: subject_index, object_head: object_index}} ->
    Enum.each([{:actor, subject_index}, {:patient, object_index}], fn {role, index} ->
      choice =
        Enum.find(List.wrap(Map.get(si, :lifg_choices)),
          &(is_map(&1) and Map.get(&1, :token_index) == index))

      if is_map(choice) do
        id = Map.get(choice, :id)
        surface = if(is_binary(id), do: hd(String.split(id, "|")), else: "")
        scores = Map.get(choice, :candidate_probs)

        same_surface_noun =
          case scores do
            %{} = rows ->
              rows
              |> Enum.filter(fn {candidate_id, _} ->
                case String.split(to_string(candidate_id), "|", parts: 3) do
                  [^surface, "noun", _] -> true
                  _ -> false
                end
              end)
              |> Enum.sort_by(fn {_id, score} -> -score end)
              |> Enum.take(10)

            _ -> []
          end

        active = Enum.filter(List.wrap(Map.get(si, :active_cells)),
          &(is_map(&1) and Map.get(&1, :id) == id))

        IO.inspect(%{
          role: role,
          chosen_id: id,
          selected_probability: if(is_map(scores), do: Map.get(scores, id), else: nil),
          scored_candidates: if(is_map(scores), do: map_size(scores), else: nil),
          probability_sum: if(is_map(scores),
            do: Enum.reduce(scores, 0.0, fn {_key, v}, acc -> acc + v end),
            else: nil),
          same_surface_noun_scores: same_surface_noun,
          active_cell_matches: length(active),
          selected_definition_matches_active_cell?:
            Enum.any?(active,
              &(Map.get(&1, :definition) == Map.get(choice, :definition))),
          role_gate:
            Enum.find(audit.contexts, &(Map.get(&1, :role) == role))
        }, label: "Selected #{role}", pretty: true, limit: :infinity)
      end
    end)

  _ ->
    IO.puts("No complete current SVO signature.")
end

IO.puts("Diagnostic only. No inferred noun meaning was promoted or stored.")
