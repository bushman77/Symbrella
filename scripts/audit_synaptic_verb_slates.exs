# Read-only investigation of real LIFG score coverage and dictionary verb forms.
#
#   mix run scripts/audit_synaptic_verb_slates.exs
#
# Explains why a 2-4-row score map may have only ONE verb sense.
# Never invents base-lemma sense IDs or changes LIFG, synapses, or BrainCells.
# Database lookups for verified base lexemes are observational only.

alias Core.Brain.SynapticSlateAudit

opts = [
  mode: :prod,
  experience_origin: :user_turn,
  session_id: "synaptic-slate-coverage-audit",
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

base_db_lookup = fn base ->
  if is_binary(base) and Code.ensure_loaded?(Db) and
       function_exported?(Db, :brain_cells_for_norm, 1) do
    try do
      cells = Db.brain_cells_for_norm(base)
      verbs = Enum.filter(cells, fn cell -> Map.get(cell, :pos) == "verb" end)
      %{
        status: :database_observed,
        base_verb_sense_count: length(verbs),
        base_verb_examples: verbs
          |> Enum.take(8)
          |> Enum.map(fn row ->
            %{id: Map.get(row, :id),
              definition: Map.get(row, :definition)
                |> case do
                  v when is_binary(v) -> String.slice(v, 0, 130)
                  _ -> nil
                end}
          end)
      }
    rescue
      _ -> %{status: :database_unavailable}
    catch
      :exit, _ -> %{status: :database_unavailable}
    end
  else
    %{status: :no_proven_base_form}
  end
end

IO.puts("\n=== Real LIFG lexical competitor and attested base-form audit ===")

results =
  sentences
  |> Enum.with_index(1)
  |> Enum.map(fn {sentence, index} ->
    si = Core.resolve_input(sentence,
      Keyword.put(opts, :session_id, "synaptic-slate-#{index}"))

    audit = SynapticSlateAudit.inspect(si)

    verbs =
      Enum.map(audit.verbs, fn entry ->
        Map.put(entry, :base_dictionary, base_db_lookup.(entry.attested_base_form))
      end)

    result = %{case: index, sentence: sentence, status: audit.status, verbs: verbs}
    IO.inspect(result, label: "Lexical case #{index}", pretty: true, limit: :infinity)
    result
  end)

all = Enum.flat_map(results, & &1.verbs)

IO.inspect(%{
  sentence_count: length(results),
  observed_verbs: length(all),
  complete_scored_slates: Enum.count(all, & &1.score_slate_complete?),
  mixed_pos_slates: Enum.count(all, &(map_size(&1.scored_pos_counts) > 1)),
  competitive_surface_verb_slates: Enum.count(all,
    &(&1.same_surface_verb_sense_count >= 2)),
  attested_surface_to_base_relations: Enum.count(all,
    & &1.base_form_relation_attested?),
  base_lexemes_with_multiple_db_verb_senses:
    Enum.count(all, fn entry ->
      case entry.base_dictionary do
        %{base_verb_sense_count: n} when n >= 2 -> true
        _ -> false
      end
    end),
  proven_surface_to_specific_base_sense_mappings: 0,
  lifg_modified?: false,
  synapses_modified?: false
}, label: "Coverage summary", pretty: true)

IO.puts("""
NOTE: A validated inflection relation identifies a base *lexeme*, not the
correct sense among that lexeme's many dictionary senses. No weights or
candidate IDs were promoted to base senses. This audit deliberately
measures the gap before an evidence-gated sense bridge is considered.
""")
