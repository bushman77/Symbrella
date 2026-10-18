# Read-only role-aware noun definition expansion vs unchanged baseline.
#
# mix run scripts/probe_role_semantic_context.exs
#
# Uses the selected actual LIFG noun senses, not every noun sense in DB.
# All selected noun definitions and their scored slates must be present.
# Requires independent human gold labels to make ANY accuracy claim.
#
# Optional: tmp/eval/semantic_sense_gold.jsonl
# {"example":1,"gold_id":"BASE|verb|N","source":"human_curated",
#  "basis":"Adjudicated exact dictionary sense"}
#
# No synapse collection, no live LIFG update, no durable writes.

alias Core.Brain.SemanticSenseValidator

opts = [
  mode: :prod,
  experience_origin: :user_turn,
  session_id: "role-context-ab",
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

gold_path = "tmp/eval/semantic_sense_gold.jsonl"

gold =
  if File.regular?(gold_path) do
    gold_path
    |> File.stream!([], :line)
    |> Enum.with_index(1)
    |> Enum.reduce(%{}, fn {line, line_no}, acc ->
      case Jason.decode(String.trim(line)) do
        {:ok, %{"example" => index, "gold_id" => id,
                "source" => "human_curated", "basis" => basis}}
            when is_integer(index) and index in 1..8 and
                 is_binary(id) and is_binary(basis) and byte_size(basis) >= 8 ->
          if Map.has_key?(acc, index), do: raise("Duplicate gold label #{index}")
          Map.put(acc, index, id)

        _ ->
          raise "Invalid annotation #{gold_path}:#{line_no}"
      end
    end)
  else
    %{}
  end

IO.puts("\n=== Strict noun-role one-hop semantic expansion A/B (READ ONLY) ===")

pairs =
  Enum.with_index(sentences, 1)
  |> Enum.map(fn {sentence, i} ->
    si = Core.resolve_input(sentence,
      Keyword.put(opts, :session_id, "role-semantic-#{i}"))

    common =
      case Map.fetch(gold, i) do
        {:ok, id} ->
          [split: :holdout, label_source: :human_curated, gold_id: id]
        :error ->
          [split: :holdout]
      end

    off = SemanticSenseValidator.evaluate(si,
      Keyword.put(common, :role_context?, false))
    on = SemanticSenseValidator.evaluate(si,
      Keyword.put(common, :role_context?, true))

    row =
      case on do
        %{status: status} when status in [:provisional_hypothesis, :abstained] ->
          %{
            case: i,
            sentence: sentence,
            baseline_decision: off.status,
            baseline_proposed: Map.get(off, :proposed_id),
            expanded_decision: status,
            expanded_proposed: on.proposed_id,
            expansion_status: on.role_expansion_status,
            role_sources: Enum.map(on.role_expansion_sources, fn ctx ->
              Map.take(ctx, [:role, :status, :reason, :noun_sense_id,
                :noun_sense_ids, :competing_sense_count, :source,
                :selected_probability, :terms])
            end),
            changed_decision?: Map.get(off, :proposed_id) != on.proposed_id,
            top_sense_rows: on.eligible_top
              |> Enum.take(3)
              |> Enum.map(fn c ->
                %{
                  id: c.id, direct: c.lexical_score,
                  actor: c.actor_support, patient: c.patient_support,
                  combined: c.combined_diagnostic_score,
                  witnesses: c.one_hop_witnesses
                }
              end),
            excluded_count: on.excluded_count,
            actual_gold_provided?: Map.has_key?(gold, i),
            gold_accepted?: on.label_status == :caller_labeled,
            verified_sense?: on.sense_meaning_verified?,
            live_lifg_modified?: on.applied_to_lifg?
          }

        _ ->
          %{case: i, sentence: sentence, result: on}
      end

    IO.inspect(row, label: "Role expansion case #{i}",
      pretty: true, limit: :infinity)

    {off, on}
  end)

baseline = Enum.map(pairs, &elem(&1, 0))
expanded = Enum.map(pairs, &elem(&1, 1))

summary = %{
  baseline: SemanticSenseValidator.aggregate(baseline),
  expanded: SemanticSenseValidator.aggregate(expanded),
  changed_proposals: Enum.count(pairs, fn {a, b} ->
    Map.get(a, :proposed_id) != Map.get(b, :proposed_id)
  end),
  cases_with_grounded_roles: Enum.count(expanded,
    &(Map.get(&1, :role_expansion_status) == :grounded)),
  sense_meaning_verified?: false,
  synapses_modified?: false,
  live_lifg_modified?: false
}

IO.inspect(summary, label: "One-hop semantic expansion A/B summary", pretty: true)

throw = Enum.at(expanded, 6)
case throw do
  %{exclusions: rows} when is_list(rows) ->
    if Enum.any?(rows, fn row ->
      row.id == "throw|verb|5" and
        {:missing_restriction_witness, :cricket_bowling} in row.exclusions
    end) do
      IO.puts("PASS: selected ball noun evidence did not bypass the cricket restriction.")
    else
      IO.puts("WARNING: throw restriction evidence missing or changed.")
    end

  other ->
    IO.puts("WARNING: throw case not evaluable: #{inspect(other)}")
end

IO.puts("""
NOTE: Role-derived evidence is a weak, single-hop lexical hypothesis only.
No noun sense, verb sense, or sense identity was independently verified.
Accuracy can be measured only with externally reviewed holdout labels.
""")
