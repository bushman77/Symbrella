# Read-only, evidence-gated semantic sense validation on the real dictionary.
#
#   mix run scripts/probe_semantic_sense_validator.exs
#
# Labels are never guessed from the dictionary or the validator. Optional
# independent human-curated gold JSONL must be prepared BEFORE looking at
# predictions: priv/semantic_sense_gold.jsonl
#
# JSONL record (example shape ONLY; not a real gold answer):
# {"example":1,"gold_id":"BASE|verb|N","source":"human_curated",
#  "basis":"Sense definition independently reviewed"}
#
# The diagnostic does not mutate LIFG, ATL, synapses, or BrainCells.
alias Core.Brain.SemanticSenseValidator

opts = [
  mode: :prod,
  experience_origin: :user_turn,
  session_id: "sense-validator-audit",
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

gold_path = "priv/semantic_sense_gold.jsonl"

gold =
  if File.regular?(gold_path) do
    gold_path
    |> File.stream!([], :line)
    |> Enum.with_index(1)
    |> Enum.reduce(%{}, fn {line, lineno}, acc ->
      case Jason.decode(String.trim(line)) do
        {:ok, %{"example" => n, "gold_id" => id,
                "source" => "human_curated", "basis" => basis}}
            when is_integer(n) and n >= 1 and n <= 8 and
                 is_binary(id) and is_binary(basis) and byte_size(basis) >= 8 ->
          if Map.has_key?(acc, n), do: raise("Duplicate gold label for case #{n}")
          Map.put(acc, n, id)

        _ ->
          raise "Invalid gold annotation at #{gold_path}:#{lineno}"
      end
    end)
  else
    %{}
  end

IO.puts("\n=== Evidence-gated real-dictionary sense validation (READ ONLY) ===")

reports =
  sentences
  |> Enum.with_index(1)
  |> Enum.map(fn {sentence, n} ->
    si = Core.resolve_input(sentence,
      Keyword.put(opts, :session_id, "sense-validation-#{n}"))

    evaluation_opts = [split: :holdout]

    evaluation_opts =
      case Map.fetch(gold, n) do
        {:ok, id} ->
          Keyword.merge(evaluation_opts,
            gold_id: id, label_source: :human_curated)

        :error ->
          evaluation_opts
      end

    result = SemanticSenseValidator.evaluate(si, evaluation_opts)

    compact =
      case result do
        %{status: status} when status in [:provisional_hypothesis, :abstained] ->
          %{
            case: n,
            sentence: sentence,
            decision: status,
            decision_reason: result.reason,
            dictionary_sense_count: result.sense_count,
            lexical_baseline: result.baseline_id,
            validated_hypothesis: result.proposed_id,
            baseline_gap: result.baseline_score_gap,
            validated_gap: result.validator_score_gap,
            excluded_count: result.excluded_count,
            excluded: result.exclusions,
            eligible_top: result.eligible_top
              |> Enum.take(3)
              |> Enum.map(fn row ->
                %{id: row.id, context_fit: row.lexical_score,
                  grammar: row.grammar, matched: row.matching_context,
                  definition: row.dictionary_gloss}
              end),
            caller_gold_label: result.gold_id,
            sense_verified?: result.sense_meaning_verified?,
            lifg_changed?: result.applied_to_lifg?
          }

        _ ->
          %{case: n, sentence: sentence, diagnostic: result}
      end

    IO.inspect(compact, label: "Sense validator #{n}",
      pretty: true, limit: :infinity)

    result
  end)

summary = SemanticSenseValidator.aggregate(reports)
IO.inspect(summary, label: "Sense validation summary", pretty: true)

case Enum.at(reports, 6) do
  %{status: :abstained, baseline_id: "throw|verb|5"} = report ->
    if Enum.any?(report.exclusions, fn row ->
      row.id == "throw|verb|5" and
        {:missing_restriction_witness, :cricket_bowling} in row.exclusions
    end) do
      IO.puts("PASS: misleading bowling-arm ball match was withheld (no sense verified).")
    else
      IO.puts("WARNING: misleading throw sense was not restricted as expected.")
    end

  other ->
    IO.puts("WARNING: unexpected real-dictionary throw evaluation: #{inspect(other)}")
end

IO.puts("NOTE: No gold accuracy can be claimed without independently curated labels.")
IO.puts("NOTE: Validation abstentions are not automatically correct sense predictions.")
