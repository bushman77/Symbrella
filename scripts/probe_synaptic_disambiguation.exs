# Strict, read-only, real-dictionary competitive disambiguation audit.
#
#   mix run scripts/probe_synaptic_disambiguation.exs
#
# No real dictionary sense is labeled correct here. We use an isolated shadow
# store seeded ONLY by independent earlier SVO reports, never the held-out
# sentence. If actual LIFG reports no COMPLETE 2+-verb-sense probability slate,
# print an explicit insufficiency result rather than inventing competition.
#
# This probe does NOT retrain LIFG, stimulate BrainCells or change live synapses.

alias Brain.Synapses.Shadow
alias Core.Brain.{SynapticDisambiguation, SynapticExperience}

opts = [
  mode: :prod,
  experience_origin: :user_turn,
  session_id: "disambiguation-probe",
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

pairs = [
  {"The artist draws a picture.", "The painter draws a picture."},
  {"The chef cooks a meal.", "The baker cooks a meal."},
  {"The farmer plants a tree.", "The gardener plants a tree."},
  {"The student writes a letter.", "The author writes a letter."},
  {"The artist paints a portrait.", "The student paints a portrait."},
  {"The builder builds a house.", "The worker builds a house."},
  {"The athlete throws a ball.", "The child throws a ball."},
  {"The sailor steers a ship.", "The captain steers a ship."}
]

# Optional independently human-curated gold labels; never infer them
# from LIFG winners or shadow output. Create BEFORE inspecting A/B predictions.
# JSONL rows: {"example":1,"gold_id":"...|verb|...","source":"human_curated",
#              "basis":"Specific sense meaning independently reviewed"}
gold_path = "tmp/eval/synaptic_disambiguation_gold.jsonl"

gold =
  if File.regular?(gold_path) do
    gold_path
    |> File.stream!([], :line)
    |> Enum.with_index(1)
    |> Enum.reduce(%{}, fn {line, lineno}, acc ->
      case Jason.decode(String.trim(line)) do
        {:ok, %{"example" => index, "gold_id" => id,
                "source" => "human_curated", "basis" => basis}}
            when is_integer(index) and index in 1..8 and
                 is_binary(id) and is_binary(basis) and byte_size(basis) >= 8 ->
          if Map.has_key?(acc, index) do
            raise "Duplicate gold label for case #{index}"
          end

          Map.put(acc, index, id)

        _ ->
          raise "Invalid gold label at #{gold_path}:#{lineno}"
      end
    end)
  else
    %{}
  end

{:ok, server} = Shadow.start_link(name: nil, durable?: false)

rows =
  Enum.with_index(pairs, 1)
  |> Enum.map(fn {{training_text, holdout_text}, index} ->
    train_si = Core.resolve_input(training_text,
      Keyword.put(opts, :session_id, "disamb-train-#{index}"))
    holdout_si = Core.resolve_input(holdout_text,
      Keyword.put(opts, :session_id, "disamb-holdout-#{index}"))

    baseline = SynapticDisambiguation.evaluate(holdout_si,
      synapse_shadow_server: server, split: :holdout)

    train_recording =
      case SynapticExperience.extract(train_si) do
        {:ok, observation} -> Shadow.observe(observation, server)
        _ -> :ineligible
      end

    eval_opts = [synapse_shadow_server: server, split: :holdout]

    eval_opts =
      case Map.fetch(gold, index) do
        {:ok, gold_id} ->
          Keyword.merge(eval_opts, gold_id: gold_id, label_source: :human_curated)
        :error ->
          eval_opts
      end

    after_learning = SynapticDisambiguation.evaluate(holdout_si, eval_opts)

    predicate_choices =
      List.wrap(Map.get(holdout_si, :lifg_choices))
      |> Enum.filter(fn
        %{id: id} when is_binary(id) -> String.contains?(id, "|verb|")
        _ -> false
      end)

    scores =
      case predicate_choices do
        [%{candidate_probs: %{} = p} | _] -> p
        _ -> %{}
      end

    %{
      example: index,
      training_text: training_text,
      holdout_text: holdout_text,
      seed: train_recording,
      baseline_status: baseline.status,
      baseline_reason: Map.get(baseline, :reason),
      projected_status: after_learning.status,
      projected_reason: Map.get(after_learning, :reason),
      genuine_competitor_count: Map.get(after_learning, :verb_sense_count, 0),
      score_map_size: map_size(scores),
      live_winner: Map.get(after_learning, :selected_lifg_id),
      unlabelled_flip?: Map.get(after_learning, :changed_decision?, false),
      signal: Map.get(after_learning, :total_signal, 0.0),
      baseline_decision: get_in(baseline, [:baseline, :decision]),
      counterfactual_decision: get_in(after_learning, [:counterfactual, :decision]),
      gold_label_provided?: Map.has_key?(gold, index),
      gold_label_accepted?: after_learning.status == :labeled_diagnostic,
      report: after_learning
    }
  end)

reports = Enum.map(rows, & &1.report)
summary = SynapticDisambiguation.aggregate(reports)
ledger = Shadow.snapshot(server)

IO.puts("\n=== Real dictionary synaptic disambiguation A/B (NO GOLD LABELS) ===")

Enum.each(rows, fn row ->
  IO.inspect(Map.drop(row, [:report]),
    label: "Real dictionary case #{row.example}", pretty: true, limit: :infinity)
end)

IO.inspect(summary, label: "Competitive lexical audit summary", pretty: true)
IO.inspect(%{
  experiences: ledger.experiences,
  connections: ledger.connections,
  live_priming?: ledger.live_priming?,
  stored_persistently?: ledger.persisted?
}, label: "Isolated shadow ledger", pretty: true)

unless ledger.persisted? == false and ledger.live_priming? == false do
  raise "Evaluation did not remain isolated from durable live cognition"
end

if summary.eligible == 0 do
  IO.puts("""
  INSUFFICIENT_COMPETITIVE_EVIDENCE: No real held-out sentence produced a
  complete 2+-verb-sense LIFG probability slate. No accuracy claim or
  score-flip claim is justified. Check the real slates / full-score mode.
  """)
else
  if summary.labeled == 0 do
    IO.puts("""
    PASS: at least one real complete competing sense slate was evaluated
    counterfactually. Results are UNLABELLED; projected flips do not prove
    better comprehension. Add independent gold labels for an accuracy audit.
    """)
  else
    IO.puts("""
    LABELED_DIAGNOSTIC_COMPLETE: Human-curated labels were applied to eligible
    real dictionary slates. Inspect regressions and abstentions before any
    consideration of live scoring. Labels are not machine-verified.
    """)
  end
end

GenServer.stop(server)
