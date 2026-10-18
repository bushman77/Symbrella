defmodule Core.Brain.SynapticDisambiguation do
  @moduledoc """
  Counterfactual lexical disambiguation with REAL complete LIFG score slates.

  Read-only, held-out diagnostic. The input must already be independently
  revalidated as a human-origin positive SVO. Requires >=2 genuine verb sense
  candidates, the exact complete candidate set and numeric per-token
  post-softmax probabilities that sum to ~1.

  Additive probability-space signal is a HYPOTHESIS, not a calibrated logit or
  real LIFG scoring behavior. A projected winner is not a live LIFG winner.
  No reranking, activation, review, learning, persistence, or gold-label inference.
  """

  alias Core.Brain.{SynapticExperience, SynapticPropagation}
  alias Core.Semantic.SVOEvent
  alias Brain.Synapses.Shadow

  @min_verb_candidates 2
  @max_candidates 64
  @min_score_sum 0.98
  @max_score_sum 1.02
  @abstain_margin 0.02

  @spec evaluate(map(), keyword()) :: map()
  def evaluate(si, opts \\ [])

  def evaluate(%{} = si, opts) when is_list(opts) do
    server = Keyword.get(opts, :synapse_shadow_server, Shadow)

    with {:ok, observation} <- SynapticExperience.extract(si),
         :ok <- not_seen(observation.evidence, server),
         %{status: :shadow_comparison, predicate_index: index,
           propagation_on: %{candidates: signal_rows}} <-
           SynapticPropagation.compare(si, opts),
         [choice] <- Enum.filter(List.wrap(Map.get(si, :lifg_choices)), fn
           %{token_index: ^index} -> true
           _ -> false
         end),
         %{} = scores <- Map.get(choice, :candidate_probs),
         %{candidates: source_candidates, status: :complete} <-
           complete_probabilities(si, index, scores),
         true <- count_verb_senses(source_candidates) >= @min_verb_candidates,
         :ok <- valid_gold(opts, source_candidates),
         true <- valid_margin?(Keyword.get(opts, :abstain_margin, @abstain_margin)) do
      supports = Map.new(signal_rows, fn row -> {row.id, row.shadow_signal} end)
      before = ranked(source_candidates, %{})
      after_signal = ranked(source_candidates, supports)
      gold = Keyword.get(opts, :gold_id)
      abstain_margin = Keyword.get(opts, :abstain_margin, @abstain_margin)

      baseline_choice = decision(before, abstain_margin)
      projected_choice = decision(after_signal, abstain_margin)

      %{
        status: if(is_binary(gold), do: :labeled_diagnostic, else: :unlabeled_diagnostic),
        label_source: if(is_binary(gold), do: Keyword.get(opts, :label_source), else: nil),
        label_verified_by_system?: false,
        gold_id: gold,
        split: Keyword.get(opts, :split, :unspecified),
        predicate_index: index,
        selected_lifg_id: Map.get(choice, :id),
        probability_kind: :per_token_post_softmax,
        abstain_margin: abstain_margin,
        adjustment_kind: :hypothetical_additive_probability_not_calibrated,
        candidate_count: length(source_candidates),
        verb_sense_count: count_verb_senses(source_candidates),
        baseline: decision_report(before, baseline_choice, gold),
        counterfactual: decision_report(after_signal, projected_choice, gold),
        changed_decision?: baseline_choice != projected_choice,
        flip_class: flip_class(baseline_choice, projected_choice, gold),
        total_signal: Enum.reduce(supports, 0.0, fn {_, v}, acc -> acc + v end),
        evidence_leakage_checked?: true,
        applied_to_lifg?: false,
        brain_cells_stimulated?: false,
        trained_on_evaluation_example?: false
      }
    else
      {:error, :seen} -> skipped(:seen_in_training_receipts)
      {:error, :unavailable} -> skipped(:ledger_unavailable)
      %{status: :incomplete} -> skipped(:incomplete_or_invalid_score_slate)
      %{status: :complete} -> skipped(:single_verb_sense)
      {:error, :gold} -> skipped(:untrusted_or_out_of_slate_gold)
      _ -> skipped(:ineligible_or_unscored)
    end
  end

  def evaluate(_, _), do: skipped(:invalid_input)

  @doc """
  Summarize reports without implying independent labels are verified.
  This counts abstentions, wrong->right flips and right->wrong flips only
  for explicitly labeled diagnostic records.
  """
  @spec aggregate([map()]) :: map()
  def aggregate(reports) when is_list(reports) do
    eligible = Enum.filter(reports, &(&1.status in [:unlabeled_diagnostic, :labeled_diagnostic]))
    labeled = Enum.filter(eligible, &(&1.status == :labeled_diagnostic))

    %{
      total: length(reports),
      eligible: length(eligible),
      labeled: length(labeled),
      unlabelled: length(eligible) - length(labeled),
      skipped: length(reports) - length(eligible),
      competing_real_slate_examples: Enum.count(eligible, &(&1.verb_sense_count >= 2)),
      projected_decision_changes: Enum.count(eligible, & &1.changed_decision?),
      wrong_to_right: Enum.count(labeled, &(&1.flip_class == :wrong_to_right)),
      right_to_wrong: Enum.count(labeled, &(&1.flip_class == :right_to_wrong)),
      abstention_to_right: Enum.count(labeled, &(&1.flip_class == :abstention_to_right)),
      right_to_abstention: Enum.count(labeled, &(&1.flip_class == :right_to_abstention)),
      baseline_correct: Enum.count(labeled, & &1.baseline.correct?),
      counterfactual_correct: Enum.count(labeled, & &1.counterfactual.correct?),
      baseline_abstentions: Enum.count(labeled, &(&1.baseline.decision == :abstain)),
      counterfactual_abstentions: Enum.count(labeled, &(&1.counterfactual.decision == :abstain)),
      measured_accuracy?: labeled != [],
      externally_verified_gold?: false,
      live_lifg_modified?: false
    }
  end

  def aggregate(_), do: %{status: :invalid_reports}

  defp skipped(reason) do
    %{status: :skipped, reason: reason, applied_to_lifg?: false,
      brain_cells_stimulated?: false}
  end

  defp not_seen(evidence, server) do
    case Shadow.recorded?(evidence, server) do
      true -> {:error, :seen}
      false -> :ok
      _ -> {:error, :unavailable}
    end
  end

  defp complete_probabilities(si, index, scores) do
    rows = get_in_slate(Map.get(si, :sense_candidates), index)

    ids =
      rows
      |> Enum.flat_map(fn
        %{id: id} when is_binary(id) -> [id]
        %{"id" => id} when is_binary(id) -> [id]
        _ -> []
      end)

    valid? =
      length(ids) in 2..@max_candidates and
        length(Enum.uniq(ids)) == length(ids) and
        map_size(scores) == length(ids) and
        MapSet.new(Map.keys(scores)) == MapSet.new(ids) and
        Enum.all?(scores, fn {id, value} ->
          is_binary(id) and is_number(value) and
            value >= 0.0 and value <= 1.0 and value == value
        end)

    total = if valid?, do: Enum.reduce(scores, 0.0, fn {_, v}, sum -> sum + v end), else: 0.0

    if valid? and total >= @min_score_sum and total <= @max_score_sum do
      %{status: :complete, candidates: scores}
    else
      %{status: :incomplete}
    end
  end

  defp get_in_slate(%{} = slate, index) do
    case Map.get(slate, index, Map.get(slate, to_string(index))) do
      rows when is_list(rows) -> rows
      _ -> []
    end
  end

  defp get_in_slate(_, _), do: []

  defp count_verb_senses(scores) do
    Enum.count(scores, fn {id, _} ->
      case String.split(id, "|", parts: 3) do
        [surface, "verb", sense] -> surface != "" and sense != "" and sense != "fallback"
        _ -> false
      end
    end)
  end

  defp valid_gold(opts, scores) do
    case Keyword.get(opts, :gold_id) do
      nil ->
        :ok

      gold when is_binary(gold) ->
        if Map.has_key?(scores, gold) and
           Keyword.get(opts, :split) == :holdout and
           Keyword.get(opts, :label_source) in [:human_curated, :external_gold] do
          :ok
        else
          {:error, :gold}
        end

      _ ->
        {:error, :gold}
    end
  end

  defp ranked(scores, signals) do
    weighted = Enum.map(scores, fn {id, p} ->
      {id, p + Map.get(signals, id, 0.0)}
    end)

    total = Enum.reduce(weighted, 0.0, fn {_, p}, sum -> sum + p end)

    weighted
    |> Enum.map(fn {id, p} -> {id, p / total} end)
    |> Enum.sort_by(fn {id, p} -> {-p, id} end)
  end

  defp decision([{top_id, top_p}, {_, next_p} | _], margin) do
    if top_p - next_p < margin, do: :abstain, else: top_id
  end

  defp decision(_, _), do: :abstain

  defp decision_report([{id, p} | rest], choice, gold) do
    runner_up = case rest do
      [{next_id, next_p} | _] -> %{id: next_id, probability: next_p}
      _ -> nil
    end

    %{
      top_id: id,
      top_probability: p,
      runner_up: runner_up,
      margin: if(runner_up, do: p - runner_up.probability, else: nil),
      decision: choice,
      correct?: if(is_binary(gold), do: choice == gold, else: nil),
      abstained?: choice == :abstain
    }
  end

  defp valid_margin?(v),
    do: is_number(v) and v >= 0 and v <= 0.2

  defp flip_class(_, _, nil), do: :unlabeled
  defp flip_class(a, a, _gold), do: :unchanged

  defp flip_class(before, after, gold) do
    cond do
      before == :abstain and after == gold -> :abstention_to_right
      before == :abstain -> :abstention_to_wrong
      before == gold and after == :abstain -> :right_to_abstention
      after == :abstain -> :wrong_to_abstention
      before == gold -> :right_to_wrong
      after == gold -> :wrong_to_right
      true -> :different_wrong_decision
    end
  end
end
