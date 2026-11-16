defmodule Core.Agency.SemanticCorrectionEvaluator do
  @moduledoc """
  Read-only comparison of a live LIFG lexical decision and a provisional
  alternative. Runs on every production Core pipeline pass, but production
  turns have no trusted gold label and therefore never claim a correction.

  compare/2 is a separate benchmark-only boundary. It compares *exact sense
  IDs* against a case-bound, caller-supplied label. Supplied labels are NOT
  authenticated by this module. Agreement with such labels is an experimental
  metric, not independently verified semantic truth or permission to learn.

  No LIFG update, response change, memory write, confidence transfer or
  policy adaptation is performed by this evaluator.
  """

  alias Core.Agency.LexicalRoleShadow
  alias Core.Pipeline.Trace

  @version 1
  @max_sentence_bytes 2_048
  @max_case_id_bytes 128
  @max_benchmark_cases 128
  @annotation_sources [:synthetic_fixture, :curated_external]

  @doc "Always attach a label-free diagnostic to the live production pipeline."
  @spec attach(map(), keyword()) :: map()
  def attach(si, opts \\ [])

  def attach(%{} = si, _opts) do
    proposal = LexicalRoleShadow.evaluate(si)

    {status, reason, count} =
      case single_candidate(proposal) do
        {:ok, _candidate} ->
          {:awaiting_reference, :independent_label_required, 1}

        {:error, reason} ->
          {:not_evaluable, reason, 0}
      end

    Trace.append(si, :semantic_correction_evaluation,
      decision: status,
      reason: reason,
      meta: %{
        v: @version,
        scope: :exact_lexical_sense,
        proposal_status: proposal.status,
        candidate_count: count,
        reference_status: :not_provided,
        baseline_vs_candidate_compared?: false,
        independently_verified?: false,
        learning_applied?: false,
        policy_change_authorized?: false,
        factual_authority?: false
      }
    )
  end

  def attach(other, _opts), do: other

  @doc """
  Score a *separately supplied* benchmark reference for the current SI.

  The caller must provide case_id, split, annotation_source, annotation_id,
  utterance_sha256, token_index and gold_sense_id. The utterance fingerprint
  binds the label to this exact current sentence but does not authenticate
  its annotator or establish ground truth. Live attach/2 never calls compare/2.
  """
  @spec compare(map(), map()) :: map()
  def compare(%{} = si, %{} = reference) do
    with {:ok, ref} <- validate_reference(si, reference),
         {:ok, candidate} <- single_candidate(LexicalRoleShadow.evaluate(si)),
         true <- candidate.token_index == ref.token_index,
         true <- selected_winner?(si, candidate),
         true <- attested_gold?(si, candidate.token_index, ref.gold_sense_id) do
      baseline_id = candidate.original_lifg_id
      candidate_id = candidate.candidate_sense_id

      outcome =
        cond do
          baseline_id == ref.gold_sense_id -> :right_to_wrong
          candidate_id == ref.gold_sense_id -> :wrong_to_right
          true -> :both_wrong
        end

      %{
        v: @version,
        status: :compared_to_supplied_label,
        reason: :exact_sense_id_comparison,
        case_id: ref.case_id,
        split: ref.split,
        annotation_source: ref.annotation_source,
        annotation_id: ref.annotation_id,
        utterance_sha256: ref.utterance_sha256,
        token_index: ref.token_index,
        baseline_sense_id: baseline_id,
        candidate_sense_id: candidate_id,
        supplied_gold_sense_id: ref.gold_sense_id,
        baseline_matches_label?: baseline_id == ref.gold_sense_id,
        candidate_matches_label?: candidate_id == ref.gold_sense_id,
        outcome: outcome,
        label_authenticated?: false,
        semantic_truth_verified?: false,
        learning_applied?: false,
        policy_change_authorized?: false
      }
    else
      {:error, reason} -> abstain(reason)
      false -> abstain(:reference_not_supported_by_current_evidence)
      _ -> abstain(:comparison_unavailable)
    end
  end

  def compare(_, _), do: abstain(:invalid_input)

  @doc """
  Aggregate bounded benchmark comparisons, including abstentions and
  regressions. Reject repeated case IDs or source spans to avoid inflated
  experimental scores. Not a claim of independent ground-truth verification.
  """
  @spec aggregate([map()]) :: map()
  def aggregate(reports) when is_list(reports) and length(reports) <= @max_benchmark_cases do
    comparable =
      Enum.filter(reports, fn
        %{status: :compared_to_supplied_label} -> true
        _ -> false
      end)

    ids = Enum.map(comparable, &Map.get(&1, :case_id))
    spans = Enum.map(comparable, &{Map.get(&1, :utterance_sha256), Map.get(&1, :token_index)})

    if Enum.all?(reports, fn
         %{status: status} when status in [:compared_to_supplied_label, :abstained] -> true
         _ -> false
       end) and
         Enum.all?(comparable, &valid_comparison_report?/1) and
         length(ids) == length(Enum.uniq(ids)) and
         length(spans) == length(Enum.uniq(spans)) do
      base = Enum.count(comparable, & &1.baseline_matches_label?)
      revised = Enum.count(comparable, & &1.candidate_matches_label?)

      %{
        v: @version,
        status: :benchmark_summary,
        total: length(reports),
        compared: length(comparable),
        abstentions: length(reports) - length(comparable),
        baseline_matches_supplied_labels: base,
        candidate_matches_supplied_labels: revised,
        wrong_to_right: Enum.count(comparable, &(&1.outcome == :wrong_to_right)),
        right_to_wrong: Enum.count(comparable, &(&1.outcome == :right_to_wrong)),
        both_wrong: Enum.count(comparable, &(&1.outcome == :both_wrong)),
        net_label_agreement_gain: revised - base,
        reference_authentication: :not_established,
        semantic_truth_verified?: false,
        learning_applied?: false,
        policy_change_authorized?: false
      }
    else
      %{v: @version, status: :invalid_reports, reason: :duplicate_or_untrusted_reports}
    end
  end

  def aggregate(_), do: %{v: @version, status: :invalid_reports, reason: :invalid_batch}

  @doc "Fingerprint the exact sentence to bind a benchmark annotation to a case."
  @spec utterance_sha256(map()) :: String.t() | nil
  def utterance_sha256(%{sentence: sentence})
      when is_binary(sentence) and byte_size(sentence) in 1..@max_sentence_bytes do
    :crypto.hash(:sha256, sentence) |> Base.encode16(case: :lower)
  end

  def utterance_sha256(_), do: nil

  defp validate_reference(si, ref) do
    case_id = Map.get(ref, :case_id)
    split = Map.get(ref, :split)
    annotation_source = Map.get(ref, :annotation_source)
    annotation_id = Map.get(ref, :annotation_id)
    utterance_hash = Map.get(ref, :utterance_sha256)
    token_index = Map.get(ref, :token_index)
    gold_id = Map.get(ref, :gold_sense_id)
    current_hash = utterance_sha256(si)

    cond do
      not (is_binary(case_id) and byte_size(case_id) in 1..@max_case_id_bytes) ->
        {:error, :invalid_case_id}

      split != :holdout ->
        {:error, :non_holdout_reference}

      annotation_source not in @annotation_sources or
          not (is_binary(annotation_id) and byte_size(annotation_id) in 1..@max_case_id_bytes) ->
        {:error, :missing_annotation_provenance}

      not is_binary(current_hash) or utterance_hash != current_hash ->
        {:error, :reference_sentence_mismatch}

      not (is_integer(token_index) and token_index >= 0 and token_index < 128) or
          not (is_binary(gold_id) and byte_size(gold_id) in 1..160) ->
        {:error, :invalid_reference_sense}

      true ->
        {:ok, %{
          case_id: case_id, split: split,
          annotation_source: annotation_source, annotation_id: annotation_id,
          utterance_sha256: utterance_hash, token_index: token_index,
          gold_sense_id: gold_id
        }}
    end
  end

  defp single_candidate(%{
         status: :provisional_role_alternatives,
         blocked_role_count: 1,
         unresolved_role_count: 0,
         alternatives: [%{
           token_index: index,
           original_lifg_id: original,
           candidate_sense_id: candidate
         } = result]
       })
       when is_integer(index) and is_binary(original) and
            is_binary(candidate) and original != candidate do
    {:ok, result}
  end

  defp single_candidate(_), do: {:error, :no_unique_exact_sense_alternative}

  defp selected_winner?(si, candidate) do
    case si |> Map.get(:lifg_choices, []) |> List.wrap() |>
           Enum.filter(&(is_map(&1) and Map.get(&1, :token_index) == candidate.token_index)) do
      [%{} = winner] ->
        (Map.get(winner, :id) || Map.get(winner, :chosen_id)) == candidate.original_lifg_id

      _ ->
        false
    end
  end

  defp attested_gold?(si, index, gold_id) do
    words =
      si |> Map.get(:tokens, []) |> List.wrap()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :index) == index and
        Map.get(&1, :n, 1) == 1))

    case words do
      [%{phrase: phrase}] when is_binary(phrase) ->
        normalized =
          phrase |> String.downcase() |> String.trim()
          |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")

        Enum.any?(List.wrap(Map.get(si, :active_cells)), fn
          %{id: ^gold_id, norm: ^normalized} -> true
          _ -> false
        end)

      _ ->
        false
    end
  end

  defp valid_comparison_report?(report) do
    baseline = Map.get(report, :baseline_sense_id)
    candidate = Map.get(report, :candidate_sense_id)
    gold = Map.get(report, :supplied_gold_sense_id)

    expected =
      cond do
        baseline == gold -> :right_to_wrong
        candidate == gold -> :wrong_to_right
        true -> :both_wrong
      end

    is_binary(Map.get(report, :case_id)) and
      is_binary(Map.get(report, :utterance_sha256)) and
      is_integer(Map.get(report, :token_index)) and
      is_binary(baseline) and is_binary(candidate) and is_binary(gold) and
      baseline != candidate and
      Map.get(report, :outcome) == expected and
      Map.get(report, :baseline_matches_label?) == (baseline == gold) and
      Map.get(report, :candidate_matches_label?) == (candidate == gold) and
      Map.get(report, :label_authenticated?) == false and
      Map.get(report, :learning_applied?) == false and
      Map.get(report, :policy_change_authorized?) == false
  end

  defp abstain(reason) do
    %{
      v: @version,
      status: :abstained,
      reason: reason,
      outcome: :not_scored,
      label_authenticated?: false,
      semantic_truth_verified?: false,
      learning_applied?: false,
      policy_change_authorized?: false
    }
  end
end
