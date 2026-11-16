defmodule Core.Agency.SemanticHoldout do
  @moduledoc """
  Sealed, explicitly reviewer-supplied lexical sense benchmark harness.

  This module does NOT authenticate reviewer identities or establish that an
  example was actually held out from training. It verifies a caller-provided
  manifest digest, two distinct agreeing review declarations per case, and
  disjointness from the declared development utterance hashes. The caller
  remains responsible for external provenance and genuine dataset isolation.

  The resolver receives ONLY the sentence (never the gold sense or reviews).
  The resulting SemanticInput is compared through the existing current-turn
  LexicalRoleShadow + SemanticCorrectionEvaluator gates. There is no learning,
  automatic correction, or ledger write in this module.
  """

  alias Core.Agency.SemanticCorrectionEvaluator, as: Evaluator

  @version 1
  @max_cases 128
  @max_development_hashes 4_096
  @max_sentence_bytes 2_048
  @max_id_bytes 128
  @sha256 ~r/\A[0-9a-f]{64}\z/

  @doc """
  Return a deterministic SHA-256 of the canonical manifest contents.

  The seal checks for subsequent modifications, NOT authenticity of the
  original review or whether the user has leaked held-out examples.
  """
  @spec seal(map()) :: String.t()
  def seal(%{} = manifest) do
    :crypto.hash(:sha256, :erlang.term_to_binary(canonical(manifest)))
    |> Base.encode16(case: :lower)
  end

  @doc "A normalized sentence digest for exact/whitespace/case overlap checks."
  @spec utterance_digest(String.t()) :: String.t() | nil
  def utterance_digest(sentence)
      when is_binary(sentence) and byte_size(sentence) in 1..@max_sentence_bytes do
    normalized =
      sentence
      |> String.trim()
      |> String.downcase()
      |> String.replace(~r/\s+/u, " ")

    :crypto.hash(:sha256, normalized) |> Base.encode16(case: :lower)
  end

  def utterance_digest(_), do: nil

  @doc """
  Validate and run a sealed manifest against a frozen or live read-only resolver.

  The passed expected_sha256 must be obtained and stored separately before
  evaluation; computing and accepting it in the same call is not protection.
  Results are agreement with *supplied* reviewer declarations only.
  """
  @spec run(map(), String.t(), (String.t() -> map())) :: map()
  def run(manifest, expected_sha256, resolver)
      when is_map(manifest) and is_binary(expected_sha256) and
             is_function(resolver, 1) do
    with :ok <- validate_seal(manifest, expected_sha256),
         {:ok, cases} <- validate_manifest(manifest) do
      reports = Enum.map(cases, &evaluate_case(&1, resolver))
      summary = Evaluator.aggregate(reports)

      if summary.status == :benchmark_summary do
        %{
          v: @version,
          status: :evaluated_against_supplied_reviews,
          dataset_id: manifest["dataset_id"],
          manifest_sha256: expected_sha256,
          report: summary,
          cases: Enum.map(reports, &public_result/1),
          external_reviewer_identity_verified?: false,
          actual_training_isolation_verified?: false,
          semantic_truth_verified?: false,
          learning_applied?: false,
          policy_change_authorized?: false
        }
      else
        invalid(:comparison_report_rejected)
      end
    else
      {:error, reason} -> invalid(reason)
    end
  end

  def run(_, _, _), do: invalid(:invalid_arguments)

  @doc "Validate a manifest's structure and declared overlap without running a model."
  @spec validate_manifest(map()) :: {:ok, [map()]} | {:error, atom()}
  def validate_manifest(%{
        "version" => @version,
        "dataset_id" => dataset_id,
        "development_utterance_sha256" => development,
        "cases" => cases
      })
      when is_list(development) and is_list(cases) do
    cond do
      not bounded_id?(dataset_id) ->
        {:error, :invalid_dataset_id}

      development == [] or length(development) > @max_development_hashes or
          not Enum.all?(development, &sha256?/1) ->
        {:error, :invalid_development_hashes}

      length(development) != length(Enum.uniq(development)) ->
        {:error, :duplicate_development_hash}

      cases == [] or length(cases) > @max_cases ->
        {:error, :invalid_case_count}

      true ->
        validate_cases(cases, MapSet.new(development))
    end
  end

  def validate_manifest(_), do: {:error, :invalid_manifest}

  defp validate_cases(cases, development) do
    with :ok <- validate_each(cases),
         case_ids <- Enum.map(cases, & &1["case_id"]),
         hashes <- Enum.map(cases, &utterance_digest(&1["sentence"])),
         true <- length(case_ids) == length(Enum.uniq(case_ids)),
         true <- length(hashes) == length(Enum.uniq(hashes)),
         false <- Enum.any?(hashes, &MapSet.member?(development, &1)) do
      {:ok, cases}
    else
      {:error, reason} -> {:error, reason}
      true -> {:error, :development_overlap_detected}
      false -> {:error, :duplicate_case_or_utterance}
      _ -> {:error, :invalid_manifest}
    end
  end

  defp validate_each(cases) do
    Enum.reduce_while(cases, :ok, fn item, _acc ->
      case validate_case(item) do
        :ok -> {:cont, :ok}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp validate_case(%{
         "case_id" => id,
         "sentence" => sentence,
         "target_index" => index,
         "gold_sense_id" => gold,
         "reviewers" => [%{"reviewer_id" => reviewer_a, "sense_id" => vote_a},
                          %{"reviewer_id" => reviewer_b, "sense_id" => vote_b}],
         "split" => "holdout"
       }) do
    cond do
      not bounded_id?(id) -> {:error, :invalid_case_id}
      not is_binary(sentence) or byte_size(sentence) not in 1..@max_sentence_bytes ->
        {:error, :invalid_sentence}
      is_nil(utterance_digest(sentence)) -> {:error, :invalid_sentence}
      not is_integer(index) or index < 0 or index >= 128 ->
        {:error, :invalid_token_index}
      not is_binary(gold) or byte_size(gold) not in 1..160 ->
        {:error, :invalid_gold_sense}
      not bounded_id?(reviewer_a) or not bounded_id?(reviewer_b) or
          reviewer_a == reviewer_b ->
        {:error, :invalid_reviewer_declarations}
      vote_a != gold or vote_b != gold ->
        {:error, :reviewer_disagreement}
      true -> :ok
    end
  end

  defp validate_case(_), do: {:error, :invalid_case}

  defp validate_seal(manifest, expected_sha256) do
    if sha256?(expected_sha256) and seal(manifest) == expected_sha256 do
      :ok
    else
      {:error, :manifest_seal_mismatch}
    end
  end

  defp evaluate_case(item, resolver) do
    sentence = item["sentence"]

    with {:ok, %{sentence: ^sentence} = si} <- safe_resolve(resolver, sentence) do
      reference = %{
        case_id: item["case_id"],
        split: :holdout,
        annotation_source: :curated_external,
        annotation_id: "declared-two-reviewer-consensus",
        utterance_sha256: Evaluator.utterance_sha256(si),
        token_index: item["target_index"],
        gold_sense_id: item["gold_sense_id"]
      }

      Evaluator.compare(si, reference)
    else
      _ ->
        %{
          v: @version, status: :abstained,
          reason: :resolver_failed_or_sentence_changed,
          case_id: item["case_id"],
          outcome: :not_scored
        }
    end
  end

  defp safe_resolve(resolver, sentence) do
    try do
      case resolver.(sentence) do
        %{} = si -> {:ok, si}
        _ -> {:error, :invalid_resolution}
      end
    rescue
      _ -> {:error, :resolver_failed}
    catch
      _, _ -> {:error, :resolver_failed}
    end
  end

  defp public_result(%{status: :compared_to_supplied_label} = result) do
    Map.take(result, [
      :case_id, :status, :outcome, :baseline_matches_label?,
      :candidate_matches_label?, :reason
    ])
  end

  defp public_result(%{status: :abstained} = result),
    do: Map.take(result, [:case_id, :status, :outcome, :reason])

  defp bounded_id?(value),
    do: is_binary(value) and byte_size(value) in 1..@max_id_bytes and
      String.trim(value) != ""

  defp sha256?(value), do: is_binary(value) and Regex.match?(@sha256, value)

  defp canonical(%{} = map) do
    map
    |> Enum.map(fn {key, value} -> {key, canonical(value)} end)
    |> Enum.sort_by(&elem(&1, 0))
  end

  defp canonical(list) when is_list(list), do: Enum.map(list, &canonical/1)
  defp canonical(value), do: value

  defp invalid(reason) do
    %{
      v: @version,
      status: :rejected,
      reason: reason,
      semantic_truth_verified?: false,
      learning_applied?: false,
      policy_change_authorized?: false
    }
  end
end
