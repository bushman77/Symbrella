defmodule Core.Agency.ReflectiveCorrection do
  @moduledoc """
  Shadow-only repair experiment for *formal* symbolic derivations.

  The initial typed expression is immutable evidence. A failed derivation is
  recomputed by the existing deterministic Reducer and independently replayed
  by Composition.Proof. Neither the caller's claimed outcome nor an LLM is
  authority for a successful correction.

  This does NOT validate lexical interpretation, world truth, or consciousness.
  No working memory, LIFG weights, goals, learned policy, or persistence change.
  """

  alias Core.Semantic.Composition.{Expression, Proof, Reducer}

  @version 1
  @max_input_bytes 16_384
  @max_trace_bytes 128

  @spec evaluate(map()) :: map()
  def evaluate(%{trace_id: id, derivation: %{initial: initial} = baseline})
      when is_binary(id) and byte_size(id) > 0 and
             byte_size(id) <= @max_trace_bytes and is_map(baseline) do
    if byte_size(:erlang.term_to_binary(initial)) <= @max_input_bytes do
      evaluate_bounded(id, initial, baseline)
    else
      receipt(id, :abstained, :input_too_large)
    end
  end

  def evaluate(_), do: receipt(nil, :abstained, :invalid_trial)

  @doc "Summarize independent formal proof results; never infer semantic correctness."
  @spec summarize([map()]) :: map()
  def summarize(trials) when is_list(trials) do
    outcomes = Enum.map(trials, &evaluate/1)

    %{
      v: @version,
      trials: length(outcomes),
      baseline_verified: Enum.count(outcomes, &(&1.status == :baseline_verified)),
      corrected: Enum.count(outcomes, &(&1.status == :corrected)),
      abstained: Enum.count(outcomes, &(&1.status == :abstained)),
      proof_validity_improvements: Enum.count(outcomes, &(&1.proof_validity_improved?)),
      learning_applied?: false,
      outcome_scope: :formal_derivation_only
    }
  end

  def summarize(_), do: %{v: @version, status: :invalid_trials}

  defp evaluate_bounded(id, initial, baseline) do
    digest = proof_fingerprint(initial)

    case Proof.verify(baseline) do
      :ok ->
        receipt(id, :baseline_verified, :already_valid)
        |> Map.merge(%{
          initial_sha256: digest,
          baseline_verified?: true,
          proof_verified?: true
        })

      {:error, :invalid_derivation} ->
        recompute(id, digest, initial)
    end
  end

  defp recompute(id, digest, initial) do
    with {:ok, type} <- Expression.type_of(initial),
         {:ok, result, steps} <- Reducer.reduce(initial),
         corrected = %{initial: initial, steps: steps, result: result, result_type: type},
         :ok <- Proof.verify(corrected) do
      receipt(id, :corrected, :baseline_derivation_invalid)
      |> Map.merge(%{
        initial_sha256: digest,
        baseline_verified?: false,
        proof_verified?: true,
        proof_validity_improved?: true,
        strategy: :recompute_canonical_reduction,
        corrected_derivation: corrected,
        correction_steps: length(steps),
        independent_check: :composition_proof_replay
      })
    else
      {:error, reason} ->
        receipt(id, :abstained, {:recomputation_unavailable, reason})
        |> Map.put(:initial_sha256, digest)

      _ ->
        receipt(id, :abstained, :independent_replay_failed)
        |> Map.put(:initial_sha256, digest)
    end
  end

  defp receipt(id, status, reason) do
    %{
      v: @version,
      trace_id: id,
      status: status,
      reason: reason,
      baseline_verified?: false,
      proof_verified?: false,
      proof_validity_improved?: false,
      strategy: :none,
      corrected_derivation: nil,
      learning_applied?: false,
      outcome_scope: :formal_derivation_only
    }
  end

  defp proof_fingerprint(initial) do
    :crypto.hash(:sha256, :erlang.term_to_binary(initial))
    |> Base.encode16(case: :lower)
  end
end
