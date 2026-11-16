defmodule Core.Agency.ReflectiveCorrectionTest do
  use ExUnit.Case, async: true

  alias Core.Agency.ReflectiveCorrection
  alias Core.Semantic.Composition.{Expression, Proof, Reducer}

  defp derivation(adjective, noun) do
    {:ok, adj} = Expression.adjective(adjective)
    {:ok, nominal} = Expression.noun(noun)
    initial = {:apply, adj, nominal}
    {:ok, type} = Expression.type_of(initial)
    {:ok, result, steps} = Reducer.reduce(initial)

    proof = %{initial: initial, steps: steps, result: result, result_type: type}
    assert :ok == Proof.verify(proof)
    assert steps != []
    proof
  end

  test "detects an invalid proof, recomputes from the original, and independently verifies" do
    valid = derivation("black", "cat")
    flawed = %{valid | steps: []}
    assert {:error, :invalid_derivation} == Proof.verify(flawed)

    trial = %{trace_id: "test|black-cat", derivation: flawed}
    original_trial = trial

    result = ReflectiveCorrection.evaluate(trial)

    assert result.status == :corrected
    assert result.reason == :baseline_derivation_invalid
    assert result.strategy == :recompute_canonical_reduction
    assert result.independent_check == :composition_proof_replay
    assert result.proof_validity_improved?
    assert result.proof_verified?
    refute result.baseline_verified?
    refute result.learning_applied?
    assert result.outcome_scope == :formal_derivation_only
    assert byte_size(result.initial_sha256) == 64
    assert result.corrected_derivation.initial == valid.initial
    assert :ok == Proof.verify(result.corrected_derivation)
    assert result.corrected_derivation.result == valid.result
    assert trial == original_trial
  end

  test "keeps an already-verified proof unchanged without claiming improvement" do
    valid = derivation("white", "dog")
    result = ReflectiveCorrection.evaluate(%{trace_id: "test|valid", derivation: valid})

    assert result.status == :baseline_verified
    assert result.baseline_verified?
    assert result.proof_verified?
    refute result.proof_validity_improved?
    refute result.learning_applied?
    assert result.strategy == :none
    assert result.corrected_derivation == nil
  end

  test "cannot promote fabricated self-reported success to a verified proof" do
    valid = derivation("small", "bird")

    forged = valid
    |> Map.put(:steps, [%{rule: :beta, before: valid.initial, after: :forged}])
    |> Map.put(:result, :forged)
    |> Map.put(:verified?, true)

    assert {:error, :invalid_derivation} == Proof.verify(forged)

    result =
      ReflectiveCorrection.evaluate(%{trace_id: "test|forged", derivation: forged})

    assert result.status == :corrected
    assert :ok == Proof.verify(result.corrected_derivation)
    assert result.corrected_derivation.result == valid.result
    refute result.learning_applied?
  end

  test "abstains when the original expression is not well typed" do
    untypable = %{
      initial: {:predicate, :unapproved_symbol, {:var, "free"}},
      steps: [],
      result: :forged,
      result_type: :truth
    }

    result =
      ReflectiveCorrection.evaluate(%{trace_id: "test|untypable", derivation: untypable})

    assert result.status == :abstained
    assert match?({:recomputation_unavailable, _}, result.reason)
    refute result.proof_verified?
    refute result.proof_validity_improved?
    assert result.corrected_derivation == nil
  end

  test "fails closed on invalid or oversized trial evidence" do
    assert ReflectiveCorrection.evaluate(%{}).status == :abstained

    oversized = %{
      trace_id: "test|oversized",
      derivation: %{initial: String.duplicate("x", 17_000), steps: [], result: nil}
    }

    result = ReflectiveCorrection.evaluate(oversized)
    assert result.status == :abstained
    assert result.reason == :input_too_large

    long_id = String.duplicate("a", 129)
    assert ReflectiveCorrection.evaluate(%{oversized | trace_id: long_id}).status == :abstained
  end

  test "a fixed multi-case trial set measures formal validity, not semantic accuracy" do
    cases = [
      {"black", "cat"},
      {"white", "dog"},
      {"small", "bird"},
      {"black", "dog"}
    ]

    trials =
      cases
      |> Enum.with_index()
      |> Enum.map(fn {{adj, noun}, i} ->
        proof = derivation(adj, noun)
        baseline = if i == 0, do: proof, else: %{proof | steps: []}
        %{trace_id: "heldout|#{i}", derivation: baseline}
      end)

    summary = ReflectiveCorrection.summarize(trials)

    assert summary.trials == 4
    assert summary.baseline_verified == 1
    assert summary.corrected == 3
    assert summary.abstained == 0
    assert summary.proof_validity_improvements == 3
    assert summary.outcome_scope == :formal_derivation_only
    refute summary.learning_applied?
  end
end
