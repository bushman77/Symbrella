# Reflective Agency v1 — formal correction shadow experiment

## Checkpoint

The initial OutcomeEvaluator confirms an inspectable self-model observation only
when a matching command receipt and a separate live Brain snapshot exist. Passing
that test does not prove a reasoning strategy is correct.

This second, **shadow-only** milestone establishes a deliberately narrower
detect -> alternate strategy -> replay -> verify loop over the existing closed
typed-composition calculus. It does not modify a live linguistic winner.

## Procedure

Call Core.Agency.ReflectiveCorrection.evaluate/1 with a map:

    %{
      trace_id: "bounded-experiment-id",
      derivation: %{
        initial: typed_expression,
        steps: reduction_steps,
        result: claimed_normal_form,
        result_type: claimed_type
      }
    }

The original expression is fixed. The evaluator:

1. Rejects malformed or oversized trials.
2. Uses Core.Semantic.Composition.Proof.verify/1 to replay the baseline trace.
3. If the baseline is invalid, obtains its type with Expression.type_of/1.
4. Tries a different bounded strategy: reconstructs the reduction with
   Reducer.reduce/1, starting from the same unchanged expression.
5. Runs Proof.verify/1 on the recomputed derivation.
6. Reports one of :baseline_verified, :corrected, or :abstained; records the
   original expression fingerprint and whether *formal proof validity* improved.

No downstream policy or memory state changes. The recovered derivation is
returned to the caller for inspection only.

### What the check can establish

- A corrupted, incomplete, or inconsistent beta-reduction trace was detected.
- A fresh derivation for **the same** typed expression passes a replay validator.
- A baseline-invalid -> candidate-valid transition improved *formal validity*.

### What the check cannot establish

- The expression was the correct interpretation of the original user sentence.
- Its lexical senses, referents, or world facts are true.
- A model experienced self-awareness, understood an error, or learned a strategy.
- The verifier is algorithmically independent of the producer:
  Proof.verify/1 uses Reducer.step/1, which shares reducer logic with
  Reducer.reduce/1. A shared defect could evade both checks.

This is a structural self-correction demonstration, not a claim of full
metacognition. The experiment must not be promoted to factual authority.

## Run regression tests

    mix test \
      apps/core/test/core/agency/reflective_correction_test.exs \
      apps/core/test/core/agency/outcome_evaluator_test.exs \
      apps/core/test/core/agency/autonomy_loop_test.exs \
      --trace

The fixed fixture suite includes already-valid proofs, missing and fabricated
beta steps, an untypable initial expression, oversized inputs, and four distinct
nominal-property compositions. Fixtures are not held-out unseen data, and they
must not be used as a generalized accuracy claim.

## Requirements for the next milestone

1. Validate the current live symbolic interpretation *without trusting the
   proposed correction*. Source lexical witnesses and evidence IDs from
   read-only Core semantic validation.
2. Produce explicit error categories, retaining abstention if evidence cannot
   distinguish competing senses.
3. Compare baseline and corrected strategies on an independently prepared,
   held-out set with immutable ground truth and no training overlap.
4. Add a verifier with an independent implementation or external adjudication
   boundary for the target semantic property.
5. Test real decision feedback under a bounded, reversible, permission-checked
   learning policy; keep shadow mode as the default.
