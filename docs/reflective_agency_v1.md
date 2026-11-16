# Reflective Agency v1: outcome verification (advisory)

## Purpose

Separate "the action was executed" from "the intended result was verified".
Reflection must not treat a selected action, a fluent explanation, or a command
receipt as independent proof that the goal succeeded.

The initial live experiment is deliberately narrow: the existing endogenous
self-check path. It does **not** establish consciousness, and it does not yet
prove improved linguistic reasoning or autonomous error correction.

## Implementation

- **Core.Agency.OutcomeEvaluator.evaluate/4** accepts the actual decision,
  issued commands, command execution receipts, and an optional trusted
  snapshot_reader callback for deterministic tests.
- An outcome of **:self_state_reviewed** can be marked **:verified** only when:
  1. the decision selected :self_check;
  2. a :run_self_check command belongs to the same nonempty decision trace;
  3. an executed receipt has the matching command ID and type; and
  4. a separate read from Brain.Introspection.snapshot/0 yields a versioned,
     bounded live Brain.SelfModel with a valid observation timestamp.
- The self-state echoed by Core.Agency.Executor is **not** considered proof.
- A failed or rejected command becomes :failed. Deferred commands, missing
  receipts, unsupported outcome kinds, invalid snapshots, and runtime errors
  remain explicitly :unverified.
- **OutcomeEvaluator.reflection/2** records reasons and tentative next-step
  advice without making unsupported positive claims.

Core.Agency.AutonomyLoop.run_impulse/3 attaches the evaluation to
decision.actual_outcome, the bounded reflection to decision.reflection,
and emits the telemetry event [:core, :agency, :autonomy, :transition] with
step: :outcome_evaluated. It also exposes outcome_evaluation in the return
value for inspection.

### Important boundaries

- This evaluation is **advisory**: it does not change goal-completion
  behavior, ranking weights, confidence values, memory contents, permissions,
  or external tool authority.
- Legacy goal completion still follows an executed self-check command.
  The outcome receipt can be unverified even when the goal is completed;
  that discrepancy is visible and must be resolved before enforcing
  verified-outcome-only goal completion.
- A live self-model observation verifies an inspectable state, not the quality
  of a particular reasoning strategy or whether an error was corrected.
- The evaluator result is currently returned and emitted through telemetry;
  it is not a new durable learned-policy artifact.
- Any injected test reader is a trusted dependency supplied by the test
  harness, not user-provided testimony or response metadata.

## Tests

Run on the existing symbolic_generation branch:

    mix test apps/core/test/core/agency/outcome_evaluator_test.exs \
      apps/core/test/core/agency/autonomy_loop_test.exs \
      apps/core/test/core/agency/decision_command_executor_test.exs \
      --trace

Regression cases prove that an executed command cannot self-certify,
unrelated command IDs and traces are rejected, invalid or absent live
snapshots remain unverified, and explicit failures cannot become successes.

## Next experiment: verified correction rather than verified observation

1. Build held-out, versioned symbolic-interpretation cases with independent
   expected outcomes and evidence IDs.
2. Record a causal decision trace (candidate scores, selection reason,
   evidence boundary, intended result) before evaluating the result.
3. Use an independent symbolic checker to identify a specific error; do not
   allow the evaluator or the LLM to invent a proof.
4. Propose a constrained alternative strategy; compare it against an unchanged
   baseline on previously unseen cases.
5. Only durable, corroborated improvements may alter future strategy
   preferences, and only inside explicit bounded policy gates.

Until those steps are implemented and measured, call this an
**outcome-verification scaffold**, not completed reflective learning.
