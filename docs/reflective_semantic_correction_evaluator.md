# Reflective Agency — semantic correction evaluation (Phase 9B)

## Status: implemented, awaiting runtime tests and independent labels

Core.Agency.SemanticCorrectionEvaluator connects the existing LIFG lexical
role-shadow evidence to the production Core pipeline, without giving Symbrella
an unearned claim that a proposed correction is correct.

**This milestone adds an evaluator and a benchmark measurement contract.**
It does not establish that corrections improve real semantic accuracy, and
does not activate any learning mechanism.

## The real production path

The phase-five chain is now:

    |> Core.Semantic.Composition.attach()
    |> Core.Agency.LexicalRoleShadow.attach(opts)
    |> Core.Agency.SemanticCorrectionEvaluator.attach(opts)

The live evaluator always produces a :semantic_correction_evaluation entry
in SemanticInput.trace, even if the caller passes an option or a "gold" label.
No live caller can turn an unsupervised correction into a verified one.

It reports:

- :awaiting_reference when there is exactly one eligible scored and attested
  *exact-sense-ID* alternative and no unresolved lexical role;
- :not_evaluable when structure, scoring evidence or an exact unique
  alternative is missing.

No gold label exists on ordinary live user turns, so these are NOT judgments
of semantic correctness. The trace records the proposal status, number of
eligible candidates, reason and explicit negative authority flags; it does
not contain text, exact sense IDs or supplied labels.

During phase eight, LexicalRoleAccountability.record/2 adds the
semantic-evaluation status to the **same existing agency_events receipt**.
The persistent event is observational; it is excluded from the agency-memory
learning-pressure window. A second semantic-evaluation database table is
unnecessary.

## Separately labeled comparison boundary

For controlled experiments, call:

    alias Core.Agency.SemanticCorrectionEvaluator, as: Evaluator

    reference = %{
      case_id: "externally-reviewed-example-001",
      split: :holdout,
      annotation_source: :curated_external,
      annotation_id: "reference-sheet-001",
      utterance_sha256: Evaluator.utterance_sha256(si),
      token_index: 3,
      gold_sense_id: "flying|verb|1"
    }

    result = Evaluator.compare(si, reference)

The reference must match the exact sentence digest, eligible token index,
and an attested sense ID in the current source lexicon. The evaluator uses
the current LIFG choice and LexicalRoleShadow's evidence-gated proposed
alternative. It does not permit a caller-supplied alternative to replace
the result.

The accepted comparison classes are:

- :wrong_to_right — the proposed exact sense ID matches the supplied label
  but the baseline does not;
- :right_to_wrong — the baseline matches but the alternative does not;
- :both_wrong — neither matches.

An unsupported, tied, unscored, mismatched, malformed or structurally
unverified case abstains.

### Critical distinction

**Caller-supplied labels are not authenticated.** The annotation_source,
annotation_id, and split are declarations, not independent proof of reviewer
identity, label correctness or evaluation-set isolation. Even comparison
results marked :wrong_to_right are merely **agreement with the supplied
reference**. They never set semantic_truth_verified? to true.

The tests use *synthetic fixtures* with deliberately chosen reference
labels, including deliberate regressions; they are not independently curated
held-out data or measured live model accuracy.

A real held-out experiment requires an externally reviewed dataset whose
provenance and membership are established separately, plus anti-leakage
controls between tuning and final evaluation.

## Compare and measure

    results = [
      Evaluator.compare(si1, independently_labeled_reference1),
      Evaluator.compare(si2, independently_labeled_reference2)
    ]

    summary = Evaluator.aggregate(results)

The aggregate reports counts for baseline agreement, candidate agreement,
wrong-to-right, right-to-wrong, both-wrong, abstentions and net label-agreement
gain. It rejects duplicate compared case IDs or repeated source spans, and
rejects inconsistent comparison receipts. It remains read-only and capped at
128 cases per aggregate.

No threshold in this implementation can authorize a LIFG weight change,
self-reported improvement, memory mutation or goal-completion promotion.

## Regression command

    mix test \
      apps/core/test/core/agency/semantic_correction_evaluator_test.exs \
      apps/core/test/core/agency/lexical_role_shadow_test.exs \
      apps/core/test/core/agency/lexical_role_accountability_test.exs \
      apps/core/test/core/pipeline_contract_test.exs \
      --trace

## Next milestone after passing tests

Establish a separately maintained, reviewer-audited, immutable held-out
lexical-sense benchmark; evaluate live LIFG snapshots or reproducibly frozen
evidence on it; publish baseline, candidate, regression, abstention and
coverage metrics. Only then consider a reversible policy-adaptation proposal,
still gated behind repeated independent evaluations.
