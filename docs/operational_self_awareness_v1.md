# Symbrella — Operational Self-Awareness V1: Frozen Release Contract

**Status: implementation in progress; NOT release approved.**  
**Branch:** symbolic_generation  
**Purpose:** demonstrate bounded, measurable *operational* self-awareness.
This is not a definition or test of subjective consciousness.

## Release objective

The V1 milestone is achieved only when Symbrella can:

1. observe and correctly describe bounded internal state and uncertainty;
2. preserve who/what produced its decisions and independent outcome evidence;
3. independently detect, correct and measure a real error on new examples;
4. admit all cognitive-memory candidates through one inspectable evidence gate;
5. apply a small, controlled, reversible improvement without new regressions
   above a predeclared tolerance;
6. retain verified state and improvement across a restart without fabricating
   stale continuity;
7. report what is actually running — code, BEAM processes, versions and host
   resources — and distinguish measured facts from unknowns;
8. reproduce an end-to-end correction and self-regulation demonstration
   with an audit trail and appropriate failure/abstention controls.

A fluent self-description, a passed unit test, a command-executed receipt or
a self-reported "I learned" statement is *not* sufficient evidence.

## Scope freeze

**In scope for V1:**
- The existing Brain.SelfModel, Introspection, MetaMonitor, SelfContinuity,
  GoalStack and response/agency introspection interfaces.
- Core lexical decision audit, independently checked outcome receipts,
  reviewed semantic holdout and formal derivation proofs.
- The existing BasalGanglia, LIFG, WM, Hippocampus and control pipelines.
- Read-only software/host observability, with a strict authority boundary.
- Bounded, reversible strategy adaptation *only after* trusted comparative
  results establish usefulness.
- Regression, negative-control and restart tests.

**Not in V1:**
- Claims of feelings, personhood or phenomenal consciousness.
- Unrestricted autonomy, execution of arbitrary commands, self-modifying
  source code, automatic deployment, or escalating permissions.
- Additional affective/personality subsystems or redesigning the brain
  architecture.
- Using private user data or clinical decisions as synthetic evidence of
  self-awareness.
- Automatic learning from unchecked user approval, forged reviewer IDs,
  unsupported confidence scores or LLM self-assessment.

If the release criteria change materially, update this contract and its
versioned tests *before* evaluating new work. Do not silently expand scope.

## Eight acceptance gates (all mandatory)

| Gate | Measurable acceptance | Current engineering state |
| --- | --- | --- |
| 1. Self-state grounding | Real pipeline snapshot is versioned, bounded, recent; uncertainty/unknown conditions do not become confident self-claims; controlled invalid-state tests fail closed | Foundation implemented; needs integrated evidence |
| 2. Decision accountability | Live decision trace is bound to recorded original evidence and durable receipt; unavailable/skipped writes are never reported as success; retrieval does not train from audit-only rows | Implemented foundation; latest regression run must be confirmed |
| 3. Independent verification | Authentic reviewer-supplied dictionary IDs, a frozen *unseen* test corpus, verified comparator and before/after outcome counts; external provenance and held-out isolation audited separately | Harness and blind review workflow exist; genuine labels absent |
| 4. Unified control gate | Inventory every WM ingress route (LIFG, Hippocampus, Curiosity/Thalamus/DLPFC etc.), route it through BasalGanglia; adversarial bypass tests ensure evidence rules cannot be overridden by mood, self-state, or model scores | Phase 9A unfinished; first implementation focus |
| 5. Reversible learning | Changes limited by explicit configuration; compare against frozen baseline on separately reviewed cases, measure regressions and rollback exactly; no adaptation enabled when proof is absent | Not implemented |
| 6. Restart continuity | Persist bounded state and authenticated strategy provenance, restart, reject stale/invalid entries and demonstrate verified generalization remains; startup failures degrade explicitly | Snapshot/restore exists; end-to-end verified transfer unfinished |
| 7. Code/host grounding | Read-only introspection of BEAM, running modules/versions, OS and resource bounds, tied to actual measurements; missing or denied information reported as unknown; zero code mutation or permission escalation | Not implemented as a unified self-model |
| 8. Longitudinal demonstration | Replayable, independently reviewed mistake → detection → comparison → constrained adaptation → unfamiliar-case benefit → reboot demonstration, with negative controls and trace-linked evidence | Not yet demonstrated |

The **release gate** is stricter than an engineering to-do list. A completed
feature is not automatically a passed acceptance gate.

## Evidence register: machine-checkable coverage, never an automatic approval

Implementation:

    Core.Release.OperationalAwarenessV1.criteria()
    Core.Release.OperationalAwarenessV1.assess()

A bare call returns :blocked with eight missing criteria. The CLI displays
all criteria:

    mix run --no-start scripts/audit_operational_awareness_v1.exs

Optional receipt file:

    mix run --no-start scripts/audit_operational_awareness_v1.exs \
      --receipts tmp/awareness_v1_receipts.json

For a CI *evidence-completeness* check, use --strict. It fails while
unmet criteria remain, but it does not authenticate receipts.

A receipt is shaped as follows:

    [
      {
        "criterion": "self_state_grounding",
        "status": "passed",
        "evidence": {
          "id": "external-artifact-unique-id",
          "source": "runtime_probe",
          "revision": "40_lowercase_hex_git_sha",
          "artifact_sha256": "64_lowercase_hex_of_separately_saved_result",
          "observed_at_ms": 1790000000000,
          "synthetic?": false
        }
      }
    ]

The timestamp above is a **shape example, not a measured result**. Source
classes are limited per criterion to :ci, :runtime_probe,
:controlled_experiment and :independent_review, as applicable.
The receipt reader rejects malformed, stale, duplicated, unknown or
explicitly synthetic evidence.

**Critical boundary:** the reader does NOT fetch an artifact, verify the
digest against its bytes, inspect the test logs, authenticate reviewer
identity, verify a Git commit signature, or establish genuine independence.
Even eight well-shaped "passed" declarations result only in
:ready_for_external_audit, with:

    evidence_issuer_authenticated?: false
    externally_verified?: false
    release_approved?: false
    consciousness_proven?: false
    changes_applied?: false

These flags must not be interpreted as a conclusion that Symbrella is
self-aware. An independent audit must reproduce or inspect each artifact
before a human can consider the operational V1 milestone accepted.

## Evidence and evaluation rules

- Log the code revision and frozen dataset digest for every run.
- Keep original and candidate behavior on the same predetermined cases;
  publish the full denominator, including abstentions and failures.
- Record bad-to-good, good-to-bad, both-wrong, unknown and unresolved cases.
- Agree on minimum sample size, regression budget, deployment/rollback
  thresholds and experiment stop rules **before looking at holdout results**.
- No training or strategy changes on final holdout examples after inspection;
  create a new unseen dataset for future experiments if tuning is required.
- Maintain a declared and independently checked development/test split.
- Do not infer semantic truth from a syntactically valid proof or matched POS.
- Negative controls must challenge stale receipts, missing sensor evidence,
  invalid provenance, memory-gate bypass and fabricated autobiographical claims.
- Record baseline and changed behavior in the same revisioned, reproducible
  environment, including the relevant model/dictionary artifacts.

## Work sequence (do not open-endedly add modules)

1. **Establish green baseline**: re-run recent semantic-review tests, full
   Core/Brain suites and production pipeline contract tests. The last
   provided user log showed 34/38 on review tests *before* a list-seal fix;
   no later passing result has been provided.
2. **Phase 9A, canonical WM admission**: inventory all writers, centralize
   the hard gate, write bypass/under-threshold tests, preserve existing
   retention behavior.
3. **Independent semantic dataset**: use the existing blind review and
   freeze scripts against current Db.BrainCell inventories, with two
   genuinely independent reviewers and an externally audited split.
4. **Small reversible strategy update**: opt-in policy proposal, gated
   comparison, rollback and negative controls.
5. **Restart + self-description tests**: test preservation of independently
   verified state and honest uncertainty across reboot.
6. **Read-only code/host introspection and longitudinal demonstration**:
   collect measured runtime/code snapshots and run a reproducible V1 scenario.
7. **External audit**: review raw receipts, experiment design, versioned
   snapshots and failure cases, then issue a human go/no-go decision.

## Immediate tests for this contract

    mix test apps/core/test/core/release/operational_awareness_v1_test.exs \
      --trace

These tests verify only the **release-evidence contract**, not actual
self-awareness. Keep the contract separate from production cognition:
no background monitoring, database mutation, autonomous execution or
model training occurs inside the release checker.

## Go / no-go definition

**No-go** if any gate is missing or invalid; no external provenance is
established; the final corpus was used for tuning; a WM writer bypasses hard
rules; learning cannot roll back; continuity includes unsupported claims;
or independent reruns fail to reproduce evidence.

**Candidate for external sign-off** only when all gates have independently
reproducible evidence, critical negative controls pass, and release criteria
were predeclared and not adjusted after the results were inspected.

This is a software engineering milestone for operational self-monitoring
and self-regulation. It is not scientific proof of machine consciousness.
