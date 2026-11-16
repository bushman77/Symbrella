# Phase 9A for Operational Self-Awareness V1 — WM Ingress Inventory

**Status: partial source-level inspection, NOT an exhaustive bypass audit.**
**Branch:** symbolic_generation  
**Release criterion:** Core.Release.OperationalAwarenessV1 gate :unified_control_gate.

This inventory is an engineering starting point, not a claim that the gate
is already complete. A full mutation-path search and tests are required
before any release acceptance receipt can be issued.

## Observed code paths (read at V1 scope freeze)

| Path | Observed admission boundary | Inspection outcome |
| --- | --- | --- |
| Brain.focus/2 -> Brain.handle_call({:focus, ...}) -> Brain.WM.Focus.run/3 | Brain.WM.Admission.run/3 -> Brain.BasalGanglia.decide/4 | Canonical route already wired |
| Brain.focus_from_recall/2 -> Brain.WM.Recall.candidates_for_focus/2 -> Brain.focus/2 | Canonical route | Recall is candidate evidence; needs adversarial proof tests |
| Brain.WM.Gate.ingest_from_si/3 (legacy) | Brain.WM.Admission.run/3 | Legacy input translator already delegates |
| Brain.WorkingMemory.ingest_stage2/4 (legacy) | Brain.WM.Admission.run/3 | Stage2 decisions are treated as candidates, not final authority |
| Brain.LIFG.Stage2.run/2 | Returns linguistic decisions with final_admission?: false | This module does not itself commit to WM |
| Core.Brain.WM.Comprehension.store/2 | Brain.focus/2 | Has deliberate per-call zero gate/evidence thresholds; must test source-scope isolation |
| Core.Brain.WM.VerifiedSemantics.admit/5 | Independent symbolic proof check, then Brain.focus/2 | Also uses zero gate/evidence thresholds on a typed pointer; must test missing/stale proof cases |
| Brain.WM.Policy | Decay, retention and eviction only | No new candidate admission observed |
| Brain.Curiosity/Brain.Thalamus | Emits curiosity and arbitration proposals | Direct WM insertion not observed in sampled paths; follow downstream subscribers in exhaustive audit |

## Observed ownership boundaries

Brain.WM.Admission.run/3 is the point where normalized candidates are
presented to Brain.BasalGanglia.decide/4. For :allow or :boost, it calls
Brain.WorkingMemory.normalize/3 and upsert/3; for :block, it does not
insert a candidate.

Brain's :focus GenServer handler is a caller of Brain.WM.Focus.run/3,
which forwards to Admission.run/3. :defocus and :configure_wm mutate the
existing memory list for removal and retention; they are not new candidate
admissions and must still be checked for count, capacity and scope rules.

The :comprehension and :verified_semantics paths use very permissive
per-call thresholds (including evidence_floor: 0.0). The existence of
BasalGanglia.decide/4 on those paths does not, by itself, prove strong
evidence admission. Proof must come from the *preceding typed evidence
requirements* and boundary tests. A candidate may never gain factual
authority merely by being in Working Memory.

## Required tests before closing :unified_control_gate

1. Enumerate all reachable WM insertion/mutation callers across Brain,
   Core and Web. Verify a new item is never inserted by an alternative
   state mutation that bypasses Admission.run/3.
2. Reproduce :allow/:boost/:block decisions for source classes with an
   explicit candidate-evidence floor. Assert :block never inserts.
3. Force maximal vigilance or minimal inhibition; prove SelfModel biases
   cannot pass a hard evidence floor or prohibited source restriction.
4. Inject an invalid, stale or forged Hippocampus recall candidate. Confirm
   it cannot be admitted as a verified fact; absence of factual proof
   remains explicit.
5. Test Stage2 :commit/:allow/:boost legacy variants; they must still pass
   the final BasalGanglia decision and cannot force a commit.
6. Test the scoped zero-floor comprehension frame. It must not influence
   unrelated lexical/episodic candidates or confer truth.
7. Test the proof-gated verified-semantics pointer with missing, altered,
   or revoked source proofs, including subsequent retrieval.
8. Test real runtime telemetry linking every admitted item with a gate
   decision and source-specific evidence, across all ingress routes.
9. Test configure/defocus/retention changes preserve capacity, duplicate
   and timestamp rules without accidentally granting new admission.

The gate remains **open** in the V1 release checklist until these tests are
implemented, passing and reviewed. Do not convert existing green unit tests
alone into a full-bypass-coverage claim.

## First bypass hardening: Stage2 source spoofing

**Change implemented; runtime tests still need to pass.**

The canonical admission normalizer accepted Stage2
{:commit, choice} / {:allow, choice} / {:boost, choice} tuples, but previously
used Map.put_new(:source, :lifg). An embedded :source could therefore
override the wrapper's lexical origin. BasalGanglia distinguishes
:lifg_min_score from the general :evidence_floor, so a malicious/buggy
Stage2 candidate labeled :comprehension or :runtime could take the weaker
generic threshold.

Now the Stage2 tuple handlers force :source to :lifg before normalization.
This affects both direct Brain.WM.Admission.run/3 and the legacy
Brain.WorkingMemory.ingest_stage2/4 translator.

New adversarial regressions:

    mix test \
      apps/brain/test/brain/wm/admission_test.exs \
      apps/brain/test/brain/wm/working_memory_stage2_ingest_test.exs \
      --trace

The tests explicitly attempt a scored, under-floor Stage2 candidate
with an embedded :comprehension / :verified_semantics / :runtime source,
including the legacy map-based commit encodings. All must be blocked
under the higher LIFG evidence floor.

**Remaining scope:** source annotations in arbitrary, direct Brain.focus/2
candidate maps are not authenticated by this change. Future V1 work must
audit source-specific authorization at that boundary and all other
entry points. A single spoofing fix does not establish full canonical
gate coverage or independent factual verification.

## First implementation slice

Prefer negative-control tests around Brain.focus/2, the legacy Stage2
translator and the two zero-floor Core admission paths before changing
their behavior. Preserve validated application behavior; only refactor
when tests show a real bypass or when a canonical ownership invariant is
not enforced.

Do not run mix format automatically or create an extra branch for this
scope freeze. Keep exact changes and tests on symbolic_generation.

## Independent release evidence

When the full audit exists, record its source revision, explicit test results,
runtime trace and independently reviewable artifact hash under the
:unified_control_gate criterion. An evidence receipt is a **declaration**
until an external party checks the linked artifacts and reproduces claims.

Reference: docs/operational_self_awareness_v1.md
