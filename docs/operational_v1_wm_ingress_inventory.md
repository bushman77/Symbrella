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

## Static production-writer inventory tripwire

**Implemented, not yet executed locally.** To reduce the chance of silently
introducing an admission bypass, the following read-only tooling scans all
\`apps/*/lib/**/*.ex\` production Elixir source:

- \`Brain.WM.WriterAudit.inventory/1\` parses the source syntax tree and
  discovers recognized \`Map.put\` / \`Map.update\`-style writes to \`:wm\`,
  immutable map-update expressions affecting \`:wm\`, and direct
  \`WorkingMemory.upsert/3\` calls.
- \`Brain.WM.WriterAudit.verify/1\` compares the entire discovered inventory
  against an explicit, reviewed, source-path-and-operation-count allowlist.
  A new writer, a missing known writer or a parse error causes failure.
- \`apps/brain/test/brain/wm/writer_audit_test.exs\` exercises both the
  real repository inventory and synthetic bypass attempts.
- \`scripts/audit_wm_writers.exs\` prints discovered file/line references
  and fails nonzero on a changed inventory.

Run:

    mix test apps/brain/test/brain/wm/writer_audit_test.exs --trace
    mix run --no-start scripts/audit_wm_writers.exs

Initial reviewed operations (pending local test confirmation):

| Production source | Recognized operation | Expected count | Role |
| --- | --- | --- | --- |
| apps/brain/lib/brain.ex | immutable WM map update | 4 | Brain GenServer's focus, Stage2, trim and defocus handlers |
| apps/brain/lib/brain/pipeline/lifg_stage1.ex | map put | 1 | State update after WMFocus / canonical admission |
| apps/brain/lib/brain/wm/gate.ex | map put | 1 | Legacy adapter after Admission.run |
| apps/brain/lib/brain/wm/policy.ex | map put | 2 | Retention-only decay and capacity eviction |
| apps/brain/lib/brain/wm/admission.ex | direct WorkingMemory.upsert | 2 | The allowed canonical insertion points |
| apps/symbrella_web/lib/symbrella_web/components/brain/brain_panels.ex | literal `Map.put_new(:wm, %{})` | 1 | LiveView component-assign default; **not** Brain WM state |

The reviewed static inventory includes **10 recognized Brain WM state-write
operations** and **1 separately classified, non-runtime UI default** across
six files. The UI default is included in the reviewed list, not silently
ignored. An identical AST operation added to an unreviewed module still
fails the check, and non-default `Map.put` updates are classified as writes.
The UI classification depends on the inspected source context, not on a
claim that every `Map.put_new(:wm, %{})` is harmless.

**Limitations:** This is a regression tripwire for *recognized syntax forms*,
not an exhaustive semantic/static analysis or a security sandbox.
\`put_in\`/\`update_in\`, dynamically generated source, macros, reflective
process-state replacement (\`:sys.replace_state\`), alternative aliases, and
runtime BEAM interventions are not fully covered. A code change can preserve
the operation count while altering its meaning. Do not mark all WM writes
proven safe based on this result alone. Runtime callback and ingress tests,
source provenance checks, and independent code review remain required.

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

## Second bypass hardening: direct source claims and scoped context frames

**Change committed; regression tests still require local execution.**

Direct Brain.focus/2 candidates may carry a caller-supplied source. Without
careful validation, a dictionary-shaped lexical winner could declare a
generic source and use :evidence_floor instead of :lifg_min_score.
Brain.BasalGanglia.evidence_floor_for/2 now enforces the maximum of
the generic and lexical floors for LIFG or dictionary-shaped
\`word|part_of_speech|sense_number\` IDs, regardless of a claimed source.
Nonlexical sources retain the generic floor.

Brain.WM.Admission.run/3 now rejects malformed source-scoped
:comprehension and :verified_semantics frames **before gate scoring**, emits
a :block canonical gate receipt with
reason: :invalid_scoped_source_evidence, and performs no WM insertion.
The required structure includes the typed frame identity and kind,
current version, an unexpired context timestamp, and non-factual-authority
metadata. Verified-semantic pointers additionally require a nonempty,
well-shaped source-proof ID list and the specific source-revalidated marker.
The existing Core comprehension and verified-semantics paths already
construct these shapes and still perform their own separate checks.

New cases cover:
- lexically shaped IDs disguised as :runtime, :intent, :ltm or :curiosity;
- generic hard evidence floor stronger than the LIFG floor;
- source labels without valid scoped frame metadata, including string labels;
- stale comprehension context, malformed frame identity and false factual
  authority;
- well-formed scoped frames still accepted by BasalGanglia;
- runtime Brain.focus/2 refusing an invented verified-semantics pointer.

**Important:** structural shape is not proof authentication. A malicious
in-process caller could reproduce the expected shape and claim arbitrary
proof IDs. The independent, persisted-source proof checks remain Core-owned;
this change is a defensive WM normalization layer, NOT a security boundary
against arbitrary BEAM code. No V1 criterion can be signed off until the
upstream proof, source authorization and bypass tests are comprehensive.

Test commands:

    mix test \
      apps/brain/test/brain/wm/admission_test.exs \
      apps/brain/test/brain/wm/runtime_admission_integrity_test.exs \
      apps/brain/test/brain/wm/working_memory_stage2_ingest_test.exs \
      --trace

    mix test \
      apps/core/test/core/brain/comprehension_wm_test.exs \
      apps/core/test/core/brain/wm/verified_semantics_contract_test.exs \
      --trace

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
