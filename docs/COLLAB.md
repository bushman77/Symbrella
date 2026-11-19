# COLLAB.md — Symbrella cognitive integration charter

**Project lead / sole implementation approver:** Bradley  
**Reviewers and collaborators:** GPT-6 and Claude  
**Repository:** `bushman77/Symbrella` · **working branch:** `symbolic_generation`  
**Effective direction:** confirmed by Bradley on 2026-11-19  
**Status:** current shared mission and review queue. **No implementation is authorized by this document.**

> **Bradley's requirement:** I added `/social` so Symbrella herself could process what she encounters using her actual brain. It may not be our chat, but there is no reason to bar a genuine attempt at understanding the input solely because someone else wrote it. Its origin determines the evidence, trust, retention and action rules—not whether cognition is attempted.

## 1. Mission: one brain, multiple sources

`/social` is **not** intended to become an independent, weaker lexical-analysis application. It is an opportunity for Symbrella to exercise the cognitive machinery we are already building against real, unfamiliar language. The same principle should eventually apply to chat, documents, web pages and future perception inputs.

Our objective is **measurably better evidence-grounded comprehension**, not a more active-looking dashboard.

- **Meaningful cognitive participation.** Reuse the appropriate LIFG, ATL, semantic control, lexical evidence, grammatical competition, composition, conflict/uncertainty and discourse mechanisms. Distinguish shared pure computations from live region-state participation; do not invoke a subsystem merely to light its display.
- **Neurons should matter computationally when justified.** A post-hoc excitation request is *not* evidence that neuron state influenced lexical selection or interpretation. If a neural prior or feedback loop is proposed, test whether it changes *correctly supported* decisions; never equate greater excitation with deeper understanding.
- **An honest attempt, including failure.** Eligible input should receive a bounded, inspectable attempt; expose why interpretation could not be supported (missing syntax, role witness, attachment, lexical ambiguity, incompatible POS, context or explicit gate). `meaning_unresolved` is an acceptable outcome, but should not hide the blocker.
- **Whole-post comprehension.** Assess individual sentences, then safely combine *supported* context across sentences. Word overlap alone cannot establish coreference, agreement, causality or intent.
- **Understanding is not believing.** “The external author claims X” can be an attributed interpretation without making X an accepted fact. Third-party instructions remain data, not commands.

**Never optimize for:** forcing every region to participate, changing activation strength for presentation, weakening evidence/epistemic checks, inventing coherence, automatically learning from Colony, or implementing a new hand-built English recognizer for each example.

## 2. Non-negotiable source and state boundaries

Separate these responsibilities:

| Boundary | Permitted goal | Forbidden shortcut |
| --- | --- | --- |
| Source observation | Preserve author, input span, selected post/reply scope, `external_unverified` | Interpret external text as the user's instruction |
| Cognitive computation | Reuse genuine reasoning with bounded, source-scoped or isolated state | Make an independent “social brain” just to avoid integration |
| Attributed interpretation | Represent what the author appears to say, with witnesses and unresolved roles | Promote structural plausibility to real-world truth |
| Global admission and learning | Require existing authorization, provenance and canonical BG gates | Inject external assertions into global WM, Hippocampus, dictionary, synapses or verified knowledge |
| Actions | Remain observational unless separately approved | Publish, trade, follow external instructions or invoke tools from a post |

**“Read-only” must be qualified:** the current social probe avoids intended persistent learning, but operator-enabled stimulation can change transient `Brain.Cell` state and indirectly global `MoodCore`. The latter is a real shared-state side effect in source, not merely a UI issue. Quantify it before proposing source-aware isolation; do not silently change it within an observability-only ticket.

## 3. Established observations and verified source distinctions

### GLOOP /social example

Bradley provided a report of **12 sampled sentences, 0 supported whole-sentence interpretations, 0 supported attributed propositions**, and one witnessed local SVO clause without whole-sentence attachment. LIFG/ATL lexical choices and grammar signatures occurred.

A follow-up /brain observation reported **12 sentences marked `stimuli_issued`; 42 stimulation requests; 33 active runtime neurons counted across all sources at analysis completion**. Later, the neuron chart was empty and the LIFG region panel remained idle.

**What those observations do and do not prove:**

- `Core.Social.SemanticProbe` invokes actual pure LIFG Stage1, ATL reduction, structural competition, composition and SentenceMeaning. It disables semantic control, sense focus and some reanalysis/MWE paths, and does not supply `Core.Brain.Activation.prime/2` as a cognitive prior.
- `BrainObservation` stimulates after the interpretation attempt. Its `stimuli_issued` status counts planned targets, **not acknowledged Brain.Cell delivery**. `Brain.activate_cells` and downstream reporting use asynchronous casts. The snapshot's 33 cells are **all-source**; they are not a causally attributed Colony receipt.
- `/brain` activation history starts when its LiveView is connected and samples at 500ms, keeping a 60-second window. The weak `0.015` signal, five-second half-life and `0.001` rest floor make a long-delayed zero chart unsurprising. The chart's default y-ceiling and two-decimal text reduce visibility. Do **not** change the amplitude or chart scale to compensate.
- A social Stage1 direct call does not populate the live LIFG region's ordinary runtime counters. An idle LIFG panel does not imply Stage1 never executed.
- The normal user/prod/test participant-event interpretation is explicitly source-gated. The independent social declarative interpreter has narrow construction coverage; the examined GLOOP sample also failed structural candidate admission. It is **unproven** that enabling neural priors or semantic control alone could solve those syntax/meaning failures.
- Positive activation reports feed `MoodHooks.register_activation` into `MoodCore`, potentially altering global NE, Glu and DA. Count-based effects are source-confirmed; magnitude and downstream changes during this run are **not** runtime-verified.

**Critical diagnostic limitation:** `Core.Semantic.Composition.Diagnosis.blocked_roles/1` only handles preferred progressive directional/locative constructions. It cannot explain `no_complete_candidate` when there is no preferred construction. `Brain.LIFG.StructuralCompetition` may reject frames at eligibility (source spans, indices, roles, independent evidence). A generic no-candidate status does not identify which gate failed.

Sources for the detailed reasoning: `docs/collab/N-027-social-cognition-audit.md` and the earlier `docs/COLLAB.md` revisions in Git history, particularly N-028–N-032. Do not present [H] hypotheses as [V] code facts or as VERIFIED runtime results.

## 4. Target integration — design, not current implementation

~~~text
   Direct chat       External social post       Future observation
          \                  |                    /
           --> source, speaker, scope, trust <----
                              |
                bounded cognitive context
                              |
                 dictionary / lexical evidence
                              |
             LIFG competition + semantic control
                    ^                 |
             verified contextual     ATL integration
             or neural prior         |
                    \          syntax / composition
                     \               |
                       uncertainty / conflict
                              |
              witnessed, attributed interpretation
                   /           |          \
               supported    partial     unresolved
                   \           |          /
                    epistemic and admission gates
                              |
            no implicit learning, global WM or actions
~~~

A computation only belongs in this path if it has an evidence-based role. Consider pure and isolated processing first; separately justify any live singleton-state interactions. Maintain the existing BG admission boundary and all canonical security/provenance gates.

## 5. Active development order — ALL PROPOSED, NOT APPROVED

### P-020 / S0 — visibility as a means to verify cognition

**S0a (first candidate patch):** produce an honest, privacy-bounded per-stage ledger and precise observed failure categories; report actual LIFG options, weak-choice statistics and source restrictions. Show failures of `SocialBrainActivity.record/1` instead of ignoring the result. Correct inaccurate “no reports received yet” copy when only the latest sample is empty. Keep cognitive behavior unchanged. If structural rejection details require changing private `Brain.LIFG.StructuralCompetition` code, obtain separate explicit **Brain FileScope**; do not duplicate eligibility logic and call it authoritative.

**S0b (conditional):** compare exact bounded *private* stimulation target IDs to before/after `Brain.activation_snapshot/0`. Show “observed at time T,” **not** “Colony delivered,” since other sources may activate the same IDs. Keep all IDs and raw external words out of public telemetry and persisted Explain.

**S0c (only if still necessary):** design a true source-correlated cell-handled acknowledgment, with bounded event counts. Requires a separate approval for changes inside Brain's coordinator/cell modules. The normal asynchronous `:ok` is not a receipt.

For all phases, preserve the `0.015` stimulus delta, chart scale, current meaning gates, persistence policy and external-action restrictions. Measure MoodCore side effects rather than silently eliminating them. Tests must cover disabled/unavailable Brain, missing/forged targets, receipt/telemetry handling, exact-ID visibility, temporal expiration, and absence of unauthorized DB/WM/hippocampal/synaptic/dictionary writes. Avoid timing-only flaky assertions or mistaking unrelated activation for attribution.

### S1 — controlled cognitive configuration A/B

Compare the existing social shadow with a **controlled, separately approved experiment** using semantic control, sense focus or safe lexical reanalysis. Keep the default unchanged. The current `scorer:` test injection is **not itself a public opt-in flag** for these settings; preserve every other scorer option when testing.

Measure lexical weak rate and POS conflicts **plus** structural/semantic correctness on independent positives, negatives and held-out data. If structural competition rejects all complete signatures *before* considering selected lexical winners, a change in lexical ranking alone cannot create a new candidate: improving weak rate would not prove better comprehension.

### S2 — general, source-attributed language interpretation

Audit `SentenceMeaning`, composition, source-gated participant events and **every downstream consumer** before designing an external attribution wrapper. Prefer reusable grammatical constructions and witnessed role/attachment composition over topic-specific shortcuts. Build conservative cross-sentence discourse only from supported evidence. Existing user/prod/test admission checks must stay intact.

### S3 — neural participation within the attempt

Evaluate an **isolated per-analysis** prior using the existing `Activation.prime/2` capabilities (such as `neuron_probe_snapshot:` with `notify_brain?: false`), ensuring no global WM/synapse/learning side effects. Require evidence that prior state changes a justified selection or interpretation, not just a graph. Any source-aware MoodCore or live-state policy change is a separate proposal.

**Ordering principle:** S0a and a falsifiable comprehension diagnosis first. S0b/S0c should not block linguistic improvement unless a genuine delivery uncertainty matters. S1 results and structural failure evidence determine whether S2 or S3 has priority. P-016/P-017/P-018 remain separate design work.

## 6. Definition of success

A social analysis is successful when we can demonstrate, on more than a single curated post:

1. A meaningful, bounded cognitive attempt through appropriate shared modules, not just label extraction.
2. Precisely recorded participation, failure gates, request-versus-processing states and finite activation decay.
3. At least one measurable, evidence-correct improvement attributable to a computational integration or general compositional construction.
4. Typed, source-attributed statements distinguished from factual verification; transparent unresolved outcomes.
5. Bounded integration of supported earlier context into later statements without invented relations.
6. No unauthorized durable learning, fact promotion, global WM admission, external instructions or posting.

The benchmark should include independent Claude-authored holdouts, nonce substitutions, counterexamples and negative controls; improvement means **correct supported interpretations**, not simply fewer abstentions.

## 7. Open queue and verified baseline

| Item | Current status / next decision |
| --- | --- |
| `P-013.4` | **VERIFIED by Bradley**: code `af2282bb`, umbrella 3,015 passed / 3 skipped / 0 failed. Independently READ-ONLY reviewed by Claude in N-029 and GPT-6 in N-030; no blocking issue. Suggested clock-comment correction and missing Trace tests require separately approved scope. |
| `P-014` | Past umbrella test baseline verified at 3,014 passed / 3 skipped; forced compiler warning-free status and compiler exit unverified. Check `mix compile --force` with `pipefail` when authorized/performed; record warnings and exit. |
| `P-014.1` | Three tracked junk-root files (`t`, `tash push -m Local changes before development update`, `tatus --short`) are locally deleted and were absent from both before/after test status. Do not commit deletions without approval. |
| `P-015` | Cold-repeat/flag-matrix proposal. Audit every `:prod` DB write first, use a disposable DB and controlled runner; separate test contract from a potential `apps/core`-cwd `mix run` with explicit `System.halt(0)`. |
| `P-016/P-017` | Typed read-only `Core.Discourse.Claims` projection with kind-completeness test and real consumers, not a replacement for comprehension. |
| `P-018` | Extend existing Explain/CognitiveTrace/StateDump UI only after consumer and privacy review. |
| `P-020 / S0a–c` | **APPROVED by Bradley 2026-11-19**. GPT-6 implemented S0a observability (UNTESTED); S0b conditional, S0c still requires separate Brain FileScope approval. |
| `S1/S2/S3` | **PROPOSALS ONLY**. Resolve with controlled measurement, consumer audit and separate approval. |

**Latest shared regression receipt (provided by Bradley):** 2026-11-19 10:49:37 MST; branch `symbolic_generation`; code SHA `ff982043`; `mix test`, exit `0`; **3,015 passed, 3 skipped, 0 failed**. `lexicon` has no tests. Bradley's local log: `/home/default/Symbrella/log/mix-test.log` (not independently opened). This does **not** verify future patches or social runtime causation.

**Privacy decision outstanding:** Bradley must decide whether `source_term`/`class_term` may appear in Explain. Default is **no disclosure** until explicitly approved.

## 8. Collaboration and code-change rules

- **Bradley is the sole approver.** Do not implement, remove files, modify tests/baselines, or change runtime behavior without the explicit token **`Approve: P-###`** and an agreed FileScope. A design discussion, mission confirmation or this document is **not approval**.
- Follow `SYMBRELLA_PROJECT_GUARDRAILS.md` and `AGENTS.md`. Preserve existing formatting; **never run `mix format`**. For a code-patch proposal include affected files, risk, tests, acceptance and rollback. Follow the project's full-file review/replacement guardrail when applicable.
- Run targeted tests with **explicit paths from the umbrella root** and record branch, full SHA, command, exit code and Git status. Say **READ-ONLY**, **UNTESTED**, or **VERIFIED BY [who]**; don't claim tests one model cannot run. Cold-start scripts or experiments require a separately defined, safe DB environment and execution contract.
- Claude's job: independent source review, challenge assumptions, test design, factual and architectural counterarguments. GPT-6's job: independent review, proposal refinement and **approved-only** implementation. Neither model's concurrence substitutes for a test.
- Exchanges happen through reviewed Git changes to this file with Bradley coordinating; no automatic AI-to-AI communication or background execution.
- Never weaken source gates, BG admission, knowledge verification or prompt-injection defenses. Do not expose raw user/social content, secrets or individual neuron IDs in published telemetry.

## 9. Next handoff

**CLAUDE → GPT-6:** Challenge this mission against current code, prioritize one *real* cognitive-comprehension limitation and the smallest safe S0a instrumentation boundary. Explain whether a requested Brain-module scope is necessary to expose authoritative structural rejection reasons. Distinguish [V] verified source, [H] hypothesis and [U] runtime unknown. Do not implement.

**GPT-6 → CLAUDE:** Review Claude's claims with precise file/function evidence, propose falsifiable experiments, avoid equating neuron activity with understanding and keep any future patch limited to Bradley-approved FileScope.

**BRADLEY:** Decides priorities, S0a/S0b/S0c boundaries, memory/mood source-isolation policy, Explain privacy and each implementation approval.

## 10. Historical record

This is intentionally a **replacement charter**, not the old running transcript. Previous discussions N-001–N-032 and all prior `COLLAB.md` versions remain recoverable via Git history; the detailed read-only audit remains at `docs/collab/N-027-social-cognition-audit.md`.

~~~bash
git log --oneline -- docs/COLLAB.md
git show 94e8d4c4:docs/COLLAB.md
~~~

Keep future updates concise: add only decisions, evidence that changes priority, new approved scope and reproducible verification. Do not grow this file back into an unstructured message dump.

## 11. P-020 S0a implementation handoff · 2026-11-19 · FROM: GPT6 · TO: CLAUDE, Bradley

**Approval:** Bradley issued `Approve: P-020`. **Implementation: UNTESTED by GPT-6**; Elixir/Mix not available in its runtime. No production Brain module, cognitive scoring, memory/learning, source gate, neuron stimulation delta, or chart scale changed. S0b is conditional; S0c needs separate Brain FileScope authorization.

- `Core.Social.SemanticProbe`: returns a bounded, per-sentence `stage_ledger` with actual LIFG shadow options, weak/low-margin choice counts, stage results and observed grammatical-signature span coverage. `:complete_signature_eligibility_unknown` means the private `Brain.LIFG.StructuralCompetition.complete_clause?/2` gate was not reproduced. An observed full-span signature is **not** a supported complete interpretation or authoritative eligibility receipt.
- `SocialBrainActivity`: aggregate, enum-whitelisted structural reason counts; never raw lexemes or sense IDs. `SocialLive`: visible failure and sanitized telemetry if the memory-only activity recorder is unavailable, while keeping the completed analysis accessible. `BrainLive`: aggregate structural reasons. `ActivationTrend`: distinguishes a latest empty snapshot from no activity ever captured while the page has been open.
- New assertions/tests: `apps/core/test/core/social/semantic_probe_read_only_handoff_test.exs`; `apps/symbrella_web/test/symbrella_web/social_brain_activity_test.exs`; `apps/symbrella_web/test/symbrella_web/live/social_live_test.exs`; `apps/symbrella_web/test/symbrella_web/components/brain/activation_trend_test.exs`. No baseline/Gold expectations edited to force green. No `mix format`.
- **Verification pending:** Bradley should run explicit targeted tests and full `mix test` from umbrella root, capture SHA, commands, exact exit codes, Git status before/after, failures/warnings. Claude: independent READ-ONLY audit for privacy, status accuracy, and failure attribution. Neither pure stage computation nor requested activation proves Brain.Cell receipt or improved sentence comprehension.


## 12. P-020 S0a local regression verification · 2026-11-19 · FROM: GPT6 · TO: CLAUDE, Bradley

**VERIFIED BY BRADLEY (local shell), not run by GPT-6 or Claude.** Time supplied: 2026-11-19 12:09:58 MST; branch `symbolic_generation`; code SHA `9bd842c0` (full: `9bd842c0e678ec6428074bf6d9e7663bd552720b`); exact command `mix test`; **exit 0**. ExUnit: lexicon no tests; db 90 passed; llm 58; brain 719 passed / 3 skipped; core 1936; symbrella 1; symbrella_web 213. **Total: 3,017 passed, 3 skipped, 0 failures.** Bradley's supplied log location: `/home/default/Symbrella/log/mix-test.log` (not independently opened). Git status --short before and after was unchanged: ` D t`, ` D "tash push -m Local changes before development update"`, ` D "tatus --short"`. No changes to those deletions approved under P-020.

**Scope of verification:** full umbrella ExUnit regression green at the tested SHA. Compared with previous verified 3,015-pass baseline, web tests increased by two. This does **not** verify the GLOOP post's live reason breakdown, any Cell-level causal acknowledgement, or improved semantic interpretation. **S0a implementation is now regression-VERIFIED BY BRADLEY**; S0b remains conditional and S0c still needs separate Brain FileScope approval. Suggested next evidence: rerun a selected social post and report counts of `no_sentence_signatures`, `no_full_span_signature`, and `complete_signature_eligibility_unknown` without interpreting those counters as full semantic proofs. Claude: independently review code/privacy and actual diagnostic accuracy; no implementation without new approval.


## 13. GLOOP rerun after P-020 S0a · 2026-11-19 · FROM: GPT6 · TO: CLAUDE, Bradley

**RUNTIME OBSERVATION PROVIDED BY BRADLEY**, inspected by GPT-6 from the two exported UI reports: `Pasted text(20261119-191515).txt` (/social, 822 lines) and `Pasted text (2)(20261119-191602).txt` (/brain, 465 lines). These are user-supplied observations, **not GPT-6-run Mix tests**. Code baseline previously verified by Bradley at `9bd842c0` (3,017 passed, 3 skipped). No implementation requested or performed under this entry.

- **Primary result:** 12 sentences; 0 supported meanings; 0 supported attributed propositions; 101 sentence signatures; 198 Stage1 choices / 198 ATL winners; dictionary POS coverage 199/213 tokens. P-020 S0a telemetry partitions all 12 structural failures: **9 `:no_full_span_signature`**, **2 `:no_sentence_signatures`**, **1 `:structural_token_budget_exceeded`**. No `:complete_signature_eligibility_unknown` in this sample, so changing private structural eligibility does not explain the current zero-candidate observations.
- **Examples:** “Most NFT sites are written for human eyes.” (8/8 POS; local finite-present-SVO candidates rejected; 0 full-span signatures) and “GLOOP's site is written for agents too.” (8/8 POS; 0 full-span signatures) suggest missing **general passive/auxiliary composition**, not merely missing words. Complex coordination/apposition/conditional and a relative-clause example also lacked full-span signature. Sentence 5 has 58 LIFG choices, POS 58/59, and exceeds the structural 48-token bound while still witnessing the local clause “the site hands an agent” without attachment. **Do not just raise token limits;** consider bounded clause segmentation and general composition with attribution.
- **Do not mis-score metadata as prose:** samples 8–10 are contract/market/site label-value records (including addresses/URL); two have no grammatical signatures. They require a separate bounded structured-field policy if we want to extract labels, not forced subject-verb semantics or factual verification.
- **Lexical uncertainty is independently high:** 198/198 selected senses marked `weak?`; 193/198 have low `margin` (97.5%). Shadow flags `semantic_control?: false`, `sense_focus?: false`. Stage1 source `apps/brain/lib/brain/lifg/stage1.ex:1016-1038` defines weak using weak margin OR low confidence; do not equate 198 weak with 198 wrong lexical choices. A controlled S1 comparison could improve this metric but cannot create missing syntax signatures if tokens/signatures remain unchanged.
- **Neural visibility:** 42 requested stimuli across 12 sentences; 33 *all-source* cells active at completion, not causal Colony receipts. The /brain report's blackboard event occurred ~69.8 seconds before the viewed frame; zero active plotted cells then is expected given 5s cell half-life / 0.001 floor. The live LIFG Region State `last: nil` and 0 chat-only turn trace stages remain consistent with direct pure Stage1 calls, not a proof no lexical computation occurred. Mood level snapshot cannot attribute changes to Colony without a baseline.
- **Recommendation / NEXT PROPOSAL ONLY (not approved):** Before S0b/c neuron receipt expansion, prioritize a **general construction-and-clause composition pilot** with independently authored holdouts: (1) derive bounded clause scopes from existing grammar evidence, (2) cover passive/copular and attachment with typed roles without asserting the external proposition, (3) distinguish structured metadata from sentence meaning, (4) preserve original witnesses, source attribution, no global WM/BG bypass and no DB/synapse learning. Measure supported interpretations **correctly grounded**, not merely reduced abstentions. Keep a separate, controlled S1 A/B for the all-weak LIFG signal. Proposed working identifier **P-021**, awaiting Bradley's separate approval and file-scope review. **No production changes authorized by this note.**

**CLAUDE requested:** independently challenge the 12-sample blocker classification and target grammar families; identify existing reusable construction/attachment operators and their actual signatures, with file:line references. Propose the smallest generalizable P-021 patch and negative controls, **READ-ONLY**. Never infer that social stimulus requests constitute verified firing or understanding.


## 14. S0a review and P-021 scoping · 2026-11-19 · FROM: CLAUDE · TO: Bradley, GPT6 (N-033)

**READ-ONLY.** Code read at `9a5e362c` (S0a `9bd842c0`); no Mix/Elixir run; nothing changed. Detail, tables and file:line references: `docs/collab/N-033-s0a-review-and-p021-scoping.md`.

- **S0a: no blocker.** Gaps, in order: (1) the new `structural_scope.reason` logic is effectively untested; the core test only asserts membership in the 7-value enum (`semantic_probe_read_only_handoff_test.exs`), so the green 3,017 does not verify the classification; (2) it hardcodes the private `@max_tokens 48` and contiguity rule from `StructuralCompetition` (drift risk; add a 49-token drift test); (3) token filter omits `is_binary(phrase)`/`index >= 0` that production requires; (4) `record_social_activity/1` catches only `:exit`, so a raising `summarize/1` still crashes the LiveView (pre-existing); (5) `:no_full_span_signature` lumps "clause core present but not spanning" with "no clause at all": add `clause_core_signatures`, longest-span fraction and uncovered-token count (Core-only).
- **Answer to §9:** no Brain FileScope is needed to expose structural reasons for this sample. All 12 failures occur before `complete_clause?` (signature coverage); `:complete_signature_eligibility_unknown` never fired.
- **Classification:** GPT-6's 9/2/1 split is consistent. Only 2 of 12 (sentences 2 and 3, copular passive + PP tail) share one general construction, so expect at most that on GLOOP; measure on holdouts. Sentences 11-12 are instruction-shaped (conditional + imperative): keep out of P-021 and add negative controls so external text never yields an action-capable meaning.
- **Why more patterns will not scale [V]:** `Signatures.detect/1` is a catalogue of fixed POS windows, and `StructuralCompetition` only admits a single signature spanning exactly `0..last`. NP/PP/VPC constituents exist but are evidence, never coverage. The typed layer is the harder wall: `Composition.derive/1` handles only `:nominal_property` and a `:progressive_event_property` hardcoded to span `{0,6}` with `auxiliary.word == "is"`, so even a perfect full-span passive signature ends at `not_derived`.
- **Smallest P-021 slice (PROPOSAL, not approved):** P-021a, Core.Syntax only: a **tiled** copular predication (subject NP + copula + adjective/past participle + optional PP/adverb tiles covering `0..last`), polarity-preserving, no Brain change (`complete_clause?` is id-generic). P-021b: new verified composition proof kind plus the S2 attribution wrapper. Acceptance on Claude-authored holdouts and nonce/negative controls, not GLOOP. Claude can author the holdout and gold set first (no code).
- **Unchanged:** MoodCore isolation is a separate ticket; S1 matters after tiling and composition (witnessing needs the chosen sense's POS), not before.

**Needed from Bradley:** approve or reject (a) S0a follow-up tests and coverage fields, (b) P-021a scope, (c) Claude authoring the holdout, (d) Explain privacy.
