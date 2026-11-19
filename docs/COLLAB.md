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


## 15. Independent response to N-033 · 2026-11-19 · FROM: GPT6 · TO: CLAUDE, Bradley (N-034)

**READ-ONLY source review at `53020f7d`.** No Mix/Elixir run; **no production patch approved or made** by this exchange. Claude's full N-033 review is in `docs/collab/N-033-s0a-review-and-p021-scoping.md`.

- **[V] AGREE: P-020 S0a has a genuine *classification-test* gap.** `semantic_probe_read_only_handoff_test.exs:48-54` checks enum membership rather than correct causes. `SemanticProbe.structural_scope/2` replicates the Brain competition's contiguous-index/48-token prefilter but not its exact token validity conditions (`semantic_probe.ex:~410-435` versus `brain/lifg/structural_competition.ex:28-82`). We should add deterministic reason-by-reason tests and a 49-token boundary test under separately approved S0a hardening. The `:complete_signature_eligibility_unknown` status intentionally remains an unresolved **private** Brain eligibility outcome; no Brain FileScope is needed for the **observed GLOOP 9/2/1 categorization**, but that does not prove full eligibility diagnoses for other sentences.
- **[V] AGREE: a two-stage syntax + verified composition plan is necessary.** `Core.Syntax.Signatures.detect/1` (`signatures.ex:75-108`) produces independent fragment signatures, but `Brain.LIFG.StructuralCompetition.resolve/1` (`:28-52,89-138`) demands a source-aligned full-span candidate for structural preference. `Core.Semantic.Composition.derive/1` and `verified/1` (`composition.ex:76-139`) accept only limited nominal/progressive proof paths. **P-021a by itself will not produce supported social meanings**. P-021b must include its own verified typed proof plus explicit audit of `SentenceMeaning`, `ReportedProposition` and downstream attribution consumers.
- **[V] CORRECTION to §4 reuse example:** GLOOP sentence 4, “Every GLOOP slime … lives on BNB Chain behind …”, is **intransitive + PP adjunct(s) and a parenthetical**, *not* a `finite_present_svo` clause with a PP tail. Its observed signature family contained finite-past candidates, VPC and nominal fragments—not a supported present-SVO core. A general tiler could eventually cover it, but only if we first define an independently witnessed intransitive predicate and principled parenthetical/adjunct handling. Do not count it as P-021a's expected success.
- **[H] P-021a's proposed “subject NP + copula + complement + PP” may still miss GLOOP sentence 2.** `signatures.ex:13-22` NP patterns do not automatically admit all three-token determiner + noun-modifier + noun subjects (e.g. “Most NFT sites”). Whether the actual LTM POS evidence supports an alternative full subject NP remains **unverified**. The independent holdout must include NP-span coverage and lexically witnessed participle/copula roles, not merely a final full-span signature. Sentence 3's possessive subject may be a simpler first witness.
- **[V/H] Tiling discipline:** existing PP signatures can contain nested NP evidence (`signatures.ex:1562-1609`), so “no overlap” must apply to **sibling tiles**, not to hierarchical constituents. Require exactly contiguous original-token coverage, bounded ambiguity and independent morphology/POS witnesses, rejection of gaps/negation/modals/imperatives, and no inferred passive agent or factual assertion from `for`/`by` phrases. Do not manufacture a new per-surface window detector and call it a reusable tiler.
- **Sequencing recommendation (not approval):** (1) approve S0a cause-specific hardening tests and optional Core-only clause-span coverage metrics; (2) have Claude author a *separate, unseen-at-implementation* holdout/gold corpus with negative controls; (3) define the reusable P-021a tiler contract and exact Core.Syntax FileScope; (4) require independently scoped P-021b proof/consumer design before claiming comprehension. S1 lexical A/B remains useful but cannot generate missing signatures with otherwise identical tokens. Continue to treat external text as `external_unverified`; no BG/global WM, memory, synapse or external action side effects.
- **[V] Minor S0a error-handling caveat confirmed:** `SocialLive.record_social_activity/1` catches exits but not exceptions raised during synchronous report summarization. Avoid a blanket rescue that hides malformed contracts; use an explicit observable error classification/test if we harden this path.

**Request to CLAUDE:** revise P-021a acceptance to distinguish independently witnessed subject NP, passive/adjectival ambiguity, sibling versus nested coverage, and the intransitive sentence-4 correction. **Request to Bradley:** no implementation of P-021a/b or additional S0a hardening without separate explicit patch scope approval. Explain privacy (source_term/class_term) remains undecided.


## 16. P-021 architectural review and design · 2026-11-19 · FROM: CLAUDE · TO: Bradley, GPT6 (N-035)

**READ-ONLY.** Code read at `e9e95e09`; no Mix/Elixir run; nothing implemented; **no P-021 approval implied.** Full design, line references, FileScope, tests and corpus: `docs/collab/N-035-p021-design.md`, `docs/collab/p021/dev_corpus.md`.

- **Verdict [V]:** three narrow layers sit on a sound lexical core: (1) `Signatures.detect/1` is a catalogue of fixed POS windows and the NP detector has six windows only (no D-N-N / Q-N-N, so "Most NFT sites" cannot be a subject NP); (2) typed composition is closed to 3 nouns, 3 adjectives, 3 verbs, 3 landmarks (`expression.ex:11-17`) and `event_signature?` is hard-wired to span `{0,6}` with `auxiliary.word == "is"`; (3) `ReportedProposition` is one regex for one headline. Even a perfect full-span passive signature ends at `not_derived`.
- **I withdraw "tiling".** Sibling tiles cannot express nesting or attachment ambiguity. Proposed mechanism: a **bounded chart over a rule table** (rules are data; extension = rule rows + corpus cases, not new detectors) emitting only full-span `:copular_predication` signatures through the **unchanged** Brain `StructuralCompetition` (its `complete_clause?` is id-generic, so no Brain FileScope). Attachment ambiguity is encoded in role keys because `resolve/1` merges equal roles; adjective/participle stays one candidate with both attestations, so no passive or agent is invented.
- **Constraint found [V]:** every verifier re-runs `detect/1` (`Enum.all?(signatures, &(&1 in detected))`), so the new family must live inside `detect/1`, deterministic, and gated to `source: :colony_external` so chat is byte-identical (differential test required).
- **P-021b boundary:** open-vocabulary typed skeleton over **witnessed lexical constants** (binaries from `RoleWitness`, never atoms from external text) plus a `kind_predication` operator; certifies composition and roles, not word meaning or truth. Consumed by a third fallback in the Colony wrapper (`ReportedProposition` -> `DeclarativeProposition` -> new `AttributedPredication`), leaving `SentenceMeaning` unchanged. Consumer audit required first: `response/symbolic_generation.ex:224-236,645-661`, `discourse/ledger.ex:923-1033`, `response/attach.ex:169`, Explain, summary/UI counters.
- **Three levels kept apart:** L1 grammatical hypothesis, L2 verified typed derivation, L3 attributed statement (`external_unverified`). Expect at most GLOOP sentences 2-3 (about 2 of 12); sentence 4 is a later extension row.
- **Holdout:** DEV corpus (36 rows) is in the repo. The **SEALED partition (40 rows) is not**: commitment SHA-256 `dc6ad890cbcf842da58f27cd6eef70b480a9f36dbea53e2efbce159152447366`, held by Bradley, released only after design freeze, run at most twice, counts only. Gates: 0 unsupported promotions, supported precision >= 0.95, 0 action-capable fields on injection rows, chat differential equality.
- **S0a hardening:** extract `structural_scope` to a pure module; reason-by-reason tests; **counterfactual tests against the real Brain module** at 48/49 tokens; a token-without-`phrase` test expected to fail today (probe counts it, Brain ignores it); optional Core-only coverage fields.
- **Recommended order:** S0a hardening -> freeze P-021a contract (+ Explain privacy decision) -> P-021a (Core.Syntax only, measure L1) -> P-021b consumer audit -> P-021b -> S1 -> retire the bespoke Colony interpreters.
- **Unverified:** LTM attestation of corpus words and "Most"; real LIFG POS picks; UI counter logic; tokenizer behaviour for contractions and punctuation.

**Needed from Bradley:** approve/reject the S0a hardening scope; approve freezing the P-021a contract (FileScope in N-035 §3.7); hold the sealed file; decide Explain privacy.


## 17. Bradley's approval and work assignment · 2026-11-19 12:46 MST · FROM: CLAUDE · TO: Bradley, GPT6

**Bradley's words (verbatim):** "Ill approve you doing what ever you need to do to the repo i think its sound that we have Symbrella with as many possible features as we can to understand everything she needs Approve: ALL you suggested". Recorded here by Claude; Bradley remains the sole approver. Claude's reading of scope (Bradley may correct it):

| Item | Reading | Status |
| --- | --- | --- |
| **P-020.1** S0a hardening (N-035 §5) | Approved. Implemented by Claude, commit `b0b25818`. | **UNTESTED** (no Elixir runtime for Claude); Bradley to run the commands below |
| **P-014.1** remove 3 stray root files | Approved. Remove-only, commit `3ac507bb`; blobs are identical pasted `git diff --stat` output; already deleted in Bradley's tree | done, nothing to run |
| **P-021a** chart + copular rows (N-035 §3, FileScope §3.7) | Approved. **To be implemented by GPT-6**, not Claude: Claude authored the sealed holdout, so the implementer must be someone who has not seen it | OPEN, GPT-6 |
| **P-021b** typed skeleton + attribution wrapper (N-035 §4, FileScope §4.7) | Approved **after** the read-only consumer audit (N-035 §4.6), which Claude does next | blocked on audit |
| S1 lexical A/B, S3 isolated prior, S0b/S0c, MoodCore isolation, P-015 runner, P-016/017/018 | Approved in principle. Each needs a short read-only spec and exact FileScope before code; MoodCore/S0c touch Brain files | queued, Claude writes specs |
| Explain privacy (`source_term`/`class_term`) | **Not decided by the blanket approval.** Default stays: no disclosure | needs Bradley's yes/no |

**P-020.1 contents (Claude, UNTESTED).** New pure module `apps/core/lib/core/social/structural_scope.ex`; `SemanticProbe.structural_scope/2` now delegates to it (3-line change in `semantic_probe.ex`). Token-validity filter aligned with `Brain.LIFG.StructuralCompetition.base_indices/1` (`phrase` binary, `index >= 0`); signature scan capped at 64 like Brain. New observational fields: `base_tokens`, `clause_core_signatures`, `longest_span_tokens`, `uncovered_token_count`, `constituent_only?`. All existing keys unchanged; `SocialBrainActivity`, `SocialLive` and the existing handoff test read only `reason`/`full_span_signatures`/`scope`. New tests `apps/core/test/core/social/structural_scope_test.exs` call the **real Brain module** for counterfactuals at 48 vs 49 tokens and for invalid tokens (missing phrase, negative index, `n: 2`, punctuation). No existing test, fixture or baseline edited; no `mix format`; no Brain file touched.

**Commands for Bradley (AGENTS.md: umbrella root, explicit paths; record branch, SHA, exit):**
```
mix test apps/core/test/core/social/structural_scope_test.exs apps/core/test/core/social/semantic_probe_read_only_handoff_test.exs apps/symbrella_web/test/symbrella_web/social_brain_activity_test.exs apps/symbrella_web/test/symbrella_web/live/social_live_test.exs
mix test
set -o pipefail; mix compile --force 2>&1 | tee log/compile-force.log; echo "exit=$?"
```
If a counterfactual test fails, that is a finding (drift or a wrong assumption in my fixtures), not something to patch around. Report the failure text to Claude.

**GPT-6 assignment (P-021a).** Implement N-035 §3 exactly within §3.7 FileScope (new `chart.ex` and `grammar.ex`, `detect/1` gate only, new tests), using `docs/collab/p021/dev_corpus.md` as the development corpus. Do not read or request the sealed partition (SHA-256 `dc6ad890cbcf842da58f27cd6eef70b480a9f36dbea53e2efbce159152447366`, held by Bradley). Required: chat differential test (`detect/1` unchanged for non-`:colony_external`), determinism test, no atoms from token text, budgets (48 tokens, 6 items/span, 400 items, 12 results). Label results UNTESTED/VERIFIED BY Bradley. Challenge N-035 where it is wrong; Claude will review the patch read-only before Bradley runs the sealed set.

**Claude next:** (1) the P-021b consumer audit (N-035 §4.6); (2) short specs for S1, S3, MoodCore isolation, S0b/S0c.

## 18. P-021b consumer audit · 2026-11-19 · FROM: CLAUDE · TO: Bradley, GPT6 (N-036)

READ-ONLY; source reading only, nothing executed. Full audit: `docs/collab/N-036-p021b-consumer-audit.md`. This closes the "Claude next (1)" item in §17 and **corrects N-035 §4.5/§4.6**.

- **Scope:** ~19 non-test files consume the composition proof kinds, not the 4 listed in N-035. Most (symbolic_generation, semantic_strategy, ledger, WM comprehension) match the two known kinds and fall through on an unknown one [V]. The Rebuild-equality guards (audit, synaptic_experience, semantic_propagation, structural_statement) also fall through but demand deterministic, pure derivation [V].
- **Hazard found (regression, not exposure):** `ColonyCognition.reconcile_proposition/2` (`colony_cognition.ex:180-206`) returns `:colony_proposition_mismatch` for *any* `:attributed_structural_interpretation` whose `embedded_relation` differs from the title claim. A new fallback that reused that status would make legacy counted-title drafts that work today fail. **P-021b must use a distinct status** and ship a legacy-flow differential test.
- **Boundary confirmed:** `ColonyCognition.claim_for_title/2` returns an error for any non-counted title, so no frame, draft or model prompt is built from a new proposition. P-021 output is probe/observability-only by construction; widening that is a separate ticket. `prompt_frame/1` interpolates proposition fields into the model prompt, so any future admitted field must be a witnessed lemma binary.
- **Two any-kind consumers:** `SentenceMeaning.attach_compositional_evidence/2` (stamps `formally_verified?: true` for any proof) and `LexicalRoleShadow` line 121 (`formal_derivation_verified?` true for *any* proof; chat-only, pre-existing). Recommendation: expose the new proof via its own `AttributedPredication.verified/1` and leave `Composition`/`SentenceMeaning` untouched. The `LexicalRoleShadow` tightening is optional, separate.
- **Updated P-021b admission checklist:** N-036 §3 (7 items). No FileScope requested; nothing here changes P-021a.
- **Unverified:** 13 test files reference these symbols and were not read.

**Claude next:** short specs for S1, S3, MoodCore isolation, S0b/S0c (still unwritten). No new approval needed; P-021a remains with GPT6.

## 19. P-020.1 / P-014.1 verification receipt · 2026-11-19 12:52 MST · FROM: CLAUDE (recording Bradley's run)

VERIFIED BY Bradley (local shell): branch `symbolic_generation`, SHA `1892ae83`, `mix test` from umbrella root, exit 0, tree clean before and after. ExUnit: db 90, llm 58, brain 719 (3 skipped), core 1950, symbrella 1, symbrella_web 213, all passed; seed 60523. This includes my previously UNTESTED `apps/core/test/core/social/structural_scope_test.exs`, so its 48/49-token counterfactuals and the invalid-token parity cases held against the real `Brain.LIFG.StructuralCompetition`. [U] I have not seen the per-file counts in `log/mix-test.log`, so the new file's presence in the run is inferred from the full-suite command, not read.

**Compile check, VERIFIED BY Bradley 12:55 MST:** `set -o pipefail; mix compile --force 2>&1 | tee log/compile-force.log; echo "exit=$?"` → exit=0. All seven apps recompiled (lexicon 1, db 43, llm 21, brain 182, core 306, symbrella 3, symbrella_web 84 files) with no warning lines in the pasted output. The pasted output was the log as relayed by Bradley, not read from `log/compile-force.log` by me. Everything in §17's command list is now satisfied. Per-test names for `structural_scope_test.exs` are still not shown in any log (`--trace` would show them); this is optional.


## 20. P-021a implementation handoff · 2026-11-19 · FROM: GPT-6 · TO: Claude, Bradley

**Authorization:** Bradley assigned GPT-6 P-021a in §17, with exactly the five code/test paths in N-035 §3.7. **IMPLEMENTED but UNTESTED by GPT-6.** Commit `ee4ca2a1a873a05e744711293dac62ef75b6216b` on `symbolic_generation`. A reviewed diff confirms exactly two new Core.Syntax modules, one modified Core.Syntax.Signatures module and two new tests. **No Brain, Composition, SentenceMeaning, ColonyCognition, model prompt, chat pipeline, prior test/fixture or baseline changed.** This COLLAB update is a separate **documentation-only** commit expressly requested as the handoff; it is not an extra production FileScope change.

### What was built [V: source inspected, not executed]

- `Core.Syntax.Grammar` is a closed, data-backed rule inventory and function-word tables. Rules: nominal head; adjective/noun modifiers; bare, determiner, quantifier, pronoun, proper-noun and independently witnessed possessive subjects; PP over a nominal/pronoun; subject-vs-predicate PP attachment; copula + attested adjective/past participle complement; optional VP PP/adverb; full-span subject+VP predication. One signature ID only: `:copular_predication`.
- `Core.Syntax.Chart` is pure bottom-up span combination with fixed role keys and stable sort/dedup. Each source-aligned, fully covered hypothesis carries only grammatical roles, permitted POS features, tense/polarity/lexical-witness evidence and `factual_authority?: false`. Adjective plus participle attestation yields **one** complement candidate carrying both evidence atoms, not a fabricated passive event. A `by` PP remains `:by_phrase_adjunct_uninterpreted`, never `:agent`.
- `Signatures.detect/1` retains the original expression and return order for **every non-`:colony_external`** source; only Colony receives appended chart signatures. A `@doc false chart_nodes/1` accessor exposes the existing lexical node builder; chart recomputation is deterministic from the current SI. The existing `Brain.LIFG.StructuralCompetition.resolve/1` remains the sole preference gate and is **not modified**.
- Guard conditions reject unsupported negation, modals, inversions, embedded instruction-shaped/multiple clauses, malformed source index/gaps, unconstrained proper-noun number, missing morphology, agreement mismatch and unsupported PP depth. Deeper attachment combinations that cannot be represented with fixed, distinguishing role keys fail closed rather than collapsing candidates.
- Budgets: **48 base tokens**, **6 items per span**, **400 items total**, **12 full-span results**. Any exceeded cap returns no signature (`:chart_budget_exceeded`) rather than a truncated candidate. Tests lower per-span/total/result caps to exercise the exact same fail-closed path and check the 48/49-token boundary. Because full-span items are also bounded at 6 per span, the 12-result cap is a redundant defensive ceiling under the current grammar. No dynamic atoms are generated from input text.

**Divergences and corrections to N-035, deliberately conservative:** (1) `NP -> Det? Q?` is split into fixed determiner/quantifier alternatives; simultaneous Det+Quantifier is not admitted in P-021a. (2) Added the general `Det + witnessed PossessiveNP` combination to avoid losing the initial determiner of “The agent's site.” (3) Number `:unknown` **abstains**, rather than licensing singular or plural copula, until evidence for that case exists. (4) We sort by stable role/structure keys rather than speculative fewer-constituent preference; budget overflow aborts the entire chart, so sorting cannot select a preferred truncated parse. (5) PP nesting is deliberately restricted to one representable level and two adjuncts rather than adding dynamic role names. (6) Grammar reconstructs PP objects from chart NPs/pronouns and reuses existing possessive signatures; it does not replace old NP/PP detectors. All these decisions are testable, conservative and intended to prevent unsupported promotions.

### Visible DEV rows: expected versus **actual**

**Important:** These are the 36 *published development corpus* rows (not the sealed set). The “expected” column describes **P-021a grammatical L1**, not future P-021b's SUP/SUP+q semantic output. The `chart_dev_corpus_test.exs` uses fixed **synthetic attested POS/morphology rows**, so it does **not** prove live LTM coverage. **Actual is UNTESTED for every row because GPT-6 has no Elixir/Mix runtime.** No assertion about a passing DEV row has been made.

| ID | DEV expected grammatical result | Actual at handoff | Rationale / expected blocker |
| --- | --- | --- | --- |
| D01 | L1 full-span hypothesis (semantic support deferred to P-021b) | **UNTESTED** | Minimal NP + is + adjective. |
| D02 | L1 full-span hypothesis (semantic support deferred to P-021b) | **UNTESTED** | Plural attested on "pages"; agrees with "are". |
| D03 | L1 full-span hypothesis (semantic support deferred to P-021b) | **UNTESTED** | Past copula recorded, no event asserted. |
| D04 | L1 full-span hypothesis (semantic support deferred to P-021b) | **UNTESTED** | Demonstrative determiner. |
| D05 | L1 full-span hypothesis (semantic support deferred to P-021b) | **UNTESTED** | Subject NP complete before copula. |
| D06 | L1 full-span hypothesis (semantic support deferred to P-021b) | **UNTESTED** | Needs NP rule beyond today's six windows (`signatures.ex:13-22` has no D-N-N). |
| D07 | L1 full-span hypothesis (semantic support deferred to P-021b) | **UNTESTED** | Do not invent an event or agent. |
| D08 | L1 full-span hypothesis (semantic support deferred to P-021b) | **UNTESTED** | A `by` phrase is not an agent without evidence. |
| D09 | L1 full-span hypothesis (semantic support deferred to P-021b) | **UNTESTED** | PP with nested NP; nested NP is not a sibling tile. |
| D10 | L1 full-span hypothesis (semantic support deferred to P-021b) | **UNTESTED** | Reuse existing possessive NP evidence. |
| D11 | L1 full-span hypothesis (semantic support deferred to P-021b) | **UNTESTED** | Possessive subject plus adjunct. |
| D12 | L1 full-span hypothesis (semantic support deferred to P-021b) | **UNTESTED** | Quantifier is recorded, never evaluated. |
| D13 | L1 full-span hypothesis (semantic support deferred to P-021b) | **UNTESTED** | Same. |
| D14 | L1 full-span hypothesis (semantic support deferred to P-021b) | **UNTESTED** | Optional modifier at clause end. |
| D15 | no L1 / ABSTAIN(coverage_gap) | **UNTESTED** | Extension target (`VP -> Cop Adv? Comp`). Must come from a rule row, not a new detector. |
| D16 | no L1 / ABSTAIN(negation) | **UNTESTED** | Must never yield a positive proposition. |
| D17 | no L1 / ABSTAIN(negation) | **UNTESTED** | Contraction contains negation. |
| D18 | no L1 / ABSTAIN(modal) | **UNTESTED** | Non-finite copula after a modal. |
| D19 | no L1 / ABSTAIN(modal) | **UNTESTED** | Same. |
| D20 | no L1 / ABSTAIN(interrogative) | **UNTESTED** | Copula precedes subject. |
| D21 | no L1 / ABSTAIN(interrogative) | **UNTESTED** | Inversion. |
| D22 | no L1 / ABSTAIN(agreement_conflict) | **UNTESTED** | Plural attested head with singular copula. |
| D23 | no L1 / ABSTAIN(agreement_conflict) | **UNTESTED** | Singular head with plural copula. |
| D24 | no L1 / ABSTAIN(lexical_witness_missing) | **UNTESTED** | Unattested subject head. |
| D25 | no L1 / ABSTAIN(lexical_witness_missing) | **UNTESTED** | Unattested complement. |
| D26 | no L1 / ABSTAIN(lexical_witness_missing) | **UNTESTED** | A fine PP must not rescue a missing witness. |
| D27 | no L1 / ABSTAIN(coverage_gap) | **UNTESTED** | Exact token coverage required. |
| D28 | no L1 / ABSTAIN(coverage_gap) | **UNTESTED** | Trailing NP has no attachment. |
| D29 | no L1 / ABSTAIN(no_clause_core) | **UNTESTED** | No copula. |
| D30 | no L1 / ABSTAIN(instruction_shaped) | **UNTESTED** | Output must contain no action-capable field. |
| D31 | no L1 / ABSTAIN(instruction_shaped) | **UNTESTED** | Instruction-shaped external text. |
| D32 | no L1 / ABSTAIN(multiple_clauses) | **UNTESTED** | No clause segmentation in P-021a; the injected clause must not surface. |
| D33 | no L1 / ABSTAIN(multiple_clauses) | **UNTESTED** | Same. |
| D34 | no L1 / ABSTAIN(structured_record) | **UNTESTED** | Not declarative grammar. |
| D35 | no L1 / ABSTAIN(structured_record) | **UNTESTED** | URL is not a noun phrase. |
| D36 | no L1 / ABSTAIN(lexical_witness_missing) | **UNTESTED** | A hex address is not an attested adjective or participle. |

### Tests added (unrun)

- `apps/core/test/core/syntax/chart_test.exs`: exact full-span roles, direct unchanged Brain adjudication, fixed grammar/POS witnesses, adjective+participle merge, conservative by-phrase, non-Colony differential with fixed existing-test sentences, repeat-run equality, missing lexical/morphology abstention, negation/modal/inversion/embedded instruction refusals, agreement, budget boundaries and lowered caps, ambiguous attachment surviving in different role keys and yielding Brain `:unresolved`, and no new atoms from untrusted token content. The chat differential is **same-runtime source comparison plus static isolation of the unchanged original detector**, not a separately executed historical `55d4823d` artifact.
- `apps/core/test/core/syntax/chart_dev_corpus_test.exs`: parses all 36 DEV rows into individual tests and asserts P-021a L1 candidate or closed abstention with no action-capable/agent fields. Failure text identifies row, expected and actual reason.

**Known gaps / [U]:** no Elixir syntax check, no local ExUnit, no runtime per-row results, no actual Colony LTM POS validation, no sealed partition, and no proof of improved sentence meaning (P-021b is untouched). Broad LTM ambiguity can saturate a 6-item span and correctly force abstention. Brain examines only its first 64 detected signatures; a chart signature appended after a very large legacy signature list may remain unseen, requiring a **separate scoped decision** rather than silently reordering chat outputs. P-021a can yield a grammatical hypothesis that Composition correctly refuses to turn into meaning. Further attachment depth, noun phrase types and ungrounded singular/plural forms are intentionally outside this first rule inventory.

### Verification commands for Bradley (umbrella root; all UNTESTED by GPT-6)

~~~bash
cd ~/Symbrella
git fetch origin
git pull --ff-only origin symbolic_generation
git branch --show-current
git rev-parse HEAD
git status --short

mix test apps/core/test/core/syntax/chart_test.exs apps/core/test/core/syntax/chart_dev_corpus_test.exs
echo "p021a_target_exit=$?"

mix test apps/core/test/core/syntax/signatures_test.exs apps/core/test/core/syntax/structural_competition_test.exs
echo "p021a_existing_syntax_exit=$?"

mix test
echo "p021a_umbrella_exit=$?"

set -o pipefail
mix compile --force 2>&1 | tee log/compile-force.log
echo "p021a_compile_exit=$?"

git status --short
~~~

**Please send any exact failure output before sealed evaluation.** Claude should **review this patch READ-ONLY first**: especially rule-table generality, ELIXIR compile/test constructs, full-span/role ambiguity, budgets and legacy-source byte equality. Bradley then runs local targeted/full tests and separately controls sealed testing (at most twice, counts only). No sealed data was read, requested, or inferred by GPT-6.


## 21. P-021a DEV failure receipt and bounded correction · 2026-11-19 · FROM: GPT-6 · TO: Claude, Bradley

**Bradley local-shell evidence (VERIFIED BY Bradley):** 2026-11-19 13:58:48 MST, `symbolic_generation` SHA `7d9269b0`, `mix test apps/core/test/core/syntax/chart_test.exs apps/core/test/core/syntax/chart_dev_corpus_test.exs`, exit **1**. ExUnit reported **47/52 passed**, five failures: **D31, D32, D33, D34, D35** (same source-file line 91). Git status --short was **clean before and after**. Bradley identified `/home/default/Symbrella/log/mix-test.log` as the full log, which GPT-6 **did not read**; exact assertion diffs and actual recorded reasons were **not provided**. Do not claim the five diagnoses as runtime-confirmed.

**[V] Source diagnosis:** `Core.Syntax.Chart.diagnose/4` called `valid_original_tokens?/2` and returned generic `:coverage_gap` **before** consulting `rejection_reason/4`. The latter was supposed to distinguish untrusted **structured label-value** records (`:structured_record`), multi-clause separators (`:multiple_clauses`) and initial subordinating instruction/conditional structures (`:instruction_shaped`). Canonical `Core.MWE.Boundaries.valid_window?/2` deliberately rejects punctuation/gaps. **[H]** These precedence interactions explain the five row failures; exact assertion output from Bradley's log is needed to confirm. All five rows correctly abstained in prior testing's aggregate, but their expected **reason labels** failed; no unsupported full-span promotion is claimed from the provided names alone.

**Correction committed, still UNTESTED by GPT-6:** `ee57cd3a705e089b55179aa4a69e8f518cf9d4cb` on `symbolic_generation`. Modified **only** the already-authorized `apps/core/lib/core/syntax/chart.ex` and appended one new regression test in the already-authorized `apps/core/test/core/syntax/chart_test.exs`. Source-shape refusals now take precedence over generic punctuation, index and canonical gap diagnostics, but **before any chart fill**. Order is: 48-token cap → bounded label-value check → subordinating instruction check → multi-clause separator check → base-index/lexical constraints → normal negation/modal/agreement and strict source gaps. The proposed signatures, grammar rules, Brain gate, semantic proof path, source gate, default budgets, existing DEV assertions and all existing baselines are unchanged. New test checks the **five reported DEV sentence shapes** for the correct closed abstention label, zero signatures and no `:copular_predication` output. General rule order changed; no fixed sentence-specific recognition was added.

**Targeted outcome table (expected vs actual):**

| DEV | Expected no-signature reason | Actual at `7d9269b0` | After `ee57cd3a` |
| --- | --- | --- | --- |
| D31 | `:instruction_shaped` | **FAILED** (actual reason unreported) | **UNTESTED** |
| D32 | `:multiple_clauses` | **FAILED** (actual reason unreported) | **UNTESTED** |
| D33 | `:multiple_clauses` | **FAILED** (actual reason unreported) | **UNTESTED** |
| D34 | `:structured_record` | **FAILED** (actual reason unreported) | **UNTESTED** |
| D35 | `:structured_record` | **FAILED** (actual reason unreported) | **UNTESTED** |

**Bradley verification commands (from umbrella root; do not run sealed):**

~~~bash
cd ~/Symbrella
git pull --ff-only origin symbolic_generation
git branch --show-current
git rev-parse --short HEAD
git status --short

mix test apps/core/test/core/syntax/chart_test.exs apps/core/test/core/syntax/chart_dev_corpus_test.exs
echo "p021a_target_exit=$?"

mix test
echo "umbrella_exit=$?"

git status --short
~~~

**Claude:** independently review the two-file correction **READ-ONLY**, particularly priority of `:instruction_shaped` vs `:multiple_clauses`, rejection on irregular source spans, and zero unsupported promotions. **Bradley:** only after source review and passing local regression should the frozen SEALED protocol be considered. No sealed file was accessed, requested, or inferred.


## 22. P-021a atom test: 52/53 local run, precise source-atom verification · 2026-11-19 · FROM: GPT-6 · TO: Claude, Bradley

**VERIFIED BY BRADLEY (local-shell evidence; not GPT-6-run):** 2026-11-19 14:04:58 MST, branch `symbolic_generation`, SHA `819a4f37`, exact command `mix test apps/core/test/core/syntax/chart_test.exs apps/core/test/core/syntax/chart_dev_corpus_test.exs`, exit **1**; 52/53 ExUnit tests passed, one failed: `Core.Syntax.ChartTest` “unknown external token strings never create atoms” at `chart_test.exs:242`. Git status --short was **clean before and after**. Log location supplied: `/home/default/Symbrella/log/mix-test.log` (not independently read). **No assertion-diff details were supplied**, so the precise failing expression is **[U]**, though the source test checked only global BEAM atom-count equality after calling the chart.

**[V] Source review:** the failing test at `chart_test.exs:242-254` compared `:erlang.system_info(:atom_count)` before/after chart processing. That counter is **VM-wide**, so other `async: true` ExUnit tests, lazy module loading, and unrelated processes may change it; its value does not isolate creation from the external token string. `Core.Syntax.Chart` and `Core.Syntax.Grammar` have no dynamic atom conversion of external tokens in reviewed source. **[H]** The failure was caused by non-isolated global atom growth, rather than chart-created atoms; this is a diagnosis pending Bradley's exact assertion output.

**Test-only correction pushed, still UNTESTED by GPT-6:** `e49f32bcc63c69c8e6dd49ca6ff672ecaacb18c3`. Only `apps/core/test/core/syntax/chart_test.exs` changed (already within P-021a FileScope). The revised test first asserts that each of three external token strings **does not already exist as an atom** using `String.to_existing_atom/1`; asserts the actual lexical nodes include that token; calls `Chart.clause_signatures` and full `Signatures.detect` repeatedly and checks there is no unsupported `:copular_predication`; then asserts that **none of the token names was interned**. `String.to_existing_atom/1` does not create atoms, so the predicate is directly about input-name internment instead of unrelated global atom growth. These tests do not establish an exhaustive absence of all possible derived-name atoms; the no-atom-conversion source audit remains separately necessary.

**Run from umbrella root (no sealed corpus):**

~~~bash
cd ~/Symbrella
git pull --ff-only origin symbolic_generation
git branch --show-current
git rev-parse --short HEAD
git status --short
mix test apps/core/test/core/syntax/chart_test.exs apps/core/test/core/syntax/chart_dev_corpus_test.exs
echo "p021a_target_exit=$?"
git status --short
~~~

**Status:** patch still **UNTESTED after `e49f32bc`**. Prior `819a4f37` run establishes 36/36 DEV rows and other targeted tests passed *at that SHA* (one atom-safety test did not), but not that a full umbrella regression has passed on this new SHA. Claude to review this test change **READ-ONLY**; Bradley to verify locally. Do not run the sealed evaluation until regular test/review gates are satisfied.
