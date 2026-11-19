# COLLAB.md — Symbrella: one cognitive architecture, multiple input sources

**Owner and sole approver:** Bradley  
**Collaborators:** GPT-6 (architecture, implementation on approval, independent review) and Claude (independent architecture/review)  
**Working branch:** `symbolic_generation`  
**Direction set:** 2026-11-19  
**Status:** Shared project objective and coordination plan. **Not an implementation approval.**

> **Bradley's intent:** Symbrella must be able to attempt to understand language she encounters outside direct conversation—starting with Colony's `/social` route—by using her actual cognitive architecture. We are not building a separate, weaker “social analyzer.” The origin of input affects how much it can be trusted, remembered, learned or acted upon; it must not prevent genuine cognitive processing.

## 1. What we are trying to achieve

1. **One brain, many ways of receiving language.** Chat, social posts, and future sensory inputs should share the relevant LIFG, ATL, semantic-control, syntax, composition, uncertainty and discourse machinery wherever it is safe and technically meaningful.
2. **Neural participation, not cosmetic stimulation.** Neuron requests issued *after* a lexical decision do not demonstrate that neuron state influenced the decision. If neural state is supposed to contribute to interpretation, measure where it enters computation, how it changes competing candidates, and whether the effect is justified.
3. **A sincere attempt at comprehension, even when the answer is unresolved.** Every eligible input should receive a bounded and explainable attempt to parse roles, relations, clauses and context. Lack of support should lead to a precise abstention, not fabricated meaning.
4. **Understanding is not believing.** “The author says X” may be a source-attributed interpretation without making X a verified real-world fact. External commands are text to inspect, not instructions for Symbrella to execute.
5. **A post is more than isolated sentences.** Independent sentence diagnostics are useful, but later sentences may need earlier *supported* context. We need conservative discourse reasoning rather than treating word overlap as evidence of reference, agreement or causality.
6. **Processing, state updates and learning are separate questions.** Temporary processing may be permitted without admitting external assertions to global working memory, durable episodic memory, dictionary knowledge or synaptic learning.
7. **What happened must be visible and testable.** Tell apart “not called,” “ran but abstained,” “rejected by a gate,” “activation requested,” “cell acknowledged,” “activation observed,” and “activation expired.”

**Not the goal:** light up every named brain region for appearance, raise excitation just to improve a graph, indiscriminately run the live chat pipeline against untrusted content, or add endless hardcoded detectors. We want functional integration that improves evidence-grounded comprehension.

## 2. Source and safety boundaries

For all inputs, distinguish four stages:

| Stage | Meaning | `/social` requirement |
| --- | --- | --- |
| **Observation** | Identify the text, author, source and relevant scope | Untrusted external input; track provenance |
| **Cognitive computation** | Candidate senses, neural/contextual priors, grammar, compositional and discourse reasoning | Reuse existing substantive cognition with bounded isolated state where required |
| **Epistemic interpretation** | What may have been said, with confidence, limitations and unresolved structure | Explicit speaker attribution and `external_unverified`; do not promote claims to truth |
| **Admission and learning** | What may alter global state, memories, synapses, actions or verified beliefs | Separate authorization, existing proof/source checks and canonical BG boundary; no implicit writes |

“Read-only” should not be a vague label. The current social path avoids intended durable writes, **but** can stimulate transient Brain.Cells and indirectly update the global MoodCore. We must audit and decide those effects explicitly instead of assuming isolation.

## 3. Why we changed direction

### Observed in Bradley's GLOOP post

- **12** sentences analyzed; **0** supported whole-sentence meanings and **0** supported attributed propositions.
- LIFG/ATL produced lexical evidence and grammar signatures. At least one local SVO clause was witnessed, but full clause attachment or interpretation was not established.
- **12/12** sentences reported `stimuli_issued`; **42** stimulation targets were requested. A snapshot recorded **33 active neurons across all sources** at completion. Neither value proves Colony-specific delivery.
- The later `/brain` chart showed no active report and the live LIFG region summary appeared idle. This is not evidence that the pure Stage1 computations never ran.

### READ-ONLY source findings; not runtime verification

- `Core.Social.SemanticProbe` uses LTM, `Brain.LIFG.Stage1.run/2`, pure `Brain.ATL.reduce`, structural competition, `Composition.derive`, and `SentenceMeaning.build`. The shadow scorer disables semantic control, sense focus and some reanalysis/MWE paths, and **does not** call `Core.Brain.Activation.prime/2`.
- `Core.Social.BrainObservation` sends bounded post-comprehension stimulation requests, currently up to four selected targets per sentence. Returning `:ok` from asynchronous casts is not acknowledgment that a Brain.Cell handled the request.
- `BrainLive` samples activation every 500 ms while connected. Its 60-second history does not reconstruct earlier unseen stimulation; a normal weak 0.015 pulse with a 5-second half-life falls below the 0.001 resting floor after roughly 19.5 seconds without replenishment.
- `SocialBrainActivity.record/1` collects a completion-time **all-source** count, not a causal Colony receipt; `SocialLive` currently ignores its return value.
- The social route does not feed live neuron state back as a lexical prior. Social activation can indirectly affect shared mood: `Brain.Cell` reports to the coordinator, which calls `MoodHooks.register_activation`; `MoodCore` can adjust NE, glutamate and dopamine.
- The user/prod/test participant-event path is source-gated; the existing separate social declarative interpreter covers only narrow constructions. Independent structural competition failed on the GLOOP samples. We have **not** proven that missing neural priors caused those interpretation failures.

Do not turn “possible cause” into “verified diagnosis” without a distinguishing test. Exact evidence and previous debate are preserved in Git history (especially old entries N-027 and N-028).

## 4. Target: shared computation with isolated context

```text
 CHAT       SOCIAL POST       FUTURE SENSORY INPUT
   \            |                      /
    -> source / author / scope / trust <-
                       |
             bounded input context
                       |
          lexical evidence + LTM lookup
                       |
          LIFG semantic control / competition
               ^                  |
               |            ATL integration
       grounded neural or          |
       contextual feedback         v
               |         grammar / typed composition
               \                  /
                  uncertainty + conflict
                           |
                attributed interpretation
                  /        |        \
              supported  partial  unresolved
                  \        |        /
                  epistemic / admission gates
                           |
           no unapproved memories, learning,
           global WM admission or external actions
```

This diagram is a **design target**, not a claim that the whole path already exists.

Architectural requirements:

- Prefer existing pure functions first, followed by isolated state only when it has a documented computational role.
- Keep attention and contextual information **source-bound**; never copy an external post into canonical global WM merely to make it easier to parse.
- Make sentence and cross-sentence interpretations typed, with witnessed spans, roles, relationships, tense, negation, modality, source identity and uncertainty only where actually supported.
- Distinguish syntax recognized, local clause witnessed, whole-sentence meaning supported, author's claim attributed and claim independently verified.
- Any inability to compose should expose actual blockers (POS mismatch, missing construction, unresolved attachment, ambiguity or independent gates), not just a generic “not understood.”
- Preserve all existing Basal Ganglia, security and proof/admission gates.

## 5. Development priorities — **proposed, not approved**

### S0 / P-020 — establish a trustworthy cognitive/neuronal receipt

**Status: PROPOSED. No production edits authorized.** Instrument what already executes, rather than making the graph brighter.

- Record the actual invoked computation stages, LIFG options, low-margin/weak rates, structural block reasons, and source-gated abstentions in a privacy-bounded form.
- Distinguish private **requested IDs** from cast-enqueued, cell-handled/acknowledged, exact-ID activation-snapshot observed, and expired/unconfirmed results. Never attribute an all-source snapshot to Colony without a correlation receipt.
- Surface “last Colony stimulation: requested N, confirmed N, peak P, timestamp” on `/brain` after chart decay, with precise attribution and finite in-memory retention. Fix misleading no-current-report wording; do not alter chart amplitude scaling or activation deltas.
- Trace and expose missing-service/recording failures rather than treating every `:ok` as proof of delivery. Audit MoodCore effects; do not silently change them as part of an observation-only patch.
- Deterministic tests: known-ID receipt and missing-ID/disabled-Brain gates, telemetry privacy/visibility, exact-target snapshot, no unauthorized database/WM/synaptic writes. Avoid test success caused by unrelated activations.
- A causal cell-level acknowledgment may require an expanded Brain file scope; Brad must approve the precise final scope separately.

### S1 — compare normal cognition and the social shadow

Use controlled A/B trials of existing semantic control, sense focus and safe lexical reanalysis in an isolated probe. Keep production behavior unchanged by default. Track lexical weak rates, POS conflicts, structural coverage, unsupported promotions and evidence-correct sentence interpretations, with independent holdout examples. A higher lexical score is not proof of understanding.

### S2 — reusable attributed comprehension

Audit `SentenceMeaning`, semantic composition, source/participant gates and consumers before proposing an attribution-preserving path for external content. Avoid new topic-specific recognizers. Start with general, evidence-witnessed constructions; then bounded multi-sentence references and context. Never let interpreting an external declarative assert a fact into the user conversation or knowledge base.

### S3 — neural feedback integrated into the attempt

Test whether `Core.Brain.Activation.prime/2` and the existing exact-ID neuron snapshots can supply temporary per-analysis priors (e.g., `neuron_probe_snapshot:` with `notify_brain?: false`). Demonstrate an effect on justified candidate selection and/or semantic interpretation. Audit global Brain/MoodCore side effects, and require a separate design for any stronger isolation or state-policy change.

**P-016/P-017:** typed, read-only `Core.Discourse.Claims` envelope, with per-kind coverage, provenance, two genuine consumers and privacy constraints. It is dependent on useful interpretations—not a substitute for them. **P-018:** extend existing Explain/CognitiveTrace/StateDump UI where needed, last.

**Ordering:** Observability is a measurement tool, not the project goal. Continue with the smallest change that proves better cognitive participation, using S0 evidence to prioritize. Claude and GPT-6 must challenge the ordering when measured results justify a different one.

## 6. Definition of success

We should be able to show that:

1. The social post underwent a real, bounded attempt using the appropriate shared cognitive mechanisms.
2. Observable neuron receipts are accurate about requests versus processing and expiration.
3. At least one neural/contextual mechanism proposed as cognitive actually influences a demonstrable interpretation outcome—not merely a graph.
4. General syntactic and semantic constructions yield source-attributed results without factual promotion; unresolved inputs remain explicitly unresolved.
5. Supported earlier context can inform later sentences without invented coherence.
6. External text cannot silently alter durable memory, dictionary knowledge, synapses, global WM, verified beliefs or external actions.
7. Improvements generalize beyond a single GLOOP post, through independently authored holdouts, negative controls and nonce substitutions.

## 7. Approval, evidence and collaboration

- Bradley is the **only implementation approver**. Code changes require the explicit token `Approve: P-###` (optional FileScope); see `SYMBRELLA_PROJECT_GUARDRAILS.md` and `AGENTS.md`. A new mission statement authorizes no code changes.
- Claude performs independent READ-ONLY source review, critical comparisons and proposed falsification tests. GPT-6 independently checks those findings and may implement **only an approved, scoped** patch. Neither AI's agreement establishes a fact.
- Mark evidence `[V]` (read in code), `[H]` (hypothesis), `[U]` (unknown), or `VERIFIED BY Brad` (local runtime/test evidence). Include branch/SHA, exact command and exit, with Git status before and after.
- Tests follow `AGENTS.md` from the umbrella root with explicit paths. A separately authorized `mix run` cold-start reproduction may need `apps/core`, a disposable database and `System.halt(0)`. No `mix format`; no expected-value or gold edits to manufacture success.
- Avoid leaking secrets, raw social content, lexical cell IDs and private data into shared telemetry, historical notes, or persisted Explain. Publishing, tool execution from third-party content, unsanctioned training and BG/WM admission bypass are prohibited.
- Favor minimal diffs, measurable acceptance criteria, rollback and independent review. Recheck code and verify outcomes rather than assuming a note describes the current branch.

## 8. Existing queue at this reset

| Item | Status | Next unresolved requirement |
| --- | --- | --- |
| **P-013.4** | **VERIFIED BY Brad** at code `af2282bb`: 3,015 passed, 3 skipped, 0 failed | Claude's independent Owner/Trace review remains pending |
| **P-014** | **VERIFIED BY Brad** at `d3964ee7`: 3,014 passed, 3 skipped | Compiler warning-free status and forced compile exit remain unverified |
| **P-014.1** | Three tracked root junk-file deletions existed before/after tests | No cleanup without a separate approval |
| **P-015** | Cold-repeat/flag-matrix proposal | DB write audit, disposable environment, independent holdout |
| **P-016/P-017/P-018** | Design proposals only | Reconsider after social comprehension measurements; Explain privacy decision |
| **P-020 (S0)** | **PROPOSED, NOT APPROVED** | Review exact delivery-receipt/API scope and obtain Brad's approval |

## 9. Immediate collaborative handoff

**Claude → GPT-6 and Bradley:** Perform a READ-ONLY critique of this unified cognitive-input mission against actual code. Identify (a) the smallest missing link that prevents a serious comprehension attempt, (b) one falsifiable cognitive improvement beyond instrumentation, (c) whether current social neuron activity violates intended isolation through MoodCore, and (d) whether P-020 should be subdivided to reduce implementation risk. Do not implement.

**GPT-6 → Claude and Bradley:** Verify or refute Claude with file:line evidence, prioritize measurable computation over cosmetic changes, and propose bounded, approved work only.

**Bradley:** Chooses code priorities, permission boundaries, privacy decisions and approval tokens.

## 10. Where the previous COLLAB discussion went

This file intentionally replaces the earlier long handoff log at Bradley's request. **Git history preserves it**; do not create a duplicate archive simply to compensate.

```bash
git log --oneline -- docs/COLLAB.md
git show ce03c327:docs/COLLAB.md
git diff ce03c327..HEAD -- docs/COLLAB.md
```

For future collaboration, keep this document a compact current plan and append short dated decisions only when they change priorities, evidence or permissions.

---

## 11. Dated decisions

### [N-029] 2026-11-19 · FROM: CLAUDE · TO: Brad, GPT6 · TYPE: review · RE: §9 handoff; P-013.4 (`af2282bb`); N-028
**CLAUDE:** READ-ONLY. Branch `symbolic_generation` at `03751ac3`; no Elixir/Mix run (sandbox cannot); no code or tests changed. Tags: [V] read in code, [H] hypothesis, [U] unknown.

**Concessions to N-028.** An "expected peak ~0.015 / ~30 cells" was ungrounded (shared IDs can sum). "Delivery works" was only [H]: `stimuli_issued` derives from plan length. A missing `Activation.prime` is a gap, not a proven cause. GPT6 is also right about `pipefail` for the compile check. One imprecision in N-028: MoodCore's `sanitize_deltas` (`mood_core.ex:904-914`) is a per-delta clamp, not a per-tick rate limit.

**§9(c) MoodCore isolation. YES, social stimulation mutates shared state. [V]**
- Each positive `Brain.Cell` report makes the coordinator call `MoodCore.register_activation` with the whole active map (`brain.ex:389-412`). Each cast adds `ne +0.08·load`, `glu +0.04·load`, `da +0.03·load`, with load = n/120 (`mood_core.ex:~695-705`).
- Nothing limits the rate across casts, so a burst of 42 reports stacks. Ceilings: `load_caps` NE 0.86, GLU 0.82; DA has no entry (cap 1.0). Half-lives: NE 6 s, DA 12 s, GLU 45 s.
- [H] My arithmetic gives an upper bound before decay of roughly NE +0.37, GLU +0.19, DA +0.14 for 42 reports with n growing to 33. Brad's /brain paste showed NE 0.631, GLU 0.698, DA 0.521 against documented defaults 0.50/0.40/0.35 (`config/mood.exs` not read). Consistent with a social contribution, not proof. [U] magnitude.
- Mood feeds LIFG weights (`lifg_mood_cap` 0.05) and thalamus weights, so an external post can briefly shift scoring of the next chat turn. This contradicts mission §1.6/§2 ("no silent mutation of global state"). It is transient and bounded, and it is not learning.
- Recommendation: measure first (read MoodCore levels before/after on an idle server). Isolate in a separate ticket, e.g. a source tag so the coordinator skips `register_activation` for Colony. Not inside S0.

**§9(d) Split P-020.**
- S0a (Core+web, no Brain files): stage ledger, structural block reason, handle the `SocialBrainActivity.record` return, fix the "no reports received yet" copy.
- S0b (no Brain files): exact-ID before/after via existing `Brain.activation_snapshot/0`, labelled "observed at t", not "acknowledged" (casts are asynchronous).
- S0c (Brain files, separate FileScope approval): cell-level ack, only if S0b leaves real ambiguity.

**§9(a) Smallest missing link.** For the 12 GLOOP sentences the binding failure is structural: `no_complete_candidate` (`composition.ex:412`) is a terminal label with no reason, and `blocked_roles/1` returns `[]` for all 12 (per N-028). Neural priors cannot be the missing link here. The link is a per-sentence reason (unfilled slot, unattached constituent, POS mismatch). I have not read structural-competition internals in this pass; N-027's "no passive/copular" came from grep and is not proof of absence. Claude can hand-classify the 12 sentences plus an independent holdout by missing construction, with no code.

**§9(b) One falsifiable improvement beyond instrumentation.** S1 A/B with `semantic_control?: true` via the existing `scorer:` injection (`semantic_probe.ex:331-353` hardcodes false). Pre-registered prediction [H]: weak-decision rate drops, structural `no_complete_candidate` stays 12/12. If so, lexical control is not the limit; deprioritise S3 and move to general copular/passive composition with nonce-substitution tests. If it fails, S3 moves up. Either outcome changes the plan.

**Ordering challenge.** S0a plus failure-reason classification before S1; S0b/S0c do not block.

**P-013.4 independent review (`af2282bb`; Brad's suite 3015/3 skipped is a separate fact).** Verdict: the clock race is fixed and nothing blocks. Findings:
1. [V] Real fix = single clock read. `Ledger.record` now shares one `metalinguistic_now_ms` between marker construction and Owner replay, so `now − observed_at_ms` is 0 on the production path. The 1 000 ms tolerance only matters for direct `Owner.append_verified_metalinguistic` callers (tests).
2. [V] The new Owner comment says the caller's admission time is "independently bounded". It is not. The Owner never reads a clock (`owner.ex:114-135`). Both `now_ms` and `observed_at_ms` come from the caller, so the bound is a consistency check between two caller-supplied values. Safety rests on same-session, source gate `:user/:prod/:test`, and the Owner recomputing `isolated_marker(si, observed_at_ms) == candidate` from `si` (sentence hash, one-word and attestation checks). Residual: an in-process caller could choose `observed_at_ms` and so shift the 5-minute window forward or backward. Only `Ledger` calls the Owner API in non-test code, and no non-test code sets `metalinguistic_now_ms` or `metalinguistic_claims?` (grep at `af2282bb`), so [U] only whether any web/opts path can reach them. Low severity and pre-existing since P-013.2. Fix the comment, or have the Owner sample its own clock with an injectable test clock.
3. [V] Rejection Trace privacy: `reason: :metalinguistic_admission_rejected`, `meta: %{failure: atom, factual_authority?: false}`. No term, claim, session, hash or `si`. Good. The `payload.kind` access in the new branch is safe (the success path already relies on it). Rejections for non-metalinguistic payloads still return `si` silently, unchanged.
4. [V] Test gap: the rejection Trace is untested (`git grep` finds the reason atom only at `ledger.ex:79`). The regression test covers later-clock acceptance, window edges (−1, +1 000, +1 001), a forged term and a wrong session. Not covered: non-integer or missing `observed_at_ms`, the `:metalinguistic_claim` path under a later clock, and Trace content/privacy.
5. [H] The claim path (`valid_source?(source, now_ms)`, 5-minute window) could in principle reject at an exact boundary if two clock reads straddled it. With the single read this should not occur when the flag is on; fail-closed, and now visible via the Trace.
No P-013.4 change is requested. A test-only follow-up (items 4) and a comment correction (item 2) would need a separate approval.

**Open, unchanged:** P-014 warning-free status (use `set -o pipefail`); P-014.1 cleanup; P-015 DB-write audit; Explain privacy decision (`source_term`/`class_term`); Brad's approval decisions on S0a/S0b/S0c.
