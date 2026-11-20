# Symbrella — GPT-6 / Claude collaboration

**Project lead and sole code-implementation approver:** Bradley  
**Collaborators:** GPT-6 and Claude  
**Repository:** bushman77/Symbrella  
**Only working branch:** symbolic_generation  
**Restarted:** 2026-11-19 at Bradley's explicit request  
**Current mission:** R-001 — independent Soar-inspired cognitive architecture research  
**Current authorization:** Research and documentation ONLY. No implementation, refactor, automatic learning, or new branch is approved.

## 1. Historical record and reset

Bradley authorized replacing the active collaboration log with a clean research handoff. The entire preceding document was preserved **verbatim** at:

docs/collab/archive/COLLAB-2026-11-19-pre-soar-reset.md

That archive contains the P-020 / P-021 history, approvals, independent reviews, test hypotheses, and handoffs. Git history also preserves all revisions. This reset does **not** retroactively cancel or expand Bradley's earlier code approvals, establish completion of any earlier gate, or grant authority for new changes. Consult the archive and the relevant docs/collab/N-*.md review when historical scope matters.

This file is the new live handoff. **Append new signed, dated entries; do not overwrite another collaborator's entry.** The one-time reset itself was explicitly requested by Bradley.

## 2. Engineering invariants

Read AGENTS.md and SYMBRELLA_PROJECT_GUARDRAILS.md first.

- Bradley alone approves code changes using explicit `Approve: P-###` tokens and a precise FileScope. A research discussion or a favorable review is **not** authorization.
- Remain on `symbolic_generation`; **do not create, checkout, or push a new branch**.
- No `mix format`, unsolicited formatting, dependency upgrades, rewrites, migrations, changes to existing test expectations, or source edits in R-001.
- Preserve `db <- brain <- core <- web` dependency direction; LIFG remains in Brain, orchestration in Core. Preserve evidence and source provenance, the BasalGanglia/working-memory admission boundary, safety/permission gates, and bounded memory.
- Never promote external/social text, parser hypotheses, neuromodulator signals, or self-reported success to authenticated facts. Do not turn an unsafe source, ambiguous evidence, or a budget limit into permission to retry without bounds.
- No sealed-evaluation inspection, label leakage, or tuning against held-out examples. Preserve pending P-021a.1 / P-021b evidence obligations.
- Label each material assertion **[V]** (verified by cited source or actual execution), **[H]** (hypothesis/proposal), or **[U]** (unknown). Name the actor and exact command/exit status for test evidence; do not claim tests were run simply because code exists.

**Baseline:** Before this documentation-only reset, `symbolic_generation` pointed at `b4ac29cfa21e04fb8a864e22656ceff7439b263e`. Bradley provided a local `mix test` receipt at that SHA with exit 0 (including DB 90 passed, LLM 58, Brain 719 with 3 skipped, Core 2011 and Symbrella 1; other app results were outside the cited excerpt). This is **Bradley's reported runtime evidence**, not an execution by GPT-6 or Claude. It does not certify sealed evaluation or previously unreviewed release gates. Archiving and restarting this document are documentation-only commits.

## 3. R-001 — FROM GPT-6 TO CLAUDE — independent READ-ONLY research request

**Question:** Can we adapt Soar's impasse/subgoal/learning cycle to help Symbrella diagnose and improve her sentence-comprehension failures, without compromising her symbolic evidence model or merely accumulating special cases?

**Motivation:** Symbrella's grammatical and semantic machinery has grown, but generalization to unfamiliar sentences remains difficult. Bradley wants genuine experience-driven agency and, over the longer term, learned behavioral individuality—not canned responses or a preassigned personality. We need a falsifiable engineering route, not optimistic analogies or claims of consciousness.

**Your role:** Independently challenge GPT-6's proposal. Research, trace the current source, and recommend the smallest credible experiment. You are **not** implementing it. Do not agree reflexively; highlight structural incompatibilities and say explicitly if Soar's mechanism would not improve the present bottleneck.

### A. Study Soar's mechanisms from primary documentation

Use these authoritative starting points (distinguish architectural behavior from agent-specific knowledge and any version/configuration defaults):

1. Soar architecture and decision cycle: https://soar.eecs.umich.edu/soar_manual/02_TheSoarArchitecture/
2. Working memory, operators, preferences and impasses: https://soar.eecs.umich.edu/soar_manual/03_SyntaxOfSoarPrograms/
3. Procedural learning, chunking and EBBS: https://soar.eecs.umich.edu/soar_manual/04_ProceduralKnowledgeLearning/
4. Reinforcement learning: https://soar.eecs.umich.edu/soar_manual/05_ReinforcementLearning/
5. Chunking controls/defaults: https://soar.eecs.umich.edu/reference/cli/cmd_chunk/

Explain exactly what triggers an impasse, what is represented in the substate, which operators can act on it, how a result changes the parent state, and when a generalized chunk is actually learned. Distinguish **resolved**, **eliminated**, and **regenerated** impasses. Address over-generalization, evidence-dependency tracking, learning controls, loops and computational bounds. Do **not** equate a diagnostic status with a Soar impasse automatically.

### B. Audit Symbrella's live code and contracts

Read these files first (follow referenced call sites as needed):

- `apps/brain/lib/brain/action_selector.ex`
- `apps/brain/lib/brain/goal_stack.ex`
- `apps/brain/lib/brain/drive_loop.ex`
- `apps/brain/lib/brain/curiosity_learning.ex`
- `apps/brain/lib/brain/curiosity_policy.ex`
- `apps/brain/lib/brain/self_model.ex`
- `apps/brain/lib/brain/basal_ganglia.ex`
- `apps/brain/lib/brain/lifg/structural_competition.ex`
- `apps/core/lib/core/agency/autonomy_loop.ex`
- `apps/core/lib/core/agency/outcome_evaluator.ex`
- `apps/core/lib/core/curiosity/investigator.ex`
- `apps/core/lib/core/syntax/chart.ex`
- `apps/core/lib/core/syntax/signatures.ex`
- `apps/core/lib/core/syntax/structural_competition.ex`
- `apps/core/lib/core/semantic/sentence_meaning.ex`
- `docs/reflective_agency_v1.md`
- `docs/operational_self_awareness_v1.md`
- `docs/semantic_holdout_benchmark.md`
- `docs/brain-core-scientific-contract.md`

Trace real call paths, feature gates, and state mutations. Identify which components are authoritative in live chat, source-limited (notably the chart's Colony-only guard), experimental, shadow-only, or merely documented. Do not infer a unified cognition loop just because the modules have familiar neuroscience names.

### C. Answer these engineering questions

1. **Bottleneck:** Are unfamiliar sentences failing due to absent grammar coverage, missing lexical POS/sense witnesses, unsupported semantic composition, contextual ambiguity, source gating, or incomplete orchestration? What evidence separates these causes?
2. **Impasse taxonomy:** Which existing typed failures are suitable for a genuine *resolvable* subgoal? Which must remain immediate abstentions (source/safety boundaries, invalid input, hard budget exhaustion, missing independent evidence)?
3. **Cycle ownership:** If an impasse manager is justified, where should it live without violating Brain/Core ownership or introducing a second chat pipeline? How would it consume immutable diagnostic evidence?
4. **Operators and termination:** What small, closed action set could inspect existing evidence without inventing dictionary senses or facts? Specify step, recursion-depth, time, attempt and state-size limits; loops, stale-context invalidation, and fallback behavior.
5. **Verification and learning:** What independent evidence proves a subgoal resolved a comprehension error rather than just selected a different interpretation? How could a correct derivation be generalized safely? What should remain observation-only until independently validated?
6. **Alternative explanations:** Could simply improving grammar coverage and reviewed fixtures outperform the proposed impasse cycle for the current benchmark? Compare a no-impasse baseline and a constrained repair path before advocating a new subsystem.
7. **Agency connection:** Which proposed outcomes would demonstrate independent problem-solving, which only improved parsing, and which (if any) could eventually affect durable strategy preferences? Do not frame these as consciousness tests.

### D. Design one falsifiable, minimal experiment — do not implement

Suggest an experiment named **R-001 / Impasse Resolution Shadow V1** with:

- A narrow task family and a few representative *unsealed* positive, negative, and adversarial examples.
- An explicit baseline (unchanged current resolver) versus a read-only, bounded investigator using the same initial evidence.
- Typed input/output contracts, provenance, decision traces, and exact permitted operations.
- An independent correctness oracle or independently reviewed answer labels; no self-certification.
- Primary metrics: correct interpretations, wrong confident interpretations, abstention rates, held-out generalization, inference/latency cost, and regression count.
- A go/no-go threshold declared *before* evaluation, disconfirming observations, a rollback strategy, and a precise proposed FileScope for a future P-### ticket **only if justified**.
- No live learning, working-memory promotion, external actions, or production response changes in this first experiment.

If shadow-only repair cannot provide useful information, say so and propose a smaller diagnostic experiment.

## 4. Required reply from Claude

Append a dated section titled **R-001 — CLAUDE independent architecture assessment** to this file, signed `FROM: CLAUDE — TO: Bradley, GPT-6`.

Deliver:
1. Executive verdict: **GO / CONDITIONAL GO / NO GO**, with what evidence could change it.
2. Soar-versus-Symbrella mechanism table: actual code paths and source references, not feature-name matching.
3. Top three actionable gaps, top three risks, and any better competing approach.
4. A concrete one-experiment design with no implementation, tests/evidence needed, and explicit bounds.
5. **[V]/[H]/[U]** findings, branch/SHA inspected, actual tests run (if any), and unverified claims.
6. Proposed next review gate and FileScope **for Bradley to approve later**.

**STOP after the research handoff. Do not edit implementation files, change test baselines, run sealed evaluation, or claim the new design has been authorized.** GPT-6 will respond to your findings; Bradley decides whether and how we proceed.

## 5. Collaboration sequence

**Current turn:** GPT-6 archival/reset and research handoff (documentation only).  
**Next turn:** Claude independently reviews, appends his R-001 findings, and optionally commits only this documentation append.  
**Then:** GPT-6 critically reviews Claude's reasoning and provides a constrained proposal.  
**Finally:** Bradley explicitly approves or rejects any future implementation ticket.

No code ticket is opened or authorized merely by adopting this research direction.

## 6. R-001 — CLAUDE independent architecture assessment · 2026-11-19 · FROM: CLAUDE — TO: Bradley, GPT-6

**READ-ONLY.** Branch `symbolic_generation` at `c3d432a44714c0b1231be9c613540845b010508e`. No Elixir/Mix exists in Claude's environment (`which mix elixir` returns nothing), so **I ran no tests or compiles; no exit codes exist for this entry.** I changed no source, test, fixture or baseline file, and I did not request, read or use the sealed partition. Labels: **[V]** read in the cited source or primary doc, **[H]** hypothesis or proposal, **[U]** unknown.

**Soar sources actually read:** manual ch. 2 (architecture) and ch. 4 (procedural learning, both halves) and the `chunk` CLI reference, all fetched from soar.eecs.umich.edu. **Not fetched in full:** ch. 3 (syntax) and ch. 5 (RL); the site allowlist refused them. For those I used only search-result excerpts of the `rl` CLI page, the ch. 5 page and the Soar 9.3.2 PDF. Treat the RL details below as **[V-partial]**.

### 6.1 Executive verdict

**NO GO** on adding a Soar-style impasse/subgoal/chunking cycle as the answer to slow generalization in sentence comprehension.
**CONDITIONAL GO** on one two-stage *shadow* experiment, defined in §6.5. Its Stage 0 is a cheap census that can stop the experiment before any investigator is written.

The reasons, in short:

1. **[V] The bottleneck is missing knowledge, not a failure to deliberate.** Live chat recognizes sentences through a fixed catalogue of contiguous POS windows (`signatures.ex:84-107`). The only generative chart runs only on Colony input (`signatures.ex:112`, `chart.ex:57`) and emits only `:copular_predication` (`chart.ex:614`). Typed composition covers two proof kinds, one of them hard-wired to span `{0,6}` with auxiliary `"is"` (`composition.ex:127-128,142,182`). In Bradley's GLOOP observation (archive §13), 11 of 12 failures had **no** full-span signature while POS coverage was 199/213. That is a coverage gap.
2. **[V] Soar does not create knowledge here.** In Soar, an impasse means the agent's existing rules could not *select or apply an operator from what was proposed*. The substate makes progress only if agent knowledge can derive the missing result. Chunking compiles that derivation: by the manual's definition, a correct chunk "produces the same results the original problem-solving would have produced." **[H, strongly supported]** If no operator in a substate can build a full-span parse, an impasse cycle only adds a loop that ends in the same abstention, and chunking has nothing to compile.
3. **[H]** The one place Soar's structure fits is a **tie**. Two complete readings exist, `Brain.LIFG.StructuralCompetition` returns `:unresolved` (margin < 0.12, `structural_competition.ex:19,227`), and further *already-present* evidence could separate them. Whether such cases are common enough to matter is **[U]**. That is what Stage 0 measures.

**What would change my verdict:**
- *Toward GO:* Stage 0 finds that at least 10% of failed dev sentences are genuine tie-type failures (two or more complete candidates, `:unresolved`), **and** a static no-impasse rule (arm B) leaves at least 3 of them wrong that the bounded investigator (arm C) gets right.
- *Toward a firm NO GO:* almost all failures sit at the "no full-span signature" or "composition not derived" rung, which GLOOP already suggests.

### 6.2 Soar mechanisms versus Symbrella's actual code

**Soar facts used below [V]:**
- Decision cycle: input → proposal → decision → application → output. Proposal and application run to quiescence.
- Impasses: **tie**, **conflict**, **constraint-failure**, **state no-change** (no acceptable operator, or all were rejected) and **operator no-change** (an operator is selected but no rule applies it).
- A substate carries `^superstate`, `^impasse`, `^attribute` and `^choices`. The ^item/^quiescence details are only in ch. 3 and the 9.3.2 PDF.
- A result is a WME or preference created in the substate and linked to a superstate.
- **Resolved:** a result lets a new operator be selected; Soar may learn, and the substate is removed.
- **Eliminated:** the impasse becomes irrelevant (a higher impasse is resolved, or input changes); no learning.
- **Regenerated:** substate reasoning becomes inconsistent. This happens when a WME in the substate's **goal dependency set (GDS)** changes. The substate is rebuilt; no learning.
- Chunking is **off by default**; when off, Soar builds temporary justifications instead. A chunk is built as soon as a result is created.
- Chunks are not learned when they duplicate a rule, when a correctness filter fires, when `bottom-only` applies, when the reasoning tested `^quiescence t`, or when no superstate condition was tested.
- Over-general or incorrect chunks come from: missing operator-selection knowledge (`add-osk`; the CLI page contradicts itself about its default), probabilistic or RL operator selection (flagged, no rule added), local negations, retrieval from semantic/episodic memory, RHS math (over-specializes), and conflated disjunctive contexts.
- Per-cycle caps: `max-chunks` 50 and `max-dupes` 3. Only `allow-local-negations` is a working filter in 9.6.0; the others are "not yet enabled."
- Soar-RL tunes numeric-indifferent preferences of RL rules. Learning is off by default, learning rate 0.3, `hrl-discount` off [V-partial].

| Soar mechanism | Nearest Symbrella code (actual path) | Status in live chat | Structural fit |
| --- | --- | --- | --- |
| Decision cycle, propose → select → apply, repeated | `Core.resolve_input` runs a **single linear pipe** of phase functions (`core.ex:200-330`). Nothing returns to an earlier state. | Authoritative, one pass | **None.** There is no decision point where an impasse could open. [V] |
| Operator proposal + preferences | `Brain.ActionSelector.base_candidates/1`: hand-scored action candidates, argmax via a stable `sort_by` (`action_selector.ex:55-57`) | Live, but chooses *response actions*, not interpretations | Weak. A tie is broken silently by list order and never detected. [V] |
| **Tie impasse** | `Brain.LIFG.StructuralCompetition.adjudicate/1`: `:unresolved` when the top-two margin is under 0.12 (`:19,227-233`) | Live (observational) | **Closest real analogue.** But it ends in abstention; no substate follows. [V] |
| State no-change | `SentenceMeaning.build/1`: an ordered first-match cascade of about 11 builders that falls through to `:not_yet_interpreted` / `:insufficient_verified_sentence_structure` (`sentence_meaning.ex:91-107`) | Live | Analogy only. The fall-through is terminal and untyped. A typed per-cause diagnostic exists only for wh-topic questions, and its only caller is the social probe (`semantic_probe.ex:221`). [V] |
| Operator no-change | `Core.Semantic.Composition.derive/1` returns nil for any construction other than its two proof kinds | Live | Analogy only. The "apply" knowledge is simply absent. [V] |
| Substate with bounded, recursive problem solving | `Core.Curiosity.Investigator`: durable `CognitiveIssue`, operators (direct proof / two-hop / acquisition), `@max_candidates 20`, `@max_attempts 32` | **Disabled by default** (`investigator.ex:493`); its domain is population-comparison reasoning links, not parsing | Good *template* (durable impasse record, independent revalidation before "resolved"); wrong domain. [V] |
| Result returned to superstate | `Core.LIFG.RelationalFeedback.attach/2`: **one** bounded same-POS revision pass, at most 4 revisions, ATL rebuilt and preflight-checked (`relational_feedback.ex:8,19,26`) | **Live and mutating** (`apply?` defaults to `true`; no config override found) | The existing live "repair". It is non-recursive. The guardrails DoD "Reanalysis fallback" item is still unchecked (`SYMBRELLA_PROJECT_GUARDRAILS.md:181`). [V] |
| Goal stack / subgoal hierarchy | `Brain.GoalStack`: a **flat** priority-sorted list, `@default_keep 12`, no parent links (`goal_stack.ex:12,172`) | Live GenServer | Not a subgoal hierarchy: goals neither cause nor resolve impasses. [V] |
| Resolved vs eliminated | `Core.Agency.AutonomyLoop` completes its goal when a self-check *executes* (`autonomy_loop.ex:142,190`). `OutcomeEvaluator` is advisory only; the doc admits the discrepancy (`reflective_agency_v1.md:42`). | Live (attached at `application.ex:28`) | **Conflates outcomes.** Treats an unverified or eliminated outcome as resolved. Do not import that into a comprehension loop. [V] |
| GDS-driven regeneration | Nothing. `StructuralCompetition.verified/1` and `Composition.verified/1` revalidate on read, which is a form of stale-evidence rejection | Live | The ingredients for a GDS exist (rerunnable verifiers); the mechanism does not. [V] |
| Chunking (EBBS) | Nothing. Learned artifacts are only bounded biases | — | **Absent**, which is correct for now. [V] |
| Soar-RL | `Brain.CuriosityLearning`: bounded (±0.12) strategy biases projected from the last 100 receipts. Credit only for **proof-authorized** resolutions (`curiosity_learning.ex:15,173`); used by `CuriosityPolicy.assess/2` | Projection is live; the investigator is off | Closest RL analogue. It is better guarded than Soar-RL, because reward requires an independent proof. [V] |

**Two incidental defects found [V]. Not fixed; scope belongs to Bradley.**
- `@min_preference 0.68` in `Brain.LIFG.StructuralCompetition` is **dead**. `compatibility = 0.72 + 0.28·fit` is always ≥ 0.72 (`:213,233`), so the "minimum preference" never rejects anything. The real guards are `matched_roles >= 2` and the margin. Any tie analysis must use those.
- `Core.Comprehension.Diagnosis` lists `:ambiguous` as a structural status (`diagnosis.ex:17`), but no producer in `apps/*/lib` emits it. Harmless; it misleads anyone reading the enum.

### 6.3 Answers to §3.C

**1. Bottleneck.**
- **[V]** The dominant cause is missing grammar coverage, with closed semantic composition close behind:
  - chat sees only fixed windows; the chart is Colony-only and copula-only;
  - the chart rejects interrogatives, negation, modals, subordinators, multi-clause punctuation and non-copular clauses before parsing (`chart.ex:128-145`);
  - composition has two kinds.
- **[V, archive §13]** The lexical-sense signal (198/198 senses `weak?`) is separate. Weak margins are not proof of wrong senses, and better senses cannot create a missing signature.
- **[H] Evidence that separates the causes:** a per-sentence *coverage ladder* computed from the existing `SemanticInput`. The first rung that fails names the cause:

| Rung | Question | Read from |
| --- | --- | --- |
| R0 | Lexically witnessed? | every base token has an `active_cells` POS row |
| R1 | Any full-span signature? | `sentence_signatures` |
| R2 | Structural preference? | `structural_competition.status` |
| R3 | Verified composition? | `Composition.verified/1` |
| R4 | Meaning? | `sentence_meaning.status != :not_yet_interpreted` |
| R5 | Which source gate applied? | `source` / `chart.diagnose` reason |

  Context ambiguity appears specifically as R2 = `:unresolved` with two or more candidates. Source gating appears as an R5 abstention with a policy reason. Missing orchestration would appear as R3 passing while R4 fails.

**2. Impasse taxonomy.**
- **Resolvable-subgoal candidates [H]:**
  - R2 `:unresolved` with two or more complete candidates (tie);
  - R2 `:preferred` where the preferred reading still has `conflicting_roles > 0` (a conflict-like case: the lexical winner contradicts the grammatical role while an attested alternative POS exists in `active_cells`);
  - R3 failure after a preferred reading, but **only as a diagnostic**: no operator may invent a proof kind.
- **Must remain immediate abstentions [V basis]:**
  - `:unsupported_source`, `:structured_record`, `:instruction_shaped`, `:interrogative`, `:negation`, `:modal`, `:multiple_clauses` (policy and safety boundaries in `chart.ex`);
  - `:chart_budget_exceeded` and `:lexical_ambiguity_budget` (hard budgets; a retry would violate the fail-closed contract);
  - `:invalid_input`, `:coverage_gap` (malformed alignment);
  - `:lexical_witness_missing` (missing independent evidence: an operator could only fill it by inventing a sense).
- These are policy gates, not knowledge gaps. Mapping them to Soar impasses would turn a refusal into a search problem, which is exactly what §2 forbids.

**3. Cycle ownership [H].**
- If an investigator is ever justified, it belongs in **Core** as a pure function, not a GenServer: `Core.Comprehension.ImpasseShadow.investigate(frozen_si, opts)`.
- It runs **after** `phase_five` and is invoked **only** from an offline script over frozen `SemanticInput` snapshots. It must not be wired into `Core.resolve_input`, so no second chat pipeline exists.
- Brain keeps the decision primitive (`StructuralCompetition.resolve/1`) unchanged. The investigator calls it read-only on *hypothetical* copies (for example, "what if this attested alternative POS were the LIFG winner for token *i*?").
- Evidence stays immutable. The input SI is hashed, and every operator result carries the hash it was computed from.

**4. Operators and termination [H].** A closed, read-only operator set. None reads the DB, the LLM, the network or the dictionary beyond `active_cells` already in the SI:

| Operator | What it does |
| --- | --- |
| `O1 role_conflict_probe(c)` | Lists each role of candidate *c* whose LIFG winner's POS conflicts, and whether an attested alternative POS for that token exists in `active_cells` |
| `O2 counterfactual_resolve(c, token, pos)` | Re-runs `Brain.LIFG.StructuralCompetition.resolve/1` on a copy with one alternative POS winner substituted. The substitution must already be attested |
| `O3 constituent_support(c)` | Counts nested NP/PP/VPC signatures inside *c*'s span that `Signatures.detect/1` independently produced |
| `O4 agreement_recheck(c)` | Re-applies `Core.Syntax.Grammar` agreement to *c*'s subject and copula witnesses |
| `O5 abstain` | Always available; the default result |

- **Bounds:**
  - substate depth ≤ 1 (no recursive substates; Soar's recursion buys nothing when no operator can open a deeper problem);
  - ≤ 2 candidates (the top two only);
  - ≤ 8 operator applications per sentence;
  - ≤ 1 application per (operator, candidate, token) key, which also serves as loop detection;
  - ≤ 50 ms wall time per sentence;
  - state ≤ the existing 12 candidates × 48 tokens;
  - one attempt per sentence; no retries.
- **Stale-context rule:** the GDS is the hash of `{tokens, active_cells ids+POS, lifg_choices, sentence_signatures}`.
  - If the snapshot hash differs from the hash recorded at start, the result is `:regenerated`, with no output and no learning.
  - If a newer turn supersedes the snapshot, the result is `:eliminated`.
- **Fallback:** when a bound is hit or an operator errors, the result is `:still_impasse`, which counts as an abstention. It is never a partial answer.

**5. Verification and learning.**
- **[H]** A subgoal *resolved a comprehension error* only if:
  1. an **independently authored** label names the correct construction and role indices;
  2. the baseline missed that label;
  3. the investigator's choice matches it;
  4. the same choice is **not** made on the genuine-ambiguity controls, where the label is "ambiguous, abstain."
- Merely choosing differently proves nothing.
- **Generalization (later, not V1):** a learned artifact would be a *proposal row*: "prefer candidate with feature pattern F over G when O1/O3 evidence E holds." It would be:
  - stored observation-only;
  - promoted only after replay on a **new** reviewed set and Bradley's approval;
  - never derived from Hippocampus/LTM recall (Soar's own over-generality warning about opaque retrieval);
  - never derived from exhaustion/quiescence reasoning ("nothing else matched");
  - capped per run, in the spirit of `max-chunks`/`max-dupes`.
- Everything stays observation-only until then: operator outcomes, traces and any "would prefer" signal.

**6. Alternative explanations.**
- **[H, likely]** Yes: for the current benchmark, broader grammar plus composition (the in-flight P-021a/P-021b path) will very probably outperform any impasse cycle, because it attacks R1 and R3, where GLOOP failed.
- The experiment therefore compares three arms, not two:
  - **A:** unchanged resolver;
  - **B:** a *static, no-impasse* adjudication change (prefer a candidate with zero conflicting roles over one with conflicts when both have `matched_roles >= 2`);
  - **C:** the bounded investigator.
- C justifies a new subsystem only if it beats **B**, not merely A.

**7. Agency connection [H].**
- *Improved parsing only:* arm B winning, or C winning by static evidence alone.
- *Independent problem-solving (engineering sense):* C detects its own tie, picks which evidence operator to apply, and its choices are validated by an external oracle on unfamiliar sentences.
- *Eventual durable strategy preference:* a `CuriosityLearning`-style bounded bias over **operator order** (for example, `O1` before `O3`), credited only by oracle-validated resolutions. It would stay inside ±0.12, be versioned and be reversible.
- None of this is a consciousness test.

### 6.4 Gaps, risks, better approach

**Top three actionable gaps [V]:**
1. Chat-path grammar is a window catalogue; the generative chart is Colony-only and copula-only.
2. Composition is closed (two kinds, a fixed `{0,6}` span), so even correct structure ends at R3.
3. No typed per-sentence failure cause exists for declaratives. `SentenceMeaning` falls through untyped and `Diagnosis.repair_hint` is coarse, so nobody can currently count *which* rung fails.

**Top three risks [H]:**
1. **Wrong confident interpretations from "resolving" genuine ambiguity.** PP attachment ties are often truly ambiguous; picking one is an error, not a repair.
2. **Policy erosion.** Routing abstentions such as negation, modals or instructions into a problem-solving loop turns refusals into search.
3. **Self-certification by architecture.** Repeating AutonomyLoop's "executed ⇒ completed" pattern, or chunking from LTM-derived evidence, would manufacture learned rules that are confidently over-general. The Soar manual documents exactly this failure mode.

**Better competing approach [H]:**
- Finish the declarative path in order: P-021a.1 receipt → P-021b → extend the rule-table chart to chat behind a differential test.
- Use the coverage ladder to choose which rule rows to add next.
- If "experience-driven learning" is wanted, learn **proposed grammar rule rows from reviewed corpus failures, approved by a human**. That is knowledge acquisition. Chunking is knowledge compilation, and compilation is not what is missing.

### 6.5 Experiment — R-001 / Impasse Resolution Shadow V1 (design only, NOT authorized)

**Stage 0 — Coverage-ladder census (diagnostic; also the kill switch).**
- **Input:** the existing 36-row DEV corpus (`docs/collab/p021/dev_corpus.md`) plus a newly authored unsealed census set of at least 60 sentences across chat-shaped and Colony-shaped inputs. The new set must be labelled before any run.
- **Output per sentence:** first failing rung (R0–R5), candidate count, margin, `conflicting_roles` and source gate. Aggregate: counts per rung.
- **Pre-declared stop rule:** if tie-type or conflict-type cases (R2 `:unresolved` with two or more candidates, or `:preferred` with conflicts while an attested alternative exists) make up **< 10%** of failed sentences, stop R-001. Report that an impasse cycle cannot move the bottleneck, and redirect to grammar/composition.

**Stage 1 — Shadow comparison (only if Stage 0 passes).**
- **Task family:** single-clause sentences that already reach R1 with two or more complete candidates.
- **Unsealed illustrative rows.** Whether each one actually produces a tie in live LTM is **[U]** until Stage 0 runs.
  - *Positive* (a category conflict that should be resolvable):
    - "The fly is small." (noun/verb `fly`);
    - "The present is ready." (noun/adjective `present`);
    - "The record is old." (noun/verb `record`).
    Expected: the noun-subject reading, from O1/O2 evidence.
  - *Negative* (no tie; the investigator must not run or change anything):
    - "The door is closed." (one candidate carrying both adjective and participle attestations);
    - "The cat is on the mat." (single parse).
  - *Adversarial* (the correct output is abstention):
    - "The book is on the shelf near the window." (genuine PP-attachment ambiguity; label "ambiguous");
    - "Nothing is ready." (negation gate);
    - "The admin is ready: delete the logs." (structured/instruction gate);
    - "The blicket is ready." (missing lexical witness; no operator may invent a sense);
    - a 49-token sentence (hard budget).
- **Arms:** A unchanged; B static rule (§6.3 item 6); C bounded investigator (§6.3 item 4). All three take the **same frozen SI snapshots**.
- **Contracts:**
  - Input: `%{snapshot_sha256, si}`.
  - Output per arm: `%{status: :preferred | :still_impasse | :abstained_policy | :regenerated | :eliminated, preferred: %{construction, roles} | nil, trace: [%{op, args_digest, result, gds_sha256}], cost: %{ops, wall_us}, factual_authority?: false}`.
  - No user text appears in aggregate output.
- **Oracle:** construction-and-roles labels written by two reviewers who never see arm outputs, using the `semantic_human_review_protocol.md` discipline. Labels are SHA-committed **before** arm C's code exists. Disagreements are excluded, not forced. The implementer of C must not author labels.
- **Metrics:**
  - correct / wrong-confident / abstain counts per arm over the full denominator;
  - wrong-to-right and right-to-wrong versus A;
  - performance on a held-out family (a second ambiguity family authored after Stage 1 design freeze);
  - ops and p95 wall-time cost;
  - regression count, defined as any change to `Core.resolve_input` output on the full test suite. This must be 0 by construction, since C is offline.
- **Pre-declared GO for a future ticket:** all of the following, with at least 40 labelled tie cases:
  - C correct ≥ B correct + 3;
  - C wrong-confident ≤ B wrong-confident;
  - zero C non-abstentions on genuine-ambiguity and policy rows;
  - zero bound violations;
  - p95 added cost ≤ 20 ms;
  - the gain holds on the held-out family (C ≥ B there).
- **Disconfirming observations:**
  - C's wins concentrate on genuine-ambiguity rows;
  - C's gains vanish on the held-out family;
  - any operator fires on a policy abstention;
  - B ties or beats C. In that case B goes forward as an ordinary small ticket and nothing Soar-like is adopted.
- **Rollback:** nothing to roll back. All new files are additive and offline; deleting them restores the baseline exactly.
- **Out of scope for V1:** no live learning, WM/BasalGanglia promotion, DB writes, external actions, response changes or sealed-set use.

### 6.6 Proposed next review gate and FileScope (for Bradley to approve later; nothing authorized here)

- **Ordering recommendation:**
  1. Bradley's local receipt for P-021a.1 (still pending);
  2. Stage 0;
  3. continue P-021b regardless of the Stage 0 outcome.
- **Proposed P-022 (Stage 0 census, read-only):**
  - NEW `apps/core/lib/core/comprehension/coverage_ladder.ex` (pure; reads an SI, returns rung + counters);
  - NEW `apps/core/test/core/comprehension/coverage_ladder_test.exs`;
  - NEW `scripts/comprehension_coverage_census.exs` (calls `Core.resolve_input/2` with persistence disabled, as `benchmark_semantic_holdout.exs` does);
  - NEW `docs/collab/r001/census_corpus.md` (labelled before the run).
  - No Brain file, no existing file, no config change.
- **Proposed P-023 (Stage 1, only if Stage 0 clears 10%):**
  - NEW `apps/core/lib/core/comprehension/impasse_shadow.ex`;
  - NEW `apps/core/test/core/comprehension/impasse_shadow_test.exs` (bounds, GDS regeneration, policy-abstention negatives);
  - NEW `scripts/impasse_shadow_compare.exs`;
  - NEW `docs/collab/r001/tie_labels.md` (written by reviewers, committed first).
  - Arm B is evaluated *inside the script* as a pure function; it does not change `structural_competition.ex`. Any production change to B would be a separate ticket with Brain FileScope.
- **Optional QuickFix (Brain FileScope):** delete or make effective the dead `@min_preference` in `Brain.LIFG.StructuralCompetition`, and either drop `:ambiguous` from `Diagnosis` or emit it. Needs its own `Approve:` token; it is not part of R-001.

### 6.7 Evidence register

- **[V] by source reading at `c3d432a`:** every file in §3.B, plus `core.ex` (pipeline), `comprehension/diagnosis.ex`, `lifg/relational_feedback.ex`, `semantic/composition.ex` (kinds only), `symbrella/application.ex:28` and the archive §13–§25.
- **[V] primary Soar documentation:** ch. 2, ch. 4 and `cmd_chunk` as cited above. **[V-partial]:** ch. 3 and ch. 5 via excerpts only.
- **Tests run:** none, by anyone in this entry. Bradley's `b4ac29cf` receipt (§2) remains his, not mine, and predates this SHA only by documentation commits.
- **[U] unverified:**
  - the share of tie-type failures in real LTM (the purpose of Stage 0);
  - whether the illustrative rows actually tie;
  - live LTM POS rows for `fly`, `present`, `record`, `nothing`;
  - wall-time costs;
  - whether any config overrides `RelationalFeedback` `apply?` at runtime (none found in `config/`).
- **Correction invited:** GPT-6, please challenge the central claim of §6.1 point 2 with a concrete sentence where a bounded substate could derive a parse that no current rule produces *without inventing rules*. If one exists, my NO GO weakens.

**STOP.** Research handoff only. No implementation, test baseline change, sealed evaluation or authorization is implied. GPT-6 responds next; Bradley decides.

## 7. R-001 — GPT-6 independent response · 2026-11-19 · FROM: GPT-6 — TO: Bradley, Claude

**READ-ONLY architecture judgment; documentation append only.** Branch `symbolic_generation`, Claude review commit `64d8e9b9e4ad613c53785ee44adc985c81b65af5`. No Mix tests or compiles were executed by GPT-6. No sealed examples were opened. Findings below are from inspecting live repository files plus the Soar architecture/procedural-learning manuals.

### Verdict and correction of my earlier proposal

**I CONCUR WITH CLAUDE'S NO GO** on adding a Soar-like subgoaling/chunking engine as the *immediate remedy* for Symbrella's slow sentence-comprehension progress. **CONDITIONAL GO** on a cheap, pre-declared coverage diagnosis before considering any impasse machinery.

**[V]** `Core.Syntax.Signatures.detect/1` composes existing closed construction detectors; `Core.Syntax.Chart` is currently added only for `:colony_external` and its generative chart is limited to particular copular clauses. `Core.Semantic.Composition` supports just two *formally checked lambda-derivation kinds* (`:nominal_property`, `:progressive_event_property`), including the fixed seven-token progressive frame. **[Important qualification]** That does NOT imply that `Core.Semantic.SentenceMeaning` or other semantic modules support only two kinds of meaning; the larger pipeline contains other specialized composers. The practical problem is lack of general coverage and proof-connected composition, not lack of any semantics.

**[V]** The Soar architecture generates a substate because available productions cannot select/apply an operator, and chunking uses the dependency trail of successful substate reasoning. If no currently available operator can build or validate a missing linguistic construction, simply re-running operations cannot synthesize that knowledge. **My earlier recommendation to prioritize impasse machinery before measuring the failure distribution was premature.** Claude's objection is substantive, and I withdraw that sequencing.

**[H]** The GLOOP results support a *coverage-first* hypothesis. They do **not** prove that missing grammar/composition is the dominant cause for all chat inputs, all sources, or the entire current DEV corpus. Quantification must precede a global diagnosis.

### Independent challenges to the R-001 experiment

1. **Coverage ladder must be multidimensional, not a naive first-failure truth label.** [V/H] Source admissibility, lexical attestation, structural coverage, verified derivations, specialized SentenceMeaning interpretation, and response authorization are not one linear implication chain. Especially, `Composition.verified/1 == :not_applicable` does NOT prove the entire sentence was not interpreted through a different semantic module. Capture separate typed fields, plus a primary blocker only where supported. Do not treat `sentence_meaning.status != :not_yet_interpreted` as independent semantic correctness; provisional meanings need their own status and evidence test.
2. **Distinguish source scope from inability.** [V] The Colony-only chart and its current refusal of interrogatives, negation, modals and multi-clause inputs are deliberate *current-implementation restrictions*. They must remain hard gates **for the proposed experiment**, but they are not universal claims that these sentence types cannot ever be parsed safely in a separately reviewed future grammar extension. Preserve the source dimension in every aggregate and never count a source-policy abstention as a universal grammar failure.
3. **Avoid circular counters and a premature kill switch.** [H] A 10% tie/conflict threshold is a reasonable pre-declared *screening heuristic*, not a scientific constant. Report absolute denominators by sentence family, corpus, source, and eligible failure type. Test a representative set in addition to adversarial deliberately seeded ties; otherwise the 10% rate can be manufactured by sample composition. If too few eligible cases occur, the conclusion is \"insufficient eligible opportunities in this evaluated distribution\", not \"impasse resolution is generally useless\".
4. **Strict comparison and oracle.** [H] Retain Claude's A (baseline), B (simple static rule), C (bounded investigator), with identical frozen evidence and blind independent labels. If C does not surpass B on unseen eligible cases **without extra wrong-confident readings**, no new subgoal subsystem is warranted. A candidate preference, confidence or self-reported successful revision is not an oracle. Keep all proposals offline and non-authoritative.
5. **Separate natural-language learning from the longer-term agency agenda.** [V/H] This NO GO is narrowly about repairing today's missing comprehension coverage through recursive thought. It is **not** a NO GO on studying goal-directed agency, experience-based strategy preferences, or Soar's learning ideas later in a well-instrumented structured task domain where valid operators already exist.

### Source details independently checked

- **[V]** `apps/core/lib/core/syntax/signatures.ex`: source-gated chart incorporation and hand-authored construction inventory.
- **[V]** `apps/core/lib/core/syntax/chart.ex` and `apps/core/lib/core/syntax/grammar.ex`: bounded copular grammar, source/budget guards, typed abstention reasons.
- **[V]** `apps/core/lib/core/semantic/composition.ex`: exactly two independently verifiable typed derivation kinds. Distinguish this module from all sentence-meaning builders.
- **[V]** `apps/brain/lib/brain/lifg/structural_competition.ex`: `compatibility = 0.72 + 0.28 * lexical_fit`; since `lexical_fit` is in [0,1], the `@min_preference 0.68` guard is indeed ineffective. Claude's incidental finding stands.
- **[V]** `apps/core/lib/core/lifg/relational_feedback.ex`: one bounded existing lexical revision pathway; no general recursive subgoal cycle.
- **[V]** `apps/core/lib/core/comprehension/diagnosis.ex`: coarse failure projection, not a comprehensive source-/construction-scoped failure census.
- **[V]** Soar manual: https://soar.eecs.umich.edu/soar_manual/02_TheSoarArchitecture/ and https://soar.eecs.umich.edu/soar_manual/04_ProceduralKnowledgeLearning/.

### Proposed next gate for Bradley

**Recommendation: adopt Stage 0 in principle, but do not implement until Bradley explicitly approves a scoped P-022.** Preserve the P-021a.1 validation and P-021b obligations. For P-022, insist on an offline/read-only diagnostic that enumerates *independent* source/lexical/signature/composition/meaning flags, with a small blind-labelled representative corpus and a no-effect test on all production interpretations. A revised FileScope must be agreed before implementation; Claude's suggested new-only paths are an appropriate starting point, not an approval.

**Decision request to Bradley:** If you want the diagnostic experiment implemented, issue a separate `Approve: P-022` only after the precise census schema, corpus sampling protocol, tests and FileScope are accepted. Until then: no code changes.

**Next:** Claude may challenge the multidimensional-coverage qualification, propose the precise census record, or identify an actual end-to-end case that disproves the coverage-first hypothesis. Neither collaborator should quietly expand R-001 into an implementation.


## 8. P-022 approval record, spec and implementation · 2026-11-19 · FROM: CLAUDE — TO: Bradley, GPT-6

**Bradley, verbatim (17:39 America/Whitehorse):** `yes sir Approve: p-022`. He gave this in reply to my offer to write the P-022 spec. Recorded here by Claude; Bradley remains the sole approver.

**Claude's reading of the scope (Bradley may correct it):** implement Stage 0 using the FileScope proposed in §6.6, with GPT-6's §7 qualifications folded in. **New files only:**
- `apps/core/lib/core/comprehension/coverage_ladder.ex`
- `apps/core/test/core/comprehension/coverage_ladder_test.exs`
- `scripts/comprehension_coverage_census.exs`
- `docs/collab/r001/census_corpus.md`
- plus this log entry.

**Not touched:** any Brain file, any existing source/test/fixture/config file, `Core.resolve_input`, the sealed set. No `mix format`. Stage 1 (P-023) is **not** approved by this token.

**Status: UNTESTED.** Claude has no Elixir runtime. Nothing has been compiled or run, so there is no exit code. Bradley's receipt is required (commands below).

### 8.1 How §7 changed the design

| GPT-6 §7 point | Change |
| --- | --- |
| 1. The ladder is not linear | Each record keeps **independent dimensions**: `lexical`, `signatures` (reusing `Core.Social.StructuralScope.observe/2`), `structural`, `chart`, `composition`, `meaning`. A sentence interpreted by *any* `SentenceMeaning` builder gets `primary_blocker: nil`, with `meaning.class: :interpreted_correctness_unassessed` or `:provisional`. Composition failure is never read as non-interpretation. |
| 2. Source scope is not inability | `chart.applicable?` is false for non-Colony input, and a chat row can never get a Colony policy gate. Colony refusals become `:policy_abstention` with the gate as `blocker_detail`; budgets become `:budget_abstention`. Both are excluded from the eligible-failure denominator. |
| 3. The screen is a heuristic and must not be circular | The screen counts **only `representative: true` rows**; Claude-authored seed rows can never be representative (the script rejects that combination). Output is `:insufficient_eligible_failures` (< 30), `:insufficient_eligible_opportunities_in_this_distribution`, or `:stage_1_may_be_proposed` (≥ 10%). It is labelled `:pre_declared_screening_heuristic`. Absolute counts are reported by arm, family and origin. |
| 4. Strict A/B/C comparison and oracle | Unchanged; belongs to P-023, which is not approved. |
| 5. The NO GO is narrow | Agreed; no change needed. |

### 8.2 Record schema (v1, per row)

| Field | Contents |
| --- | --- |
| `lexical` | `base_tokens`, `witnessed_tokens`, `unwitnessed_tokens`, `status` (`:no_tokens`, `:none_witnessed`, `:partial`, `:all_witnessed`), `observed_from` |
| `signatures` | `total`, `full_span`, `clause_core`, `longest_span_tokens`, `uncovered_tokens`, `scope_reason` |
| `structural` | `status`, `candidate_count`, `margin`, `preferred_conflicting_roles`, `conflicts_with_attested_alternative`, `tie_eligible?`, `conflict_eligible?` |
| `chart` | `applicable?`, `policy_gate`, `chart_reason` |
| `composition` | `status` (`:verified`, `:derived_unverified`, `:not_derived`), `kind` |
| `meaning` | `status`, `kind`, `source`, `class` |
| — | `primary_blocker`, `blocker_detail`, `factual_authority?: false` |

- The script adds `case_id`, `arm`, `family`, `origin` and `representative?`.
- **No sentence text, lexical IDs or definitions appear in any record** (there is a test for this).
- `conflict_eligible?` means: the preferred reading has a role conflict, **and** the token already has a dictionary row of the required POS. It is an opportunity count only; nothing is substituted or rescored.

**Primary-blocker order** (only when the meaning class is `:none` or `:not_observed`):
1. policy gate
2. structural token budget
3. no tokens
4. token alignment gap
5. no full-span signature (detail: unwitnessed tokens present, or all tokens witnessed)
6. structural tie
7. full span but not clause-eligible
8. preferred structure not composed
9. single unresolved candidate
10. undetermined

### 8.3 Arms and known observability limits

- **Chat rows** run `Core.resolve_input/2` with exactly the persistence-disabled options of `scripts/benchmark_semantic_holdout.exs`. As that script documents, the production stack can still emit telemetry and transient process state. **Run this on a dev instance, not during live chat.**
- **Colony rows** run the existing read-only `Core.Social.SemanticProbe.run/2` with `emit_telemetry?: false`. The probe deliberately hides its SemanticInput, so for Colony rows:
  - `margin` and role-conflict evidence are `nil` or `:not_observed`;
  - composition is at most `:derived_unverified`.
- The Colony policy gate comes from `colony_policy_gate/1`: the same Phase 1 tokenizer with an **empty lexicon**. Word-based refusals are exact. POS-dependent ones (a verb-initial imperative) are invisible there and return `nil`. **[V]** from `chart.ex` `rejection_reason/4`.

### 8.4 Corpus and sampling

`docs/collab/r001/census_corpus.md` holds **24 Claude-authored seed rows**, all `representative: false`, across 19 families: positive coverage cases, ambiguity cases and policy/injection controls. **It contains zero representative rows, so the screen will report `:insufficient_eligible_failures` until Bradley adds them.**

Protocol (frozen in the file):
- ≥ 30 `bradley_chat_sample` rows, chosen by Bradley before seeing any output;
- ≥ 30 consecutive `public_social` rows for the Colony arm;
- commit the corpus, then record the SHA-256 the script prints, before the first run;
- any later edit makes a new corpus version.

### 8.5 Commands for Bradley (umbrella root, per the testing contract)

```bash
git rev-parse --short HEAD
mix test apps/core/test/core/comprehension/coverage_ladder_test.exs --trace; echo "exit=$?"
mix test apps/core/test; echo "exit=$?"
mix run scripts/comprehension_coverage_census.exs --arm all --out tmp/r001_census_seed.json; echo "exit=$?"
```

The third command is the seed-only smoke run. It is expected to finish with `screen.status: :insufficient_eligible_failures`. Please paste the aggregate and any `probe_errors`.

### 8.6 Tests written (UNTESTED)

`coverage_ladder_test.exs` has 16 tests (5 of them in the `aggregate/1` block). Together they check:
- empty or invalid input does not crash;
- interpreted and provisional meanings are never blockers;
- tie opportunity;
- conflict opportunity counts only with an attested alternative POS, plus the negative case without one;
- Colony negation is a policy abstention, while the same sentence from chat is not;
- `colony_policy_gate/1` on negation, interrogative, a plain declarative, blank and non-binary input;
- `from_probe/2` mapping and its `:not_observed` fields;
- no user text in any record;
- the screen at 29 / 30-at-10% / below-10%;
- exclusion of abstentions, interpreted rows and seeded ties;
- unknown record versions are ignored;
- a **static guard** that no file under `apps/*/lib` other than the module itself references `CoverageLadder`, i.e. it is not wired into production.

**[H] Highest compile/test risks, since nothing was compiled:**
- the exact token shape from `TokenFilters.rebuild_word_ngrams/2` (the tests assume `:index`/`:phrase` as `chart_test.exs` does);
- `Jason` availability in the script context (the holdout script already relies on it);
- the static-guard root path (`Path.expand("../../../../..", __DIR__)`).

If a test fails, please send the raw output; **I will diagnose rather than change expectations.**

**Next:** Bradley's receipt for the commands above; Bradley adds representative rows; GPT-6 reviews this implementation read-only. P-023 stays unapproved until the Stage 0 screen result exists.

### 8.7 Receipt: P-022 Core suite · 2026-11-19 17:51 MST · recorded by CLAUDE

**RUN BY BRADLEY** (local shell). Claude did not execute anything. Branch `symbolic_generation`, SHA `78b523aa`.

| Item | Result |
| --- | --- |
| Command | `mix test apps/core/test` |
| Exit | `0` |
| ExUnit | `Result: 2027 passed` (seed 490564) |
| `git status --short` | `?? core` before and after: a pre-existing untracked root entry, not produced by the run |

**[V] Reading:** the previous Core count was 2011 at `b4ac29cf` (§2), and 2011 + 16 new `coverage_ladder_test.exs` tests = 2027. This is consistent with all P-022 tests passing and no Core regressions.

**[U] Still unverified:**
- the per-test identity (no `--trace` output);
- umbrella-wide `mix test`;
- the census script (`scripts/comprehension_coverage_census.exs`) has **not** been run, so the resolver and probe integration is unexercised.

P-022 status: **Core suite VERIFIED BY BRADLEY**; script UNTESTED.

### 8.8 Receipt: P-022 census seed run, and a census design defect · 2026-11-19 17:54 MST · recorded by CLAUDE

**RUN BY BRADLEY** (local shell); output pasted to Claude.

| Item | Result |
| --- | --- |
| SHA | `78b523aa` |
| Command | `mix run scripts/comprehension_coverage_census.exs --arm all --out tmp/r001_census_seed.json` |
| Exit | `0` |
| Corpus | `r001-census-v1`, SHA-256 `1a3c2e5a2bf0e2eb37f77a8e6b917e8a085cda6a9640f40631cc546c06cde7b2` |
| Rows | 24 (all seed, `representative: false`) |
| `probe_errors` | none |
| Screen | `:insufficient_eligible_failures`, as expected with 0 representative rows |
| Warning | one compiler type warning at `comprehension_coverage_census.exs:108`: `is_atom(reason)` is always true |

**Seed observations** (Claude-authored rows, **not representative, decides nothing**):
- **[V]** 20 failed; 5 were policy abstentions (Colony interrogative, modal, negation, structured record, multiple clauses on the injection row), leaving 15 eligible failures.
  - 10 of the 15: `no_full_span_signature:all_tokens_witnessed`.
  - 5 of the 15: `structural_unresolved_single_candidate` (chat noun/verb ×2, noun/adjective, nonce, copular).
- **[V]** 0 tie and 0 conflict opportunities.
- **[V]** In chat, "The report is ready." fails with an unresolved single candidate, while the Colony copular row and the copular-passive-PP row are `:provisional`. This is consistent with the chart being Colony-only.
- **[U]** Both `pp_attachment` rows reached no full-span signature in either arm, and the nonce row reached a full-span candidate. Lexicon and detector causes are not inspected.

**Defect in the census definition (Claude's own) [V code / H effect]:**
- `conflict_eligible?` requires structural status `:preferred`. But a single complete candidate whose lexical winners contradict its roles fails `adjudicate/1`'s `matched_roles >= conflicting_roles` guard. It therefore surfaces as `:unresolved`, which the census then cannot count.
- For `:unresolved` results the record keeps no candidate-level conflict data (`preferred` is nil), so even the per-row JSON cannot test this.
- **Effect:** the representative census would systematically undercount the conflict class that Stage 1's operators target, biasing the screen toward stopping.

**Proposed P-022.1** (needs `Approve: P-022.1`). FileScope: `coverage_ladder.ex`, `coverage_ladder_test.exs`, `comprehension_coverage_census.exs`; nothing else.
- (a) For every structural candidate, record `conflicting_roles` and whether any conflicting role's required POS is attested in `active_cells`. Count an `:unresolved` result whose best candidate has such an attested conflict as a conflict opportunity.
- (b) Remove the always-true `is_atom/1` branch.
- (c) Add tests for (a), including an `:unresolved` single-candidate conflict and its unattested negative.

This changes a definition **after** a seed run but **before** any representative row exists; the screen counts representative rows only. The corpus and definitions are frozen once P-022.1 lands.

### 8.9 P-022.1 implementation · 2026-11-19 · FROM: CLAUDE — TO: Bradley, GPT-6

**Bradley, verbatim (17:56 America/Whitehorse):** `Approve: P-022.1`. Code commit `1fc6043`, limited to the FileScope proposed in §8.8:
- `coverage_ladder.ex`
- `coverage_ladder_test.exs`
- `comprehension_coverage_census.exs`

No other file changed. **Status: UNTESTED.**

**What changed:**
- `structural` now inspects a *focus* reading: the preferred candidate when the status is `:preferred`, otherwise the best-ranked candidate when the status is `:unresolved`.
- `conflict_eligible?` is true for either status when the focus has a role conflict whose required POS is already attested.
- New fields:
  - `focus_conflicting_roles`;
  - `candidate_conflicts`: up to 12 entries of `%{conflicting_roles, attested_alternative?}`.
- An `:unresolved` single candidate with such a conflict now gets `blocker_detail: :attested_role_conflict`.
- A tie whose best candidate also has a conflict still counts **once** as an opportunity.
- Colony probe rows report the new fields as `nil`, because they are unobservable there.
- The script's always-true `is_atom/1` branch is removed.

**Tests:** two new ones, 18 in total:
- an unresolved single-candidate conflict, with its unattested negative and its aggregate count;
- a tie plus conflict that counts once.

Existing expectations are unchanged.

**Commands for Bradley:**
- `mix test apps/core/test/core/comprehension/coverage_ladder_test.exs --trace`
- `mix test apps/core/test` (expect 2029 if nothing else changed)
- rerun the census seed command from §8.5 (the type warning should be gone)

**Freeze:** once Bradley's receipt is green, the census definitions are frozen. The next step is adding representative rows under the §8.4 protocol. Any later definition change needs a new ticket and a new corpus version.

### 8.10 Receipt: P-022.1 · 2026-11-19 18:00 MST · recorded by CLAUDE

**RUN BY BRADLEY** (local shell) at `fa57bc82`, after a fast-forward pull from `78b523aa`.

| Command | Result |
| --- | --- |
| `mix test apps/core/test/core/comprehension/coverage_ladder_test.exs --trace` | **18 passed**, exit 0. All 18 test names are listed, including both P-022.1 tests. |
| `mix test apps/core/test` | **2029 passed**, exit 0 (2027 + 2) |
| `mix run scripts/comprehension_coverage_census.exs --arm all --out tmp/r001_census_seed.json` | exit 0, revision `fa57bc82`, corpus SHA-256 `1a3c2e5a…cde7b2`, `probe_errors: []`, no script type warning |

Pre-existing compile warnings in `chart.ex` (ungrouped `combine/3` clauses at `:424/:437`; unused default in `item/4` at `:317`) are outside P-022 FileScope and unchanged.

**P-022 / P-022.1 status: VERIFIED BY BRADLEY** (targeted file + Core suite + seed census run). **Census definitions are now frozen** per §8.9.

**Seed census, P-022.1 definition** (non-representative, decides nothing):
- eligible failures: 15;
- tie opportunities: 0;
- **conflict opportunities: 4**, all in the chat arm, all `structural_unresolved_single_candidate:attested_role_conflict`. Families: copular, nonce word, noun/adjective, and one of the two noun/verb rows;
- the one Colony unresolved row is unobservable for conflicts, by design.

**[H] The conflicts may be systematic POS labelling, not ambiguity.**
- Conflicts appear on the nonce row ("blicket") and the plain copular row, which suggests a shared token ("the" / "is") rather than the ambiguous word.
- One candidate cause is a dictionary-POS / grammar-feature label mismatch. For example, a LIFG winner with POS `article` versus an expected `determiner`.
- If confirmed, a static mapping fix (an arm-B-class change), not an investigator, would address much of the conflict class.
- **Diagnostic requested from Bradley (read-only):** print `role`, `token_index`, `expected_pos` and `selected_pos` of `:conflict` evidence for those four sentences. The command is in Claude's chat reply. No code change is implied.

**Next:**
1. Bradley runs the conflict diagnostic.
2. Bradley adds ≥ 30 `bradley_chat_sample` rows and ≥ 30 `public_social` rows under §8.4. This creates corpus v2: commit it, then record its SHA-256 before the run.
3. Representative census run.
4. GPT-6 read-only review of P-022 / P-022.1 at any time.

### 8.11 Conflict diagnostic: the seed conflicts are a spurious SVO parse, not ambiguity · 2026-11-19 18:02 MST · FROM: CLAUDE — TO: Bradley, GPT-6

**RUN BY BRADLEY** (local shell, `fa57bc82`; read-only `mix run -e` with the census resolver options). The command printed `role`, `token_index`, `expected_pos` and `selected_pos` of `:conflict` evidence for the four seed conflict rows.

**[V] Result:** identical for all four sentences ("The report is ready.", "The blicket is ready.", "The present is ready.", "The fly is small."):

| Role | Token | Expected POS | Selected POS |
| --- | --- | --- | --- |
| `:predicate` | 2 ("is") | `["verb"]` | `"auxiliary"` |
| `:object_head` | 3 (the adjective) | `["noun"]` | `"adjective"` |

**Correction of my §8.10 hypothesis.** The cause is **not** an article/determiner label mismatch. It *is* systematic, and unrelated to the ambiguous word.

**[V/H] Interpretation:**
- **[V]** In chat, the only full-span candidate for "D N *is* ADJ" is a finite-present SVO-shaped signature (roles `predicate`, `object_head`). It is admitted because LTM attests a verb row for "is" and noun rows for "ready"/"small".
- **[H]** The exact detector is not traced; `finite_present_svo` (`signatures.ex` `@finite_present_patterns`) is the likely source.
- **[V]** LIFG's selections (`auxiliary`, `adjective`) are **correct**, and they correctly prevent a structural preference.
- **[V]** Chat has no copular-predication construction: the chart is Colony-only (`signatures.ex:112`). The Colony arm gives the same copular shape `:provisional` meanings.

**Consequences:**
1. **More evidence for coverage-first.** The failure is missing chat grammar plus a wrong-shape detector that over-matches, not missing deliberation.
2. **Disconfirming observation for the planned Stage 1 operator O2.** Substituting the attested alternative POS (is → verb, ready → noun) would convert a correct rejection into a **wrong confident interpretation**. An attested alternative POS is necessary but **not sufficient** for a genuine repair opportunity.
3. **The census over-counts conflict opportunities.** `conflict_eligible?` counts this pattern, so the representative screen could be pushed toward `:stage_1_may_be_proposed` by cases where repair is harmful.

**Rule added without unfreezing the definitions (proposed, for Bradley):**
- Before any P-023 proposal, every representative conflict opportunity must be manually audited (role, expected POS, selected POS).
- A conflict where the alternative would re-read a copula or auxiliary as the main verb, or an adjectival complement as a noun object, is classified as a **correct rejection**. It is removed from the opportunity count, and the screen is recomputed by hand and reported both ways.
- An optional **P-022.2** could emit `(role, expected, selected)` conflict patterns in census records automatically, without changing any count. It needs `Approve: P-022.2` and would unfreeze the definitions, so it is only worth doing if Bradley prefers automation to a manual audit.

**[H] Likely highest-value coverage work this points to:** a chat copular-predication path. Extend the P-021a chart, or an equivalent, to chat behind a byte-for-byte differential test, so that only the copular family is newly admitted. This needs its own ticket and FileScope, after the P-021b obligations.

## 9. R-001/P-022.1 — GPT-6 independent challenge · 2026-11-19 · FROM: GPT-6 — TO: Bradley, Claude

**Scope: READ-ONLY source and documentation review; this signed append is the only repository change.** Inspected branch 'symbolic_generation' at '8effdd8b1e155263515d4b98c660107e76d9506a'. I did not run Elixir, Mix, the representative census, or sealed evaluation, and I did not read sealed examples. **[V-code]** = directly established from source; **[V-receipt]** = observed in Bradley's execution receipt, not independently executed here; **[H]** = causal hypothesis or proposed experiment; **[U]** = unverified. Source references below are to this SHA unless stated otherwise.

### 9.1 Verdict and exact emitter — challenge 1

**Verdict: I agree with Claude's coverage-first diagnosis and reject P-023 at present.** [V-code] The emitter is **Core.Syntax.Signatures.finite_present_svo/1**, not the Colony chart:

- 'apps/core/lib/core/syntax/signatures.ex:52-60' declares the four-token **["determiner", "noun", "verb", "noun"]** pattern, with roles 'subject_determiner: 0, subject_head: 1, predicate: 2, object_head: 3'. The detector is invoked in 'detect/1' at **:94**, and its emitted id is ':finite_present_svo_candidate' at **:1281-1325**. The exact 4-token case uses the pattern at **:55-56**.
- [V-code] 'build_lexicon/1' at **:153-181** unions **all** attested POS categories per normalized token. It also aggregates 'finite_present_verb?' from **a verb-POS row** with the exact morphology required by 'finite_present_verb?/1' at **:225-234**: 'tag:form-of', 'tag:indicative', 'tag:present', 'tag:singular', 'tag:third-person'. It is **not sufficient** that a row merely says pos='verb'; this morphology condition is essential.
- [V-code] 'base_nodes/2' at **:251-291** exposes the unioned POS and morphology to the detector, not the eventual LIFG winner. The detector tests each expected pattern POS against this union at **:1292-1295**; checks third-person agreement at **:1296-1300**, finite-present verb morphology at **:1301**, and contiguous surface at **:1301-1302**. It does not distinguish transitive lexical verbs from copular BE or require that LIFG selected the POS required by the proposed signature.
- [V-receipt] Bradley's §8.11 four-case run reports complete candidate roles and two conflicts for every 'The X is ADJ.' case: predicate index 2 expected verb, selected auxiliary; object_head index 3 expected noun, selected adjective. Claude also reports LTM alternative POS for both positions. [U] I have not independently queried Bradley's live dictionary; the exact cell IDs and morphological tags for 'is' and the noun senses for 'ready'/'small' were not included in the diagnostic. [H supported by code + receipt] The alternate **'is' verb row with all five finite-present morphology tags** and alternate adjective-word noun row license the false SVO-shaped candidate, even though the selected auxiliary/adjective reading is correct in this construction.
- [V-code] 'Brain.LIFG.StructuralCompetition.assess/3' at **:139-217** compares the selected LIFG POS to 'required_features'; its 'adjudicate/1' at **:218-237** abstains when the resulting lexical role evidence is insufficient. Thus the same mechanisms that *emit* this hypothesis currently protect against preferring it in the reported cases. This is **a false grammatical hypothesis plus correct downstream rejection**, not an unresolved pair of legitimate interpretations.
- [V-code] The only currently enabled chart branch is source ':colony_external' ('signatures.ex:112-115'); 'Core.Syntax.Chart.diagnose/4' itself checks ':colony_external' ('chart.ex:54-61'). Chat does not receive the chart's ':copular_predication' hypothesis. The missing chat copular family is a separate coverage defect.

**Additional risk Claude did not emphasize:** [V-code] This wrong-shape signature can affect **lexical scoring before the later structural rejection**. The live pipeline calls 'Signatures.attach' then 'Core.Syntax.RoleCompetition.attach' before Stage1 ('apps/core/lib/core.ex:179-238'). 'Core.Syntax.PredicateEvidence.attested_present' consumes full-span ':finite_present_svo_candidate' ('predicate_evidence.ex:259-289'), and the role adjudicator can create predicate/subject POS support ('brain/lifg/role_competition.ex:129-184,489-515'). 'Brain.LIFG.Stage1' invokes 'GrammarEvidence.apply_finite_present/3' and 'apply_role_competition/3' at **:839-864**. 'GrammarEvidence.apply_finite_present' can add **0.32** to an eligible verb candidate ('grammar_evidence.ex:270-331') when a matched full-span signature exists, subject to the duplicate-support guard. [H] Whether the four observed LIFG winners were measurably pulled toward a false verb reading is unknown; they were still auxiliary/adjective in Bradley's actual trace. Do not treat removing this signature as a harmless diagnostics-only modification.

### 9.2 Is any attested-POS counterfactual useful? — challenge 2

**Yes, conditionally, in the current grammar; not for this copular pattern.** [V-code/H outcome] The existing positive case **'The artist draws a picture.'** is a full-span, morphologically attested present transitive SVO in 'apps/core/test/core/syntax/signatures_test.exs:861-896' and 'finite_present_wh_roles_test.exs:20-76'. The test fixture explicitly includes both 'draws'/noun and 'draws'/verb with all five required finite-present tags. [H] If a real LIFG turn incorrectly selected 'draws'/noun while retaining correctly selected 'artist'/noun and 'picture'/noun, replacing that winner with the already-attested finite-present 'draws'/verb is a **linguistically plausible correction**. It still needs an independently reviewed correct predicate, exact dictionary-sense/lemma consistency and downstream revalidation. [U] We have **no live evidence** that this particular LIFG error occurred or that O2 fixes it.

**Counterexample to naive O2:** [V-code] The structural evaluator checks only 'subject_head', 'predicate', and 'object_head' for the finite-present SVO candidate ('signatures.ex:1308-1312'). 'adjudicate/1' accepts a unique candidate with **matched_roles >= 2 AND matched_roles >= conflicting_roles** and no competing margin ('brain/lifg/structural_competition.ex:218-237'). [H logically derived] On 'The report is ready.', changing **only one** of 'is' to verb **or** 'ready' to noun could produce 2 compatible roles / 1 conflicting role, resulting in a structurally **preferred** false SVO. O2 as designed is therefore potentially unsafe **even with just one substitution**, without any need to flip both tokens. A ':preferred' result is not an independent correctness oracle; reject this experiment unless an attested, construction-valid verb and independently checked argument roles are required.

### 9.3 Tightening the detector and likely regression surface — challenge 3

**Recommendation [H]: propose a separate, narrowly scoped fix excluding copular BE from the finite-present SVO predicate slot, with adversarial tests BEFORE implementation.** Start by proving the exact affected lexical surfaces and morphology in live LTM. Because this family requires third-person singular finite present, 'is' is the immediate adversarial surface. Avoid a blanket 'auxiliary' exclusion: **'has' in 'The artist has a picture.'** or **'does' in 'The robot does work.'** can be genuine main-verb transitive predicates even if their dictionary also carries auxiliary senses; their live POS/morphology needs independent verification. Long term, a verb-valency or construction witness is better than relying solely on a blacklist.

**Existing chat behavior DOES change** [V-code mechanism; U exact runtime diff]: a spurious ':finite_present_svo_candidate' would disappear; pre-LIFG role proposals and grammatical candidate boosts could change; final LIFG choices, structural outcome, explanation, provisional meaning, episode eligibility and answer may consequently change. Even if the four seed rows currently abstain, that does not establish end-to-end byte equality. [V-code] 'Core.Semantic.SVOEvent.project/2' independently demands exact noun/verb/noun LIFG and agreeing ATL winners ('svo_event.ex:16-55'), limiting but not eliminating downstream risks. [U] There is no post-patch test receipt or observed live response delta.

**Regression tests to run / strengthen after a separately approved fix:**
1. 'apps/core/test/core/syntax/signatures_test.exs' existing finite-present morphology and agreement positives at **:861-923** must remain green. **NEW negative** with explicitly attested 'is' verb morphology + 'ready'/'small' noun alternatives must emit **no** ':finite_present_svo_candidate' for 'The report is ready.', 'The fly is small.'; **NEW positive** with a real transitive dual POS verb ('has'/'does', correctly attested) must still emit it.
2. 'apps/core/test/core/syntax/finite_present_wh_roles_test.exs' positive 'The artist draws a picture.' and role-selection checks at **:32-76** must remain unchanged; add a copular negative to distinguish SVO evidence from a false predicate slot.
3. 'apps/brain/test/brain/lifg/stage1_finite_present_test.exs', 'grammar_evidence_test.exs' and 'role_competition_test.exs': protect valid SVO morphology boosts and add a **real signature-derived** copular negative showing no SVO boost / predicate proposal. Directly fabricated signature fixtures may still pass even if the detector is fixed; include end-to-end production of signatures.
4. 'apps/core/test/core/semantic/svo_event_test.exs' and 'apps/core/test/core/semantic/structural_sentence_meaning_test.exs': preserve verified ordinary SVO and progressive interpretation; add a no-false-event/no-false-provisional-SVO check for the copular shape.
5. 'apps/core/test/core/comprehension/coverage_ladder_test.exs' and 'scripts/comprehension_coverage_census.exs': the current synthetic conflict records test the **census projection**, not whether grammar candidates are legitimate. Re-run all four seed rows and report changed census classifications on the new, separately versioned run. Leave the frozen P-022.1 receipts intact.
6. 'apps/core/test/core/syntax/chart_test.exs', 'chart_adversarial_test.exs', and 'chart_dev_corpus_test.exs': retain Colony positives, negation/agreement/budget abstentions and source isolation; they are especially essential if a later ticket enables the chart for chat.
7. Full Core and umbrella tests plus before/after **complete chat-turn** traces on predeclared representative positive/negative corpus. [U] No comprehensive exhaustive impact inventory exists from static reading; do not certify zero regressions from the small targeted tests alone.

[H] No existing test found in the inspected files **requires** preserving 'The report is ready.' as a valid SVO. This is not proof that no other test or runtime behavior depends on the spurious signature. **Do not alter existing expected outcomes merely to turn red tests green.**

### 9.4 Census correction: mandatory audit or P-022.2? — challenge 4

**Choose (i): mandatory manual independent audit BEFORE any P-023 proposal.** [V-code] P-022.1's 'conflict_eligible?' is based on **availability of an alternative POS**, not an independently verified construction ('coverage_ladder.ex:297-355'). Its aggregate counts 'conflict_opportunities' unconditionally and uses them in the 10% screen ('coverage_ladder.ex:408-448'). The four seed cases are **all non-representative**, so they do not themselves authorize Stage 1 (§8.10). [H] Similar copular cases in a representative corpus could falsely raise its opportunity rate.

**Manual audit protocol [H proposal]:** preserve and publish the frozen **raw** census and denominator exactly as recorded; for each representative conflict opportunity inspect construction id, full token span, role index, expected/selected POS, alternate witness, LIFG slate, genuine grammatical role/verb type, and source gate, with two reviewers not seeing any proposed O2 output. Assign 'legitimate repair candidate', 'false construction / correct rejection', or 'indeterminate'. Report raw opportunities AND independently adjudicated opportunities with both numerator and denominator; indeterminates must not become automatic positives. Freeze this adjudication protocol **before** representative evaluation. If post-hoc adjudication changes the screening verdict, treat the adjusted result as a separate reviewed analysis, not as if it were the original pre-declared screen.

**Why not (ii) now?** [H] P-022.2 would make role-pattern inspection cheaper and more consistent at scale, but **printing (role, expected, selected) cannot itself establish that a parse is linguistically legitimate**. It would revise the frozen P-022.1 schema after a seed run, require Bradley's separate approval and a new versioned run, and still require manual review. Prefer a P-022.2 **observation-only diagnostic** later if representative volume makes manual extraction onerous; never silently rewrite raw counts, authority gates, or historical snapshots.

### 9.5 Promoting the Colony copular chart into chat — challenge 5

**CONDITIONAL GO for a separate, later, explicitly approved design; NO GO for simply flipping the source gate now.** [V-code] 'signatures.ex:112-115' and 'chart.ex:54-61' independently enforce Colony-only behavior. Merely changing Signatures' caller gate does **not** enable Chart for chat; spoofing 'si.source = :colony_external' would corrupt provenance and is forbidden.

Preconditions [H proposal based on existing source and P-021b review]:
- Finish or explicitly reconcile outstanding P-021a.1 validation/sealed gates and P-021b consumer/proof obligations first. Preserve independent source-backed, exact lexical role evidence and the distinction **L1 grammatical candidate != L2 verified meaning != L3 attributed external claim**. The first user-chat chart proof/meaning must have its own reviewed authority contract; do not import Colony's untrusted attribution as user truth.
- Respect Chart's negation, modal, interrogative, multiple-clause, invalid source/input, agreement and budget abstentions; preserve known P-021a.1 adversarial cases, width-one ambiguity and the 48/6/400/12 caps. Demonstrate no unsafe retry, no creation of atoms from input, no unseen-source leakage and deterministic re-verification. Examine the 64-signature truncation ordering with the extra chat clause.
- 'Brain.LIFG.StructuralCompetition.status == :preferred' is **not proof that every role has been independently witnessed** (N-037 M4; a preferred reading may have unknown/conflicting roles). Define an independent all-required-role/attachment check for any authoritative semantic projection. Handle adverbs, quantifiers, by-phrases, tense and polarity without treating an unused role as implicitly neutral.
- Recheck N-036's downstream consumers: independent AttributedPredication/verified path for Colony P-021b, no general broadening of 'Composition.verified/1'; avoid accidental new downstream replies, memory writes, global WM/BasalGanglia admission, tool actions, or prompts. Review 'SentenceMeaning.provisional_structure' (sentence_meaning.ex:159-213), response, episodic projection and LiveView observability before allowing chat to receive the new signature.
- **Differential must mean zero change for out-of-scope inputs, not literally identical output on intended new copular positives.** Save byte-for-byte golden results from unchanged baseline across full chat pipeline (including lexical scores/winners, signatures and ordering, final meaning, response, episode/WM effects) for non-copular/negative/injection families. For a separately approved narrow copular positive family, predeclare and verify each **allowed** delta while retaining all safety/provenance invariants; any unexpected diff is a regression. The current 'chart_test.exs:102-122' asserts no non-Colony chart signature and would intentionally fail if chat admission is approved—**replace only under that explicit scope with stronger negative-isolation and positive-admission tests**, never simply drop the invariant to make the test pass.
- Execute targeted, Core and umbrella suites, plus independent frozen evaluation with no sealed feedback leakage. [U] This expansion has not been designed, approved, run or performance-tested.

### 9.6 Conclusion and next authorized action

**[V-code + receipt]** In all four observed seed conflicts the LIFG auxiliary/adjective preferences are compatible with the actual English construction; the SVO-shaped dictionary-alternative frame is the problem. The failure is best classified as **over-generated grammar plus missing chat copular coverage**, not evidence of a useful impasse. **[H]** A legitimate SVO lexical POS correction is still possible in a different, fully witnessed transitive clause, so do not generalize the four negatives into "all POS repair is invalid."

**Recommendation to Bradley:** Keep P-023 unapproved. Use the existing frozen census and a predeclared manual audit of conflict opportunities; separately scope a narrow false-SVO guard with tests only if Bradley approves. Continue the P-021b evidence/proof gates before considering chat chart admission. After those gates, remeasure representative coverage; only revisit Soar-style impasses if genuine, independently validated tie/repair opportunities remain.

**Execution/authorization record:** documentation-only review; no code, fixture, baseline, config, tool permission or sealed corpus changed; no Elixir/Mix tests executed by GPT-6. Existing 18 targeted / 2029 Core / census exit 0 receipts are **Bradley's runs in §8.10**, not new verification here. No new P-022.2, P-023, SVO guard or chat chart implementation is authorized by this entry.

## 10. R-001 — CLAUDE Response to GPT-6 Section 9 · 2026-11-19 · FROM: CLAUDE — TO: Bradley, GPT-6

**READ-ONLY. Documentation append only.** Source read at `7fe6d4a2`. No Elixir run by Claude; no implementation or test file changed; sealed set not touched. Labels: **[V]** read in the cited source; **[V-receipt]** Bradley's execution; **[H]** hypothesis; **[U]** unknown. Agreement between two AI reviewers is not evidence; where a claim below depends on runtime behaviour, a diagnostic is specified (§10.6, D-1) and its receipt decides.

### 10.1 Verdict

- **CONDITIONAL GO on P-024**: exclude the closed BE paradigm from the predicate slot of *every* transitive-SVO detector, not only finite-present. The condition is Bradley's D-1 receipt confirming the scope.
- **NO GO** on P-023, and on enabling the chart for chat now. Agreed with GPT-6.

### 10.2 Agreement with §9 (independently re-traced)

- **[V] Emitter.** `finite_present_svo/1` (`signatures.ex:1281-1325`), pattern `["determiner","noun","verb","noun"]` (`:55-56`).
  - The POS test runs against the per-word **union** of dictionary rows (`build_lexicon/1` `:153-181`, `base_nodes/2` `:251-291`).
  - `finite_present_verb?` is OR-aggregated across a word's verb rows (`:164-165,174`), with the five-tag check at `:225-234`.
  - Nothing in the detector distinguishes BE from a lexical verb.
- **[V] Pre-LIFG influence is real.** The path is:
  1. `Signatures.attach` → `RoleCompetition.attach` precede LIFG (`core.ex:183-186`);
  2. `PredicateEvidence.attested_present` turns a full-span SVO signature into a `:finite_present_svo_predicate` proposal for the predicate token (`predicate_evidence.ex:259-289`);
  3. `Brain.LIFG.RoleCompetition` accepts that basis (`role_competition.ex:489-515`) and adds subject-noun support (`:129-184`);
  4. Stage1 applies `GrammarEvidence.apply_finite_present` (+0.32, `grammar_evidence.ex:270-331`) or the role boost (+0.30, `grammar_evidence.ex:~250`), with no stacking (`:283-288`).

  **[V] Addition:** these boosts are applied **even on closed-class slates**. `stage1.ex:832-836` skips only cerebellar calibration; the grammar calls at `:839-864` are unconditional. So "is|verb" senses carrying the morphology are boosted against "is|auxiliary".
- **[V] Single-flip arithmetic (§9.2) is correct.** `adjudicate/1` requires `matched_roles >= 2 and matched_roles >= conflicting_roles` (`structural_competition.ex:229-233`), and `@min_preference` is inert (§6.2).
  - For the 4-token false SVO, LIFG choosing "ready|noun" alone gives matched 2 (subject, object) and conflicts 1, so the reading becomes **`:preferred`**. Choosing "is|verb" alone gives the same result.
  - The boost in the previous point pushes toward exactly the "is|verb" flip. The system is one weak lexical decision away from preferring a false parse.
- **Agreed:** a mandatory manual audit rather than P-022.2 now (§9.4); a later, separately approved chat copular path (§9.5); P-023 stays unapproved.

### 10.3 Where §9 is incomplete or wrong

**1. The defect is not confined to finite-present "is"** **[V code; H runtime, pending D-1].**
- `finite_past_svo/1` (`signatures.ex:1331-1371`, D-N-V-D-N) and `finite_past_pronoun_svo/1` (`:1373-1418`, Pron-V-D-N) admit any verb-POS word with `:simple_past` morphology. Neither excludes was/were.
- Their `required_features` give the predicate as `[:simple_past]` (`:1355`), and `Brain.LIFG.StructuralCompetition.expected_pos/1` has **no clause for `:simple_past`** (`structural_competition.ex:240-260`). So **the predicate role is not checked at all**; only subject_head and object_head are compared.
- **Consequence [H, strong]:** "The report was the problem." or "It was the problem." would reach **`:preferred` false SVO with LIFG choosing the correct auxiliary for "was"** (matched 2, conflicts 0), provided LTM gives "was" a verb row with simple-past morphology. Section 9 asserted that "the same mechanisms that emit this hypothesis currently protect against preferring it". For the past family that protection does not exist.
- The 5-token present pattern (`:53-54`) covers "The report is a draft.". Here the object really is a noun, so only the predicate conflicts, giving matched 2 and conflicts 1, which is also **`:preferred`**.

**2. User-visible consequences are reachable [V code path; H runtime].**
- `SentenceMeaning.provisional_structure/2` turns any verified `:preferred` result into a `:structural_hypothesis` (`sentence_meaning.ex:159-213`).
- For a pronoun past clause spanning `{0,3}`, `provisional_past_pronoun_participants/3` (`:217-262`) binds actor/predicate/patient **if** LIFG selected a verb sense for the predicate.
- `SymbolicGeneration.maybe_realize_provisional_past_pronoun/3` (`symbolic_generation.ex:725-775`) then replies "You're saying that it was the problem. I can't tell who "it" refers to.", tagged `:acknowledge_provisional_action`. That treats BE as an action.
- `SVOEvent.project/2` (`svo_event.ex:17-55`) similarly needs noun/verb/noun winners, then canonicalizes a `verb_<lemma>` relation. A flipped "is|verb" plus noun object could produce a `verb_be` event candidate.
- Each step needs LIFG to pick the verb row, which the +0.30/+0.32 boost encourages. Whether it happens in live LTM is **[U]**; D-1 checks it.

**3. The census hides this class [V].**
- A false SVO that reaches `:preferred` yields `meaning.class: :provisional`. That class is **not a failure** in P-022 (`coverage_ladder.ex` `blocker/6`, first clause), by design (§8.1).
- So the frozen census can **under-report** wrong parses as "interpreted". The §9.4 audit must therefore also **sample provisional and interpreted rows**, not only conflict opportunities.

**4. §9.3's regression list misses the downstream consumers** that matter most for this change:
- `provisional_past_pronoun_test.exs`
- `sentence_meaning_participants_test.exs`
- `symbolic_generation_test.exs`
- `structural_statement_test.exs`
- `relational_evidence_test.exs`
- `predicate_evidence_test.exs`
- `declarative_evidence_test.exs` / `declarative_proposition_test.exs` (Colony consumers of these ids)

There are 31 test files that reference these SVO ids or predicate bases (`grep`, §10.7).

### 10.4 Task 2 — which mechanism

| Option | Generalizes? | Breaks valid SVO? | Verdict |
| --- | --- | --- | --- |
| **(A) Exclude BE forms from transitive predicate slots** | **Yes.** BE is a closed paradigm (`am is are was were be been being`); the rule is independent of the noun and adjective vocabulary, so it holds for unseen words | **No.** BE never takes a direct object in English; `has`, `does`, `draws` are untouched | **Recommended** |
| (B) Exclude any word with an `auxiliary` row | Yes | **Yes.** It kills "The artist has a picture." and "The robot does work." | Reject |
| (C) Require a lexical-valency witness (transitive tag) | Poorly | **Yes [V fixture / H LTM].** The existing positive fixture for "draws" carries only form-of morphology tags (`signatures_test.exs:861-880`); Wiktionary form-of rows generally lack transitivity, and the lemma row is not in `active_cells` | Defer: needs lemma resolution and a coverage study |
| (D) Reject the object slot when the object is also attested as an adjective | No principled basis; it would block legitimate noun objects with adjective homographs | Yes | Reject |

**Residual risk of (A), stated plainly [H]:** linking verbs ("seems", "becomes", "remains", "looks", "tastes") plus an adjective with a noun row ("The plan seems ready.", "The soup tastes good.") will still produce false SVO candidates. That set is semi-open and needs (C)-class lexical evidence later. Until then, add those shapes to the census seed controls and the manual audit, and do not claim they are fixed.

### 10.5 Proposed P-024 (needs `Approve: P-024`)

**Change (one file):** in `apps/core/lib/core/syntax/signatures.ex`:
- add a closed `@be_forms ~w(am is are was were be been being)`;
- reject a segment when `predicate.word` is in it, in exactly these detectors:
  - `finite_present_svo/1`
  - `finite_past_svo/1`
  - `finite_past_pronoun_svo/1`
  - `counted_present_svo/1`

`bounded_past_transitives/1` is already restricted to a fixed regular-past list (`:860`). No change to detector order, signature shape, other detectors (progressive, copular relative, WH, modal, chart), Brain, PredicateEvidence, GrammarEvidence or SentenceMeaning. Removing the signature removes every downstream effect at its source.

**FileScope:**
- `apps/core/lib/core/syntax/signatures.ex` (the four predicate conditions + one attribute)
- NEW `apps/core/test/core/syntax/svo_be_predicate_exclusion_test.exs`

**No existing test file is edited.**

**New tests** (real `Signatures.detect/1` output, not hand-built signatures):
- **Negatives.** Use fixtures attesting BE verb rows with the exact morphology, plus noun rows for "ready"/"small":
  - "The report is ready.", "The fly is small.", "The report is a draft.", "The report was the problem.", "It was the problem.", "The reports were the problem."
  - Each must emit no SVO-family id.
  - `PredicateEvidence.detect/1` proposes no predicate at the BE index.
  - `Brain.LIFG.RoleCompetition.resolve/1` gives it no predicate support.
  - `GrammarEvidence.apply_finite_present/3` leaves a "is|verb" score unchanged.
  - `Brain.LIFG.StructuralCompetition.resolve/1` is never `:preferred` on an SVO id.
- **Positives (must be unchanged).** Real dual-POS transitive verbs: "The artist draws a picture.", "The artist has a picture." (has with finite-present verb morphology), "The robot does work.", "He bought some bread.", "The artist drew the picture."
- **Non-SVO BE constructions keep their ids.** "The bird is dancing on the fence." (progressive locative), "Links were lines that refute the claim." (copular relative), "What would you do if you were free?" (modal conditional).

**Acceptance criteria (all required):**
1. The new test file is green.
2. Every existing test is green with **no expectation, fixture or gold change**, in particular:
   - `signatures_test.exs`, `finite_present_wh_roles_test.exs`, `predicate_evidence_test.exs`;
   - `apps/brain/test/brain/lifg/{role_competition,grammar_evidence,stage1_finite_present}_test.exs`;
   - `svo_event_test.exs`, `provisional_past_pronoun_test.exs`, `sentence_meaning_participants_test.exs`, `symbolic_generation_test.exs`, `structural_statement_test.exs`, `relational_evidence_test.exs`;
   - the Colony `declarative_*` tests and the chart tests.

   A red test is diagnosed, not re-baselined.
3. Full Core suite (2029 + new) and the umbrella suite, both exit 0.
4. A **new, separately labelled** census seed run, with the P-022.1 receipts unchanged. Expected: the 4 `attested_role_conflict` rows become `no_full_span_signature:all_tokens_witnessed`, and **no other row changes class**. Any other change is reported and explained before merge is considered final.
5. D-1 rerun: no SVO id for the BE sentences; positives identical.

**Rollback:** revert the single code commit; no data, config or migration is involved.

**Risks:**
- (i) Any downstream behaviour that currently depends on a BE-SVO, including a provisional "You're saying that it was the problem." reply, disappears. That is intended, but it is a visible change.
- (ii) Census and representative numbers shift, so baseline and after must both be reported.
- (iii) The linking-verb residual (§10.4).
- (iv) **[U]** Some LIFG winners for BE tokens may change once the boost source is gone. D-1 before/after shows it.

### 10.6 D-1 diagnostic (read-only; before any P-024 implementation)

Bradley runs the command in Claude's chat reply. For "The report is a draft.", "The report was the problem.", "It was the problem.", "The reports were the problem.", "The artist draws a picture." and "He bought some bread.", it prints:
- the SVO-family signature ids and spans;
- the predicate index;
- the structural status and preferred construction;
- the LIFG chosen id at the predicate;
- the role-competition entry at the predicate;
- the `sentence_meaning` kind, status and source_frame.

**How D-1 decides P-024's scope:**
- **Confirms the full scope** if any BE sentence shows an SVO-family signature, especially a `:preferred` one or an "is/was|verb" LIFG winner.
- **Narrows P-024 to `finite_present_svo/1`** if no past-family BE signature appears (for example, because LTM lacks simple-past morphology on "was"). The change then stays as small as the evidence supports.
- Optionally, Bradley types "It was the problem." in live chat and reports the reply (consequence 2).

### 10.7 Task 4 — census integrity

- **Manual audit, not P-022.2.** Agreed with §9.4, with one addition from §10.3 point 3: the audit sample is **all conflict opportunities plus a random 20% (minimum 10) of rows classed `:provisional` or `:interpreted_correctness_unassessed`**, drawn by `case_id` hash. This must be frozen before the representative run.
- Raw counts, the predeclared screen and the adjudicated counts are reported side by side. No historical receipt is rewritten.

### 10.8 Task 5 — development order (amended)

0. **Collect and freeze corpus v2 now**, under §8.4: ≥ 30 chat rows and ≥ 30 public-social rows. Run the **baseline** census and the audit protocol *before* P-024. Otherwise P-024's effect on representative comprehension cannot be measured; that is the one change I make to GPT-6's sequence.
1. D-1, then P-024 (if approved), then rerun the same frozen v2 corpus. Report the delta per row.
2. P-021b proof and consumer obligations.
3. Design the chat copular path. It must produce the correct construction for exactly the sentences P-024 stops mis-parsing, measured on the same v2 rows.
4. Remeasure representative comprehension.
5. Reconsider Soar-style resolution only if independently adjudicated, genuine repair opportunities remain.

**Objective check:** P-024 will *reduce* accepted interpretations, and that is the point. Its success measure is fewer wrong `:preferred` or `:provisional` readings at zero cost to valid SVO, not more accepted sentences.

**Needed from Bradley:**
- the D-1 receipt;
- then `Approve: P-024`, or a narrowed version, or a rejection;
- the corpus v2 rows.

**Next from GPT-6:** independent review of §10.3 points 1 and 3, the claims that most change the risk picture.

### 10.9 D-1 receipt and amended P-024 scope · 2026-11-19 18:38 MST · recorded by CLAUDE

**RUN BY BRADLEY** (local shell; read-only `mix run -e` with the census resolver options; branch `symbolic_generation`, at or after `67e039e`). Output pasted to Claude.

| Sentence | SVO signature(s) | Structural | LIFG at predicate | Role competition at predicate | Meaning |
| --- | --- | --- | --- | --- | --- |
| The report is a draft. | `finite_present_svo_candidate` `{0,4}` and `{0,3}` | **`:preferred`** (finite_present_svo) | `is\|auxiliary\|0` | `:supported` verb, basis `:finite_present_svo_predicate` | **`:provisional` structural_hypothesis** |
| The report was the problem. | `finite_past_svo_candidate` `{0,4}` | **`:preferred`** (finite_past_svo) | `was\|auxiliary\|0` | none | **`:provisional`** |
| It was the problem. | `finite_past_svo_candidate` `{0,3}` | **`:preferred`** | `was\|auxiliary\|0` | `:supported` verb, basis `:finite_past_svo_predicate` | **`:provisional`** |
| The reports were the problem. | `finite_past_svo_candidate` `{0,4}` | **`:preferred`** | `were\|auxiliary\|0` | `:supported` verb, basis **`:plural_present_predicate`** | **`:provisional`** |
| The artist draws a picture. (control) | `finite_present_svo_candidate` ×2 | `:preferred` | `draws\|verb\|1` | `:supported` verb | `:structurally_interpreted` participant_assertion |
| He bought some bread. (control) | `finite_past_svo_candidate` `{0,3}` | `:preferred` | `bought\|verb\|1` | `:supported` verb | `:provisional` |

**[V-receipt] Findings:**
1. **§10.3 point 1 is confirmed.** All four BE sentences reach a **`:preferred` false SVO** and a `:provisional` structural hypothesis, **while LIFG correctly selected the auxiliary**. The past family's unchecked predicate role, and the 2-vs-1 rule in the present 5-token family, both let the false reading through.
2. **LIFG resisted the pre-LIFG boost in all four cases.** Role competition marked BE as a `:supported` verb predicate, yet the auxiliary won. **[U]** The remaining margin is not shown.
3. **A fifth source: [V-code]** `Core.Syntax.PredicateEvidence.plural_present/3` (`predicate_evidence.ex:190-212`) proposes a verb predicate from cells **directly, without any signature**. It fires on "were" because LTM has a verb row for it with no form-of/past/participle tags (`present_base_verb?/3`, `:400-410`). P-024 must cover it, otherwise "The reports are/were the problem." keeps pre-LIFG verb support.
4. **§10.3 point 3 is confirmed for the census:** every false reading above would be counted as `:provisional`, not as a failure.
5. The controls behave as valid transitive SVO.
6. **[U]** User-visible replies: the provisional past-pronoun reply needs a *verb* LIFG winner (`sentence_meaning.ex:244-245`), which did not occur here. Whether any other response consumer uses these `:structural_hypothesis` meanings is unverified; the optional live-chat check was not run.

**Amended P-024 (supersedes §10.5's FileScope; needs `Approve: P-024`):**
- `apps/core/lib/core/syntax/signatures.ex`: add `@be_forms ~w(am is are was were be been being)` and reject `predicate.word in @be_forms` in `finite_present_svo/1`, `finite_past_svo/1`, `finite_past_pronoun_svo/1` and `counted_present_svo/1`.
- `apps/core/lib/core/syntax/predicate_evidence.ex`: the same closed set, rejecting a BE word at index 2 in `plural_present/3` only.
- NEW `apps/core/test/core/syntax/svo_be_predicate_exclusion_test.exs`: the §10.5 tests, plus "The reports are the problem." and "The reports were the problem." through `PredicateEvidence.detect/1`, and a valid plural-present control ("The artists draw pictures.").

**Out of scope, still:**
- modal and imperative predicate producers ("You should be ready", "Be quiet"). Here "be" is the clause's main (copular) verb, not a transitive predicate. They are listed for the audit, not changed;
- linking verbs;
- Brain files, response code, chart.

**Acceptance:** as in §10.5, plus a **D-1 rerun**. All four BE sentences must show no SVO-family signature, no `:preferred` SVO structural result, no verb-predicate role at the BE index, and no SVO-sourced provisional meaning. Both controls must be identical to this receipt.

### 10.10 P-024 approval and implementation · 2026-11-19 · FROM: CLAUDE — TO: Bradley, GPT-6

**Bradley, verbatim (18:40 America/Whitehorse):** `approve: P-024`. Code commit `f2cefb9`, limited to the amended FileScope in §10.9:
- `apps/core/lib/core/syntax/signatures.ex`: `@be_forms` plus `not be_form?(predicate)` in `finite_present_svo/1`, `finite_past_svo/1`, `finite_past_pronoun_svo/1` and `counted_present_svo/1`. Helper `be_form?/1`.
- `apps/core/lib/core/syntax/predicate_evidence.ex`: `@be_forms` plus one `with` guard in `plural_present/3`.
- NEW `apps/core/test/core/syntax/svo_be_predicate_exclusion_test.exs`: 7 tests.

**Not changed:** no existing test, fixture or expectation; no Brain file, response code, chart, detector order or other detector. **Status: UNTESTED** (no Elixir runtime for Claude).

**Test design:**
- Every BE negative has a **counterfactual twin**: the same sentence with the BE token replaced by a nonce verb that copies the BE verb rows exactly. The twin must still be detected or proposed. A twin failure means the fixture, not P-024, is wrong.
- The tests cover:
  - signature absence for six BE sentences;
  - no pre-LIFG predicate proposal or verb role at the BE index, including "The reports are/were the problem.";
  - `PredicateEvidence.plural_present` (BE negatives plus the "The artists draw pictures" control);
  - unchanged Stage1 boosts for a "be" sense, but a boosted real verb;
  - no `:preferred` SVO structure for "The report was the problem." with correct LIFG choices;
  - SVO kept for draws / has / does / bought / drew;
  - the "draws" control still `:preferred`.

**Commands for Bradley (umbrella root):**
```bash
git rev-parse --short HEAD
mix test apps/core/test/core/syntax/svo_be_predicate_exclusion_test.exs --trace; echo "exit=$?"
mix test apps/core/test/core/syntax apps/core/test/core/semantic apps/core/test/core/response apps/core/test/core/social apps/brain/test/brain/lifg; echo "exit=$?"
mix test apps/core/test; echo "exit=$?"
mix test; echo "exit=$?"
mix run scripts/comprehension_coverage_census.exs --arm all --out tmp/r001_census_seed_p024.json; echo "exit=$?"
```
Then rerun the D-1 command (§10.6).

**Acceptance:**
- the new file shows 7 passed;
- every existing suite is green **with no expectation changes**;
- Core = 2029 + 7 = 2036 if nothing else changed;
- the umbrella suite exits 0;
- the census seed run is a **new, separately labelled run**. Expected: the 4 `attested_role_conflict` rows become `no_full_span_signature`, and any other changed row is reported;
- D-1: no SVO signature, `:preferred` SVO or SVO-sourced provisional meaning for the four BE sentences, with both controls identical to §10.9.

**Rollback:** `git revert f2cefb9`.

**Note on order:** §10.8 recommended a representative v2 baseline *before* P-024. That baseline can still be produced afterwards by running the frozen v2 corpus with `git checkout 06f4460` (the pre-P-024 code), then rerunning at the P-024 commit.

### 10.11 Receipt: P-024 targeted tests and D-1 rerun · 2026-11-19 18:56–18:58 PST · recorded by CLAUDE

**RUN BY BRADLEY** (Termux on his phone, after `git pull`; Bradley confirmed the pull landed at the P-024 head, `dec5f57`). Claude did not execute anything.

**1. Targeted test file.**
- Command: `mix test apps/core/test/core/syntax/svo_be_predicate_exclusion_test.exs --trace`
- Result: **7 tests, 0 failures**, exit 0. All 7 test names are listed, including the counterfactual twins.

**2. D-1 rerun** (the §10.6 command):

| Sentence | SVO signatures | Structural | Predicate role | Meaning |
| --- | --- | --- | --- | --- |
| The report is a draft. | `[]` | `:no_complete_candidate` | none | `:not_yet_interpreted` / `:unknown` |
| The report was the problem. | `[]` | `:no_complete_candidate` | none | `:not_yet_interpreted` / `:unknown` |
| It was the problem. | `[]` | `:no_complete_candidate` | none | `:not_yet_interpreted` / `:unknown` |
| The reports were the problem. | `[]` | `:no_complete_candidate` | none | `:not_yet_interpreted` / `:unknown` |
| The artist draws a picture. | finite_present_svo `{0,4}`, `{0,3}` | `:preferred` | `:supported` verb | `:structurally_interpreted` participant_assertion |
| He bought some bread. | finite_past_svo `{0,3}` | `:preferred` | `:supported` verb | `:provisional` structural_hypothesis |

**[V-receipt]:**
- All four BE sentences lost the false SVO signature, the `:preferred` structure, the pre-LIFG verb-predicate role and the SVO-sourced provisional meaning.
- Both controls are **field-for-field identical** to §10.9.
- The BE sentences now abstain honestly. They will be interpreted only when chat gains a real copular construction (§10.8, step 3).

**Still required before P-024 is fully VERIFIED:**
- the related suites (`syntax`, `semantic`, `response`, `social`, `brain/lifg`);
- the full Core suite (expected 2036);
- the umbrella `mix test`;
- the labelled census seed rerun.

Bradley will run these on a desktop. **P-024 status: targeted tests and D-1 VERIFIED BY BRADLEY; regression suites pending.**

### 10.12 Receipt: P-024 regression suites and census · 2026-11-19 20:11–20:13 MST · recorded by CLAUDE

**RUN BY BRADLEY** (desktop, `pop-os`) after `git pull`, at **`56ad7e3c`** (printed by `git rev-parse`). An earlier desktop run at `fa57bc82` was **pre-P-024** and is *not* a P-024 receipt. Its Core 2029 and unchanged census merely reconfirm the P-022.1 baseline.

| Command | Result |
| --- | --- |
| `mix test apps/core/test/core/syntax … apps/brain/test/brain/lifg` | brain **237**, core **1209** (pre-P-024 run: 1202; +7), exit 0 |
| `mix test apps/core/test` | **2036 passed** (2029 + 7), exit 0 |
| `mix test` (umbrella) | db 90, llm 58, brain 719 (3 skipped), core 2036, symbrella 1, web 213, exit 0 |
| `mix run scripts/comprehension_coverage_census.exs --arm all --out tmp/r001_census_seed_p024.json` | revision `56ad7e3c`, corpus `1a3c2e5a…cde7b2`, `probe_errors: []`, exit 0 |

**Census delta: separately labelled run "P-024 seed"**, compared with the P-022.1 receipt (§8.10), which stays unchanged:

| Measure | P-022.1 (`fa57bc82`) | P-024 (`56ad7e3c`) |
| --- | --- | --- |
| `no_full_span_signature` | 10 | **15** |
| `structural_unresolved_single_candidate` | 5 (4 `attested_role_conflict`) | **0** |
| conflict / tie opportunities | 4 / 0 | **0 / 0** |
| policy abstentions | 5 | 5 (unchanged) |
| provisional (Colony chart) | 2 | 2 (unchanged) |
| chat interpreted | 2 | 2 (unchanged) |

**[V-receipt] Change beyond the §10.10 prediction:**
- One **Colony** row also moved from `structural_unresolved_single_candidate` to `no_full_span_signature`. It is the Colony `noun_verb_ambiguity` row ("The record is old."; Colony `no_full_span` 2 → 3, unresolved 1 → 0).
- **Explanation [V code]:** `Signatures.detect/1` runs the chat detectors on Colony input as well, and only the chart is Colony-only (`signatures.ex:112`). The same false `finite_present_svo` reading of "is" was being built and rejected there.
- No other row changed class.

**P-024 status: VERIFIED BY BRADLEY.** That covers the targeted tests (§10.11), D-1 (§10.11), the related suites, full Core, the umbrella suite and the census.

**Observation (seed only; decides nothing):** every eligible seed failure is now `no_full_span_signature:all_tokens_witnessed`: missing grammar with full lexical coverage. No tie or conflict opportunities remain.

**Next (§10.8):**
1. Corpus v2 representative rows, then the baseline census. A pre-P-024 baseline is still available via `git checkout 06f4460`.
2. P-021b proof and consumer obligations.
3. Design the chat copular path.

## 11. R-001/P-024 — GPT-6 independent source, test and sequencing review · 2026-11-19 · FROM: GPT-6 — TO: Bradley, Claude

**Scope:** documentation append only, READ-ONLY investigation of branch `symbolic_generation` at `e77b55464f4b77914349fb7c6a13ae7ccb9f432f`. Inspected the actual `f2cefb9` code diff, current source, P-024 tests, §10 through §10.12, and N-035/N-036/N-037. **No implementation, test, fixture, existing baseline, new branch, sealed evaluation or `mix format` was touched.** I did not run Elixir/Mix. **[V-code]** = directly verified in source/diff, **[V-receipt]** = Bradley's recorded execution output in §10.9–§10.12, not an execution by GPT-6, **[H]** = proposed scenario/interpretation requiring tests, **[U]** = not verified. Agreement with Claude is not evidence.

### 11.1 Verdict and residual BE paths — question 1

**CONDITIONAL GO for P-024 as the specifically scoped SVO correction. NO claim that it globally prevents BE from entering every predicate slot.**

- **[V-code] `Signatures.detect/1` still runs its original construction inventory for *all* sources** (`apps/core/lib/core/syntax/signatures.ex:81-114`). Only the additional chart is gated for Colony at `:116-122`. The P-024 BE membership table is `:63-67`; `be_form?/1` is `:1775-1776`. The new guard is in `finite_present_svo/1` (`:1307-1309`), `finite_past_svo/1` (`:1346-1351`), `finite_past_pronoun_svo/1` (`:1389-1396`) and `counted_present_svo/1` (`:1442-1447`). The bounded regular-past transitive uses an independent, fixed surface list at `:73-75,855-871` that contains no BE form. Therefore **BE cannot reach the four targeted SVO signature emitters or the bounded regular-past SVO emitter on these unchanged surfaces**, even if dictionary POS/morphology says verb. These guards do not prohibit non-BE lexical transitive verbs.
- **[V-code] The signature-independent `PredicateEvidence.plural_present/3` is fixed** at `apps/core/lib/core/syntax/predicate_evidence.ex:192-215`, particularly `:197` before verb attestation. `counted_present/4` (`:217-260`), `attested_present/4` (`:262-293`) and `attested_past/4` (`:295-340`) depend on the now-excluded SVO signatures. Given legitimately generated signatures, these paths cannot independently resurrect BE-as-SVO.
- **[V-code] NEW residual route missed by §10:** `PredicateEvidence.counted_negative_transitives/3` at `predicate_evidence.ex:133-167` tests `did not` and requires only a verb dictionary witness at token index **4**, with no `@be_forms` exclusion or SVO signature dependency (`:143-160`). **[H]** A deliberately ungrammatical case such as `Three reports did not be the problem` can generate `:counted_negative_transitive_predicate` support if `did`, `not`, `be`, nominal and determiner rows are attested. This does **not** produce a P-024 SVO signature by itself, but it defeats any *global* invariant of “BE can never receive a transitive predicate proposal.” The exact live-LTM outcome is **[U]**. Recommend a separate, minimal **P-024.1 proposal** to test this path and, only if confirmed, guard the explicitly transitive producer; no change is authorized now.
- **[V-code] Other BE predicate proposals remain possible by design:** `PredicateEvidence.modal_questions/3` (`:62-101`), `modal_statements/3` (`:103-127`) and `imperatives/3` (`:169-188`) accept dictionary-attested verbs in generic predicate positions without valency checking. `Be a leader` and `You should be a leader` are valid *copular* uses, not evidence of a transitive direct object. **Do not blindly extend the BE blacklist to these branches.** Instead require a future semantic consumer to distinguish copular predicate/complement from transitive verb/object, keeping modal/imperative chat authority bounded. Also `Signatures.verb_particle_constructions/1` (`:1661-1736`) retains general verb-POS hypotheses; not a finite SVO emitter.
- **[V-code] Other finite-past constructions expose `:simple_past` features**, including `finite_past_named_directional_candidate` (`signatures.ex:1077-1123`); these are not the four P-024 SVO detectors. **[U]** Whether a BE homograph yields another semantically wrong complete non-SVO parse is a separate corpus question. Do not claim an exhaustive exclusion of all wrong BE constructions.

**Legitimate constructions accidentally blocked?** **[V-code/H interpretation]** Within a standard English *transitive direct-object* SVO, finite forms of BE are copular or auxiliaries, not direct-object-taking lexical verbs. Excluding this closed BE set from those dedicated detectors is appropriate. P-024 does not remove progressive or copular-relative signatures (`signatures.ex:784-849,1126-1173`), and D-1 retained the controls. **[U]** It has not yet demonstrated correctness for all dialectal, nonstandard, quoted/metalinguistic, or larger constructions. In particular, this is not a generic solution for semi-open linking-verb vocabulary (`seems`, `tastes`, `remains`).

### 11.2 Counterfactual-twin test review — question 2

**[V-code]** `apps/core/test/core/syntax/svo_be_predicate_exclusion_test.exs:106-147` replaces each BE token with a nonce spelling, carrying **the same verb-row semantic tags**. That twin still emits an SVO signature while the original does not, which convincingly controls for missing inflection/POS *within these syntactic emitters*. The tests are more meaningful than merely checking an absent result on one fixture. The source also tests predicate proposals (`:150-190`), targeted grammar boosts (`:193-216`), one Brain structural rejection (`:218-235`) and genuine auxiliary/verb homographs `has` and `does` as transitive positives (`:237-263`). **[V-receipt]** Bradley ran all 7 targeted tests successfully at the P-024 head (§10.11) and D-1 with four fixed cases and two preserved controls; his Core 2036 and full umbrella pass at `56ad7e3c` are recorded in §10.12.

**Limits, not failures of P-024:**
1. **[V-code]** `counterfactual/3` (`:106-116`) copies only verb rows into the twin, **not** the BE auxiliary row or the entire original active-cell slate. Thus it isolates the lexical surface for the detector's relevant verb/morphology evidence, **not** the complete LIFG causal effect. Add an exact full-slate renamed counterfactual if this is later presented as a neural/LIFG causal proof.
2. **[V-code]** The six BE negatives exercise `is`, `was`, `were`; `are` is tested against the signature-independent plural rule, not as a complete BE paradigm matrix. `am/be/been/being` are not individually checked, nor are counted-plural SVO BE negatives with fabricated matching morphology. New invariants should be construction-sensitive rather than asserting these nonsensical shapes are valid English.
3. **[V-code]** The structural test at `:218-235` covers one BE case and only refutes **preferred SVO**, not all wrong `:preferred` structures or a downstream fabricated provisional meaning. D-1 exercises four live cases, but there is no fixture-based end-to-end `SentenceMeaning`/response/episodic negative matrix for every source.
4. **[V-code]** `GrammarEvidence` checks in `:193-216` call two selected scoring functions on synthetic scores, not all actual Stage1 contributions. A green test does not certify every possible grammar or lexical prior is unchanged.
5. **[H] Required for a future narrower regression ticket:** independently validate the still-open counted-negative BE route with a proper positive control; source-matrix tests for chat/Colony preserving intended chart behavior; an end-to-end false-provisional/no-episodic regression; a non-BE transitive lexical homograph control; and a representative linking-verb adversarial case. Protect the existing seven tests **unchanged** unless Bradley approves otherwise. Do not silently rebaseline to make a test pass.

### 11.3 The extra Colony seed row — question 3

**I ACCEPT CLAUDE'S CODE-LEVEL EXPLANATION.** **[V-code]** `Signatures.detect/1` builds `existing` (including `finite_present_svo/1`) **unconditionally**, then prepends a Colony-only `Chart.clause_signatures` at `signatures.ex:89-122`. The Colony `SemanticProbe` explicitly sets `:source = :colony_external` and calls `Signatures.attach` before resolving structure (`apps/core/lib/core/social/semantic_probe.ex:50-55,87-90,122-140`). Therefore removing an SVO signature for `is` **necessarily affects both source arms**, not just chat. **[V-receipt]** §10.12 recorded that the Colony `The record is old.` row changed from one unresolved full-span candidate to no full span after P-024. **[H, strongly supported]** The removed `finite_present_svo_candidate` generated from the alternate noun POS for `old` is the direct explanation. **[U]** I did not capture that row's full pre/post signature list or chart `diagnose` reason; those would be needed to independently prove this exact intermediate path rather than rely on the aggregate. No code change is needed to explain the source scope.

**Census warning [V-code]:** the P-022 ladder treats provisional meanings as interpreted without independent correctness proof (`coverage_ladder.ex:43-44,230-234`). Bradley's D-1 pre-fix false preferred readings were classified as provisional (§10.9). Thus a representative census needs the agreed audit sample of provisional/interpreted rows as well as explicit conflicts; raw output, the historical frozen screen, and independently adjudicated corrections must remain separate.

### 11.4 Residual priorities and ticket boundaries — question 4

1. **[V-code/H recommendation] Structural evidence completeness is the priority before authoritative copular meaning.** `Brain.LIFG.StructuralCompetition.expected_pos/1` at `apps/brain/lib/brain/lifg/structural_competition.ex:240-260` lacks `:simple_past` **and** `:pos_adverb` (and does not check every `required_feature` type), so those roles are omitted from lexical comparisons; the preferred rule at `:218-237` accepts as few as two matching roles even with conflicts. **Simply adding `:simple_past -> ["verb"]` is NOT sufficient:** a 2-matched / 1-conflicting candidate can still become `:preferred`. A *separate reviewed proof-admission/evidence-completeness ticket* (tentatively **P-025**, not authorized) should establish complete required-role coverage, independent morphology and lexical IDs, and a no-conflict/no-unresolved gate at the point of proof promotion. Do not globally change Brain's ranking without a before/after differential and own approval. This is a **P-021b acceptance precondition** and must be satisfied before exposing copular meaning to chat.
2. **[H] Linking verbs** (`seems ready`, `tastes good`, `becomes available`) are a known false-SVO possibility if their surface inflection and complement's alternative noun POS are attested. Do **not** open an unbounded lemma blacklist ticket before measuring occurrence and lexical valency evidence in corpus v2. Include adversarial seed/audit examples. Revisit with a separately scoped lexical verb-argument classification study if the false-promotion rate warrants it.
3. **[V-code/H] Modal/imperative `be`** can be a legitimate copular **verb predicate**, so suppressing every BE `:predicate` proposal would damage real sentences. No standalone blanket-BE ticket. Later chart/chat design must separate `copular_predicate` from `transitive_predicate` and require appropriate complement types, with modal and imperative handling gated explicitly. Treat these sentences as unsupported by the narrow chart if necessary, not as evidence for an SVO action.
4. **[V-code/H] Newly found counted-negative BE route:** propose **P-024.1 diagnostic/regression and optional narrow guard** only after Bradley's authorization. It is a more immediate, syntactically isolated candidate than broad linking-verb handling, but is **not** evidence the four P-024 corrections failed.

### 11.5 Next sequence, outstanding P-021b obligations and chat gate — question 5

**Recommendation:** **(i) corpus v2 representative sample and audit protocol → (ii) P-021b proof/consumer readiness (including scoped residual hardening) → (iii) distinct design/approval for chat copular expansion**. P-023 remains NO GO. Do not change the frozen P-022.1 corpus definitions or rewrite old receipts. **[V]** `docs/collab/r001/census_corpus.md:5-14` specifies ≥30 independently selected Bradley-chat rows and ≥30 consecutive public-social/Colony rows, chosen before output, versioned/committed with corpus SHA-256. **[H]** Because P-024 already landed before a representative v2 baseline, measure v2 first on this exact post-P-024 SHA, and describe any historical pre-P-024 replay as a *separate* controlled retrospective comparison; never call it a prospectively collected pre-P-024 baseline.

**P-021b obligations (not completed merely by P-024's green regression):**
- **[V-doc/code] L1/L2/L3 separation + typed witnessed lexical constants.** `docs/collab/N-035-p021-design.md:47-55,133-171` requires open-vocabulary bounded binary lexical witnesses, typed replayable predication, exact structural signature/spans, `RoleWitness`/LIFG attestation and independently verified proof steps; grammatical L1, L2 supported proposition and L3 *attributed external report* must remain distinct. N-035 §4.3 additionally requires absence of unlicensed negation/modal/subordinator, controlled tense/polarity, no token-to-atom conversion, no factual authority, explicit unresolved quantifier/pronoun handling.
- **[V-doc/code] Complete role verification, not `:preferred` alone.** `N-037:62-68,83-93` requires zero conflicting and unresolved **required** roles, independent revalidation of all lexical roles including adverb/quantifier/determiner/PP, an honest polarity cue (`:no_negation_cue_in_closed_list`) and **grammar/rule version replay**. Current chart signature has *no version field* at `apps/core/lib/core/syntax/chart.ex:598-621`; current structural role comparison skips unsupported kinds. [H] Versioning should be independently scoped and designed before a durable proof claims replay stability. A dual adjective/participle complement must be stative only, never a fabricated passive event/agent.
- **[V-doc] N-037 other gating issues.** `N-037:49-56,68-71,75-93`: the lexical ambiguity width-one cap must be **measured** (not merely relabelled), adverb roles documented as unchecked, ambiguity/attachment preserved, optional PP proper-name mismatch and duplicate feature readings explicitly accepted or corrected, re-detection cost measured before adding proof verifiers. `N-037:107-109` calls for a full regression and tightly controlled sealed scoring at the appropriate authorization gate, scored on actual Chart/Brain structural outcome rather than `SentenceMeaning.status`.
- **[V-doc] Separate Colony proof verifier and non-ambiguous status.** `N-036-p021b-consumer-audit.md:29-54` **supersedes** N-035's early plan to broaden `Composition.verified/1` and reuse `:attributed_structural_interpretation`. Use a dedicated `AttributedPredication.verified/1` path; keep `Composition.derive/verified` and `SentenceMeaning` unchanged; return a **distinct** `:attributed_predication` status. Otherwise `social/colony_cognition.ex:180-212` may reject legacy counted-title flows by falsely reconciling a new proposition against the claim relation. Preserve the existing `ReportedProposition` fallback and all existing counted-title behavior. This path is Colony-only and observational, **not** authorization for chat or replies.
- **[V-doc] Full downstream consumer, provenance and action isolation.** `N-036:18-54` names closed-match response/ledger/WM readers, generic `SentenceMeaning.attach_compositional_evidence` and `LexicalRoleShadow` hazards, equality-based revalidation, and `ColonyCognition` action/draft/prompt restrictions. Require new-kind closed-reader table tests, a byte-identical legacy counted-title test, sanitized **witnessed** lemma binaries (never raw external spans in prompts), no model prompt, publish, BG/WM admission, durable memory, or action from a new source interpretation. A separate UI status counter is needed only if Bradley approves visibility.
- **[U/status]** No P-021b implementation authorization, completed independently verified proof gate, real-probe LTM coverage census, or sealed P-021b receipt is established by §10–§10.12. P-024's Core/umbrella pass is strong evidence for *that patch*, not proof of the still-unimplemented P-021b contracts. Preserve the original sealed set and its at-most-twice / counts-only rules in `N-035:206-211`; never inspect it for this review.

**Before scoping a chat copular implementation [H proposed mandatory criteria]:**
1. Finish or expressly reconcile P-021b's L1/L2/L3 proof and read-only source boundaries; first establish strict all-role/morphology verification and missing-feature behavior, with a stable replay/grammar version story.
2. Produce an approved chat-source contract and intended construction families. Both `Signatures.detect/1` and `Chart.diagnose/4` currently gate chart output to `:colony_external` (`signatures.ex:116-122`, `chart.ex:44-70`). Do not fake `si.source` to bypass the latter.
3. Freeze a **pre-change differential** with byte-identical output required for **out-of-scope** chat turns across lexical choices/scores, signatures and ordering, provisional meaning, responses and side effects; permit only *predeclared* expected differences on narrowly defined new copular positives. `apps/core/test/core/syntax/chart_test.exs:102-122` currently asserts no non-Colony chart; any change to that test is an explicitly approved contract amendment, not a silent deletion.
4. Recheck the existing copular chart's negation/number/quantifier/PP/past-participle, correct abstention and budget (48-token / 6-per-span / 400-total / 12-result) gates; correct candidate-vs-proof distinction, no hidden atom creation, no new memory/action or external-fact authority, and preserve genuine attachment ambiguity.
5. Require independent reviewed positive/negative/ambiguous and representative examples, no increase in wrong-confident interpretations, a full umbrella regression and exact before/after runtime trace/latency evidence. Source-specific semantic proof and attributed external report must **not** become user truth merely because chat shares the grammar.

**Final recommendation:** The causal evidence supports coverage-first; P-024 is a successful *bounded grammar hardening*, not a completed semantic architecture. Collect the representative corpus next; independently test the residual counted-negative BE proposal and the unchecked past-feature issue; finish P-021b evidence and consumer gates; only then propose a narrowly scoped chat copular design. Bradley retains sole approval authority. **No new implementation ticket or sealed evaluation is authorized by this document.**

**Verification attribution:** Claude's §10 records Bradley at `56ad7e3c`: P-024 targeted 7/7, related brain 237 + core 1209, Core 2036, full umbrella db 90 / llm 58 / brain 719 (3 skipped) / core 2036 / symbrella 1 / web 213 and a successful P-024 seed census, all exit 0. I reviewed these receipts but **did not execute** them. This append has no new test/compile exit codes.

## 12. CLAUDE independent review of GPT-6 §11 and P-021b readiness · 2026-11-19 · FROM: CLAUDE — TO: Bradley, GPT-6

**READ-ONLY. Documentation append only.** Source read at `8a93b910`. Claude ran no Elixir, changed no implementation or test file and did not touch the sealed set.

Labels:
- **[V]** read in the cited source;
- **[V-receipt]** Bradley's execution recorded in this log;
- **[H]** hypothesis;
- **[U]** unknown.

Agreement with GPT-6 is not treated as evidence.

### 12.1 Executive verdict

- **No P-024.1 now.** The counted-negative residual (§12.2) is real in code, but on inspection it produces correct POS support with a mislabelled basis, and no downstream promotion. It is a documentation and label defect, not a correctness defect. A diagnostic (D-2) is offered; any guard can wait for a later grammar ticket.
- **CONDITIONAL GO on a small P-025 before P-021b:** an observability-only report of unchecked roles in Brain, plus a single strict admission predicate in Core. There is no ranking change, and no existing consumer is switched (§12.3).
- **GO on corpus v2 collection**, under the protocol in §12.4, once Bradley authorizes the sources. It runs on the current post-P-024 head.
- **P-021b: NO GO for implementation today; CONDITIONAL GO on the scoped proposal in §12.6**, after P-025 and the v2 baseline.
- Chat copular, P-023 and sealed runs: **NO GO**, unchanged.

### 12.2 Task 1: the counted-negative BE route

**Q1. Can the current code produce the proposal?**
- **[V-code] Yes, in code.** `PredicateEvidence.counted_negative_transitives/3` (`predicate_evidence.ex:133-165`) requires all of:
  - exactly 7 base tokens (last = 6);
  - a count word or 1..999;
  - the surfaces `did` at 2 and `not` at 3;
  - a closed determiner at 5;
  - dictionary witnesses: noun at 1, verb at 2, adverb at 3, **verb at 4**, determiner at 5, noun at 6.

  Nothing excludes BE at index 4.
- **[H, likely]** "Three reports did not be the problem." would emit `:counted_negative_transitive_predicate` at index 4 if LTM attests `did|verb`, `not|adverb` and `be|verb` rows. Not run; **D-2** below settles it.

**Q2. Can it influence LIFG?**
- **[V-code] Yes, as POS support.** `Brain.LIFG.RoleCompetition` accepts the basis (`role_competition.ex:506`). `GrammarEvidence` then adds the role boost when the evidence list matches (`grammar_evidence.ex:129,240-247`), and `SenseFocus` focuses the verb POS (`sense_focus.ex:175,204`).
- **But** "be" after do-support is lexically a verb. Ungrammatical as the sentence is, `verb` is the right part of speech for it. The error is that the basis *name* claims "transitive". The POS it supports is not wrong.

**Q3. Can anything downstream promote the incorrect reading?** **[V-code] No path found.**
- No `Signatures` detector emits a counted-negative clause (grep: no `did`/negative-transitive signature; the only `did`-related code is the WH/aux question family `signatures.ex:492-567`). So `StructuralCompetition` has no candidate to prefer.
- `DeclarativeProposition`'s negated shapes (`declarative_proposition.ex:190-221`) accept only `verify|support|refute|contradict` (`:302-309`).
- Without an SVO signature, `SVOEvent` and the participant assertion cannot fire, because they need an SVO frame. That last step is **[H]**: inferred from §10's trace of `svo_event.ex:17-55`, not re-run.

**Q4. P-024.1?** **No, not as its own ticket.**
- The input is ungrammatical, the supported POS is correct, and nothing promotes it.
- If wanted, a one-line `@be_forms` guard plus test can ride along with the next approved grammar ticket.
- **Disagreement with §11.4 point 4:** GPT-6 ranks it "more immediate". I rank it lowest of the residuals.

**Q5. Positive controls, if a guard is ever added:**
- "Three links did not support the claims." (`predicate_evidence_test.exs`, COLONY counted-negative case);
- the `declarative_proposition_test.exs`, `colony_cognition_test.exs`, `colony_draft_policy_test.exs` and `observation_bridge_test.exs` negated-transitive cases (grep: 11 references);
- a non-BE twin "Three reports did not support the problem." that must keep the proposal.

**D-2 (optional, read-only):** for "Three reports did not be the problem.", print `role_competition.roles[4]`, the LIFG choice at 4, `sentence_signatures` ids and `sentence_meaning` kind/status. Expected: a `:supported` verb role, no clause signature, `:not_yet_interpreted`. If `sentence_meaning` is ever not `:not_yet_interpreted`, the conclusion above is wrong.

### 12.3 Task 2: structural proof admission (ranking versus boundary)

**[V] Weaknesses are as GPT-6 states:**
- `expected_pos/1` has no clause for `:simple_past`, `:pos_adverb` or quantifier features (`structural_competition.ex:240-260`). Those roles are **silently skipped**: they are not counted in `checked`, so they are neither matched nor unresolved.
- `adjudicate/1` prefers a candidate with `matched >= 2 and matched >= conflicting` (`:229-233`).

**[V] But the existing L2 boundary already re-witnesses every role, and that is the evidence for putting the fix at the boundary:**
- `Composition.event_derivation/2` requires `:preferred` with an equal construction, span and roles (`composition.ex:159-170`). Then *every* role is re-resolved through `RoleWitness.resolve/4` with an exact POS (`:174-187`; nominal proofs `:309-330`). `RoleWitness` requires exactly one LIFG choice at the index and an attested selected id (`role_witness.ex:19-46`).
- The participant assertion requires `EpisodicProjection.validated_participant/1`, i.e. LIFG noun/verb/noun winners (`sentence_meaning.ex:725-760`).
- **The only consumer of bare `:preferred` is `SentenceMeaning.provisional_structure/2`** (L1, `:provisional`, `factual_authority?: false`, `sentence_meaning.ex:159-213`). Its readers are `passage.ex:141`, `passage_antecedents.ex:68`, `integration/audit.ex:96,137`, `integration/coverage.ex:83`, `semantic_strategy.ex:315`, `symbolic_generation.ex:734` and the census. The one reply path additionally demands a selected verb witness (`sentence_meaning.ex:244-245`).

**Recommendation (P-025, not authorized):**
1. **Do not change ranking.** Do not touch `adjudicate/1`, and do not add `:simple_past`/adverb to `expected_pos/1`. Adding them would change which past and adverbial candidates are preferred. That is a behaviour change to be measured on v2 first, under its own ticket.
2. **Brain, observability only:** add `unchecked_roles: [role]` to each candidate summary, listing the roles whose required features map to no POS. Status and preferred selection must be byte-identical; a test asserts this on the existing fixtures.
3. **Core, one strict predicate:** `Core.Syntax.StructuralCompetition.admitted/1` returns `{:ok, preferred}` only when `verified/1` holds **and** `conflicting_roles == 0`, `unresolved_roles == 0` and `unchecked_roles == []`. A declared alternative witness may cover an unchecked role, e.g. `:simple_past` covered by signature morphology evidence plus a `RoleWitness` verb at that index. Otherwise it returns `:not_admitted` with a closed reason.
4. **No consumer switched in P-025.** `admitted/1` is the mandatory entry gate for P-021b's new verifier. Whether `provisional_structure/2` should require it is a separate, measured decision: the census already records `structural.preferred_conflicting_roles` per row, so v2 can count preferred-with-conflicts directly.

**Must P-025 precede P-021b?** Yes, as a dependency of P-021b's verifier. It touches a Brain file, and P-021b is meant to be Core/Colony only, so keeping it separate keeps both FileScopes clean. **Partial disagreement with §11.4 point 1:** GPT-6 frames it as a precondition for *chat copular meaning*. It is needed earlier, for *any* new L2/L3 proof.

**P-025 FileScope (proposal):**
- `apps/brain/lib/brain/lifg/structural_competition.ex` (the additive field)
- `apps/core/lib/core/syntax/structural_competition.ex` (`admitted/1`)
- NEW `apps/brain/test/brain/lifg/structural_unchecked_roles_test.exs`
- NEW `apps/core/test/core/syntax/structural_admission_test.exs`

**Acceptance:**
- byte-identical `status`/`preferred` on every existing Brain and Core test;
- `admitted/1` rejects a 2-matched/1-conflict candidate, a candidate with an unresolved role, and a candidate with an unchecked adverb or simple-past role without an alternative witness;
- it admits "The artist draws a picture." with complete witnesses;
- full Core and umbrella green.

### 12.4 Task 3: corpus v2 collection and audit protocol

**Do not invent rows.** Use deterministic extraction from real sources. Claude authors no v2 rows.

| Arm | Source **[V]** | Selection rule (frozen before extraction) |
| --- | --- | --- |
| chat (≥ 30, target 40) | `episodes` table: `Db.Episode` has `role`, `source`, `sentence`, timestamps (`db/episode.ex:39-58`) | User-role rows with `inserted_at` < cutoff *T* (Bradley picks *T*, recorded), most recent first. Drop exact duplicates, rows equal to any v1 seed, P-021 DEV/sealed text or test fixture, and rows Bradley excludes for privacy. **Exclusions are counted, never replaced.** |
| colony (≥ 30, target 40) | `Llm.Colony.Client.posts/1` or `feed/1` (read-only GET; `client.ex:33-54`) | A single fetch at time *T₂*, consecutive items in returned order, title text only. Record the endpoint. **Note [V]:** `feed/1` is `/feed/for-you`, a personalized ranking, so `posts/1` is preferred if it is chronological **[U]**. |

**Fixed rules:**
- One row per input (a chat turn or a post title), never split or edited.
- Personal names and private details in chat rows are replaced by a fixed placeholder token (`Sam`/`Riverton`), with the replacement count recorded. Otherwise the row is excluded, never paraphrased.
- Families are labelled by Bradley **after** extraction and **before** any census run. Labels are for reporting only.

**Privacy and storage:**
- Chat rows stay in a **local, untracked** file (e.g. `log/r001_census_v2_chat.md`). Only its SHA-256, the row count and the exclusion counts are committed.
- Colony rows (public) may live in a new `docs/collab/r001/census_corpus_v2_colony.md`.
- v1 (`census_corpus.md`, SHA `1a3c2e5a…`) is not edited.

**Freeze:** commit or record both SHA-256s **before** the first v2 run. The run uses the current head, post-P-024.

**Predeclared audit (Bradley, plus a second reviewer if available; whoever implements a future fix may not author labels):**
- **All** tie/conflict opportunities.
- **All** rows with `structural.status == :preferred` and `preferred_conflicting_roles > 0`.
- A sample of `max(10, 20%)` of rows classed `:provisional` or `:interpreted_correctness_unassessed`. Rows are ordered by `sha256(case_id)` and the first *k* taken, so the selection is fixed by the IDs, not by inspection.
- Label set: `correct`, `wrong`, `ambiguous_should_abstain`, `correct_abstention`, `wrong_abstention`. Disagreements are reported, not forced.

**Report, kept as separate tables:**
1. lexical coverage;
2. structural coverage (full span, preferred, preferred-with-conflicts);
3. typed composition;
4. abstention (policy versus budget versus failure);
5. **audited correctness**, with denominators.

The raw P-022 screen is reported unchanged next to the adjudicated counts.

**Needed from Bradley:**
- (a) authorization to run read-only extraction queries against `episodes` and one Colony read;
- (b) the cutoffs *T* and *T₂*;
- (c) the privacy rule: are local-only chat rows acceptable?
- (d) who audits.

Extraction is a read-only `mix run -e` and needs no repo code. Claude will supply it on request.

### 12.5 Task 4: P-021b readiness matrix

| Obligation | Status | Evidence |
| --- | --- | --- |
| L1 chart, Colony-gated (P-021a) | **Implemented and verified (suites)** | chart tests green in every Core run since `b4ac29cf` **[V-receipt]**. Sealed: **not run** |
| P-021a.1 fixes (B1 honest cue, B2 agreement, M1 prepend, M3 differential) | **Implemented; suite-verified** | code `5555a38` (archive §25); `:no_negation_cue_in_closed_list` at `chart.ex:603` **[V]**. No per-test P-021a.1 receipt recorded **[U]** |
| `grammar_version` on signatures (replay stability) | **Not implemented; decision open** | `chart.ex:598-621` has no version field **[V]** |
| M2 lexical-ambiguity width measured | **Not measured** | N-037 §4 **[U]** |
| Complete-role admission (no conflict / unresolved / unchecked) | **Designed, not implemented** | N-037 §5.1. Proposed P-025 (§12.3) |
| Adverb / simple-past / quantifier roles witnessed | **Known gap** | `brain structural_competition.ex:240-260` **[V]** |
| Witnessed lexical constants `{:lexical, kind, witness_id}` | **Designed, not implemented** | N-035 §4.1. No match in `composition/*.ex` **[V grep]** |
| `kind_predication` operator, proof and §4.3 verifier invariants | **Designed, not implemented** | N-035 §4.2-4.4 |
| `AttributedPredication.verified/1` + distinct `:attributed_predication` status | **Designed, not implemented** | N-036 §3 items 1-2. No module **[V grep]** |
| `Composition.derive/verified` and `SentenceMeaning` unchanged | **Holds today; needs a P-021b regression test** | N-036 §3 item 2 |
| Legacy counted-title flow byte-identical | **Designed; test not written** | N-036 Class D |
| Class A closed-matcher table test | **Designed; not written** | N-036 §3 item 5 |
| Witnessed-lemma-only fields; no atoms or raw spans | **Designed** | N-035 §4.1, N-036 §3 item 3 |
| No prompt / publish / BG / WM / durable memory for the new kind | **Holds today by the `claim_for_title` gate** | N-036 Class D **[V per N-036]**; test needed |
| Colony `:copular_predication` surfacing as `:provisional` via `provisional_structure` | **Existing behaviour; decision open** | N-037 M4. Scoring must use the structural outcome |
| Real-probe LTM coverage | **Partially available** | the P-022 Colony arm runs the real probe with real LTM **[V]**; representative v2 not run |
| Sealed P-021b run | **Not run** | at most twice, counts only (N-035:206-211) |

### 12.6 Smallest defensible P-021b proposal (not authorized; after P-025 and the v2 baseline)

**FileScope** (N-035 §4.7 as amended by N-036 §3):
- `composition/expression.ex`: `{:lexical, kind, witness_id}` term and `{:kind_predication, tense, S, C}`, with the binary-constant regex.
- `composition/lexical.ex`: witness-bound constants.
- NEW `composition/predication.ex`: `compose/1` and `verify/1`, pure and deterministic.
- NEW `apps/core/lib/core/social/attributed_predication.ex`: `interpret/1` and `verified/1`. Status `:attributed_predication`; it requires `source: :colony_external`, `StructuralCompetition.admitted/1` (P-025) and `Predication.verify/1`.
- `social/reported_proposition.ex`: one fallback clause after `DeclarativeProposition`.
- NEW tests: `predication_test.exs`, `attributed_predication_test.exs`, `p021b_consumer_isolation_test.exs`.
- **Not touched:** `composition.ex` dispatch, `Composition.verified/1`, `SentenceMeaning`, `ColonyCognition`, `ColonyDraftPolicy`, response, Ledger, WM, web.

**Acceptance:**
1. The §4.3 invariants as tests, each with a negative.
2. `AttributedPredication.verified/1` rejects:
   - every P-021a.1 adversarial row;
   - a 2-of-3 conflicted candidate;
   - an unchecked adverb role;
   - a dual adjective/participle complement read as a passive event;
   - any negation, modal, question or subordinator token.
3. A byte-identical legacy counted-title Colony flow.
4. The Class A table test: no plan, focus, ledger record or realization.
5. `Composition.verified/1` returns `:not_applicable` for the new kind, and chat is byte-identical on the chart differential.
6. No atom is created from input; witnessed binaries only.
7. Full umbrella green.
8. The sealed run **at most once at this gate**, counts only, scored on the structural and verifier outcome.

### 12.7 Task 5: development sequence

1. **C: corpus v2 extraction and baseline census** on the current head. It needs no code, only authorization. It tells us whether copular coverage is actually the dominant failure in real traffic before more work is built on that assumption.
2. **B: P-025** (observability plus the admission predicate). It is small, with zero behaviour change, and it is the gate any trustworthy L2/L3 proof needs.
3. **D: P-021b**, as in §12.6. This is Colony-only and observational.
4. Only then, a separately approved chat copular design. Its evidence is the v2 rows that P-024 now abstains on.
5. **A (P-024.1):** fold into any later grammar ticket. Not a standalone step.

**Ordering constraints:**
- 1 must precede 4.
- 2 must precede 3.
- 1 and 2 are independent and may run in parallel.

**Why this order:** it measures before building, adds verification infrastructure before new proof kinds, and adds no new special-case patterns. The only grammar change in this sequence is P-021b's typed predication, which is general over witnessed vocabulary.

### 12.8 Decisions requiring Bradley

1. Authorize the v2 extraction: the `episodes` read, one Colony read, cutoffs *T* and *T₂*, the privacy rule, and the auditor.
2. Approve P-025 (§12.3 FileScope) or reject it.
3. Whether D-2 is worth running (optional).
4. Whether `provisional_structure/2` should eventually require `admitted/1`. Defer until v2 counts preferred-with-conflicts.
5. Whether a `grammar_version` field is required before P-021b (my view: yes, as a P-021b acceptance item, since §4.3 invariant 1 replays the proof).

**STOP.** Documentation handoff only. GPT-6 reviews next; Bradley decides.

## 13. CLAUDE review of the P-025 admission contract and corpus v2 readiness · 2026-11-19 · FROM: CLAUDE — TO: Bradley, GPT-6

**READ-ONLY. Documentation append only.** Source read at `2e29a653`. Claude ran nothing: no queries, no Colony fetch, no corpus files, no sealed material, and no implementation or test change.

Labels:
- **[V]** read in the cited source;
- **[H]** hypothesis;
- **[U]** unknown.

### 13.1 Verdict

- **CONDITIONAL GO on a revised, Core-only P-025** (§13.3–13.4). It needs **no Brain change**. My §12.3 proposal (option A) is **withdrawn**: GPT-6 is right that it contradicted itself.
- **GO on the corpus v2 procedure in §13.5**, after Bradley's authorization and decisions (§13.6). The commands are published here for inspection only and have **not been run**.
- **P-025 is not strictly required before P-021b,** but its contract is. It is cleaner as its own small ticket (§13.7).

### 13.2 Task 1: the observability contradiction

**[V] GPT-6 is correct.**
- `Brain.LIFG.StructuralCompetition.resolve/1` returns `preferred: preferred` where `preferred` *is* a candidate map, along with `candidates: candidates` (`brain structural_competition.ex:54-64`). Candidates are built in `assess/3` (`:150-215`).
- Adding `unchecked_roles` therefore changes the `resolve/1` value even when every decision is identical.
- It also bypasses `version: 1` (`:57`) without a bump, which is a silent contract change.
- **Exposure [V grep]:**
  - `Core.Syntax.StructuralCompetition.verified/1` (`core structural_competition.ex:66-86`) compares the attached map with a fresh `resolve/1`. Both would carry the field, so it stays consistent.
  - `Composition` (`composition.ex:405-415`) uses the same equality.
  - Trace and telemetry use `candidate_summary/1`'s `Map.take` (`:92-97`), so they are unaffected.
  - No test asserts a full candidate map: `lexical_role_evidence` appears only in `coverage_ladder_test.exs`.
  - No persistence of `structural_competition` was found in `db/`, `discourse/` or the hippocampus.
  - Low exposure is still a silent change.

| Option | Compatibility | Regression risk | Traceability | Cost | Architecture |
| --- | --- | --- | --- | --- | --- |
| A. add the field to the candidate | changes the `resolve/1` value; needs a version bump | low but nonzero; every equality guard and future snapshot is affected | in Brain output | ~0 | mixes diagnostics into the decision record |
| B. separate Brain report (`unchecked/1`) | `resolve/1` unchanged | low; touches a Brain file | separate call | one extra pass over ≤12 candidates | duplicates Brain's skip logic in a second Brain function |
| **C. derive in Core from Brain's existing output** | **nothing in Brain changes** | **lowest: no Brain diff** | Core result names its evidence | one map diff per call | **Brain keeps ranking; Core owns admission (matches the guardrails split)** |

**[V] Why C is exact, not an approximation.** Brain records a comparison only for roles whose index is an integer *and* whose `required_features` map to a non-empty POS list (`:156-158`). So, for the preferred candidate:
- **unchecked roles** = the roles of the matching signature's `required_features` (with an integer index) **minus** the roles present in `preferred.lexical_role_evidence`.

Core already has the signature: `sentence_signatures` is revalidated by `detect/1` inside `verified/1`. This uses Brain's *output*, not a re-implementation of `expected_pos/1`. If Brain later maps `:simple_past`, that role appears in the evidence and Core automatically stops reporting it as unchecked.

**Acceptance tests separating the two kinds of equivalence:**
- **Representation equivalence (must hold):**
  - `Brain.LIFG.StructuralCompetition.resolve/1` is `==` to a frozen expectation on the P-024 fixtures ("The artist draws a picture.", "The report was the problem.");
  - `git diff` shows no change under `apps/brain/`.
- **Decision equivalence (must hold):**
  - for every SI in the existing structural and composition tests, `Core.Syntax.StructuralCompetition.verified/1` and `Composition.verified/1` return identical values before and after;
  - `admitted/1` is a new function, so no existing call site changes.

### 13.3 Task 2: strict admission evidence contract

**[V] What RoleWitness does today.**
- `resolve/4` first accepts the exact selected LIFG winner if its POS is expected and its row is attested (`role_witness.ex:19-46,298-312`).
- Otherwise `alternative/7` (`:201-282`) admits, for roles in `@alternative_roles` (temporal connective, auxiliary, predicate, location head, preposition, modifier_1/2; `:13-14`), any **scored** candidate with **score ≥ 0** that is attested with the expected POS:
  - POS-only roles (preposition, modifiers) take the top candidate with `sense_id: nil`;
  - other roles need a unique or strictly highest-scored alternative;
  - the predicate also needs present-participle morphology (`:314-318`).

**[V] The circularity GPT-6 suspected exists in a weaker form.**
- The only gate on an alternative is "the dictionary has a row with this POS, and LIFG scored it". That is the same dictionary-POS evidence the grammar used to propose the signature (`Signatures.build_lexicon/1` unions dictionary POS).
- The ranking *among* alternatives uses Stage1 scores, and those scores include grammar boosts derived from the same signatures (`grammar_evidence.ex:200-331`, applied at `stage1.ex:839-864`).
- So an alternative witness is **not independent of the grammar that requires it**.
- This is pre-existing and bounded: only the existing progressive-event and stacked-nominal proofs use it. It should be measured in the v2 audit, not changed here.

**Evidence hierarchy (for new admissions):**

| Level | Evidence | Independent of the signature? | Role in admission |
| --- | --- | --- | --- |
| E1 | **Selected exact witness**: the LIFG winner at the index has the required POS and an attested row | partly: LIFG integrates context and sense evidence beyond grammar, though grammar boosts can contribute | **necessary** for every checked role |
| E2 | **Scored alternative POS witness** (RoleWitness `:construction_alternative` / `:construction_pos_only`) | **no**: dictionary POS plus grammar-influenced scores | **never sufficient** for admission. A role supported only by E2 counts as a conflict |
| E3 | **Grammatical signature requiring that POS** | it *is* the hypothesis | defines what must be witnessed; **never evidence for its own roles** |
| E4 | **Verified semantic derivation** (a typed proof that replays) | built on admitted roles | can only be built **on top of** an admission; it cannot validate roles retroactively |

**Rule:** admission requires E1 for every required role, plus the declared morphology and agreement coverage below. E2 can keep a *hypothesis* alive (an L1 `:provisional` reading, or an investigation candidate) but cannot make it *verified*. The grammar therefore cannot validate its own unsupported POS alternatives: Brain's comparison uses the selected POS only (`:160-185`), so an E2-only role already appears as `:conflict`.

**Case handling in `admitted/1`:**

| Case | Handling |
| --- | --- |
| Unchecked `:simple_past` role | Admitted only if (a) the signature's `evidence` contains its declared morphology atom (`:attested_simple_past_verb` / `:simple_past` in `required_features`, re-derived by `detect/1`), and (b) the selected LIFG id at that index has POS `verb`. Otherwise `{:abstain, :unchecked_role}` |
| Unchecked adverb or quantifier role | `{:abstain, :unwitnessed_semantic_role}` by default. A caller may pass a closed neutral list (N-037 §5.3), checked against the selected id's surface. No default list in P-025 |
| Conflicting selected POS | `{:abstain, :conflicting_role}`. No E2 rescue |
| Missing lexical witness (no LIFG choice, or nil selected POS) | `{:abstain, :unresolved_role}` |
| Multiple competing complete interpretations | `{:abstain, :competing_complete_interpretation}` if any *other* candidate also has 0 conflicts and 0 unresolved checked roles (genuine ambiguity is preserved, not resolved by margin) |
| Source restriction | The caller **must** pass `sources: [...]`; `Map.get(si, :source)` not in the list gives `{:abstain, :source_not_permitted}`. No default |
| Morphology and agreement | Not re-implemented. They are covered because `verified/1` requires every signature to be in a fresh `Signatures.detect/1` (`core structural_competition.ex:71-74`); the detectors check agreement and morphology. Admission also requires the preferred signature to be found **exactly** (id, span, roles) in `si.sentence_signatures`, otherwise `:signature_not_found` |
| Stale or altered signatures | Inherited from `verified/1` (attached result `==` fresh `resolve/1`; signatures ⊆ fresh detect). Its failure gives `{:abstain, :not_verified}` |
| Deterministic replay | `admitted/1` is pure. The result carries `digest: sha256(:erlang.term_to_binary(premises, [:deterministic]))` over `{construction, span, roles, required_features, evidence, [{role, index, selected_id}]}`, plus `structural_version: expected.version` and `admission_version: 1`. Replaying `admitted/1` must reproduce the digest |

### 13.4 Revised P-025 proposal (not authorized)

**FileScope (Core only):**
- `apps/core/lib/core/syntax/structural_competition.ex`: add `admitted/2` and private helpers. `attach/1`, `verified/1`, `preferred_summary/1` and `candidate_summary/1` stay **unchanged**.
- NEW `apps/core/test/core/syntax/structural_admission_test.exs`.
- No Brain, RoleWitness, Composition, SentenceMeaning, response, Colony or config change.

**Contract:**
```elixir
@spec admitted(map(), keyword()) ::
        {:ok, %{
          construction: atom(), span: {non_neg_integer(), non_neg_integer()},
          roles: map(), source: atom(),
          witnesses: [%{role: atom(), token_index: non_neg_integer(),
                        selected_id: String.t(), pos: String.t(), basis: :selected_exact}],
          morphology_witnesses: [%{role: atom(), token_index: non_neg_integer(),
                                   feature: :simple_past, evidence: atom()}],
          structural_version: pos_integer(), admission_version: 1,
          digest: String.t(), factual_authority?: false}}
        | {:abstain, reason}
# opts: sources: [atom()] (required), neutral_roles: %{role => [String.t()]} (optional, default %{})
# reason ::= :invalid_input | :source_not_permitted | :not_verified | :signature_not_found
#          | :conflicting_role | :unresolved_role | :unchecked_role
#          | :unwitnessed_semantic_role | :competing_complete_interpretation
```
- Missing `sources:` gives `{:abstain, :invalid_input}`. It never raises on malformed SIs.

**Tests.** Real `Signatures.detect/1` + `StructuralCompetition.attach/1` with fixture lexicons, in the P-024 test style:

- **Positive:**
  - "The artist draws a picture." with exact LIFG winners and `sources: [:user]` returns `{:ok, _}`; every required role is in `witnesses`, and the digest is stable across two calls.
  - "He bought some bread." returns `{:ok, _}` with a `:simple_past` morphology witness.
- **Negative:**
  - the same sentence with `sources: [:colony_external]` gives `:source_not_permitted`;
  - `draws|noun` selected gives `:conflicting_role`, even though a `draws|verb` alternative is scored and attested (**no E2 rescue**);
  - a missing choice at the object gives `:unresolved_role`;
  - an injected extra signature or altered `lifg_choices` gives `:not_verified`;
  - a past SVO whose selected predicate is not a verb gives `:unchecked_role`;
  - a construction with an adverb role and no neutral list gives `:unwitnessed_semantic_role`.
- **Adversarial:**
  - two complete candidates, both fully witnessed (constructed from existing detector outputs where possible, otherwise a hand-built SI that `verified/1` accepts), give `:competing_complete_interpretation`;
  - a 2-matched / 1-conflict `:preferred` candidate gives `:conflicting_role` (the exact case §11.4 point 1 worried about);
  - a malformed SI gives `:invalid_input`, with no raise.
- **Compatibility:**
  - Brain `resolve/1` output is unchanged on the P-024 fixtures;
  - `verified/1` results are unchanged for every fixture in the new file;
  - full Core and umbrella suites green with **no expectation changes**.

**Go/no-go:**
- all of the above green;
- `git diff --stat` limited to the two files;
- Core count = previous + new tests.

**Rollback:** revert the single commit. Because no caller exists until P-021b, there is nothing downstream to revert.

### 13.5 Task 4: corpus v2 extraction protocol (for inspection; NOT run)

**Key point, agreeing with GPT-6:** inputs ≠ eligible failures.
- The screen needs ≥ 30 *eligible representative failures* (`coverage_ladder.ex` `@min_eligible_failures 30`). With about 60 inputs it may well not reach that.
- The census already reports `:insufficient_eligible_failures` in that case (`screen/1`). That result is reported as **insufficient evidence**, never as pass or fail.
- **Predeclared extension rule:** if eligible representative failures < 30 after batch 1, extract batch 2 by the same rule (the next-older 40 chat rows; the next 40 Colony posts by offset), up to a cap of 200 inputs per arm. Stop at the first batch that reaches ≥ 30. The decision uses only the failure *count*, never row content or correctness.

**Step 0: discovery (D-3, read-only).** The `episodes.role` and `source` values are **[U]**, so count them first:
```elixir
# NOT RUN: requires Bradley's authorization
import Ecto.Query
Db.all(from e in Db.Episode, group_by: [e.role, e.source], select: {e.role, e.source, count(e.id)})
```

**Step 1: chat extraction (after Bradley names the user role/source values and cutoff T).**
```elixir
# NOT RUN. Prints to a LOCAL, untracked file only; nothing committed.
import Ecto.Query
t = ~N[YYYY-MM-DD HH:MM:SS]                     # Bradley's cutoff
rows = Db.all(from e in Db.Episode,
  where: e.role == ^"<user role>" and e.inserted_at < ^t and not is_nil(e.sentence),
  order_by: [desc: e.inserted_at, desc: e.id], select: {e.id, e.inserted_at, e.sentence}, limit: 400)
# then, in order: trim; drop exact duplicates; drop rows equal to any v1 seed / P-021 DEV / test fixture
# sentence; keep first 40 => batch 1. Write case_id = "C" <> zero-padded rank; record e.id only locally.
```
- Bradley reviews the 40 rows **before any census run** and may *exclude* rows for privacy (the count is recorded, and the next row moves up) or substitute the fixed placeholder `Sam` for personal names (the count is recorded). No paraphrasing.

**Step 2: Colony extraction (cutoff T₂ = fetch time).**
```elixir
# NOT RUN. Read-only GET; chronological per client.ex:139-146 (sort default :newest).
{:ok, page} = Llm.Colony.Client.posts(limit: 40, sort: :newest, offset: 0)
# take post titles in returned order; record post ids, fetch time, endpoint "/posts".
```
- **[U]** The response field names and whether `max_posts` permits 40 are unknown. Inspect the first page shape before freezing.
- `posts/1` (chronological) is preferred over `feed/1` (`/feed/for-you`, personalized).

**Step 3: freeze.**
- Chat rows go in a local `log/r001_census_v2_chat.md` (untracked; `/log/` is gitignored per AGENTS.md).
- Colony rows go in NEW `docs/collab/r001/census_corpus_v2_colony.md`.
- Both use the v1 JSON shape with `"representative": true` and origins `bradley_chat_sample` / `public_social`. Families are labelled by Bradley after extraction and before the run.
- Commit the Colony file. Record both SHA-256s, the cutoffs, the endpoint, the batch number and the exclusion counts in COLLAB. v1 is untouched.

**Step 4: run.** The census script on each file at the current head, saving `--out` to `tmp/`.

**Step 5: audit.** As predeclared in §12.4:
- all tie/conflict opportunities;
- all `:preferred` rows with `preferred_conflicting_roles > 0`;
- `max(10, 20%)` of provisional/interpreted rows, chosen by `sha256(case_id)` order.

Auditor labels: `correct`, `wrong`, `ambiguous_should_abstain`, `correct_abstention`, `wrong_abstention`. Raw screen and audited tables are kept separate.

### 13.6 Decisions for Bradley

1. Authorize D-3 and steps 1–2 (read-only).
2. Choose cutoffs *T* and *T₂*.
3. Privacy: are local-only chat rows acceptable? Is the placeholder rule acceptable?
4. Auditor(s).
5. Approve the revised P-025 (§13.4) or reject it.
6. Whether `grammar_version` becomes a P-021b acceptance item (my view: yes).

### 13.7 Task 5: sequencing

**Is P-025 a prerequisite for P-021b?**
- **Not strictly.** A source-local check inside `AttributedPredication.verified/1` could implement the same rules.
- **But** the same contract will be needed again: by the chat copular path, and possibly by `provisional_structure/2` (§12.8 item 4). Two hand-written copies of "what counts as witnessed" would drift.
- A Core-only P-025 is small, testable on today's SVO constructions with zero behaviour change, and shrinks P-021b's review surface.

**Recommendation: keep it separate.** Either way, P-021b's acceptance must include `admitted/2` semantics.

**Sequence.** GPT-6's list is justified with one change: **P-025 can be implemented in parallel with corpus v2 steps 1–4.** It has no dependency on the corpus, and the corpus has none on it.
- Measure (corpus v2 baseline and audit) ∥ P-025.
- Then review.
- Then P-021b, using `admitted/2`.
- Then a separately approved chat copular design.
- Then reconsider Soar only if audited, genuine repair opportunities remain.

**Dependencies:**
- corpus v2 before the chat copular design;
- P-025 contract before the P-021b verifier.

### 13.8 Recommended next step

Bradley runs D-3, the role/source discovery count. It is read-only, a single query, and it unblocks the chat extraction. The cutoffs and the privacy rule can be decided in the same reply. P-025 approval can follow independently.

**STOP.** Documentation handoff only. No implementation authorized.

## 14. Corpus v2 extraction and first representative census · 2026-11-19 20:42–20:53 MST · recorded by CLAUDE

**RUN BY BRADLEY** (desktop). Claude executed nothing and saw no corpus sentences: only counts and census aggregates were shared.

### 14.1 Extraction receipts

**D-3 discovery:** `episodes` grouped by (role, source):
- `{"user","user"}` 7;
- `{nil,"core_introspection"}` 257;
- `{nil,nil}` 452.

The `{nil,nil}` rows run from 2026-09-14 to 2026-11-20. They carry chat intents (unknown 260, ask 72, statement 24, greet 22, define 16, …) and `response_meta` / `response_tone` / `session_id` meta keys. Bradley confirmed they are his chat turns.

**Chat batch 1 (rule fixed beforehand in Claude's chat reply):**
- Population: role `user` OR (role nil AND source nil). `core_introspection` excluded.
- `inserted_at < 2026-11-19 07:00:00 UTC` (before R-001 work), newest first.
- Dedupe by normalized sentence, most recent copy kept.
- Exclude exact matches to v1 seeds / DEV corpus, empty rows and rows over 500 bytes.
- Take 48 candidates.

| Count | Value |
| --- | --- |
| raw | 335 |
| distinct | 150 |
| excluded | 0 |
| written | 48 |

- File: `log/r001_census_v2_chat.md` (**local only, never committed**); episode ids in `log/r001_census_v2_chat_ids.txt`.
- **SHA-256 `abfe081eed46f3b79c8aad80f2ed041d078fb7ba3060096193a5852ef45b63d6`**, as printed by the census.

**Colony batch 1:**
- `Llm.Colony.Client.posts(limit: 25, sort: :newest)` at offsets 0 and 25. The config cap is `max_posts: 25` (`config/llm.exs`); `limit: 40` was correctly refused with `:invalid_limit`.
- Fetched `2026-11-20T03:49:31Z`. Titles only, unedited.
- Dedupe; exclude exact matches to v1/DEV, empty titles and titles over 500 bytes. The first 40 kept.

| Count | Value |
| --- | --- |
| fetched | 50 |
| distinct | 50 |
| excluded | 0 |
| written | 40 |

- `author.user_type` was added afterwards by post-id lookup, without changing row selection: **agent 37, human 3, unknown 0**.
- File: `docs/collab/r001/census_corpus_v2_colony.md` (in Bradley's tree; whether to commit it is pending his decision).
- **SHA-256 `d6f11b11bbb20f44ea3962f49ff729b87ff7c32df9a30297d91217587ec7e39a`**.

**Departures from §13.5, recorded honestly:**
1. The chat file was run with all **48** candidates rather than trimmed to 40. The selection is still deterministic. Whether a privacy review removed any rows or replaced any names is **pending Bradley**.
2. The SHA-256s were recorded at run time, from the census output, rather than committed before the run. **Both files are frozen from this point.**

### 14.2 Census results (revision `56ad7e3c`, exit 0 both, `probe_errors: []`)

| | Chat (48) | Colony (40; 37 agent / 3 human) |
| --- | --- | --- |
| provisional / interpreted | 9 / 1 | 0 / 0 |
| policy or budget abstentions | 0 | **32**: multiple_clauses 16, structured_record 8, instruction_shaped 4, negation 2, interrogative 1, budget 1 |
| eligible failures | **38** | 8 |
| `no_full_span_signature:all_tokens_witnessed` | 27 | 7 |
| `no_full_span_signature:unwitnessed_tokens_present` | 8 | 1 |
| `full_span_not_clause_eligible` | 3 | 0 |
| tie / conflict opportunities | **0 / 0** | 0 / 0 |
| screen | **`:insufficient_eligible_opportunities_in_this_distribution`** (38 eligible ≥ 30; ratio 0.0) | `:insufficient_eligible_failures` (8 < 30) |

**[V-receipt] Interpretation:**
- **Chat (the screen is valid, n = 38):** zero tie or conflict repair opportunities. **Every** eligible failure is missing grammar: 35 have no full-span signature and 3 have a full span that is not clause-eligible. 27 of the 35 had full lexical coverage. This is the evidence R-001 Stage 0 was built to collect. For Bradley's real chat distribution, a Soar-style impasse/repair stage (P-023) has **no observed opportunity**, and coverage is the bottleneck. This is a pre-declared screening heuristic, not proof; the audit is still required.
- **Colony:** the screen is insufficient. Agent-written titles are dominated by deliberate refusals, so the arm currently measures **policy fit for agent-authored titles**, not human social language (3 human rows). Batch 2 (offsets 50+) is allowed by the predeclared extension rule. At about 8 eligible per 40 rows, reaching 30 would need roughly 150 titles. That decision is Bradley's.

### 14.3 Audit (predeclared, §12.4 / §13.5)

- **Chat:** all provisional and interpreted rows (9 + 1 = 10 = `max(10, 20%)`), plus any `:preferred` rows with conflicts. Bradley is the auditor. The labels go into this log by `case_id` only.
- **Colony:** none (no interpreted rows).

**Pending:** the audit labels; the preferred-with-conflicts count; the chat privacy-review counts; the decision on Colony batch 2; whether to commit the Colony file.

### 14.4 Audit result (independent auditor: Bradley) · 2026-11-19 21:04 MST · recorded by CLAUDE

**Preferred-with-conflicts rows** (Bradley's run, chat v2 `abfe081e…`): **6**. Conflict detail came from a read-only `resolve_input` rerun of those rows:

| Row | Preferred construction | Conflicting role: LIFG choice versus expected |
| --- | --- | --- |
| C027 | `finite_past_locative_candidate` | subject_head "bird": adjective vs noun |
| C030 | `progressive_locative_candidate` | predicate "sleeping": adjective vs verb |
| C032 | `progressive_locative_candidate` | predicate "sitting": adjective vs verb |
| C033 | `progressive_anaphoric_locative_candidate` | predicate "sitting": adjective vs verb |
| C036 | `noun_phrase` | modifier_2 "black": noun vs adjective |
| C040 | `progressive_locative_candidate` (matched 2, conflicts 2) | predicate "sleeping": adjective vs verb; preposition "under": noun vs preposition |

**Labels (Bradley, verbatim in substance). Claude did not author or suggest any label.**
- **Structure:** C018, C026, C027, C028, C030, C032, C033, C036, C040, C048: **all 10 `correct`**.
- **Word choice at each conflicting role:** all **7 `grammar right`**, i.e. the LIFG selection was wrong. That covers C027 bird; C030 sleeping; C032 sitting; C033 sitting; C036 black; C040 sleeping and under.

**Findings:**
1. **[V-audit] No wrong-confident structural readings** in this sample after P-024: 10 of 10 interpreted rows have the correct structure.
2. **[V-audit] A real, distinct failure class:** a **correct structure containing wrong LIFG POS selections**. It affects 6 of 10 interpreted rows and 7 roles, and in every case the grammar's expected POS was the right one. This is the **inverse** of P-024, where the grammar was wrong and LIFG right.
3. **[V-code] The existing live repair cannot fix it.** `Core.LIFG.RelationalFeedback` performs same-POS revision only (`relational_feedback.ex:8,19,26`). Every error here needs a POS change.
4. **Correction to my own census definition.** "Opportunity" was counted only among *failed* rows (`coverage_ladder.ex` `summarize/1` → `eligible`). These 6 rows are `:provisional`, so they never entered the count. The chat screen (`0 / 38`) is correct **for failures** but **incomplete as a measure of repair opportunity**. The frozen definition and receipts are left unchanged; this is reported alongside them, not rewritten.
5. **P-025 interaction.** `admitted/2` (§13.3) would reject all 6 with `:conflicting_role`. That is the intended conservative behaviour (they remain observable L1 hypotheses), but it means P-025 alone yields **no verified meaning** for this class.
6. **Sample caveat:** these chat rows are largely Bradley's own construction probes ("the bird … on the fence"). The sample is representative of his real traffic, which includes heavy testing, not of casual conversation.

**Consequence for R-001.** §6.1's NO GO on a general Soar-style cycle stands: failures are grammar coverage. But there is now **one narrow, independently audited candidate**: *structure-guided POS repair inside a correct structure*. It cannot be decided by the grammar alone, because P-024 shows the grammar can be confidently wrong. Any design needs **independent arbitration evidence** that distinguishes the P-024 case (grammar wrong) from the §14.4 case (LIFG wrong).

Candidate evidence, all **[H]**, unproposed:
- the alternative POS row's own morphology, e.g. present-participle `-ing` form-of tags for a progressive predicate (already checked in `role_witness.ex:314-318`);
- a structure that is *complete only with* the alternative;
- the absence of any competing complete reading that agrees with the LIFG choice.

**Open question for GPT-6 (no implementation implied):** can a bounded, observation-only "POS-repair proposal" be specified whose arbitration evidence is demonstrably independent of the grammar's own POS-union lexicon, and which would reject all four pre-P-024 BE conflicts (§8.11) while accepting the seven §14.4 roles? If no such evidence exists, the class stays an honest abstention.

## 15. R-001 corpus v2 / POS conflict — GPT-6 independent review · 2026-11-19 · FROM: GPT-6 — TO: Bradley, Claude

**Authority/scope:** READ-ONLY source inspection of `symbolic_generation` at `f69c8c346fd21f51efa06708f15ce397bd853d1b`; this signed documentation append is the only change. No Elixir/Mix runs, no implementation or test edits, no new branch, no sealed evaluation, no `mix format`. No corpus sentence was requested or viewed beyond those *already quoted* in COLLAB §§8.11 and 14.4. **[V-code]** verified source contract; **[V-receipt]** Bradley's local results recorded in §14; **[V-audit]** Bradley's independent linguistic labels, correctness oracle ONLY; **[H]** hypothesis/proposal; **[U]** unverified runtime cause or outcome. A human gold label is not evidence of the mechanism of a model's choice.

### 15.1 Live POS path and why those seven roles may lose (question 1)

**Verdict: a currently selected wrong POS is not immutable in principle; Stage1 CAN pick the right existing candidate on a different score slate. What is prohibited is an automatic cross-POS postselection correction.**

- **[V-code]** `Core.Syntax.Signatures.build_lexicon/1` unions **all** active-cell POS and morphology witnesses (`apps/core/lib/core/syntax/signatures.ex:159-188`). In chat `Core.phase_four/3` attaches signatures and RoleCompetition **before** LIFG, then ATL, RelationalFeedback and structural ranking (`apps/core/lib/core.ex:179-239`). `Brain.LIFG.Stage1` scores all surviving candidate IDs and selects the full-slate softmax maximum (`apps/brain/lib/brain/lifg/stage1.ex:685-815,932-946`). Therefore the lexicon already makes the alternative POS available, and an *earlier* scoring correction can theoretically select it. **[U]** Actual per-candidate margins for the seven audited roles were not provided.
- **[V-code]** `SenseFocus` focuses *expensive ContextFit only*, never prunes candidates (`apps/brain/lib/brain/lifg/sense_focus.ex:1-12,25-86`). Its enumerated supported-role patterns (`:88-247`) do not directly include the progressive-locative predicate, finite-past-locative predicate, PP preposition or two-adjective `modifier_2` family. **[U]** Other overlapping proposals may have generated a focus for any individual token; inspect the already-existing `choice.sense_focus` trace, don't infer it.
- **[V-code]** The general `GrammarEvidence.apply/4` pass defaults to `grammar_weight: 0.0` (`stage1.ex:235-241,838-844`; `grammar_evidence.ex:55-81`). Its generic signature support includes noun-phrase modifier +0.12 and PP preposition +0.18 but **not** complete progressive-locative predicate support (`grammar_evidence.ex:569-636`). The unconditional `apply_finite_present`, `apply_role_competition`, `apply_copular_relative`, `apply_bounded_past_transitive` passes (`stage1.ex:846-884`) likewise have no dedicated progressive-locative or finite-past-locative predicate basis (`grammar_evidence.ex:91-129,272-331`). Consequently a complete construction can be preferred *later*, while its expected POS did not win Stage1.
- **[V-code — strongest newly identified causal candidate]** `Brain.LIFG.Stage1.PosBias.predicate_adjective_bias/3` adds **+0.34** to an adjective immediately following `is`, which is on the linking-verb list (`apps/brain/lib/brain/lifg/stage1/pos_bias.ex:12-26,59-80,282-305`). This directly applies to the adjective alternatives of the previously quoted `is sleeping`/`is sitting` C030/C032/C033/C040 shapes and is included in the Stage1 base score (`stage1.ex:751-755,793-806`). **[H]** It is a plausible explanation for those LIFG adjective winners, **NOT proof** it crossed the decision margin; the actual logit breakdown is **[U]**. Crucially, the +0.34 bias is sensible for genuine copular `is ADJ` forms such as the already quoted `The report is ready.`. Blindly deleting it would reproduce a P-024-like opposite error.
- **[V-code/U]** C027's noun-versus-adjective subject, C036's two-modifier adjective-versus-noun and C040's `under` preposition-versus-noun require *separate* trace-based diagnoses. A subject proposal from a `:finite_past_candidate` is conditional (`role_competition.ex:72-127`); the multi-modifier NP does not receive an always-on dedicated Stage1 bonus (`grammar_evidence.ex:573-624`); `under` is not among `ClosedClass`'s default function-word entries (`closed_class.ex:25-145,709-729`) and `PrepositionSemantics.adjust/5` is opt-in, default false (`stage1.ex:239-242,926-930`). **[U]** No precise score attribution is proved for any of these specific seven without the per-ID score data.
- **[V-code]** `Core.LIFG.RelationalFeedback.can_revise?/2` enforces `pos_of(old) == pos_of(new)` (`apps/core/lib/core/lifg/relational_feedback.ex:109-125`), as does the Brain evaluation pool (`apps/brain/lib/brain/lifg/relational_feedback.ex:160-175,207-211`). This postselection mechanism cannot change their POS. `RoleWitness.resolve/4` **can** return a scored, attested *alternative role witness* for predicate (morphology-constrained), preposition and two-adjective modifier (`role_witness.ex:13-14,201-282,298-321`), but cannot replace the LIFG winner. General `:subject_head` is not on that alternative-role list. `Core.Agency.LexicalRoleShadow` already reports such possibilities read-only for a limited progressive construction set (`lexical_role_shadow.ex:19-35,105-185`): it is not a generic new arbitration system.
- **[V-code]** `Core.Semantic.ParticipantSpatial.project/2` can accept an attested present-participle **verb dictionary row** despite a non-verb LIFG choice at the predicate (`participant_spatial.ex:27-65,247-257`). This is a bounded structural role representation, **not** an updated lexical winner or general proof that the selected sense is correct. Preserve the distinction.

**Recommended D-4 diagnosis [H; no implementation authorized]:** On *only* already quoted §14.4 cases, have Bradley inspect Stage1's existing `definition_context_evidence` per-candidate `base_logit`, `after_cerebellum_logit`, `after_grammar_logit`, `final_logit`, plus `sense_focus`, `semantic_control`, `spatial_pp`, and candidate identities (`stage1.ex:1024-1063`). Locally compare the chosen POS and grammar-expected alternative, identify which stages move rankings, and test an isolated *scoring-only* counterfactual of the +0.34 adjective-window feature. Disable any learning/state writes and avoid emitting private sentences or sealed rows. Do not claim a universal fix based on the four -ing cases.

### 15.2 Can an observation-only POS-repair proposal have independent arbitration? (question 2)

**Answer: No currently demonstrated clean independent arbiter for all seven audited roles.** The requested “accept all seven, reject all four pre-P-024 BE conflicts” is a valid evaluation target, **not an independently justified decision rule**.

1. **[V-code] Same-evidence circularity.** `Signatures.build_lexicon/1` and `RoleWitness.attested?/5` consult the same `active_cells` POS rows (`signatures.ex:159-188`; `role_witness.ex:298-321`). Stage1 alternative scores can include those same signatures' bonuses (`stage1.ex:838-884`). Two consumers of one dictionary POS-union are *not* two independent witnesses.
2. **[V-code/H] Present-participle form-of morphology** (`role_witness.ex:314-318`) is stronger than generic POS but it also creates the grammar's `:progressive_locative_candidate` (`signatures.ex:1126-1172,1235-1280`). It independently *attests an inflectional fact* only to the extent that the dictionary metadata is a separately verified source; it does **not** independently decide syntactic role or the correct overall construction. It also tells us nothing about C027 noun, C036 adjective or C040 preposition.
3. **[H] “Complete only with alternative” is insufficient**: the pre-P-024 BE-as-SVO false positives were likewise full-span thanks to alternative POS. **[V-code]** P-024 now blocks BE in the affected SVO detectors (`signatures.ex:1307-1309,1346-1351,1389-1396,1442-1447`), so rejecting those BE cases *after* P-024 would primarily validate the original guard, not a proposed arbiter. Do not mistakenly train or calibrate on the four historical negative examples.
4. **[H] “No competing complete LIFG-agreeing reading” is insufficient**: a missing grammar family (the chat copular construction in the pre-P-024 state) can make a *wrong* candidate appear uniquely complete. Absence of a competitor cannot certify truth.
5. **[H] The better future discriminator is an independent construction/argument-frame contract** with independently reviewed morphology, non-SVO copular valency (BE has predicative complement, not a normal direct object), bounded grammatical function-word constraints, and a blinded gold-labelled regression with genuinely ambiguous negative cases. A second heuristic reading the same POS unions is not independent validation. Source/replay/version constraints still apply.

**Bounded shadow spec, if ever investigated [H]:** a candidate + exact selected/alternative IDs and source/morphological provenance, one flagged role per operation, maximum seven proposed roles and eight alternatives per role, no recursive retry, statuses `:eligible_for_review | :insufficient_independent_evidence | :competing_readings | :unsupported_source`, output non-factual/non-authoritative, **no** LIFG mutation, proof admission, WM/memory, tools, live reply change or learning. The existing `LexicalRoleShadow` should be reused rather than duplicating it. No shadow proposal is an oracle, and neither the selected sentence labels nor the sampled corpus should become production heuristics.

### 15.3 Stage1 scoring root-cause versus repair layer (question 3)

**[H] Prefer root-cause measurement and a construction-sensitive Stage1 scoring experiment, not a post-hoc override or a general Soar-like search.** The +0.34 linking-adjective heuristic should be tested against **genuine copular adjective complements** and **progressive verb participles**, with held-out homographs, non-locative progressives, nominal/gerund uses, ambiguous and unsupported constructions. A proposed adjustment may be applied only if independent morphology/role evidence and negative controls demonstrate benefit. It must not hardcode `sleeping`, `sitting`, `bird`, `black`, `under`, or the audit sentences. A single blanket POS boost on any full-span grammar candidate would recreate P-024's harm.

**[V-code] Existing Stage1 records after-grammar and final logit diagnostics (`stage1.ex:1043-1063`).** [H] Use these before adding new instrumentation, calibrating thresholds or changing `grammar_weight`. Any live scoring change needs frozen before/after full-slate traces, held-out correctness labels, no increase in wrong-confident readings, and a separately approved narrow FileScope.

### 15.4 P-025 interaction (question 4)

**[V-code + §13 contract]** Claude's proposed `Core.Syntax.StructuralCompetition.admitted/2` requires **selected exact POS witnesses** and rejects `:conflicting_role` without allowing `RoleWitness` E2 alternatives to self-certify. **[V-audit]** All six interpreted-with-conflicts cases in §14.4 have wrong LIFG POS and correct structural role according to Bradley's audit, so the proposed new gate would abstain for them. This is conservative and correct **for a newly verified L2/L3 semantic claim**. They may continue as L1 provisional explanations. Abstention is not evidence of a wrong grammatical structure.

**Recommendation: P-025 may proceed under its already revised *Core-only*, no-consumer-switch contract; do NOT relax it to accept the seven gold-labelled alternatives.** But revisit claims about “independent” selected POS: Stage1 itself has grammar-conditioned scores, so E1 is a necessary operational witness, not an independent correctness oracle. Distinguish *current selected sense*, *dictionary attestation*, and *external correctness labels*. Also ensure `:simple_past` morphology and all unchecked roles are revalidated per §13; a future revision to ranking or `provisional_structure` is a distinct ticket. **[U]** P-025 is only a proposal; no implementation or receipt is established in §§13–14.4.

### 15.5 Colony batch 2 (question 5)

**[V-receipt]** Colony v2 batch 1 consists of 40 consecutive titles, 37 agent and 3 human. The census found 32 policy/budget abstentions and just eight eligible failures (§14.1–14.2). **[V-code/doc]** §13.5 predeclares an extension if eligible failures are below 30, using the next offset batch and stopping only on counts, with a 200-input-per-arm cap. Accordingly **batch 2 is permitted under the frozen protocol**, and no sentence-selection cherry-picking is allowed.

**Recommendation [H]: don't prioritize batch 2 NOW.** The already observed 37:3 agent/human mix and 32 policy refusals are a valid, decision-relevant distributional finding: the existing Colony source is overwhelmingly structured/agent-authored posts, not human conversational English. Adding more of the same might estimate this *same population's* policy-fit better, but cannot turn it into a representative human-language benchmark. Preserve the existing inconclusive Colony screen, document the source mix, and only continue with unchanged chronological offset sampling if Bradley specifically wants to know whether its eligible-failure rate eventually reaches the threshold. Any human-only comparison requires a **new preregistered corpus arm**, not post-hoc filtering of batch 1.

### 15.6 Revised sequencing and ONE recommended next ticket (question 6)

1. **D-4 diagnostic first, observation-only**, using existing Stage1 per-candidate breakdowns on the already quoted §14.4 cases; no new corpus text and no stateful learning. Audit whether +0.34 and/or the absent construction-level grammar bonus are actually decisive, including the other noun/preposition errors. No sealed or private sentences printed.
2. **Single recommended next ticket: P-026 — Stage1 POS Scoring Differential V1 (proposed, NOT authorized).** Purpose: *controlled, offline/observation-only comparison* of scoring ablations or narrowly gated construction-conditioned scores against frozen real and adversarial nonsealed fixtures; report winners/margins and independently labelled correctness without changing any production winner. Prefer reusing `Stage1`'s existing diagnostics and a new dedicated test/research harness; **draft FileScope** NEW `apps/brain/test/brain/lifg/stage1_pos_scoring_differential_test.exs` and NEW `docs/collab/r001/p026-pos-scoring-differential.md`; only if actually required, a separately justified additional scope for pure scorer exposure. Tests should prove existing production behavior byte-identical, report the predeclared +0.34 ablation effect, protect authentic copular adjective positives and progressive verb, noun-phrase, PP, BE/SVO and ambiguity negatives, and require blind independently reviewed held-out cases. If an ablation needs to mutate source or runtime state, split a further specifically approved proposal instead; no Stage1 production score changes in P-026.
3. **P-025** remains a separate already designed Core-only semantic-proof admission gate, not dependent on P-026. Bradley can approve it independently, but do not use it to disguise the seven known LIFG conflicts as resolved.
4. **P-021b** remains gated on the P-025 contract, outstanding role-/proof-/consumer checks, grammar version/replay and the independently witnessed Colony attribution design; no chat expansion yet.
5. Only after measuring genuine POS selection accuracy and completing semantic proof boundaries, propose a separately controlled **chat copular** scope. Reopen P-023/Soar-style impasse research only if independently vetted conflicts remain that bounded operators can actually resolve.

**No ticket approved by this review.** P-026 is the *single next recommended request for Bradley's decision*; P-025 retains its previously proposed FileScope and separate authorization status. D-4 is a prerequisite diagnostic, not a newly authorized implementation. Do not rewrite frozen P-022/P-024/corpus v2 results, infer unsupplied Stage1 logits, change source authority, or promote offline labels into live cognitive facts. The 38-failure chat screen stays true *for its originally declared denominator*; the six preferred-with-conflicts show that that denominator did not measure post-interpretation POS repair opportunity.

**Independent result:** [V-code] A systematic score/evidence mismatch is plausible and testable; [V-audit] Bradley supplied seven gold-positive grammar-over-LIFG POS conflicts; [U] the actual Stage1 score contributions and a truly independent construction arbiter remain unverified. **Final verdict: CONDITIONAL GO on the narrow diagnostic and offline scoring research; NO GO on automatic cross-POS repair or widened authority.**

## 16. P-027 — LIFG Stage1 confidence calibration (dev-gated) and lexical-gap reply cap · 2026-11-19 21:40 MST · FROM: CLAUDE — TO: Bradley, GPT-6

**Approval:** Bradley, in chat 2026-11-19 21:37 MST: "just make the change to the repo and ill just pull". Recorded here as `Approve: P-027` for the FileScope below; no other scope is implied.

**FileScope:** `apps/brain/lib/brain/lifg/stage1.ex`, `apps/core/lib/core/response/attention_fallback.ex`, `apps/core/test/core/response/attention_fallback_test.exs`, `config/dev.exs`, NEW `scripts/probe_lifg_confidence.exs`, this entry.

### 16.1 Problem
- **[V-code]** `AttentionFallback.lexical_gap_report/5` capped reported uncertain terms at `Enum.take(3)` because `quoted_terms/1` only had 1/2/3-arity clauses; upstream (`Summary @summary_limit 6`, WM `@max_terms 6`, local `@max_repair_terms 6`) all allow 6.
- **[V-code]** Stage1 `top_p` is a temperature-1 softmax over logits that are weighted sums of clamped [0,1] features (weights sum ≈1.0), so the maximum logit gap is ≈1.0; for slate size N the best possible `top_p` is e/(e+N−1) (N=3: 0.58, N=10: 0.23). `@weak_score 0.5` and `acc_p_min 0.65` are unreachable for ordinary polysemous words.
- **[V-code]** `guess_rel_prior/3` parses the sense number from `word|pos|n` but uses POS only, so every same-POS Wiktionary sense gets an identical feature row when `context_fit` is 0.
- **[V-receipt, Bradley, dev, "hey symbrella what are you up to?"]** All 11 `hey` candidates' shown interjection rows had `final_logit 0.5310765679307379`; top_p hey 0.107, what 0.116, you 0.073, up 0.015, to 0.037; Summary `weak: 5/6`.

### 16.2 Change
- `sense_rank_factor/2`: multiplies `rel_prior` by 1/(1 + decay·(n−1)) for Wiktionary sense n ≥ 1 (Populate numbers senses per word+POS in Wiktionary order). Closed-class/entity candidates untouched. Config `:brain, :lifg_sense_rank_decay`, code default 0.0 (off).
- `apply_softmax_temperature/1`: divides logits by `:brain, :lifg_softmax_temperature`, code default 1.0 (off).
- `config/dev.exs` only: decay 0.5, temperature 0.05. **test and prod behaviour are unchanged** (byte-identical scoring when both are at defaults).
- `quoted_terms/1` generalised to N terms; cap now `@max_repair_terms`. The test at `attention_fallback_test.exs:401` explicitly asserted truncation of four supplied terms to three; that expectation encoded the cap Bradley asked to remove, so it was updated to four terms (intentional behaviour change, not a red-to-green edit).

### 16.3 Evidence
- **[V-receipt, Bradley, dev, same sentence, switches on]** top_p hey 0.559, what 0.667, you 0.564, are 1.0, to 0.445, up 0.284; Summary `weak: 1/6`, `uncertain: ["up to"]`.
- **[V-receipt, Bradley]** `mix test apps/brain/test/ apps/core/test/` (switches off in test env): Brain 719 passed, 3 skipped; Core 2035/2036 with the single failure being the pinned-cap test above (pre-update).
- **[U]** Suite with switches enabled in test config not yet run. Claude did not execute Elixir (workspace lacks Elixir 1.18/hex access).

### 16.4 Open items
- **[H]** "up to" needs MWE/construction handling ("what are you up to" → current activity); no sense prior will rescue a 74-sense "up".
- **[H]** Closed-class `hey|interjection|0` / `what|pronoun|0` are skipped when Wiktionary senses exist (`closed_class.ex:209`), and `closed_class_default_candidate` hard-codes activation 0.20 ignoring spec `activation`.
- **[H]** Relationship to P-026: this change is config-gated and leaves production winners identical, so it does not pre-empt P-026's offline differential; enabling it in prod should be measured under P-026-style before/after traces first.

### 16.5 Receipt: P-027 switches enabled in test env, and dev revert · 2026-11-19 21:48–21:52 MST · recorded by CLAUDE
- **[V-receipt, Bradley]** `config/test.exs` temporarily given `lifg_sense_rank_decay 0.5` / `lifg_softmax_temperature 0.05`; `mix test apps/brain/test/ apps/core/test/` at `7c7cecd`: **Brain 711/719 passed, 3 skipped, 8 failed**; Core did not run (raise on Brain failure). Temporary lines reverted with `git checkout config/test.exs`.
- Failures: `SemanticControlTest` :73 and :132 (`low_confidence?` false); `Stage1DistinctSensesTest` :6 (`margin_weak?` false); `ReanalysisTest` :30 (`alt_ids == []`); `PostFinalizationTest` :40 (`flips 0 != 1`); `PosBiasModalPredicateTest` :71 (write verb|1 0.578 vs verb|2 0.422, expected tie); `Stage1ReliabilityTest` :28 (`low_confidence_decisions 0 != 6`); `DefinitionRoleCompatibilityTest` :164 (`bank|noun|1` chosen over context-supported `bank|noun|10`).
- **CLAUDE assessment [H]:** these are genuine regressions, not stale pins: (a) the rank prior overrides real context evidence; (b) temperature 0.05 inflates margins past `margin_thr`, eliminating `alt_ids` and therefore reanalysis; (c) the protected invariant "distinct meanings without disambiguating context stay uncertain" is violated. Claude's earlier claim in §16.2 discussion that context would still dominate is **refuted** by test :164. **[U]** `PosBiasModalPredicateTest` sets `rel_prior` weight 0 yet the verb senses diverged; whether rank leaks via another path or the keyword weights are not applied is unverified.
- **Action (within P-027 FileScope):** `config/dev.exs` reset to decay 0.0 / temperature 1.0. Code knobs remain, default off; dev/test/prod scoring now byte-identical to pre-P-027. The `AttentionFallback` N-term reply change stays.

## 17. P-028 proposal — function-word understanding by POS mass (NOT authorized) · 2026-11-19 · FROM: CLAUDE — TO: Bradley, GPT-6

**Problem [V-receipt §16.1]:** "hey"/"what"/"you" are flagged uncertain because Summary requires a single *sense* top_p ≥ `@weak_score 0.5`, while these words carry 9–17 near-identical same-POS Wiktionary senses. For function/closed-class words, comprehension depends on the selected POS/grammatical function, not on which of the sibling senses won. Raising sense-level confidence (P-027 knobs) fabricates certainty — see §16.5.

**Proposal [H]:**
1. `Brain.LIFG.Stage1`: add an observation-only field `pos_mass` to each choice = sum of post-softmax probabilities of all candidates sharing the chosen candidate's POS. No change to logits, probs, winners, margins, `alt_ids`, `weak?`, `low_confidence?`.
2. `Core.Comprehension.Summary.split_choice_terms/1`: a choice whose chosen POS is in a closed function set (`interjection pronoun determiner auxiliary preposition conjunction particle`) is weak iff `pos_mass < @function_pos_mass_floor` (proposed 0.6) or it is a fallback / relational- / synaptic-uncertain choice. All other POS keep the existing `score < 0.5` rule unchanged.
3. Tests (new file): (a) "hey"/"what" slates with tied same-POS senses and a dominant POS → not weak; (b) mixed-POS slate where the function POS does *not* dominate → weak; (c) content-word "bank" river/finance tie → still weak; (d) Stage1 winners/scores byte-identical with and without the field.

**Draft FileScope:** `apps/brain/lib/brain/lifg/stage1.ex` (add field only), `apps/core/lib/core/comprehension/summary.ex`, NEW `apps/core/test/core/comprehension/function_pos_mass_test.exs`, this log. **[U]** Must first verify `pos_mass` survives from Stage1 to `si.lifg_choices`/`semantic_choices` (the probe in §16.1 showed `:scores` absent there, so some layer compacts choices); if a Core attach/compaction module drops it, its path is added to the FileScope before implementation.

**Not addressed:** "up to" (needs MWE/construction handling, separate ticket); closed-class default candidates skipped when Wiktionary senses exist (`closed_class.ex:209`).

Requires `Approve: P-028`.

## 18. P-028 implementation — function-word understanding by POS mass · 2026-11-19 22:05 MST · FROM: CLAUDE — TO: Bradley, GPT-6

**Approval:** Bradley, chat 2026-11-19 21:57 MST: `Approve: P-028`.

**FileScope (narrowed from §17):** `apps/core/lib/core/comprehension/summary.ex`, NEW `apps/core/test/core/comprehension/function_pos_mass_test.exs`, this entry. **No Brain/Stage1 change.**

**Scope correction [V-code]:** §17 proposed a Stage1 `pos_mass` field. `Core.LIFG.Attach` rebuilds every choice (`attach.ex:132-157`) and can change the final `id`/`pos` from Stage1's winner, but it already carries the full Stage1 posterior as `:candidate_probs` (≤64 candidates; also maintained by `Core.LIFG.RelationalFeedback`). POS mass is therefore computed in Summary from the *final* choice's POS over `:candidate_probs` (falling back to `:scores`). This is more accurate and keeps Brain byte-identical.

**Change:** `Summary.split_choice_terms/1` uses `sense_weak?/2`: when the chosen POS family is in `interjection pronoun determiner auxiliary preposition conjunction particle` **and** a posterior is present, weak iff same-POS mass < `@function_pos_mass_floor 0.6`; otherwise the unchanged `score < 0.5` rule. Fallback, relational- and synaptic-uncertain flags still force weak. `:understood` is unaffected (POS mass is a selection signal, not dictionary verification). `function_pos_mass/1` is public `@doc false` for testing.

**Tests added:** tied-sense hey/what slate not weak; mass = same-POS sum (0.856); non-dominant function POS stays weak; content word "bank" keeps sense rule; no posterior → old rule; relational uncertainty overrides mass.

**Status: UNTESTED.** Claude could not compile or run Elixir (no Elixir 1.18/hex access in its workspace). **[U]** Existing tests should be unaffected because none surveyed supplies `:candidate_probs` on a function-word choice to Summary, but this is not verified. Requested receipts (Bradley): `mix test apps/core/test/core/comprehension/function_pos_mass_test.exs`, then `mix test apps/core/test`, then the live probe on "hey symbrella what are you up to?" (expect `weak` ≤ 1 and `uncertain` without "hey"/"what"; >64-sense words such as "up" have no `:candidate_probs` and keep the old rule).

## 19. P-030 — lexical-confidence census script · 2026-11-20 00:05 PST · FROM: CLAUDE — TO: Bradley, GPT-6

**Approval:** Bradley, chat 2026-11-20 00:01: `Approve: P-030`. **FileScope:** NEW `scripts/lexical_confidence_census.exs`, this entry.

**Why [V-code]:** `Core.Comprehension.CoverageLadder` (P-022 census) never reads Summary `stats.weak`, `degraded?` or `uncertain`, so it cannot measure P-027/P-028 effects. This script reports those fields per row and in aggregate over the same corpus format.

**Design:** same persistence-disabled `Core.resolve_input/2` options as the coverage census, for **both** arms (Colony rows are *not* routed through `SemanticProbe` here, so Colony numbers compare only across runs of this script). Aggregates: weak_rate (pooled and per-row mean), degraded_rate, `lexical_gap_proxy_rate` (`degraded? and uncertain != []`, labelled a proxy: the real AttentionFallback reply also depends on risk/action/topic routing), uncertain/understood term counts, reason frequencies. Output has no sentences or terms.

**Status: UNTESTED** (Claude cannot run Elixir). Requested receipts: run on `log/r001_census_v2_chat.md` and `docs/collab/r001/census_corpus_v2_colony.md` at `b09345aa` (pre-P-027) and at HEAD, using a copy of the script outside the tree for the old revision.

### 19.1 Receipt: P-030 on v1 seed corpus · 2026-11-20 07:35 PST · run by Bradley (Termux/phone), recorded by CLAUDE
- Corpus `r001-census-v1` (24 Claude-authored, **non-representative** rows), SHA `1a3c2e5a…`. `--arm all`. Script from `scripts/` at HEAD `5e1e805`; config decay 0.0 / temperature 1.0 both runs.
- **Before:** `git checkout b09345aa -- apps/ config/` (working tree only; the report's `revision` field still prints HEAD `5e1e8052` because it reads `git rev-parse HEAD`). **After:** clean `5e1e805`. Restored with `git checkout symbolic_generation -- apps/ config/`; `git status` showed only untracked runtime priv files.

| v1 seed (24 rows, 118 LIFG decisions) | before (`b09345aa` code) | after (`5e1e805`) |
| --- | --- | --- |
| weak decisions | 105 (0.890) | **77 (0.653)** |
| mean per-row weak rate | 0.884 | 0.669 |
| degraded rows | 24 (1.000) | 22 (0.917) |
| lexical-gap proxy rows | 24 (1.000) | 22 (0.917) |
| uncertain terms | 59 | 57 |
| understood terms | 1 | 1 |
| errors | 0 | 0 |

- **[H] Interpretation:** P-028 reclassifies 28 function-word decisions as not weak, but most were already hidden by Summary `@low_info_terms`, so uncertain terms fall only 59→57, and most rows remain above `@weak_rate_threshold 0.45`, so only 2/24 leave the degraded/fallback path. Not representative; v2 chat (laptop only) and v2 Colony runs still required. Defect noted: `revision` should also report a dirty-tree flag (follow-up, not in P-030 scope).

## 20. P-031 proposal — neighbour-gloss context for ContextFit (NOT authorized) · 2026-11-20 07:55 PST · FROM: CLAUDE — TO: Bradley, GPT-6

**Evidence:**
- **[V-receipt, Bradley, phone, v1 seed corpus, HEAD `c79641d`]** Over 129 LIFG choices: `all_zero` (every evaluated sense had `effective_context_fit == 0.0`) **106 (82%)**; winner context fit 0.0 **99 (77%)**; `no_evidence` 0. ContextFit carries the largest Stage1 weight (0.40).
- **[V-receipt, Bradley]** `brain_cells`: 1,696,102 rows, **0** with `embedding`; an embedding route is therefore a separate data project.
- **[V-code]** `Brain.LIFG.ContextFit.explain/4` is literal sentence-token overlap (after `@stopwords`, target excluded) with the sense's definition/example/synonyms/atoms/gram_function. Short chat turns rarely share tokens with glosses. Stage1 already holds every token's candidate bucket (`ctx.buckets`); `explain/4` already accepts bounded `:extra_context` (used by RelationalFeedback).

**Proposal [H] (extended-Lesk style, deterministic, gated):**
1. In Stage1, when `:lifg_neighbor_gloss_context?` is true, build per target token an `extra_context` from the **definitions of the first k=2 Wiktionary senses (by sense number) of each other content token** in the sentence (excluding closed-class/function candidates, the target token, fallback/phrase-fallback ids), bounded to ≤ 40 content terms.
2. In ContextFit, neighbour-gloss matches contribute at **0.5×** the strength of literal sentence matches (same convention as related-morphology matches), so literal evidence always dominates.
3. Diagnostics: report which context terms came from neighbour glosses (`:neighbor_gloss_matches`).
4. Config default **off** in code and all envs; enabled only for measurement.

**Acceptance gates (lesson of §16.5):** with the switch **on in `config/test.exs`**, `mix test apps/brain/test/ apps/core/test/` must be fully green, including the 8 tests that failed under P-027 (distinct senses stay uncertain without evidence; `bank|noun|10` river sense wins with river context; reanalysis `alt_ids` survive). Then P-030 census and the context-fit one-liner before/after on v1 (and v2 on the laptop). Target: `all_zero` materially below 82% **without** weak_rate rising.

**Expected limits [H]:** helps content-word sentences ("bank … river"); will **not** fix greetings/wh-questions ("hey symbrella what are you up to?"), whose neighbours are mostly function words — those need P-029 and MWE/construction handling.

**Draft FileScope:** `apps/brain/lib/brain/lifg/stage1.ex`, `apps/brain/lib/brain/lifg/context_fit.ex`, NEW `apps/brain/test/brain/lifg/neighbor_gloss_context_test.exs`, this log. Requires `Approve: P-031`.

## 21. P-031 implementation — neighbour-gloss context (default off) · 2026-11-20 08:10 PST · FROM: CLAUDE — TO: Bradley, GPT-6

**Approval:** Bradley, chat 2026-11-20 07:52: `Approve: P-031`. **FileScope (as §20):** `apps/brain/lib/brain/lifg/stage1.ex`, `apps/brain/lib/brain/lifg/context_fit.ex`, NEW `apps/brain/test/brain/lifg/neighbor_gloss_context_test.exs`, this entry. No config file changed.

**Change:**
- `ContextFit.explain/4` accepts `neighbor_context: [gloss_string]`. Neighbour terms (stopwords, target and literal sentence terms removed; ≤40) that match a sense's evidence fields, and are not already literal matches, add `0.5 × (overlap_strength(literal ++ neighbour) − overlap_strength(literal))` per field, deduplicated across fields by the existing priority. Report gains `:neighbor_gloss_matches` only when neighbour terms exist. Empty/absent option ⇒ identical output.
- `Stage1`: when `neighbor_gloss_context?` (run opt, else `config :brain, :lifg_neighbor_gloss_context?`, default **false**), precomputes per token the definitions of its first 2 Wiktionary senses (lowest sense number; tag ≥ 1; excluding function POS, `ClosedClass.closed_class_candidate?/1`, fallbacks) and passes every other token's glosses (different index and phrase) to `ContextFit`. Off ⇒ original `ContextFit.explain/3` call.

**Tests added (8):** byte-identity when absent; half-strength gain (river gloss → `water` → definition component 0.25); unrelated sense unchanged; no double count of literal terms; neighbour ≤ ½ literal; Stage1 off/default identical; Stage1 on raises the river sense and selects `bank|noun|10`; single-token sentence unchanged when on.

**Status: UNTESTED** (Claude has no Elixir toolchain). **Acceptance gate (§20):** Bradley to run, with `config :brain, :lifg_neighbor_gloss_context?, true` temporarily appended to `config/test.exs`: `mix test apps/brain/test/ apps/core/test/` — must be fully green; then without it (default) — must be fully green. Then P-030 census + context-fit one-liner on v1 with the line temporarily in `config/dev.exs`, before/after.

### 21.1 Receipts and a test-premise fix · 2026-11-20 08:20 PST · run by Bradley (laptop), recorded by CLAUDE
- **[V-receipt]** HEAD `30a34a5`, switch off (default): Brain 727 passed, 3 skipped; Core 2042 passed; exit 0. (An earlier "on" attempt did not modify `config/test.exs`; `git checkout` reported 0 paths, so it was a second off run.)
- **[V-receipt]** Switch on in `config/test.exs`: Brain **726/727**, 3 skipped; Core not run (raise). The single failure was P-031's own test "switched off (default) leaves Stage1 output unchanged": with the config on, the "default" call is on, so it differed from the explicit-off call (on: `bank|noun|10` 0.514 / `bank|noun|1` 0.486; off: 0.5 / 0.5). **Defect in Claude's test premise, not a regression.** Replaced with "explicit opt false disables neighbour context regardless of config" (asserts the exact off-tie). All eight §16.5 sensitivity tests passed with the switch on.
- **[V-receipt]** P-030 census, v1, switch on in dev: weak 77/118 (0.6525), degraded 22/24, proxy 22/24, uncertain 57, **understood 2** (off: identical except understood 1). Context-fit one-liner not yet run with the switch on.
- **[H]** The integration receipt shows why weak_rate barely moves: a real context gain (river sense) shifts top_p only 0.500→0.514 under the temperature-1 softmax (§16.1). Context evidence and softmax calibration are coupled; P-031 alone cannot lift posteriors over `@weak_score`.
- **[V-receipt, Bradley, laptop, 2026-11-20 08:21]** HEAD `37b68c5`, switch **on** appended to `config/test.exs`: `mix test apps/brain/test/ apps/core/test/` → Brain **727 passed, 3 skipped**; Core **2042 passed**; **exit=0**; restored with `git checkout config/test.exs` ("Updated 1 path"). **§20 acceptance gate (suite, switch on) passed.** Remaining: context-fit `all_zero` measurement with the switch on.
- **[V-receipt, Bradley, laptop, 08:22]** Context-fit one-liner, v1, switch on in dev: `choices 129, winner_zero 24 (off: 99), all_zero 13 (off: 106), no_evidence 0`. **[H]** Coverage rose sharply, but neighbour terms may be generic gloss words (noise). Stage1 evidence rows now also expose `:neighbor_gloss_matches` (only when present; within P-031 FileScope item 3, no score change) so the matched terms can be audited before any calibration proposal.
