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
