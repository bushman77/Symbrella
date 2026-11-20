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
