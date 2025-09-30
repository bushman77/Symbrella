# Symbrella Project Guardrails
*(Directory Structure & Approval Protocol — living doc)*  
Last updated: 2025-09-30 01:27

> **Purpose.** A single source of truth we both refer to before any refactor or file replacement, so we don’t mangle the project. This is a **gentle, persistent checklist** for directory layout, module boundaries, and our pair‑programming approval flow.

---

## TL;DR
- **Never ship changes without an explicit approval token:** `Approve: P-###` (e.g., `Approve: P-002`).  
- **One umbrella‑root supervisor**: start everything under `Symbrella.Application`. **No per‑app Application modules.**  
- **Acyclic deps:** `db ← brain ← core ← web` (left depends on nothing to the right).  
- **LIFG lives in `apps/brain`** (symbolic brain); **Core orchestrates** the pipeline and can call `Lexicon`.  
- **LIFG path invariants:** no char‑grams, boundary guard, MWE injection, sorted spans, config defaults set in `test/dev`.
- **Deliverables:** Prefer **downloadable files** or a **single .zip patch pack**; no unsolicited multi‑pane diffs.

---

## Canonical Umbrella Layout
This is the canonical, minimal structure. If a directory/file isn’t listed here, treat it as optional. If we add new dirs, we add them here first.

```
symbrella/
├─ README.md
├─ AGENTS.md
├─ PROJECT-RESUME-PLAYBOOK.md
├─ README-PATCH.md
├─ _config.yml
├─ config/
│  ├─ config.exs
│  ├─ dev.exs
│  ├─ test.exs
├─ apps/
│  ├─ db/                         # Umbrella-wide Repo (Ecto/Postgres)
│  │  ├─ lib/
│  │  │  ├─ db.ex                 # Db (Ecto.Repo)
│  │  │  └─ db/brain_cell.ex      # Db.BrainCell schema
│  │  └─ priv/repo/migrations/
│  ├─ brain/                      # Brain + LIFG (symbolic disambiguation)
│  │  ├─ lib/brain/
│  │  │  ├─ brain.ex              # Brain (GenServer, STM focus, activation log)
│  │  │  ├─ cell.ex               # Brain.Cell (process + data conventions)
│  │  │  ├─ lifg.ex               # Brain.LIFG (core algorithm)
│  │  │  └─ lifg/
│  │  │     └─ boundary_guard.ex  # Brain.LIFG.BoundaryGuard (sanitize/guards)
│  │  ├─ bench/
│  │  │  └─ brain_lifg_bench_v2_1.exs
│  │  └─ test/
│  ├─ core/                       # Orchestration + semantics
│  │  ├─ lib/core/
│  │  │  ├─ core.ex               # public facade (resolve_input/1, etc.)
│  │  │  ├─ semantic_input.ex     # %Core.SemanticInput struct
│  │  │  ├─ tokenizer.ex          # Core.Tokenizer (word-first)
│  │  │  ├─ lifg_input.ex         # Core.LIFG.Input (prepares tokens for LIFG)
│  │  │  ├─ mwe/
│  │  │  │  └─ injector.ex        # Core.MWE.Injector (word-level n-grams)
│  │  │  ├─ intent_resolver.ex    # IntentMatrix + fallback heuristics
│  │  │  ├─ intent_matrix.ex
│  │  │  ├─ runtime_bind.ex       # Runtime binding to active cells (STM→focus)
│  │  │  └─ lexicon.ex            # Core.Lexicon facade (public lookups)
│  │  └─ test/
│  └─ web/                        # Phoenix LiveView app (optional/present)
│     ├─ lib/symbrella_web/
│     │  └─ live/home_live.ex     # snapshot prints, UI pipeline hooks
│     └─ test/
├─ priv/
│  └─ mini_intent/                # model params, if used (hot-loadable)
└─ scripts/                        # ops/dev utilities
```

**Module boundaries**
- **Brain owns LIFG.** Keep parsing/disambiguation primitives here.
- **Core orchestrates**: `tokenize → Brain.stm → Db.ltm → Lexicon.all`, and coordinates intent resolution.
- **Db**: schemas and Repo only; no business logic.
- **Web** presents UI and can pull a snapshot: `GenServer.call(Brain, :snapshot)` **(printing allowed but not mutating)**.

---

## Golden Pipeline
```elixir
phrase
|> Core.Token.tokenize()   # word tokens first; sentence-aware spans
|> Brain.stm()             # short-term focus/activation (processes)
|> Db.ltm()                # long-term memory fetch (rows)
|> Core.Lexicon.all()      # dictionary lookups as needed
```
**Bandwidth principle:** tokenize → check active cells → DB → lexicon (only if still unresolved).

---

## LIFG Path: Definition of Done (DoD) — reference
(*Track progress in code reviews; we’ll mark ✅ as items are completed*)
- [ ] No char-grams in LIFG path (enforced + unit test)
- [ ] Boundary guard (drop non‑word‑boundary substrings unless `mw: true`)
- [ ] MWE injection pass (word-level n‑grams before LIFG)
- [ ] Sense slate in SI (`si.sense_candidates` keyed by token index)
- [ ] Reanalysis fallback (flip to next‑best on integration failure)
- [ ] Telemetry tripwire (log/drop if a char‑gram reaches LIFG)
- [ ] Priming cache (optional; recency boost for recent winners)
- [ ] Invariant tests (spans sorted; no char‑grams; boundary-only unless `mw: true`)
- [ ] Config defaults (test/dev: `tokenizer_mode: :words`, `tokenizer_emit_chargrams: false`)

**Additional invariants**
- Tokens **sorted by start**; spans hydrated from the sentence.
- **ID convention**: `"{word}|{pos}|{sense}"` for `Db.BrainCell.id` (string PK).

**Guard usage rule**
- Keep guards simple. **No complex function calls** (e.g., `String.contains?/2`) inside guards. Precompute booleans before `when`.

---

## Configuration Defaults
Set in `config/test.exs` and `config/dev.exs`:
```elixir
config :core,
  tokenizer_mode: :words,
  tokenizer_emit_chargrams: false
```
Phoenix/Web may print `IO.inspect(GenServer.call(Brain, :snapshot), limit: :infinity)` for visibility **without mutating** state.

---

## Approval Protocol (Pair‑Programming Guardrails)
**Nothing merges or “goes live” without an explicit approval token.**

1) **Proposal**: I present a **patch proposal** with ID `P-###` that includes:
   - **Summary** (what/why), **risk level** (Safe / Risky / Breaking), and **scope**.
   - **Before/After** code excerpts for key files.
   - **File list** with replacements or additions.
   - **Tests** to add/update.
   - **Rollback plan** (git commands).

2) **Deliverable format** (your preference):
   - **Downloadable files** for 1–3 files; otherwise a **`.zip` “patch pack”**: `brain_lifg_P-###.zip`.
   - One module per “pane” if you want preview, but **downloadable artifacts win**.

3) **Approval**: You respond with `Approve: P-###`.  
   - If **not approved**, we do **no file replacements**. We can iterate with `P-###.1` etc.

4) **Special tags** (for visibility):
   - **QuickFix-###**: small, surgical changes.
   - **BREAKING-###**: migrations, API changes, or module moves.
   - **Hotfix-###**: urgent bug fix; still needs `Approve: …` before applying.

5) **After approval** I provide final artifacts only, without extra refactors beyond the approved scope.

**Template**
```text
Patch ID: P-###
Title: <one-line summary>
Risk: Safe | Risky | BREAKING
Files touched:
  - apps/brain/lib/brain/lifg.ex (replace)
  - apps/core/lib/core/tokenizer.ex (edit)
Summary:
  - <plain-language explanation>
Tests:
  - <list of tests added/updated>
Rollback:
  - git checkout HEAD~1 -- <files>  (or)
  - git revert <commit>
```

---

## Module & Dependency Rules
- **One umbrella‑root supervisor**: `Symbrella.Application` starts Brain + others.  
- **No per‑app Application supervisors.**
- **Acyclic dependencies**: `db ← brain ← core ← web`.  
  - Brain can depend on Db. Core can depend on Brain and Db. Web depends on Core/Brain.  
  - **Never** reverse this chain.

---

## Rollback & Recovery (Git Cookbook)
- See file history for one file:  
  `git log --follow -- apps/brain/lib/brain/lifg.ex`
- Show diffs over file history:  
  `git log -p --follow -- apps/brain/lib/brain/lifg.ex`
- Restore a file to main:  
  `git checkout origin/main -- apps/brain/lib/brain/lifg.ex`
- Revert the last commit (create a new inverse commit):  
  `git revert <commit_sha>`
- Discard local changes to a file:  
  `git restore --source=HEAD -- apps/core/lib/core.ex`

---

## Working Session Conventions
- If you say **“hold off until I paste more files”**, I pause changes and only review.
- If the task is time‑boxed or ambiguous, I deliver a **small, safe delta**, plus the proposal for the next step.
- I won’t introduce **new modules/dirs** without a `P-###` that updates this doc’s layout.
- **No hidden background work**: everything ships in the current message.

---

## “Known-Good” Patterns (snapshots)
- **Web print** before DOM push:  
  `IO.inspect(GenServer.call(Brain, :snapshot), limit: :infinity)`
- **Core pipeline** (orchestration only): keep LIFG in Brain; Core may call `Lexicon` as a public facade.

---

### Appendix: Glossary
- **STM**: short‑term memory / active focus in Brain processes.
- **LIFG**: left‑inferior‑frontal‑gyrus (symbolic disambiguation layer).
- **MWE**: multi‑word expression (inserted as tokens prior to LIFG).
- **DoD**: Definition of Done; check before PR approval.

---

**How we’ll use this doc:** Before any refactor or file drop‑in, we sanity‑check against this document. If the change requires updating this document, that update is **part of the patch**.
