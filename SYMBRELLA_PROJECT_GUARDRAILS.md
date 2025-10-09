# Symbrella Project Guardrails
*(Directory Structure & Approval Protocol — living doc)*  
**Last updated: 2025-10-09**

> **Purpose.** A single source of truth we both refer to before any refactor or file replacement, so we don’t mangle the project. This is a gentle, persistent checklist for directory layout, module boundaries, and our pair-programming approval flow.

---

## TL;DR

- **Approval tokens required.** Never ship changes without an explicit token: `---Approve: P-###---` (e.g., `---Approve: P-002---`).
- **Full-File Patch Guardrail.** Before any code is written, paste the entire current file; responses are **full-file replacements only**. Approvals must include FileScope, e.g.  
  `---Approve: P-005 (FileScope: apps/.../file.ex)---`.
- **One umbrella-root supervisor.** Start everything under `Symbrella.Application`. **No per-app Application modules.**
- **Acyclic deps:** `db ← brain ← core ← web` (left depends on nothing to the right).
- **LIFG lives in `apps/brain`.** Core orchestrates the pipeline and can call Lexicon.
- **LIFG path invariants:** no char-grams, boundary guard, MWE injection, sorted spans, config defaults set in test/dev.
- **Deliverables:** Prefer chat-bubble code blocks only (UTF-8, LF). Avoid ZIPs/binaries. Multi-file outputs are sent as consecutive chat bubbles with clear filename headers. **No unsolicited multi-pane diffs.**  
  *(Exception: you may attach a small .zip only when explicitly requested.)*

---

## Current Status Snapshot (Oct 9, 2025)

**LIFG DoD (Definition of Done) — working checklist**

- [ ] No char-grams in LIFG path (**enforced + unit test**)
- [ ] Boundary guard (**drop non-word-boundary substrings unless `mw: true`**)
- [ ] MWE injection pass (**word-level n-grams before LIFG**)
- [✅] Sense slate in SI (**`si.sense_candidates` keyed by token index**)
- [ ] Reanalysis fallback (**flip to next-best on integration failure**)
- [ ] Telemetry tripwire (**log/drop if a char-gram reaches LIFG**)
- [ ] Priming cache (**optional; recency boost for recent winners**)
- [ ] Invariant tests (**spans sorted; no char-grams; boundary-only unless `mw: true`**)
- [ ] Config defaults (**test/dev:** `tokenizer_mode: :words`, `tokenizer_emit_chargrams: false`)

**Notes**
- Telemetry test harness (`Support.TelemetryHelpers`) is in place and green.
- BoundaryGuard + Tripwire modules exist; wiring + tests still to be finalized.
- MWE injector lives in Core; needs full pass + confirmation tests.
- Priming scaffolding exists; not yet engaged as a scored feature in Stage1.

---

## Canonical Umbrella Layout (reflects current repo)

This is the canonical, minimal structure we track. If a directory/file isn’t listed here, treat it as optional. When we add new dirs, **we update this doc first**.

```
symbrella/
├── AGENTS.md
├── PROJECT-RESUME-PLAYBOOK.md
├── README.md
├── README_BRAIN_CHAIN.md
├── README_guardrails_snippet.md
├── SYMBRELLA_PROJECT_GUARDRAILS.md   ← (this file)
├── _config.yml
├── apps
│   ├── brain
│   │   ├── README.md
│   │   ├── bench/
│   │   │   ├── brain_lifg_bench_v2_1.exs
│   │   │   ├── brain_stage1_bench.exs
│   │   │   └── profile_stage1.exs
│   │   ├── lib/brain/
│   │   │   ├── acc.ex
│   │   │   ├── atl.ex
│   │   │   ├── attention.ex
│   │   │   ├── basal_ganglia.ex
│   │   │   ├── cell.ex
│   │   │   ├── episodes/writer.ex
│   │   │   ├── hippocampus/            # config/dup/evidence/normalize/recall/scoring/telemetry/window
│   │   │   ├── hippocampus.ex
│   │   │   ├── lifg/
│   │   │   │   ├── boundary_guard.ex
│   │   │   │   ├── gate.ex
│   │   │   │   ├── guard.ex
│   │   │   │   ├── hygiene.ex
│   │   │   │   ├── input.ex
│   │   │   │   ├── reanalysis.ex
│   │   │   │   ├── stage1.ex
│   │   │   │   └── stage1_guard.ex
│   │   │   ├── lifg.ex
│   │   │   ├── pmtg.ex                  # Posterior MTG (ensure correct file name)
│   │   │   ├── telemetry.ex
│   │   │   ├── utils/{control_signals,numbers,safe,tokens}.ex
│   │   │   └── working_memory.ex
│   │   └── brain.ex
│   ├── core
│   │   ├── README.md
│   │   ├── lib/core/
│   │   │   ├── brain.ex
│   │   │   ├── brain_adapter.ex
│   │   │   ├── brain/index.ex
│   │   │   ├── input.ex
│   │   │   ├── invariants.ex
│   │   │   ├── lex_id.ex
│   │   │   ├── lexicon/{normalize,senses,stage}.ex
│   │   │   ├── lexicon.ex
│   │   │   ├── lifg_input.ex
│   │   │   ├── mwe_injector.ex
│   │   │   ├── neg_cache.ex
│   │   │   ├── phrase_repo/{default.ex}
│   │   │   ├── phrase_repo.ex
│   │   │   ├── recall/{execute,gate,plan}.ex
│   │   │   ├── runtime_bind.ex
│   │   │   ├── segmenter.ex
│   │   │   ├── semantic_input.ex
│   │   │   ├── sense_slate.ex
│   │   │   ├── text.ex
│   │   │   ├── token.ex
│   │   │   ├── token_filters.ex
│   │   │   ├── vectors.ex
│   │   │   └── ../core.ex
│   ├── db
│   │   ├── README.md
│   │   ├── lib/db/
│   │   │   ├── brain_cell.ex
│   │   │   ├── episode.ex
│   │   │   ├── episodes.ex
│   │   │   ├── lexicon.ex
│   │   │   ├── my_embeddibgs.ex         # (typo acknowledged; fix in a future patch)
│   │   │   └── postgrex_types.ex
│   ├── lexicon
│   │   ├── README.md
│   │   └── lib/lexicon/{behaviou.ex,lexicon.ex}   # (typo acknowledged)
│   ├── llm
│   │   ├── README.md
│   │   └── lib/llm.ex
│   ├── symbrella
│   │   └── lib/symbrella/{application.ex,mailer.ex,symbrella.ex}
│   └── symbrella_web
│       ├── assets/{css,js}/...
│       └── lib/symbrella_web/{application.ex,components,controllers,endpoint.ex,live,router.ex,telemetry.ex}
├── assets/css/app.css
├── config/{config.exs,dev.exs,prod.exs,runtime.exs,test.exs}
├── mix.exs
└── mix.lock
```

**Module boundaries**

- **Brain** owns LIFG and WM/episodic coordination. Keep parsing/disambiguation primitives here.
- **Core** orchestrates: tokenize → Brain.STM → Db.LTM → Lexicon; handles SI, MWE injection, invariants.
- **Db** provides Ecto schemas and Repo. No business logic.
- **Web** presents UI; may read snapshot via `GenServer.call(Brain, :snapshot)` for visibility (no mutation).

---

## Golden Pipeline

```
phrase
|> Core.Token.tokenize()   # word tokens first; sentence-aware spans
|> Brain.stm()             # short-term focus/activation (processes)
|> Db.ltm()                # long-term memory fetch (rows)
|> Core.Lexicon.all()      # dictionary lookups as needed
```

**Bandwidth principle:** tokenize → check active cells → DB → lexicon (only if still unresolved).

---

## Hippocampus Options (runtime)

- `window_keep` (int, default `300`): Maximum episodes retained in the rolling window.
- `half_life_ms` (int, default `300_000`): Recency half-life (ms) for recall scoring. Higher = slower decay.
- `recall_limit` (int, default `3`): Default top-K returned by `recall/2` when `:limit` isn’t specified.
- `min_jaccard` (float `0.0–1.0`, default `0.0`): Minimum raw Jaccard overlap to consider an episode (before recency).
- `scope` (map, optional): When passed to `recall/2`, only episodes whose `episode.meta` contain every `{k, v}` in scope are eligible. Atom/string keys match case-sensitively without creating new atoms.

**Operational notes**

- **Dedup-on-write:** consecutive writes with the same token set refresh the head timestamp instead of appending a new episode. Meta on the head is preserved; only the timestamp moves forward. Insert a spacer write if you need two identical token sets recorded back-to-back.
- **Telemetry (stable):**
  - **Write:** `[:brain, :hippocampus, :write]` → measurements `%{window_size}`; metadata `%{meta}`.
  - **Recall:** `[:brain, :hippocampus, :recall]` → measurements `%{cue_count, window_size, returned, top_score}`; metadata `%{limit, half_life_ms}`.

---

## LIFG Path: Definition of Done (DoD) — reference

*(Track progress in code reviews; we’ll mark ✅ as items are completed)*

- [ ] No char-grams in LIFG path (enforced + unit test)
- [ ] Boundary guard (drop non-word-boundary substrings unless `mw: true`)
- [ ] MWE injection pass (word-level n-grams before LIFG)
- [✅] Sense slate in SI (`si.sense_candidates` keyed by token index)
- [ ] Reanalysis fallback (flip to next-best on integration failure)
- [ ] Telemetry tripwire (log/drop if a char-gram reaches LIFG)
- [ ] Priming cache (optional; recency boost for recent winners)
- [ ] Invariant tests (spans sorted; no char-grams; boundary-only unless `mw: true`)
- [ ] Config defaults (test/dev: `tokenizer_mode: :words`, `tokenizer_emit_chargrams: false`)

**Additional invariants**

- Tokens sorted by `start`; spans hydrated from the sentence.
- ID convention: `---{word}|{pos}|{sense}---` for `Db.BrainCell.id` (string PK).

**Guard usage rule**

- Keep guards simple. No complex function calls (e.g., `String.contains?/2`) inside guards. Precompute booleans before `when`.

---

## Configuration Defaults

Set in `config/test.exs` and `config/dev.exs`:

```elixir
config :core,
  tokenizer_mode: :words,
  tokenizer_emit_chargrams: false
```

Phoenix/Web may print:
```elixir
IO.inspect(GenServer.call(Brain, :snapshot), limit: :infinity)
```
for visibility without mutating state.

---

## Approval Protocol (Pair-Programming Guardrails)

Nothing merges or “goes live” without an explicit approval token.

**Full-File Patch Guardrail (required)**

- Before any code, assistant requests the **full current file** for each file to be edited.
- Assistant returns **full-file replacements only**; no diffs or fragments.
- Approval tokens should include FileScope, e.g.,  
  `---Approve: P-005 (FileScope: SYMBRELLA_PROJECT_GUARDRAILS.md)---`.
- No edits outside the approved FileScope without a new approval.

**Flow**

1) **Proposal:** I present a patch proposal with ID `P-###` that includes:  
   - Summary (what/why), risk level (**Safe / Risky / BREAKING**), and scope.  
   - Key before/after code excerpts.  
   - File list with replacements or additions.  
   - Tests to add/update.  
   - Rollback plan (git commands).

2) **Deliverables:** Prefer chat-bubble code blocks only (UTF-8, LF). Avoid ZIPs/binaries unless requested. Multi-file outputs are sent as consecutive chat bubbles with clear filename headers. No unsolicited multi-pane diffs. One module per “pane”.

3) **Approval:** You respond with `---Approve: P-###---`.  
   - If not approved, we do no file replacements. We can iterate with `P-###.1`, etc.

4) **Special tags (for visibility):**
   - `QuickFix-###`: small, surgical changes.  
   - `BREAKING-###`: migrations, API changes, or module moves.  
   - `Hotfix-###`: urgent bug fix; still needs `---Approve: …---` before applying.

5) **After approval** I provide final artifacts only, without extra refactors beyond the approved scope.

**Template**

```
Patch ID: P-###
Title: <one-line summary>
Risk: Safe | Risky | BREAKING
Files touched:
  - apps/brain/lib/brain/lifg.ex (replace)
  - apps/core/lib/core/tokenizer.ex (edit)

Summary:
  <plain-language explanation>

Tests:
  <list of tests added/updated>

Rollback:
  git checkout HEAD~1 -- <files>   # or
  git revert <commit>
```

---

## Module & Dependency Rules

- **One umbrella-root supervisor:** `Symbrella.Application` starts Brain + others.
- **No per-app Application supervisors.**
- **Acyclic dependencies:** `db ← brain ← core ← web`.
  - Brain can depend on Db. Core can depend on Brain and Db. Web depends on Core/Brain.
  - **Never reverse this chain.**

---

## Rollback & Recovery (Git Cookbook)

- See file history for one file:  
  `git log --follow -- apps/brain/lib/brain/lifg.ex`

- Show diffs over file history:  
  `git log -p --follow -- apps/brain/lib/brain/lifg.ex`

- Restore a file to `main`:  
  `git checkout origin/main -- apps/brain/lib/brain/lifg.ex`

- Revert the last commit (create a new inverse commit):  
  `git revert <commit_sha>`

- Discard local changes to a file:  
  `git restore --source=HEAD -- apps/core/lib/core.ex`

---

## Working Session Conventions

- If you say “hold off until I paste more files”, I pause changes and only review.
- If the task is time-boxed or ambiguous, I deliver a small, safe delta, plus the proposal for the next step.
- I won’t introduce new modules/dirs without a `P-###` that updates this doc’s layout.
- **No hidden background work:** everything ships in the current message.

---

## “Known-Good” Patterns (snapshots)

- **Web print before DOM push:**
  ```elixir
  IO.inspect(GenServer.call(Brain, :snapshot), limit: :infinity)
  ```

- **Core pipeline (orchestration only):** keep LIFG in Brain; Core may call Lexicon as a public facade.

---

### Appendix: Glossary

- **STM:** short-term memory / active focus in Brain processes.
- **LIFG:** left-inferior-frontal-gyrus (symbolic disambiguation layer).
- **MWE:** multi-word expression (inserted as tokens prior to LIFG).
- **DoD:** Definition of Done; check before PR approval.

---

**How we’ll use this doc:** Before any refactor or file drop-in, we sanity-check against this document. If the change requires updating this document, that update is part of the patch.
