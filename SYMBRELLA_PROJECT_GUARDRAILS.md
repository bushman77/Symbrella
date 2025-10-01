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
- **Deliverables:** Prefer **chat-bubble code blocks only** (UTF-8, LF). Avoid ZIPs/binaries due to encoding issues. Multi-file outputs are sent as consecutive chat bubbles with clear filename headers. No unsolicited multi-pane diffs.

---

## Canonical Umbrella Layout
This is the canonical, minimal structure. If a directory/file isn’t listed here, treat it as optional. If we add new dirs, we add them here first.

```
symbrella/
├── AGENTS.md                                   — High-level agent roles/notes
├── PROJECT-RESUME-PLAYBOOK.md                  — “Resume the project quickly” runbook
├── README-PATCH-v2.md                          — Patch-series readme (v2)
├── README-PATCH.md                             — Patch-series readme (v1)
├── README.md                                   — Main project overview
├── README_BRAIN_CHAIN.md                       — Brain chain architecture notes
├── README_guardrails_snippet.md                — Guardrails snippet for reuse
├── SYMBRELLA_PROJECT_GUARDRAILS.md             — Canonical guardrails doc (this)
├── _config.yml                                 — GitHub Pages/Jekyll config (if used)
├── apps
│   ├── brain
│   │   ├── README.md                           — Brain app overview
│   │   ├── bench
│   │   │   └── brain_lifg_bench_v2_1.exs       — LIFG micro-benchmark script
│   │   ├── brain_lifg_v2_1_2_pack.zip          — Zipped drop-in pack (LIFG)
│   │   ├── lib
│   │   │   ├── brain
│   │   │   │   ├── acc.ex                      — Activation accumulator helpers
│   │   │   │   ├── ag.ex                       — Angular Gyrus (semantic integration)
│   │   │   │   ├── amygdala.ex                 — Amygdala (salience/emotional tagging)
│   │   │   │   ├── atl.ex                      — Anterior Temporal Lobe (concept hub)
│   │   │   │   ├── bg.ex                       — Basal Ganglia (gating/selection)
│   │   │   │   ├── cell.ex                     — Brain cell process/state (per-cell API)
│   │   │   │   ├── cerebellum.ex               — Cerebellum (timing/coordination)
│   │   │   │   ├── chain.ex                    — Brain chain supervisor/flow wiring
│   │   │   │   ├── hippocampus.ex              — Episodic memory API (write/recall)
│   │   │   │   ├── lifg                        — LIFG subsystem (directory)
│   │   │   │   │   └── boundary_guard.ex       — Boundary guard (DoD: word-boundary rule)
│   │   │   │   ├── lifg.ex                     — LIFG core: disambiguation engine
│   │   │   │   ├── lifg_guard.ex               — LIFG guards (invariants, prechecks)
│   │   │   │   ├── lifg_stage1.ex              — LIFG Stage 1 (initial candidate pass)
│   │   │   │   ├── lifg_tripwire.ex            — Telemetry tripwire (drop/log on violations)
│   │   │   │   ├── mtl.ex                      — Medial Temporal Lobe (memory interface)
│   │   │   │   ├── neuromodulator.ex           — Dopamine/serotonin modulation
│   │   │   │   ├── ofc.ex                      — Orbitofrontal Cortex (valuation/policy)
│   │   │   │   ├── pmtg.ex                     — Posterior MTG (controlled retrieval)
│   │   │   │   ├── priming.ex                  — Priming cache/recency boost
│   │   │   │   ├── reanalysis.ex               — Reanalysis fallback on integration failure
│   │   │   │   ├── region.ex                   — `use Brain.Region` macro + region plumbing
│   │   │   │   ├── telemetry.ex                — Metrics/events for Brain/LIFG
│   │   │   │   ├── thalamus.ex                 — Thalamus (routing/signal relay)
│   │   │   │   └── working_memory.ex           — STM/WM interface within Brain
│   │   │   └── brain.ex                        — Brain supervisor/server (umbrella-started)
│   │   ├── mix.exs                             — Brain app mix file
│   │   └── test
│   │       ├── brain
│   │       │   ├── bench/bench_brain_lifg_bench.exs — Bench runner
│   │       │   ├── boundary_guard_test.exs     — Tests: boundary guard invariants
│   │       │   ├── brain_lifg_integration_test.exs — End-to-end LIFG path tests
│   │       │   ├── brain_lifg_property_test.exs — Property tests for LIFG rules
│   │       │   ├── brain_lifg_test.exs         — Unit tests for LIFG core
│   │       │   ├── lifg_guard_test.exs         — Guard unit tests
│   │       │   ├── lifg_priming_integration_test.exs — Priming + LIFG integration
│   │       │   ├── priming_test.exs            — Priming behaviors
│   │       │   └── reanalysis_test.exs         — Reanalysis behavior tests
│   │       └── test_helper.exs                 — ExUnit setup for Brain
│   ├── core
│   │   ├── README.md                           — Core app overview
│   │   ├── lib
│   │   │   ├── core
│   │   │   │   ├── brain
│   │   │   │   │   └── index.ex                — Brain index facade used by Core
│   │   │   │   ├── brain.ex                    — Thin Brain adapter (calls into Brain)
│   │   │   │   ├── brain_adapter.ex            — Port/adapter boundary for Brain APIs
│   │   │   │   ├── invariants.ex               — Core invariants (sorted spans, no char-grams)
│   │   │   │   ├── lex_id.ex                   — `{word}|{pos}|{sense}` ID helpers
│   │   │   │   ├── lexicon
│   │   │   │   │   ├── normalize.ex            — Text normalization for lexicon lookup
│   │   │   │   │   ├── senses.ex               — Sense retrieval/composition helpers
│   │   │   │   │   └── stage.ex                — Lexicon pipeline stage orchestration
│   │   │   │   ├── lexicon.ex                  — Public Lexicon facade (Core-facing)
│   │   │   │   ├── lifg_input.ex               — Build LIFG-ready input (SI → LIFG)
│   │   │   │   ├── mwe_injector.ex             — MWE injection (word n-grams → tokens)
│   │   │   │   ├── neg_cache.ex                — Negative cache (avoid repeat misses)
│   │   │   │   ├── phrase_repo
│   │   │   │   │   └── default.ex              — Default phrase repository (MWEs/phrases)
│   │   │   │   ├── phrase_repo.ex              — Phrase repo behaviour/dispatch
│   │   │   │   ├── recall
│   │   │   │   │   ├── execute.ex              — Execute recall plan
│   │   │   │   │   ├── gate.ex                 — Gate recall by policy/signal
│   │   │   │   │   └── plan.ex                 — Build recall plan (what/when)
│   │   │   │   ├── runtime_bind.ex             — Bind tokens ↔ snapshot cells (STM→SI)
│   │   │   │   ├── segmenter.ex                — Sentence/token segmentation utilities
│   │   │   │   ├── semantic_input.ex           — `%SemanticInput{}` struct + helpers
│   │   │   │   ├── sense_slate.ex              — `si.sense_candidates` slate
│   │   │   │   ├── text.ex                     — Core text utilities
│   │   │   │   ├── token.ex                    — Token struct + tokenizer (words first)
│   │   │   │   ├── token_filters.ex            — Token filtering (boundary, sorting)
│   │   │   │   └── vectors.ex                  — Vector helpers (pure Elixir)
│   │   │   ├── core.ex                         — Orchestrator: tokenize → Brain.stm → Db.ltm → Lexicon
│   │   │   └── math
│   │   │       └── math.ex                     — `Core.Math` (pure-Elixir math ops)
│   │   ├── mix.exs                             — Core app mix file
│   │   ├── priv/negcache/negcache.dets         — DETS store for negative cache
│   │   └── test
│   │       ├── core
│   │       │   ├── invariants_test.exs         — Invariant tests (no char-grams, etc.)
│   │       │   ├── mwe_injector_test.exs       — MWE injection tests
│   │       │   ├── resolve_input_test.exs      — Golden pipeline tests
│   │       │   ├── runtime_bind_test.exs       — Binding tests
│   │       │   ├── sense_slate_test.exs        — Sense slate tests
│   │       │   ├── token_mw_test.exs           — Tokenization with MWEs tests
│   │       │   ├── tokenizer_defaults_test.exs — Config defaults tests
│   │       │   └── tokenizer_wordgrams_test.exs— Word n-gram scanning tests
│   │       └── test_helper.exs                 — ExUnit setup for Core
│   ├── db
│   │   ├── README.md                           — Db app overview
│   │   ├── lib
│   │   │   ├── db
│   │   │   │   ├── brain_cell.ex               — `Db.BrainCell` Ecto schema (string PK)
│   │   │   │   ├── episode.ex                  — `Db.Episode` Ecto schema
│   │   │   │   ├── episodes.ex                 — Episodes context (read/write helpers)
│   │   │   │   ├── lexicon.ex                  — Lexicon tables/context
│   │   │   │   ├── my_embeddibgs.ex            — (typo?) Embeddings context/helpers
│   │   │   │   └── postgrex_types.ex           — Custom Postgres types
│   │   │   └── db.ex                           — Repo + public DB facade used by Core/Brain
│   │   ├── mix.exs                             — Db app mix file
│   │   ├── priv
│   │   │   ├── db/migrations
│   │   │   │   ├── 20250914053633_create_brain_cells_consolidated.exs — Consolidation migration
│   │   │   │   └── 20251001000000_create_episodes.exs                 — Episodes migration
│   │   │   └── repo/migrations
│   │   │       └── 20250708150554_create_brain_cells.exs              — Initial brain_cells migration
│   │   └── test
│   │       ├── db/episodes_test.exs            — Episodes tests
│   │       ├── db_test.exs                     — Db facade tests
│   │       └── test_helper.exs                 — ExUnit setup for Db
│   ├── lexicon
│   │   ├── README.md                           — Lexicon app overview
│   │   ├── lib
│   │   │   ├── lexicon/behaviou.ex             — (typo?) Behaviour spec for adapters
│   │   │   └── lexicon.ex                      — Lexicon adapter implementation
│   │   ├── mix.exs                             — Lexicon app mix file
│   │   └── test
│   │       ├── lexicon_test.exs                — Lexicon adapter tests
│   │       └── test_helper.exs                 — ExUnit setup for Lexicon
│   ├── symbrella
│   │   ├── README.md                           — Umbrella “utility” app readme
│   │   ├── lib
│   │   │   ├── symbrella/application.ex        — Umbrella-root supervisor (single tree)
│   │   │   ├── symbrella/mailer.ex             — Swoosh mailer (if used)
│   │   │   └── symbrella.ex                    — Umbrella helpers
│   │   ├── mix.exs                             — App mix file
│   │   └── test/test_helper.exs                — ExUnit setup
│   └── symbrella_web
│       ├── README.md                           — Phoenix app overview
│       ├── assets/...                          — Frontend assets (Tailwind/JS/hooks)
│       ├── lib
│       │   ├── symbrella_web
│       │   │   ├── application.ex              — Phoenix supervision tree (no extra Apps)
│       │   │   ├── components/core_components.ex — Shared UI components
│       │   │   ├── components/layouts/*.ex/.heex — Layouts
│       │   │   ├── controllers/*.ex/.heex      — Controllers + templates
│       │   │   ├── endpoint.ex                 — Endpoint (Bandit)
│       │   │   ├── gettext.ex                  — i18n scaffolding
│       │   │   ├── live/brain_live.ex          — LiveView: Brain UI (reads snapshot)
│       │   │   ├── live/home_live.ex           — LiveView: Home
│       │   │   ├── router.ex                   — Routes
│       │   │   └── telemetry.ex                — Phoenix telemetry
│       │   └── symbrella_web.ex                — Phoenix web macros
│       ├── mix.exs                             — Web app mix file
│       ├── priv/...                            — Compiled assets, gettext, static
│       └── test/...                            — Controller tests + setup
├── assets/css/app.css                          — Root-level shared styles (if used)
├── benchmark_result.txt                        — Last benchmark output
├── config
│   ├── config.exs                              — Shared config
│   ├── dev.exs                                 — Dev config (tokenizer defaults here)
│   ├── prod.exs                                — Prod config
│   ├── runtime.exs                             — Runtime env config
│   └── test.exs                                — Test config (tokenizer defaults here)
├── mix.exs                                     — Umbrella root Mixfile
└── mix.lock                                    — Locked deps
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

2) **Deliverables:** Prefer **chat-bubble code blocks only** (UTF-8, LF). Avoid ZIPs/binaries due to encoding issues. Multi-file outputs are sent as consecutive chat bubbles with clear filename headers. No unsolicited multi-pane diffs.
   - One module per “pane”.

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
