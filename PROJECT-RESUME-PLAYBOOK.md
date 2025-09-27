# Project→Resume Playbook — Symbrella
**Goal:** Turn your Elixir neuro-symbolic AI (Symbrella) into a standout, recruiter-friendly resume and demo.

---

## 0) Elevator Pitch (paste into README & LinkedIn)
**One-liner:** Neuro‑symbolic AI in Elixir (OTP + Ecto + Nx/Axon) with a Phoenix UI and a production-style umbrella. Words/senses are **BrainCells** (GenServers) backed by Postgres. A new **LIFG** module performs fast, auditable Stage‑1 word-sense disambiguation and emits control signals into the cell layer.

**Why it matters:** Traceable reasoning + ML signals. Clean UI ⇄ Brain ⇄ Core boundaries. Real-time activation dynamics with telemetry, tests, and benchmarks.

---

## 1) What’s Done (Now) vs What’s Next (7–14 days)

### ✅ Done
- **Brain.LIFG v2**: Stage‑1 disambiguation (`disambiguate_stage1/3`) with softmax/maxnorm, margins, audit trail, and concurrency flag.
- **Brain.lifg_stage1/3 (side-effectful)**: Brain fan-out applies boosts/inhibitions to cells; telemetry hook `[:brain,:pipeline,:lifg_stage1,:stop]`.
- **Brain.Cell**: per‑cell activation + status, registered via `Brain.Registry` and supervised under `Brain.CellSup`.
- **Db.BrainCell**: consolidated Ecto schema (Postgres) as the source of truth for cells.
- **Core integration**: `Core.resolve_input/1` wires SI → LTM → Lexicon → Brain activation; LIFG orchestration API exposed in Core.
- **Tests**: unit + integration + property tests for LIFG; doctests; StreamData properties for normalization invariants.
- **Benchmarks**: micro-bench suite comparing softmax vs maxnorm and serial vs parallel; sub‑millisecond decisions on mobile CPU.
- **Git hygiene**: master/default set; stale branches pruned.

### ▶ Next (low-risk, high-value)
- **Stage‑2 context gating** (Top‑K + learned thresholds) and **Stage‑3 phrase-level reconciliation**.
- **Cell decay/inhibition model** (configurable half-life) at Brain layer.
- **Telemetry dashboard** (LiveDashboard or OpenTelemetry exporter).
- **CI** badges: Actions + ExCoveralls + Dialyzer; `mix format` + `credo` baseline.

---

## 2) README Hero (drop-in)
```md
# Symbrella — Neuro‑symbolic AI in Elixir
> BrainCells (GenServer + Ecto) + LIFG disambiguation + Nx/Axon signals + Phoenix UI.

**Why it matters**
- Traceable reasoning with supervised “neurons” (GenServers) backed by Postgres.
- Fast, auditable WSD via **Brain.LIFG** with telemetry and property tests.
- Clean boundaries: UI ⇄ Brain ⇄ Core; production umbrella layout.

**Demo**
- Quickstart: `mix setup && iex -S mix phx.server`
- Brain hook: `Brain.lifg_stage1(cands, ctx, normalize: :softmax)`

**Stack:** Elixir/OTP • Phoenix LiveView • Ecto/Postgres • Nx/Axon • ExUnit/StreamData • Dialyzer
**Repo map:**
- `apps/brain/` — Brain supervisor, `Brain.Cell`, `Brain.LIFG`, telemetry
- `apps/core/` — Pipeline (`SemanticInput`, Lexicon, recall plan)
- `apps/symbrella_web/` — Phoenix UI (isolated from Core)
```
---

## 3) Skills Matrix (map features → hiring signals)

| Project Artifact                             | Hiring Signal                                            |
|----------------------------------------------|----------------------------------------------------------|
| `Brain.LIFG.disambiguate_stage1/3`           | Probabilistic WSD, scoring/normalization, auditability   |
| `Brain.lifg_stage1/3` side-effects           | Concurrency, backpressure, telemetry                     |
| `Brain.Cell` (GenServer + Registry + Sup)    | OTP process design, supervised state, message passing    |
| `Db.BrainCell` + migrations                  | Data modeling, Ecto schemas, migrations, conflict upserts|
| LIFG tests (unit/integration/property)       | TDD, invariants with StreamData, doctests                |
| Bench suite (Benchee)                        | Perf engineering, tradeoffs (serial vs parallel)         |
| Phoenix UI boundary                          | Clean interfaces; LiveView familiarity                   |
| Git/CI hygiene                                | Shipping discipline; reproducible builds                 |

> Action: Copy table into README and link each artifact to source files.

---

## 4) STAR Resume Bullets (tailored, paste into your resume)
- **Designed & built** a neuro‑symbolic AI stack in Elixir where words/senses are supervised **BrainCells** backed by Postgres and orchestrated via a **Brain** coordinator; **improved debuggability** with a single `SemanticInput` pipeline and cell status introspection.
- **Implemented** **Brain.LIFG** (Stage‑1 word‑sense disambiguation) with softmax/maxnorm scoring, margin awareness, and full audit trail; **integrated** as a side effect in `Brain.lifg_stage1/3` that emits activation boosts/inhibitions to running cells.
- **Hardened** the system with **unit, integration, and property tests** (StreamData), catching normalization edge cases and group‑sum invariants; **added** Telemetry instrumentation and a simple handler for timing.
- **Benchmarked** serial vs parallel paths with Benchee; achieved **sub‑millisecond** per‑sentence decisions on mobile‑class CPU in typical inputs; selected defaults based on P95 latency and memory tradeoffs.
- **Shipped** repo hygiene (branch cleanup, default to `master`), added READMEs and a demoable Quickstart to lower onboarding friction.

*(Add concrete numbers when you re-run on your target machine: e.g., “~180 µs avg on g8_s3_d128; ~5.5 K ops/sec serial maxnorm”.)*

---

## 5) Architecture Diagram (Mermaid)
```mermaid
graph TD
  UI[Phoenix UI (LiveView)] -->|HTTP/WebSocket| Brain[Brain Supervisor]

  subgraph Brain App
    Brain --> LIFG[Brain.LIFG (Stage-1 WSD)]
    Brain --> Cell[Brain.Cell (GenServer)]
    Cell -->|Ecto| DB[(Postgres)]
  end

  Brain -->|API calls| Core[Core Pipeline]

  subgraph Core App
    Core --> Tok[Token/Tokenizer]
    Core --> Sem[SemanticInput]
    Core --> Lex[Lexicon/Senses]
    Core --> Recall[Recall/Plan]
  end
```
---

## 6) Demo Script (2 minutes)
1. `iex -S mix phx.server` and hit the UI; show UI ⇄ Brain log line.
2. `Core.resolve_input("hello there")` → print `SemanticInput` and `:db_cells`.
3. Build `cands` + `ctx` in IEx → `Brain.lifg_stage1/3` → show choices + deltas.
4. `Brain.snapshot().active_cells` → show boosted/inhibited cell activations.
5. Close with a 30/60/90 plan (below).

---

## 7) Evidence Log (track what changed)
- Tests ↑ to N; property tests added for normalization & margins.
- P95 LIFG latency ~XXX µs (serial, g8_s3_d64); memory footprint ~YYY KB.
- Telemetry event count Z/session; handler outputs timing and config.
- Branches consolidated; default set to `master`; CI badge added.

---

## 8) Quality & Hygiene Checklist
- [ ] `mix format` + `credo --strict`
- [ ] `dialyzer` (add `@spec`s; fail CI on new warnings)
- [ ] `excoveralls` badge
- [ ] GitHub Actions: `mix deps.get && mix compile --warnings-as-errors && mix test && mix dialyzer`
- [ ] `CHANGELOG.md` + `LICENSE` + `CONTRIBUTING.md`

---

## 9) Why Elixir for AI (recruiter paragraph)
Elixir’s OTP model gives resilient, inspectable concurrency for AI pipelines. Modeling neurons as supervised processes with Postgres persistence blends **traceable symbolic structure** with **ML components (Nx/Axon)**—a practical bridge between reliability and learning. With LIFG’s audited disambiguation and Telemetry, Symbrella demonstrates production habits that translate directly to real systems.

---

## 10) 30/60/90 (hiring-manager friendly)
**30**: finalize Stage‑2/3 disambiguation, wire decay model, green CI.  
**60**: add LiveDashboard + OTEL exporter; Top‑K attention + recall planner v2.  
**90**: embeddings (pgvector), retraining loop, dataset + public benchmarks.

---

## 11) Quickstart (copy-paste)
```bash
asdf install  # or your toolchain
mix setup
MIX_ENV=dev iex -S mix phx.server
```

Add a tiny `mix demo.lifg` that builds a context vector and candidates, runs `Brain.lifg_stage1/3`, and prints the audit and deltas.

---

## 12) Interview Packet
- README hero first, 2-min video, Mermaid diagram.
- Short code tour: `Brain.LIFG`, `Brain` (wiring + side-effects), `Db.BrainCell`.
- One design tradeoff PR/commit (e.g., serial vs parallel default).

---

### License
You can include this file as `PROJECT-RESUME-PLAYBOOK.md`. Adapt under your repo’s license.
