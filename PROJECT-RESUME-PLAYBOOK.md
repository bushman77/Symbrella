# Project→Resume Playbook — Symbrella / elixir_ai_core

**Goal:** Make your Elixir neuro‑symbolic AI project your strongest resume and hiring demo.

---
## 0) Elevator Pitch (paste into README & LinkedIn)
**One‑liner:** Neuro‑symbolic AI framework in Elixir (OTP + Ecto + Nx/Axon) with a Phoenix UI, treating each *BrainCell* as a stateful GenServer backed by Postgres.

**Value:** Traceable reasoning + ML signals; clean separation between **UI ⇄ Brain ⇄ Core**; production‑style umbrella layout.

---
## 1) 7‑Day Shipping Plan (visible progress, fast)
- [ ] **README hero** (use template below) with GIF/screenshot and 3 bullets of impact.
- [ ] **Quickstart**: `git clone → mix setup → mix phx.server` (copy‑paste ready).
- [ ] **2‑minute demo video** (screen capture; captions OK).
- [ ] **Architecture diagram** (Mermaid) showing **UI ⇄ Brain ⇄ Core** and Postgres.
- [ ] **CI badge** (GitHub Actions) running `mix test` + `dialyzer`.
- [ ] **Coverage badge** (excoveralls) — even partial coverage.
- [ ] **Example dataset** + one `mix task` to run a tiny Axon training loop.
- [ ] **Release tag v0.1.0** + CHANGELOG (What’s ready / What’s next).

---
## 2) README Hero Template (drop‑in)
```md
# Symbrella / elixir_ai_core

> Neuro‑symbolic AI in Elixir: stateful BrainCells (GenServer + Ecto) + intent pipeline (Nx/Axon) + Phoenix UI.

**Why it matters**
- 🔍 Traceable reasoning with GenServer “neurons” backed by Postgres
- 🧠 Intent pipeline blending patterns + ML (Axon/Nx)
- 🧩 Clean boundaries: UI ⇄ Brain ⇄ Core (umbrella apps)

**Demo**
- 2‑minute walkthrough: <link to video>
- Quickstart: `mix setup && iex -S mix phx.server`

**Stack**: Elixir/OTP • Phoenix LiveView • Ecto/Postgres • Nx/Axon • Dialyzer • ExUnit

**Repo map**
- `apps/brain/` — Brain supervisor + `Brain.Cell` processes
- `apps/core/` — Core pipeline (tokenize → classify → plan) + `SemanticInput`
- `apps/symbrella/` — Phoenix UI (talks to Brain; Core is isolated)

**Roadmap**: POS disambiguation • ResponsePlanner v2 • Contingent Axon retraining • Observability
```

---
## 3) Skills Matrix (map features → hiring signals)
| Project Artifact | Hiring Signal |
|---|---|
| `Brain.Cell` (GenServer + Ecto schema) | OTP concurrency, DB‑backed state, process supervision |
| `Core.SemanticInput` + Token/Intent modules | Pipeline design, typed contracts, testability |
| Axon training loop + inference | Nx/Axon, ML fundamentals, CPU‑only ops |
| Phoenix LiveView UI | Real‑time UX, clean API boundary |
| Postgres migrations (pgvector optional) | Data modeling, migrations, extensions |
| CI (tests, dialyzer, coveralls) | Production hygiene, reliability |

> Action: Add this table to README and link to code locations.

---
## 4) STAR Resume Bullets (tailored examples)
- **Built** a neuro‑symbolic AI framework in Elixir (OTP umbrella) where each *BrainCell* is a GenServer backed by Ecto/Postgres; **reduced** cross‑module coupling and improved debuggability with a single **Core.SemanticInput** pipeline.  
- **Implemented** intent classification (pattern matrix + Axon/Nx model) and **exposed** a Phoenix LiveView UI that speaks only to the Brain boundary; **cut** UI/Core coupling to near zero and simplified future ML swaps.  
- **Stabilized** compile/runtime by refactoring schemas/modules (e.g., `Db.BrainCell` ⇄ `Brain.Cell`), **added** CI (ExUnit + Dialyzer + Coveralls) and a one‑command Quickstart, **accelerating** onboarding for new contributors.

> Tip: Keep bullets outcome‑first. Add numbers where honest (tests added, lat/throughput, reduced build warnings, etc.).

---
## 5) Architecture Diagram (Mermaid — paste in README)
```mermaid
graph TD
  UI[Phoenix UI (LiveView)] -->|HTTP/WebSocket| Brain[Brain Supervisor]
  subgraph Brain App
    Brain --> Cell[Brain.Cell (GenServer)]
    Cell -->|Ecto| DB[(Postgres)]
  end
  Brain -->|API calls| Core[Core Pipeline]
  subgraph Core App
    Core --> Tok[Token/Tokenizer]
    Core --> Sem[SemanticInput]
    Core --> Intent[IntentClassifier/Matrix]
    Core --> Plan[ResponsePlanner]
  end
```

---
## 6) Demo Script (2 minutes)
1) Start server; show **UI ⇄ Brain** request in logs.  
2) Type sentence → show `SemanticInput` printed (intent, keyword, confidence).  
3) Toggle a *BrainCell* and show state in DB.  
4) Run tiny Axon inference (toy input) → show response.  
5) Close with roadmap: what you’d build in 4 weeks at $COMPANY.

---
## 7) Evidence Log (README or `/docs/evidence.md`)
Track a few **before/after** metrics you can cite:
- Build warnings ↓ from X → Y  
- Test count ↑ from X → Y; coverage to Z%  
- p95 response time ↓ (local) from Xms → Yms  
- CI duration and pass rate

---
## 8) Quality & Hygiene Checklist
- [ ] `mix format` + `credo` (style/nits)
- [ ] `dialyzer` (typespec hygiene)
- [ ] `excoveralls` badge
- [ ] GitHub Actions: `mix deps.get && mix compile --warnings-as-errors && mix test && mix dialyzer`
- [ ] `CHANGELOG.md` + `LICENSE` + `CONTRIBUTING.md`

---
## 9) “Why Elixir for AI?” (one paragraph for recruiters)
Elixir’s OTP model gives resilient, inspectable concurrency for AI pipelines. By treating neurons as supervised processes and persisting to Postgres, we mix **traceable symbolic structure** with **ML components (Nx/Axon)**—a practical bridge between reliability and learning.

---
## 10) 30/60/90 Roadmap (hiring‑manager friendly)
**30 days:** finalize Core pipeline; green CI; demo video; docs site.  
**60 days:** POS disambiguation + ResponsePlanner v2; observability (telemetry, logs).  
**90 days:** pgvector embeddings; simple retraining loop; public sample dataset + benchmarks.

---
## 11) Copy‑paste Quickstart (final polish)
```bash
asdf install # or your toolchain
mix setup
MIX_ENV=dev iex -S mix phx.server
```

> Add a `mix task` like `mix demo.intent` to generate a sample `SemanticInput` and print intent/keyword/confidence.

---
## 12) What to Show in Interviews
- Repo (README hero first), 2‑min video, architecture diagram, short code tour of `Brain.Cell` and `SemanticInput`.
- A single issue you opened/closed that demonstrates design tradeoffs.
- The roadmap and what you’d build next if they hired you.

---
### License
You can include this file in your repo as `PROJECT-RESUME-PLAYBOOK.md`. Feel free to adapt and license under MIT/Apache‑2.0 per your repo’s choice.
