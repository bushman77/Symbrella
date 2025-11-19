# Symbrella — Neuro-Symbolic Synthetic Intelligence (NSSI)

**Erlang/OTP:** 28 • **Elixir:** 1.18.x • **Phoenix:** 1.8.x (Bandit)

> Symbrella is an NSSI (Neuro‑Symbolic Synthetic Intelligence) umbrella app.  
> It models brain‑inspired regions (LIFG, PMTG, Hippocampus, ACC, Basal Ganglia, Thalamus, DLPFC, etc.) as OTP processes, combining symbolic structure with learned signals (Axon/Nx, embeddings, episodic recall).

Symbrella treats your phone (and eventually real robots) as a **body**, and this umbrella as a **brain**.  
It combines:

- **Neural models** (Axon/Nx, embeddings)  
- **Symbolic control** (GenServers, policies, Ecto-backed memory)  
- **Brain-inspired regions** (LIFG, PMTG, Hippocampus, WM, Mood, Curiosity, Thalamus, DLPFC, Cerebellum, etc.)

…into a single, inspectable cognitive system you can run locally.

---

## Why Symbrella?

Most AI systems today look like this:

> “Send text to a big model in the cloud → get a text answer back.”

Symbrella is for people who want more:

- A **long-lived brain** with state, mood, memories, and habits.  
- A **cognitive pipeline** you can actually see and debug.  
- A path from **phone** → **VR cockpit** → **physical robot** without changing the mental model.

Instead of treating AI as a black-box API, Symbrella treats it as a **living system** with regions, episodes, and working memory.

Modern AI stacks are converging on a similar pattern:

> **Neural core** + **symbolic wrapper** + **tools & memory**.

Reasoning models (e.g. “reasoning” LLMs and agent frameworks) follow this pattern, even if they don’t use brain metaphors or region names.

Symbrella lives in that same ecosystem, but pushes harder on the **“actual brain”** side:

- Named cortical/subcortical regions instead of anonymous “agents”  
- Explicit Working Memory and episodic recall stages  
- Mood / neuromodulators that can influence control flow  
- An explicit path to **embodiment** (phone sensors, VR, robots)

See **SYMBRELLA_PROJECT_GUARDRAILS.md** for module boundaries, approval protocol, and invariants.

---

## Quickstart

```bash
# from the umbrella root
mix deps.get

# (first time on a machine) install asset tool binaries
cd apps/symbrella_web
mix tailwind.install --if-missing
mix esbuild.install --if-missing
cd ../../

# build assets (manual on purpose)
mix tailwind default
mix esbuild default

# run the server
mix phx.server

# App: http://localhost:4000
# Re-run the two commands whenever you change CSS/JS:
#   mix tailwind default
#   mix esbuild default
```

---

## Apps at a glance

- **apps/brain** — Brain regions + working memory. Owns LIFG selection, the Curiosity → Thalamus → BasalGanglia → DLPFC → WM loop, and episodic coordination (Hippocampus).
- **apps/core** — Orchestrates the pipeline (tokenize → Brain STM → Db LTM → Lexicon). SemanticInput, MWE injection, invariants.
- **apps/db** — Ecto schemas (BrainCell, Episode, Lexicon) + Repo; pgvector support for embeddings.
- **apps/lexicon** — Adapter layer for dictionary/lexicon sources.
- **apps/llm** — LLM client surface (work in progress).
- **apps/symbrella** — Umbrella runtime; single supervisor tree.
- **apps/symbrella_web** — Phoenix LiveView UI (brain view, chat hooks, etc.).

---

## Symbrella in the AI landscape

At a high level, you can think of Symbrella next to a “typical” modern reasoning stack:

```text
┌────────────────────────────┐        ┌────────────────────────────┐
│ OpenAI-style Reasoning     │        │ Symbrella Brain Stack      │
└────────────────────────────┘        └────────────────────────────┘

User prompt                          Sensor / user input
      │                                         │
      ▼                                         ▼
Orchestrator / agent                Core.resolve_input/2
(choose tools, model, plan)         (Tokenize → LTM → MWE → LIFG)
      │                                         │
      ▼                                         ▼
Reasoning model (LLM)                Brain regions:
(internal chain-of-thought)          LIFG ⇄ ATL ⇄ Hippocampus ⇄ WM ⇄ Mood ⇄ Cerebellum
      │                                         │
      ▼                                         ▼
Tools (search, code, DB, …)          Curiosity, Thalamus, BasalGanglia, DLPFC, tools/sensors
      │                                         │
      ▼                                         ▼
Answer synthesis                     Phrase / action / UI output
      │                                         │
      ▼                                         ▼
   User                                 User / robot / VR scene
```

Both sides have:

- A **neural engine** that does the heavy “thinking”  
- A **symbolic / programmatic layer** that controls flow, tools, and memory  
- Some notion of **context and history** beyond a single request  

The difference is how explicit and “brain-like” those pieces are.

### Neural core

| Aspect                | OpenAI-style reasoning stack                                 | Symbrella                                             |
|----------------------|--------------------------------------------------------------|-------------------------------------------------------|
| Engine               | Large transformer reasoning model                            | Axon/Nx models, embeddings, small task-specific nets |
| How it’s used        | Called via API by an orchestrator / agent                    | Called from Brain/Core as one piece of the pipeline  |
| Visibility           | Internal chain-of-thought mostly hidden from users           | Activations and decisions can be exposed via WM, telemetry, UI |

Symbrella leans into **small, inspectable circuits** instead of a single giant opaque model.

### Symbolic / control layer

| Aspect          | OpenAI-style reasoning stack                     | Symbrella brain stack                                 |
|----------------|---------------------------------------------------|-------------------------------------------------------|
| Orchestration  | Agents, planners, tool routers                    | `Brain` as central coordinator + `use Brain, region:` macros |
| Control flow   | “If task X → call tool Y → call model Z”          | Region graph: LIFG → ATL → Hippocampus → WM/Mood/etc. |
| Representation | JSON, workflows, system prompts                   | GenServers, structs, Ecto schemas, SemanticInput state |

Symbrella’s “agents” are literally **named brain regions** with explicit responsibilities.

### Memory

| Type                 | OpenAI-style reasoning stack                        | Symbrella                                                 |
|----------------------|-----------------------------------------------------|-----------------------------------------------------------|
| Short-term context   | Chat history + hidden scratchpad tokens             | Working Memory (WM) focus set + SI trace + `active_cells` |
| Long-term knowledge  | Model weights, plus external RAG/vector stores      | BrainCell DB, Lexicon, and future pgvector recall         |
| Episodic memory      | Often app-specific logs or vector DB entries        | `Brain.Hippocampus` + `Db.Episode`, recency & outcome uplift |

Symbrella treats **episodic memory** as a first-class brain stage, not just a feature of a retrieval library.

### Attention, gating, and meta-control

| Aspect          | OpenAI-style reasoning stack                  | Symbrella                                           |
|----------------|-----------------------------------------------|-----------------------------------------------------|
| Attention      | Transformer attention + orchestrator hints    | WM admission policy + focus thresholds + gate scores |
| Gating         | System prompts, reasoning_effort, tool rules  | `Brain.WM.Policy`, LIFG Stage-1 scores, ACC/OFC/Thalamus gates |
| Meta-signals   | Mostly hidden policies and heuristics         | **Mood** (dopamine/serotonin), Curiosity, ACC inputs |

Symbrella’s control flow is designed to be **mood-sensitive**: neuromodulators and curiosity can actually shift thresholds and choices.

### Tools, sensors, and embodiment

| Aspect          | OpenAI-style reasoning stack         | Symbrella                                                |
|----------------|---------------------------------------|----------------------------------------------------------|
| Tools          | HTTP APIs, search, code execution     | Sensors (camera), HTTP APIs, DB, future robot actuators |
| Environment    | Cloud services, browser clients       | Termux on Android, Phoenix/LiveView, VR/3D dashboards    |
| Embodiment     | Usually none (pure service)           | **Phone as body**, VR “desk” as cockpit, future robots  |

A key design goal of Symbrella is to **live inside a body**—starting with a phone and VR headset, expanding to hardware.

### Introspection and debugging

| Aspect        | OpenAI-style reasoning stack          | Symbrella                                        |
|---------------|----------------------------------------|--------------------------------------------------|
| Telemetry     | Extensive internal metrics (not public) | `Brain.Introspect`, region status, WM snapshots   |
| Visuals       | Internal dashboards                    | BrainLive, SVG region overlays, VR desk cockpit  |
| Granularity   | High internally, low externally        | You can expose **every stage**, every region, every WM slot |

Symbrella is intentionally built as a **transparent brain**, not a sealed black box.

---

## Curiosity loop — current implementation (Nov 2025)

The **curiosity loop** is a concrete example of how regions cooperate:

> Curiosity → Thalamus → BasalGanglia → DLPFC → WorkingMemory

Roughly:

1. **`Brain.Curiosity`**  
   - Generates small “what if?” probes on demand (via `nudge/0` or future schedulers).  
   - Emits telemetry on `[:curiosity, :proposal]` with a `probe` payload:
     - `id`, `lemma`, `score` in `[0,1]`, `reason: :curiosity`, `source: :runtime`.

2. **`Brain.Thalamus`**  
   - Listens for curiosity proposals.  
   - Blends:
     - base curiosity score,
     - **OFC** value (exploit vs explore),
     - **ACC** conflict (brake),
     - **Mood** (exploration/inhibition/vigilance/plasticity).  
   - Emits `[:brain, :thalamus, :curiosity, :decision]` telemetry with:
     - measurements: `score` in `[0,1]`,  
     - metadata: `decision` (`:allow | :boost | :block`), `ofc_value`, `ofc_weight`,  
       `acc_conflict`, `acc_alpha`, mood fields, etc.  
   - Parameters are surfaced via `Brain.Thalamus.get_params/0` and wired to
     `Application` env (e.g. `:thalamus_ofc_weight`, `:thalamus_acc_alpha`, `:thalamus_mood_cap`).

3. **`Brain.BasalGanglia`** (stateless gate)  
   - `decide/4` takes:
     - current WM (newest-first),
     - the candidate probe,
     - an attention context,
     - config (capacity, thresholds, source preferences, cooldown).  
   - Returns `{decision, score}` where `decision ∈ :allow | :boost | :block`.  
   - Uses:
     - WM fullness / capacity,
     - duplicate detection and cooldown rebump,
     - per-source boost/dispreference.

4. **`Brain.DLPFC`**  
   - First-class region (`use Brain, region: :dlpfc`).  
   - Subscribes to:
     - curiosity proposals (`[:curiosity, :proposal]`) to **cache the last probe**, and  
     - Thalamus decisions (`[:brain, :thalamus, :curiosity, :decision]`).  
   - When `:act_on_thalamus` is enabled and decision is `:allow` or `:boost`,
     DLPFC calls `Brain.focus/2` with the cached probe → a WM item is inserted.

5. **`Brain.WorkingMemory`**  
   - Normalizes the candidate probe into a WM item with a consistent shape:  
     `id`, `source`, `activation`, `score`, `ts`, `inserted_at`, `last_bump`, `payload`.  
   - Handles:
     - decay over time (half-life style),
     - duplicate merging,
     - trimming to capacity.

The whole loop is covered by tests such as:

- `Brain.CuriosityFlowTest` — end-to-end:  
  `Curiosity → Thalamus(+OFC/ACC/mood) → BG → DLPFC → WM` inserts a `reason: :curiosity` item.
- `Brain.ThalamusParams_Test` — config → params contract.  
- `Brain.ThalamusTelemetryContract_Test` — telemetry shape and math (`ofc_weight`, `acc_alpha`, score).  
- `Brain.ThalamusMathProps_Test` — monotonicity and braking properties.  
- `Brain.BasalGanglia*Test` — gating edges and smoke tests.

---

## Architecture (high level)

```mermaid
flowchart TD
  A["Phoenix UI (LiveView)"]
  B["Brain (OTP regions)"]
  C["Core (pipeline)"]
  D[(Postgres + pgvector)]

  A --> B
  A --> C
  B --> C
  C --> D
  B --> D
```

**Golden pipeline**

```elixir
phrase
|> Core.Token.tokenize()   # word tokens first; sentence-aware spans
|> Brain.stm()             # short-term focus/activation (processes)
|> Db.ltm()                # long-term memory fetch (rows)
|> Core.Lexicon.all()      # dictionary lookups as needed
```

---

## LIFG DoD — working checklist (Oct 9, 2025 snapshot)

- [ ] No char-grams in LIFG path (enforced + unit test)
- [ ] Boundary guard (drop non-word-boundary substrings unless `mw: true`)
- [ ] MWE injection pass (word-level n-grams before LIFG)
- [✅] Sense slate in SI (`si.sense_candidates` keyed by token index)
- [ ] Reanalysis fallback (flip to next-best on integration failure)
- [ ] Telemetry tripwire (log/drop if a char-gram reaches LIFG)
- [ ] Priming cache (optional; recency boost for recent winners)
- [ ] Invariant tests (spans sorted; no char-grams; boundary-only unless `mw: true`)
- [ ] Config defaults (test/dev: `tokenizer_mode: :words`, `tokenizer_emit_chargrams: false`)

Details and rationale live in **SYMBRELLA_PROJECT_GUARDRAILS.md**.

---

## Development tips

```bash
# run everything
mix test

# format + compile
mix format
mix compile

# run a single test file
mix test apps/brain/test/brain/lifg_guard_test.exs

# curiosity / thalamus loop
mix test apps/brain/test/brain/curiosity_flow_test.exs
mix test apps/brain/test/brain/thalamus_*test.exs

# micro-benchmarks (examples)
mix run apps/brain/bench/brain_stage1_bench.exs
mix run apps/brain/bench/brain_lifg_bench_v2_1.exs
```

**Telemetry testing**  
`test/support/telemetry_helpers.exs` provides `capture/3` (assert emitted) and you can add `refute_emitted/3` similarly.

**Tokenizer defaults (dev/test):**

```elixir
config :core,
  tokenizer_mode: :words,
  tokenizer_emit_chargrams: false
```

---

## Repository layout (current snapshot)

```text
├── AGENTS.md
├── PROJECT-RESUME-PLAYBOOK.md
├── README.md
├── README_BRAIN_CHAIN.md
├── README_guardrails_snippet.md
├── SYMBRELLA_PROJECT_GUARDRAILS.md
├── _config.yml
├── apps
│   ├── brain
│   │   ├── README.md
│   │   ├── bench
│   │   │   ├── brain_lifg_bench_v2_1.exs
│   │   │   ├── brain_stage1_bench.exs
│   │   │   └── profile_stage1.exs
│   │   ├── brain_lifg_v2_1_2_pack.zip
│   │   ├── lib
│   │   │   ├── brain
│   │   │   │   ├── acc.ex
│   │   │   │   ├── atl.ex
│   │   │   │   ├── attention.ex
│   │   │   │   ├── basal_ganglia.ex
│   │   │   │   ├── cell.ex
│   │   │   │   ├── curiosity.ex
│   │   │   │   ├── dlpfc.ex
│   │   │   │   ├── episodes
│   │   │   │   │   └── writer.ex
│   │   │   │   ├── hippocampus
│   │   │   │   │   ├── config.ex
│   │   │   │   │   ├── dup.ex
│   │   │   │   │   ├── evidence.ex
│   │   │   │   │   ├── normalize.ex
│   │   │   │   │   ├── recall.ex
│   │   │   │   │   ├── scoring.ex
│   │   │   │   │   ├── telemetry.ex
│   │   │   │   │   └── window.ex
│   │   │   │   ├── hippocampus.ex
│   │   │   │   ├── lifg
│   │   │   │   │   ├── boundary_guard.ex
│   │   │   │   │   ├── gate.ex
│   │   │   │   │   ├── guard.ex
│   │   │   │   │   ├── hygiene.ex
│   │   │   │   │   ├── input.ex
│   │   │   │   │   ├── reanalysis.ex
│   │   │   │   │   ├── stage1.ex
│   │   │   │   │   └── stage1_guard.ex
│   │   │   │   ├── lifg.ex
│   │   │   │   ├── ptmg.ex
│   │   │   │   ├── telemetry.ex
│   │   │   │   ├── thalamus.ex
│   │   │   │   ├── utils
│   │   │   │   │   ├── control_signals.ex
│   │   │   │   │   ├── numbers.ex
│   │   │   │   │   ├── safe.ex
│   │   │   │   │   └── tokens.ex
│   │   │   │   └── working_memory.ex
│   │   │   └── brain.ex
│   │   ├── mix.exs
│   │   └── test
│   │       ├── brain
│   │       │   ├── basal_ganglia_edges_test.exs
│   │       │   ├── basal_ganglia_smoke_test.exs
│   │       │   ├── basal_ganglia_test.exs
│   │       │   ├── bench
│   │       │   │   └── bench_brain_lifg_bench.exs
│   │       │   ├── boundary_guard_test.exs
│   │       │   ├── brain_lifg_integration_test.exs
│   │       │   ├── brain_lifg_property_test.exs
│   │       │   ├── brain_lifg_test.exs
│   │       │   ├── curiosity_flow_test.exs
│   │       │   ├── hippocampus_attach_episodes_test.exs
│   │       │   ├── hippocampus_behavior_test.exs
│   │       │   ├── hippocampus_recall_test.exs
│   │       │   ├── hippocampus_stub.exs.disabled
│   │       │   ├── hippocampus_telemetry_test.exs
│   │       │   ├── lifg_alignment_test.exs
│   │       │   ├── lifg_gating_test.exs
│   │       │   ├── lifg_guard_test.exs
│   │       │   ├── lifg_mwe_fallback_test.exs
│   │       │   ├── lifg_priming_integration_test.exs
│   │       │   ├── lifg_scores_mode_test.exs
│   │       │   ├── lifg_tripwire_test.exs
│   │       │   ├── no_self_calls_test.exs
│   │       │   ├── pmtg_rerun_test.exs
│   │       │   ├── pmtg_sense_compat_test.exs
│   │       │   ├── priming_test.exs
│   │       │   ├── reanalysis_test.exs
│   │       │   ├── recall_gating_test.exs
│   │       │   ├── thalamus_math_props_test.exs
│   │       │   ├── thalamus_params_test.exs
│   │       │   ├── thalamus_telemetry_contract_test.exs
│   │       │   ├── wm_dynamics_test.exs
│   │       │   ├── wm_eviction_decay_test.exs
│   │       │   ├── wm_gate_integration_test.exs
│   │       │   └── wm_gating_integration_test.exs
│   │       ├── support
│   │       │   └── telemetry_helpers.exs
│   │       └── test_helper.exs
│   ├── core
│   │   ├── README.md
│   │   ├── lib
│   │   │   ├── core
│   │   │   │   ├── brain
│   │   │   │   │   └── index.ex
│   │   │   │   ├── brain.ex
│   │   │   │   ├── brain_adapter.ex
│   │   │   │   ├── input.ex
│   │   │   │   ├── invariants.ex
│   │   │   │   ├── lex_id.ex
│   │   │   │   ├── lexicon
│   │   │   │   │   ├── normalize.ex
│   │   │   │   │   ├── senses.ex
│   │   │   │   │   └── stage.ex
│   │   │   │   ├── lexicon.ex
│   │   │   │   ├── lifg_input.ex
│   │   │   │   ├── mwe_injector.ex
│   │   │   │   ├── neg_cache.ex
│   │   │   │   ├── phrase_repo
│   │   │   │   │   └── default.ex
│   │   │   │   ├── phrase_repo.ex
│   │   │   │   ├── recall
│   │   │   │   │   ├── execute.ex
│   │   │   │   │   ├── gate.ex
│   │   │   │   │   └── plan.ex
│   │   │   │   ├── runtime_bind.ex
│   │   │   │   ├── segmenter.ex
│   │   │   │   ├── semantic_input.ex
│   │   │   │   ├── sense_slate.ex
│   │   │   │   ├── text.ex
│   │   │   │   ├── token.ex
│   │   │   │   ├── token_filters.ex
│   │   │   │   └── vectors.ex
│   │   │   ├── core.ex
│   │   │   └── math
│   │   │       └── math.ex
│   │   ├── mix.exs
│   │   ├── priv
│   │   │   └── negcache
│   │   │       └── negcache.dets
│   │   └── test
│   │       ├── core
│   │       │   ├── invariants_test.exs
│   │       │   ├── lifg_input_test.exs
│   │       │   ├── mwe_injector_test.exs
│   │       │   ├── recall
│   │       │   │   └── execute_hippo_integration_test.exs
│   │       │   ├── resolve_input_test.exs
│   │       │   ├── runtime_bind_test.exs
│   │       │   ├── semantic_input_sense_candidates_test.exs
│   │       │   ├── sense_slate_test.exs
│   │       │   ├── token_mw_test.exs
│   │       │   ├── token_test.exs
│   │       │   ├── tokenizer_defaults_test.exs
│   │       │   └── tokenizer_wordgrams_test.exs
│   │       └── test_helper.exs
│   ├── db
│   │   ├── README.md
│   │   ├── lib
│   │   │   ├── db
│   │   │   │   ├── brain_cell.ex
│   │   │   │   ├── episode.ex
│   │   │   │   ├── episodes.ex
│   │   │   │   ├── lexicon.ex
│   │   │   │   ├── my_embeddibgs.ex
│   │   │   │   └── postgrex_types.ex
│   │   │   └── db.ex
│   │   ├── mix.exs
│   │   ├── priv
│   │   │   ├── db
│   │   │   │   └── migrations
│   │   │   │       ├── 20250914053633_create_brain_cells_consolidated.exs
│   │   │   │       └── 20251001000000_create_episodes.exs
│   │   │   └── repo
│   │   │       └── migrations
│   │   │           └── 20250708150554_create_brain_cells.exs
│   │   └── test
│   │       ├── db
│   │       │   └── episodes_test.exs
│   │       ├── db_test.exs
│   │       └── test_helper.exs
│   ├── lexicon
│   │   ├── README.md
│   │   ├── lib
│   │   │   ├── lexicon
│   │   │   │   └── behaviou.ex
│   │   │   └── lexicon.ex
│   │   ├── mix.exs
│   │   └── test
│   │       └── test_helper.exs
│   ├── llm
│   │   ├── README.md
│   │   ├── lib
│   │   │   └── llm.ex
│   │   ├── mix.exs
│   │   └── test
│   │       ├── llm_test.exs
│   │       └── test_helper.exs
│   ├── symbrella
│   │   ├── README.md
│   │   ├── lib
│   │   │   ├── symbrella
│   │   │   │   ├── application.ex
│   │   │   │   └── mailer.ex
│   │   │   └── symbrella.ex
│   │   ├── mix.exs
│   │   └── test
│   │       └── test_helper.exs
│   └── symbrella_web
│       ├── README.md
│       ├── assets
│       │   ├── css
│       │   │   └── app.css
│       │   ├── js
│       │   │   ├── app.js
│       │   │   └── hooks
│       │   │       └── chat.js
│       │   ├── package.json
│       │   ├── postcss.config.js
│       │   ├── tailwind.config.js
│       │   ├── tsconfig.json
│       │   └── vendor
│       │       ├── daisyui-theme.js
│       │       ├── daisyui.js
│       │       ├── heroicons.js
│       │       └── topbar.js
│       ├── lib
│       │   ├── symbrella_web
│       │   │   ├── application.ex
│       │   │   ├── components
│       │   │   │   ├── core_components.ex
│       │   │   │   ├── layouts
│       │   │   │   │   └── root.html.heex
│       │   │   │   └── layouts.ex
│       │   │   ├── controllers
│       │   │   │   ├── error_html.ex
│       │   │   │   ├── error_json.ex
│       │   │   │   ├── page_controller.ex
│       │   │   │   ├── page_html
│       │   │   │   │   └── home.html.heex
│       │   │   │   └── page_html.ex
│       │   │   ├── endpoint.ex
│       │   │   ├── gettext.ex
│       │   │   ├── live
│       │   │   │   ├── brain_live.ex
│       │   │   │   └── home_live.ex
│       │   │   ├── router.ex
│       │   │   └── telemetry.ex
│       │   └── symbrella_web.ex
│       ├── mix.exs
│       ├── package.json
│       ├── priv
│       │   ├── gettext
│       │   │   ├── en
│       │   │   │   └── LC_MESSAGES
│       │   │   │       └── errors.po
│       │   │   └── errors.pot
│       │   └── static
│       │       ├── assets
│       │       │   ├── app.css
│       │       │   └── app.js
│       │       ├── favicon-91f37b602a111216f1eef3aa337ad763.ico
│       │       ├── favicon.ico
│       │       ├── images
│       │       │   ├── logo-06a11be1f2cdde2c851763d00bdd2e80.svg
│       │       │   ├── logo-06a11be1f2cdde2c851763d00bdd2e80.svg.gz
│       │       │   ├── logo.svg
│       │       │   └── logo.svg.gz
│       │       ├── robots-9e2c81b0855bbff2baa8371bc4a78186.txt
│       │       ├── robots-9e2c81b0855bbff2baa8371bc4a78186.txt.gz
│       │       ├── robots.txt
│       │       └── robots.txt.gz
│       └── test
│           ├── support
│           │   └── conn_case.ex
│           ├── symbrella_web
│           │   └── controllers
│           │       ├── error_html_test.exs
│           │       ├── error_json_test.exs
│           │       └── page_controller_test.exs
│           └── test_helper.exs
├── assets
│   └── css
│       └── app.css
├── brain_stage1_bench.md
├── config
│   ├── config.exs
│   ├── dev.exs
│   ├── prod.exs
│   ├── runtime.exs
│   └── test.exs
├── mix.exs
└── mix.lock
```

---

## Docs & references

- **Guardrails:** `SYMBRELLA_PROJECT_GUARDRAILS.md`
- **Brain chain notes:** `README_BRAIN_CHAIN.md`
- **Agents overview:** `AGENTS.md`
- **Resume playbook:** `PROJECT-RESUME-PLAYBOOK.md`

---

## Approval protocol

Nothing merges or “goes live” without an explicit approval token (see `SYMBRELLA_PROJECT_GUARDRAILS.md`).  
Example: `Approve: P-021 (FileScope: README.md)`.
