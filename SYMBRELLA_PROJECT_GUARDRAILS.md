
# Symbrella Project Guardrails
*(Directory Structure, Semantics & Approval Protocol — living doc)*  
**Last updated: 2025-11-18**

> **Purpose.** This is the single source of truth we both refer to before any refactor or file replacement, so we don’t mangle the project. It encodes directory layout, module boundaries, semantic contracts, and our pair‑programming approval flow.

---

## TL;DR

- **Approval tokens required.** Never ship changes without an explicit token in chat:  
  `Approve: P-###` or `Approve: P-### (FileScope: apps/.../file.ex)`.
- **Full‑File Patch Guardrail.** Before any code is written, paste the entire current file; responses are **full‑file replacements only**. Approvals must include FileScope when possible.
- **One umbrella‑root supervisor.** Start everything under `Symbrella.Application`. **No per‑app Application modules.**
- **Acyclic deps:** `db ← brain ← core ← web` (left depends on nothing to the right).
- **LIFG lives in `apps/brain`.** Core orchestrates the pipeline and can call Lexicon / Brain as a client.
- **LIFG path invariants:** words‑only (no char‑grams), boundary guard, MWE injection, sorted spans, config defaults set in test/dev.
- **Curiosity loop invariants:** `Brain.Curiosity → Brain.Thalamus → Brain.BasalGanglia/WM → Brain.DLPFC` is telemetry‑first, side‑effect bounded, and always gates via WM policy (no rogue focus inserts).
- **Deliverables:** Prefer chat‑bubble code blocks only (UTF‑8, LF). Avoid ZIPs/binaries. Multi‑file outputs are sent as consecutive chat bubbles with clear filename headers. **No unsolicited multi‑pane diffs.**  
  *(Exception: a small `.zip` is OK only when explicitly requested.)*

See also: **README.md** (high‑level), **README_BRAIN_CHAIN.md** (pipeline notes).

---

## Semantics Guiderail (Core ↔ Brain)

This section captures the **semantic contracts** that guide development across Core + Brain. Implementation can be staged over time, but design should not drift from these rules.

### 1. Tokenization contract (LIFG path)

- **Words‑only into LIFG.**
  - `Core.Token.tokenize/1` (or equivalent) must feed **word tokens only** into the LIFG path.
  - Char‑grams are allowed only in **search / fuzzy recall** paths, never in LIFG Stage‑1.
- **MWE pass before sense selection.**
  - `Core.MWEInjector` runs before sense selection; it injects phrase tokens with `mw: true`.
  - MWEs are formed with **word‑level n‑grams** (no char‑gram MWEs).
- **Boundary guard.**
  - Anything handed to LIFG must respect word boundaries.
  - Substring spans that don’t start and end on word boundaries are dropped **unless** `mw: true`.
- **Config defaults (dev/test):**
  - `config :core, tokenizer_mode: :words, tokenizer_emit_chargrams: false`.

### 2. SemanticInput (SI) contract

`Core.SemanticInput` is the **carrier** of semantic state between Core and Brain.

Required fields (by intent of design):

- `sentence` — original string (when available).
- `tokens` — normalized token list.
- `token_structs` — rich token structs with spans, POS, flags, etc.
- `pos_list` — POS tags per token.
- `intent`, `keyword`, `confidence` — resolved intent triple.
- `phrase_matches` — info about matched phrases/MWEs.
- `activation_summary` — snapshot from Brain (active cells, WM summary).
- `pattern_roles` — pattern/role tags used by intent matrix, later ACC/OFC.
- `sense_candidates` — **map keyed by token index**:

  ```elixir
  si.sense_candidates[token_index] = [
    %{
      id: "lemma|pos|sense",
      pos: :noun | :verb | ...,
      lemma: "word" | "phrase",
      mwe?: boolean(),
      score_parts: %{lex_fit: float(), rel_prior: float(), activation: float(), intent_bias: float()}
    },
    ...
  ]
  ```

Guarantees:

- When PMTG has run, any token it touched has an entry in `si.sense_candidates`.
- LIFG Stage‑1 **reads from** `si.sense_candidates` rather than re‑inventing candidates.

### 3. Intent resolution contract

- **Classifier → matrix fallback.**
  - Primary classifier produces `(intent, keyword, confidence)`.
  - If `confidence < threshold`, Core falls back to `Core.IntentMatrix` (or equivalent) **without discarding context** (tokens, POS, keyword).
- **Telemetry.**
  - On fallback, emit `[:core, :intent, :fallback]` with measurements and features (confidence, patterns, etc.).
- **Ownership.**
  - Intent logic lives in **Core**, not Brain. Brain can receive intent and bias scores but does not own the classifier.

### 4. PMTG (sense picking) rules

- **MWE ↔ sense compatibility.**
  - If a token is a phrase (`mw: true`), candidate senses must be compatible (phrase senses first, or phrase‑safe fallbacks).
  - When no compatible MWE senses exist, PMTG either:
    - backs off to head‑word senses, or
    - records a “no‑MWE‑senses” event and uses a safe default.
- **Surface winners + near‑winners.**
  - PMTG doesn’t just pick a single winner; it pushes winners and near‑winners (with margins) into `si.sense_candidates[token_index]`.
- **Telemetry.**
  - Emit something like `[:brain, :pmtg, :no_mwe_senses]` when MWE candidates are missing or filtered.

### 5. LIFG Stage‑1 rules

- **Hard char‑gram rejection.**
  - Stage‑1 must **reject** any candidate whose surface form is a char‑gram, with a telemetry tripwire:
    - `[:brain, :lifg, :chargram_violation]`.
- **Boundary guard (again).**
  - If a candidate span violates word boundaries and isn’t `mw: true`, it is dropped and logged:
    - `[:brain, :lifg, :boundary_drop]`.
- **Feature mix.**
  - Stage‑1 score is a weighted mix of:
    - `lex_fit` (lexical fit),
    - `rel_prior` (relative prior),
    - `activation` (Brain activity / WM),
    - `intent_bias` (intent‑based nudge),
    - plus an eventual `episode_boost` from Hippocampus recall.
- **Ownership.**
  - LIFG Stage‑1 lives entirely in **apps/brain**.

### 6. Hippocampus contract (episodes)

- **Write path.**
  - Episodic writes happen at **ATL finalize** (or equivalent consolidation point).
  - Episodes carry:
    - token/lemma set,
    - meta (scope, outcome),
    - timestamps.
- **Recall path.**
  - Recall runs during PMTG / LIFG context building.
  - Scoring combines:
    - Jaccard overlap with cue tokens,
    - recency half‑life (`half_life_ms`),
    - optional outcome uplift (bounded).
- **Evidence wiring.**
  - Recall writes into `si.evidence[:episodes]`.
  - LIFG and WM can use `episodes` as a bias, but not as a hard override.

See the **Hippocampus Options** section below for runtime config.

### 7. MoodCore modulation

- Mood vectors (`expl`, `inhib`, `vigil`, `plast`) can tilt thresholds but **must remain bounded**.
- Thalamus (see below) is one of the primary consumers of mood:
  - `mood_cap` limits contribution from mood to the curiosity gate.
  - `mood_weights` define per‑dimension influence.
- Mood effects are always:
  - logged via telemetry,
  - observable via Brain snapshots / BrainLive,
  - never silently overriding hard safety floors.

### 8. Provenance & correctness

- Any surfaceable claim Symbrella makes about **its own internal reasoning** should, when possible, link back to:
  - one or more `Db.BrainCell` ids, and/or
  - one or more `Db.Episode` ids.
- Negative knowledge (unknown words/phrases) is tracked via `Core.NegCache`:
  - Cached with TTL,
  - Usable for Curiosity / exploration,
  - **Never** treated as a LIFG decision signal on its own.

### 9. Telemetry & invariants (tests)

- Invariants (enforced via tests wherever possible):
  - No char‑grams reach LIFG Stage‑1.
  - Token spans are sorted by `start` and hydrated from `sentence`.
  - Non‑MWE substrings violating word boundaries are dropped pre‑LIFG.
  - If PMTG ran, `si.sense_candidates` exists and is populated for touched tokens.
- Property tests:
  - Boundary guard maintains ordering and does not create overlaps.
  - MWE injection produces deterministic spans and ids.

### 10. Operational DoD (per PR / patch)

For any patch touching the semantic pipeline (Core or Brain), we aim for:

- Relevant telemetry events show up in test logs.
- Invariant/property tests are green.
- At least one **“reason trace”** (small, textual) demonstrating:
  - input sentence,
  - SI summary,
  - LIFG decision(s),
  - WM / episode influences (if any).

---

## Curiosity → Thalamus → BG/WM → DLPFC Loop

This section encodes the contract for the “tiny exploratory acts” pipeline.

### Roles

- **Brain.Curiosity**
  - Region that produces curiosity probes (internally or via `nudge/0`).
  - Emits telemetry: `[:curiosity, :proposal]` with:
    - measurements: `%{score :: 0.0..1.0}`,
    - metadata: `%{probe: %{id, source, reason, ...}}`.
  - Does **not** directly mutate WM.

- **Brain.Thalamus**
  - Listens to:
    - `[:curiosity, :proposal]`,
    - `[:brain, :ofc, :value]`,
    - `[:brain, :acc, :conflict]`.
  - Computes a blended curiosity decision using:
    - OFC value (`ofc_weight`),
    - ACC conflict brake (`acc_alpha`),
    - optional mood modulation (`mood_cap`, `mood_weights`).
  - Emits: `[:brain, :thalamus, :curiosity, :decision]` with:
    - measurements: `%{score :: 0.0..1.0}`,
    - metadata containing (at minimum):

      ```elixir
      %{
        decision: :allow | :boost | :block,
        probe: map(),
        ofc_blended?: boolean(),
        ofc_value: float() | nil,
        ofc_weight: float(),
        acc_applied?: boolean(),
        acc_conflict: float() | nil,
        acc_alpha: float(),
        mood_cap: float(),
        mood_weights: map(),
        source: :test | :runtime | String.t(),
        v: integer()  # contract version
      }
      ```

  - Config surface:
    - `Brain.Thalamus.get_params/0` returns `%{ofc_weight, acc_alpha, mood_cap, mood_weights}`.
    - `Brain.Thalamus.set_params/1` overlays runtime overrides, clamped into safe ranges.

- **Brain.BasalGanglia** (stateless WM gate)
  - Pure module implementing `decide/4 :: (wm, cand, attn, cfg) → {decision, score}`.
  - Decisions ∈ `:allow | :boost | :block`.
  - Uses:
    - WM fullness,
    - duplicate detection (by `id` + fuzzy key),
    - cooldown rebump,
    - source preferences (`prefer_sources`, `disprefer_sources`),
    - per‑source boosts (`source_boosts`).
  - Never mutates WM directly; it only returns a decision and adjusted score.

- **Brain.WorkingMemory** (pure utilities)
  - Normalizes candidates into WM items via `normalize/3`:

    ```elixir
    %{
      id: term(),
      source: atom() | String.t() | nil,
      activation: float(),
      score: float(),
      ts: non_neg_integer(),
      inserted_at: non_neg_integer(),
      last_bump: non_neg_integer(),
      payload: map()
    }
    ```

  - Supports `upsert/3`, `decay/3`, `trim/2`, `remove/2`.
  - Identity is by `id`; merging preserves best score/activation and latest timestamps.

- **Brain.DLPFC**
  - First‑class region (`use Brain, region: :dlpfc`).
  - Subscribes to:
    - `[:curiosity, :proposal]` (caches latest probe),
    - `[:brain, :thalamus, :curiosity, :decision]` (hears decisions).
  - On curiosity proposal:
    - Caches `probe` with:
      - normalized `score`,
      - `source: :runtime` (preferred for WM),
      - `reason: :curiosity`.
    - Emits `[:brain, :dlpfc, :proposal_cached]`.
  - On thalamus decision:
    - Emits `[:brain, :dlpfc, :heard]` with decision + flags.
    - If started with `act_on_thalamus: true` **and**:
      - decision ∈ `:allow | :boost`,
      - cached probe exists,
      - then calls `Brain.focus/2` with the cached probe (which, in turn, uses WM + BG).

### Loop invariants

- Curiosity never calls `Brain.focus/2` directly.
- Thalamus never calls `Brain.focus/2` directly.
- DLPFC is the **only** component in this loop allowed to call `Brain.focus/2` for curiosity probes.
- WM writes always pass through:
  - `Brain.BasalGanglia.decide/4`,
  - `Brain.WorkingMemory.normalize/3` + `upsert/3`.
- Tests:
  - `Brain.ThalamusTelemetryContract_Test` validates telemetry shape/versioning.
  - `Brain.ThalamusParams_Test` keeps config wiring honest.
  - `Brain.CuriosityFlowTest` asserts that a curiosity nudge can result in a WM item
    whose payload `reason` is `:curiosity`.

---

## Current Status Snapshot (Nov 18, 2025)

### LIFG DoD (Definition of Done — working checklist)

- [ ] No char‑grams in LIFG path (**enforced + unit test**)
- [ ] Boundary guard (**drop non‑word‑boundary substrings unless `mw: true`**)
- [ ] MWE injection pass (**word‑level n‑grams before LIFG**)
- [✅] Sense slate in SI (**`si.sense_candidates` keyed by token index**)
- [ ] Reanalysis fallback (**flip to next‑best on integration failure**)
- [ ] Telemetry tripwire (**log/drop if a char‑gram reaches LIFG**)
- [ ] Priming cache (**optional; recency boost for recent winners**)
- [ ] Invariant tests (**spans sorted; no char‑grams; boundary‑only unless `mw: true`**)
- [ ] Config defaults (**test/dev:** `tokenizer_mode: :words`, `tokenizer_emit_chargrams: false`)

**Notes**

- Telemetry test harness (`Support.TelemetryHelpers`) is in place and green.
- BoundaryGuard + Tripwire modules exist; wiring + tests still to be finalized.
- MWE injector lives in Core; needs full pass + confirmation tests.
- Priming scaffolding exists; not yet engaged as a scored feature in Stage1.

### Curiosity / Thalamus / DLPFC snapshot

- `Brain.ThalamusTelemetryContract_Test` and `Brain.ThalamusParams_Test` are green.
- `Brain.CuriosityFlowTest` is green under full test suite.
- Thalamus params surface:
  - `ofc_weight`, `acc_alpha`, `mood_cap`, `mood_weights`.
- WM pipeline for curiosity probes is exercised via integration tests.

---

## Canonical Umbrella Layout (reflects current repo)

This is the canonical, minimal structure we track. If a directory/file isn’t listed here, treat it as optional. When we add new dirs, **we update this doc**.

```text
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
│   │   ├── bench
│   │   │   ├── brain_lifg_bench_v2_1.exs
│   │   │   ├── brain_stage1_bench.exs
│   │   │   └── profile_stage1.exs
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
│   │   │   │   ├── ptmg.ex                  # Posterior MTG (ensure correct file name)
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
│   │   │   │   ├── my_embeddibgs.ex        # (typo acknowledged; fix in a future patch)
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
│   │   │   │   └── behaviou.ex            # (typo acknowledged)
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

## Module boundaries & ownership

- **Brain**
  - Owns LIFG and WM/episodic coordination.
  - Hosts all brain‑inspired regions: LIFG, Hippocampus, ACC, OFC, Basal Ganglia, Curiosity, Thalamus, DLPFC, etc.
  - Responsible for:
    - WM policy (via `Brain.BasalGanglia` + `Brain.WorkingMemory`),
    - episodic memory (Hippocampus),
    - mood modulation hooks,
    - curiosity loop execution.
  - Does **not** own tokenization or intent classification.

- **Core**
  - Orchestrates: `tokenize → Brain.STM → Db.LTM → Lexicon`.
  - Responsible for:
    - `Core.SemanticInput`,
    - MWE injection,
    - intent classification + IntentMatrix fallback,
    - invariants over SI and tokens,
    - recall planning (`Core.Recall`).
  - Treats Brain as a **service**: no back‑refs from Brain into Core.

- **Db**
  - Provides Ecto schemas and Repo (`Db`).
  - Owns:
    - `Db.BrainCell`,
    - `Db.Episode`,
    - `Db.Lexicon`,
    - pgvector‑related types.
  - Contains persistence only; no heavy brain logic.

- **Web (symbrella_web)**
  - Presents UI.
  - May call into Brain/Core for snapshots and actions (e.g., `GenServer.call(Brain, :snapshot)`).
  - Does not mutate DB directly except through sanctioned Core/Brain APIs.

- **Acyclic dependencies**
  - Hard rule: `db ← brain ← core ← web`.
  - Never reverse this chain.
  - No cross‑app cycles; if a new module would violate this, redesign before writing code.

---

## Golden pipeline

```elixir
phrase
|> Core.Token.tokenize()   # word tokens first; sentence-aware spans
|> Brain.stm()             # short-term focus/activation (processes)
|> Db.ltm()                # long-term memory fetch (rows)
|> Core.Lexicon.all()      # dictionary lookups as needed
```

**Bandwidth principle:**  

- Prefer: tokenize → check active cells → DB → lexicon (as a last resort).  
- Avoid: calling external lexicon or embeddings when `Brain` already knows enough.

---

## Hippocampus options (runtime)

- `window_keep` (int, default `300`): Maximum episodes retained in the rolling window.
- `half_life_ms` (int, default `300_000`): Recency half‑life (ms) for recall scoring. Higher = slower decay.
- `recall_limit` (int, default `3`): Default top‑K returned by `recall/2` when `:limit` isn’t specified.
- `min_jaccard` (float `0.0–1.0`, default `0.0`): Minimum raw Jaccard overlap to consider an episode (before recency).
- `scope` (map, optional): When passed to `recall/2`, only episodes whose `episode.meta` contain every `{k, v}` in scope are eligible. Atom/string keys match case‑sensitively without creating new atoms.

**Operational notes**

- **Dedup‑on‑write:** consecutive writes with the same token set refresh the head timestamp instead of appending a new episode. Meta on the head is preserved; only the timestamp moves forward. Insert a spacer write if you need two identical token sets recorded back‑to‑back.
- **Telemetry (stable):**
  - **Write:** `[:brain, :hippocampus, :write]` → measurements `%{window_size}`; metadata `%{meta}`.
  - **Recall:** `[:brain, :hippocampus, :recall]` → measurements `%{cue_count, window_size, returned, top_score}`; metadata `%{limit, half_life_ms}`.

---

## LIFG path: Definition of Done (DoD) — reference

*(Track progress in code reviews; we’ll mark ✅ as items are completed)*

- [ ] No char‑grams in LIFG path (enforced + unit test)
- [ ] Boundary guard (drop non‑word‑boundary substrings unless `mw: true`)
- [ ] MWE injection pass (word‑level n‑grams before LIFG)
- [✅] Sense slate in SI (`si.sense_candidates` keyed by token index)
- [ ] Reanalysis fallback (flip to next‑best on integration failure)
- [ ] Telemetry tripwire (log/drop if a char‑gram reaches LIFG)
- [ ] Priming cache (optional; recency boost for recent winners)
- [ ] Invariant tests (spans sorted; no char‑grams; boundary‑only unless `mw: true`)
- [ ] Config defaults (test/dev: `tokenizer_mode: :words`, `tokenizer_emit_chargrams: false`)

**Additional invariants**

- Tokens sorted by `start`; spans hydrated from the sentence.
- ID convention: `word|pos|sense` for `Db.BrainCell.id` (string PK).

**Guard usage rule**

- Keep guards simple. No complex function calls (e.g., `String.contains?/2`) inside guards. Precompute booleans before `when`.

---

## Configuration defaults

Set in `config/test.exs` and `config/dev.exs`:

```elixir
config :core,
  tokenizer_mode: :words,
  tokenizer_emit_chargrams: false
```

Where appropriate, use `Application.compile_env!/2` to fail fast on misconfig that would break invariants.

Phoenix/Web may print:

```elixir
IO.inspect(GenServer.call(Brain, :snapshot), limit: :infinity)
```

for visibility without mutating state.

---

## Approval protocol (pair‑programming guardrails)

Nothing merges or “goes live” without an explicit approval token in chat.

**Full‑File Patch Guardrail (required)**

- Before any code, paste the **full current file** for each file to be edited.
- Assistant returns **full‑file replacements only**; no diffs or fragments.
- Approval tokens should include FileScope when possible, e.g.,  
  `Approve: P-005 (FileScope: SYMBRELLA_PROJECT_GUARDRAILS.md)`.
- No edits outside the approved FileScope without a new approval.

**Flow**

1. **Proposal.** A patch proposal with ID `P-###` includes:
   - Summary (what/why), risk level (**Safe / Risky / BREAKING**), and scope.
   - Key before/after code excerpts.
   - File list with replacements or additions.
   - Tests to add/update.
   - Rollback plan (git commands).

2. **Deliverables.**
   - Prefer chat‑bubble code blocks only (UTF‑8, LF).
   - Avoid ZIPs/binaries unless explicitly requested.
   - Multi‑file outputs are sent as consecutive chat bubbles with clear filename headers.
   - No unsolicited multi‑pane diffs. One module per “pane”.

3. **Approval.**
   - You respond with `Approve: P-###` (+ optional FileScope).
   - If not approved, we do no file replacements. Iterations can use `P-###.1`, etc.

4. **Special tags (for visibility).**
   - `QuickFix-###`: small, surgical changes.
   - `BREAKING-###`: migrations, API changes, or module moves.
   - `Hotfix-###`: urgent bug fix; still needs an approval token before applying.

5. **After approval** only the agreed‑upon artifacts are produced; no hidden refactors beyond the approved scope.

**Template**

```text
Patch ID: P-###
Title: <one-line summary>
Risk: Safe | Risky | BREAKING
Files touched:
  - apps/brain/lib/brain/lifg.ex (replace)
  - apps/core/lib/core/token.ex (edit)

Summary:
  <plain-language explanation>

Tests:
  <list of tests added/updated>

Rollback:
  git checkout HEAD~1 -- <files>   # or
  git revert <commit>
```

---

## Module & dependency rules

- **One umbrella‑root supervisor:** `Symbrella.Application` starts Brain + others.
- **No per‑app Application supervisors.**
- **Acyclic dependencies:** `db ← brain ← core ← web`.
  - Brain can depend on Db. Core can depend on Brain and Db. Web depends on Core/Brain.
  - **Never reverse this chain.**
- Avoid “clever” module layouts that hide these relationships; keep them obvious.

---

## Rollback & recovery (Git cookbook)

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

## Working session conventions

- If the human says “hold off until I paste more files”, pause changes and only review.
- If a task is time‑boxed or ambiguous, prefer a small, safe delta plus a proposal for the next step.
- Do not introduce new modules/dirs without a `P-###` that updates this doc’s layout.
- **No hidden background work:** everything ships in the current message.

---

## “Known‑good” patterns (snapshots)

- **Web print before DOM push:**

  ```elixir
  IO.inspect(GenServer.call(Brain, :snapshot), limit: :infinity)
  ```

- **Core pipeline (orchestration only):**

  ```elixir
  phrase
  |> Core.Token.tokenize()
  |> Brain.stm()
  |> Db.ltm()
  |> Core.Lexicon.all()
  ```

  Keep LIFG in Brain; Core may call Lexicon as a public facade.

---

### Appendix: Glossary

- **STM:** short‑term memory / active focus in Brain processes.
- **LIFG:** left‑inferior‑frontal‑gyrus (symbolic disambiguation layer).
- **MWE:** multi‑word expression (inserted as tokens prior to LIFG).
- **DoD:** Definition of Done; check before PR approval.
- **WM:** Working Memory; list of normalized items under gating control.
- **ACC:** Anterior Cingulate Cortex; conflict monitor (feeds conflict into Thalamus).
- **OFC:** Orbitofrontal Cortex; value estimation (feeds value into Thalamus).
- **DLPFC:** Dorsolateral Prefrontal Cortex; execution/goal holder for small acts.
- **Curiosity:** Region that proposes exploratory probes, subject to Thalamus & BG gating.
