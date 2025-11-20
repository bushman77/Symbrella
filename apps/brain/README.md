# Brain â€” Process Layer of Symbrella

This app hosts the **process-level â€œbrainâ€** for Symbrellaâ€™s Neuro-Symbolic Synthetic Intelligence (NSSI).

Where `core` is mostly about **data structures and orchestration**, `brain` is about **running processes**:
OTP regions for things like LIFG, Hippocampus, Thalamus, Curiosity, Working Memory, and gating/valuation loops.

At a high level:

- Models multiple **brain-inspired regions** as OTP processes (LIFG, Hippocampus, ACC, OFC, DLPFC, Basal Ganglia, Thalamus, Curiosity, etc.).
- Maintains **working memory (WM)** as a normalized, capacity-limited list with decay and gating.
- Coordinates **episodic memory** (Hippocampus) and its windowed recall.
- Runs the **LIFG Stage-1 sense selection** over candidates prepared by `core` (no tokenization lives here).
- Implements the **Curiosity â†’ Thalamus â†’ BG/WM â†’ DLPFC** loop that decides which small exploratory probes deserve focus.
- Exposes **telemetry events** that the UI (and tests) rely on to explain what the brain is doing.

> Design rule: `db â† brain â† core â† web`  
> Brain depends on `db`, **never** on `core` or `symbrella_web`. Core calls into Brain as a client.

---

## Responsibilities

### 1. Regions as OTP processes

Each major region is a module under `Brain.*` and typically `use Brain, region: :name`:

- `Brain.LIFG` â€” Stage-1 sense selection (competitive scoring over `si.sense_candidates`).
- `Brain.Hippocampus` â€” episodic memory entry point (writes/recall/telemetry).
- `Brain.Thalamus` â€” relay & arbitration layer for curiosity/value/conflict/mood signals.
- `Brain.Curiosity` â€” proposes exploratory probes (does not write WM directly).
- `Brain.BasalGanglia` â€” pure WM gate; decides `:allow | :boost | :block`.
- `Brain.WorkingMemory` â€” normalizes items, applies decay, and keeps WM bounded.
- `Brain.DLPFC` â€” turns approved probes into **focus actions** via `Brain.focus/2`.
- `Brain.MoodCore` / `Brain.MoodPolicy` â€” mood vector calculation and modulation hooks.
- Additional regions (ACC, OFC/vmPFC, etc.) sit alongside these and communicate via telemetry and messages.

Most regions:

- Have a **pure core** of scoring / math utilities.
- Wrap that logic in a **GenServer** (or equivalent) for long-running behaviour.
- Expose a small public API (`nudge/0`, `snapshot/0`, `set_params/1`, etc.) plus telemetry.

### 2. Working Memory (WM)

`Brain.WorkingMemory` is a pure module; it does not own a process. It defines the WM item shape and the operations used everywhere:

- `normalize/3` turns arbitrary candidates into WM items:

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

- `upsert/3` merges duplicates (by `id`) and refreshes timestamps.
- `decay/3` applies half-life style decay based on the last bump / insertion time.
- `trim/2` enforces WM capacity.
- `remove/2` drops items by `id` or predicate.

WM is **always** updated via:

1. Basal Ganglia (`Brain.BasalGanglia.decide/4`) to determine whether to admit/boost/block.
2. `Brain.WorkingMemory.normalize/3` + `upsert/3` to actually change the WM list.

No other module should be hand-crafting WM items.

### 3. Hippocampus (episodic memory)

`Brain.Hippocampus` and its `hippocampus/*` helpers implement a small, windowed episodic memory:

- Writes happen at **consolidation time** (e.g., ATL finalize) with:
  - token set,
  - meta scope/outcome,
  - timestamps,
  - optional embedding (via `db`).
- In-memory window is bounded (configurable `window_keep`).
- Recall combines:
  - **token overlap** (Jaccard),
  - **recency** (configurable half-life),
  - optional **outcome uplift**.
- Recall results are attached back to `Core.SemanticInput` via `evidence[:episodes]` so LIFG/WM can bias decisions without hard overrides.

Runtime tuning is available via config and `Brain.Hippocampus` helper functions.

### 4. LIFG Stage-1 (competitive sense selection)

`Brain.LIFG` and `Brain.LIFG.Stage1` implement the **first pass** of sense selection. Core prepares a **sense slate** in `si.sense_candidates[token_index]`; LIFG consumes that slate and emits winners + audit.

Main responsibilities:

- Read candidate senses from `si.sense_candidates[token_index]` (prepared by Core).
- Enforce LIFG path invariants, implemented by `Brain.LIFG.BoundaryGuard` and `Brain.LIFG.Stage1Guard`:
  - **No char-grams** in the LIFG path (hard drop + audit counter).
  - **Boundary guard:** spans must align to word boundaries unless `mw: true`.
  - MWEs are always formed from **word-level n-grams** upstream in Core; LIFG never generates char-grams.
- Score candidates with a weighted mix of:
  - lexical fit,
  - prior / activation,
  - intent bias,
  - (eventually) episode-based boosts from Hippocampus.
- Produce, per token index:
  - the chosen sense id,
  - full ranking (`finalists`),
  - `margin` vs. the runner-up,
  - an `audit` map describing drops and weak decisions.

Typical `last` state from `Brain.LIFG.status/0` includes:

- `tokens` â€” the candidate tokens/MWEs with spans and `mw` flags.
- `guards` â€” raw guard info (e.g. `chargram_violation`, `rejected_by_boundary` indices).
- `audit` â€” summarized counts: `kept_tokens`, `dropped_tokens`, `boundary_drops`, `chargram_violation`, `weak_decisions`, etc.
- `choices` / `finalists` â€” winners and rankings per `token_index`.
- `intent` / `confidence` â€” intent bias used during scoring.
- `opts` â€” resolved runtime config (weights, scores mode, `margin_threshold`, `mwe_fallback`, ACC tau, pMTG mode).

Tests under `apps/brain/test/brain/lifg_*_test.exs` enforce:

- No char-grams in LIFG Stage-1 input.
- Boundary-only spans unless `mw: true`.
- Stable span ordering by start position.
- MWE fallback behaviour when phrase-level interpretations win over word senses.
- Telemetry + audit contracts for violations and decisions.

### 5. Curiosity â†’ Thalamus â†’ BG/WM â†’ DLPFC

This loop encodes â€œtiny exploratory actsâ€ that may or may not earn a slot in WM:

1. `Brain.Curiosity` emits a probe as telemetry (`[:curiosity, :proposal]`).  
   - Provides `probe.id`, `lemma`, `score` in `[0,1]`, and `reason: :curiosity`.
2. `Brain.Thalamus` listens, blends in **OFC value**, **ACC conflict**, and **Mood**, and emits a decision:  
   - Telemetry: `[:brain, :thalamus, :curiosity, :decision]` with `decision â^^ :allow | :boost | :block` and a blended score in `[0,1]`.
   - Runtime parameters are managed via `Brain.Thalamus.get_params/0` and `set_params/1` (e.g. `:ofc_weight`, `:acc_alpha`, `:mood_cap`, `:mood_weights`).  
   - Property tests (`Brain.ThalamusMathProps_Test`) enforce monotonicity and clamping.
3. `Brain.DLPFC` hears the decision; when allowed/boosted, it calls `Brain.focus/2` with the cached probe.
4. `Brain.focus/2` runs the WM pipeline, which always passes through:
   - Basal Ganglia (pure decision),
   - WorkingMemory (normalize + upsert/trim).

Key invariants (enforced by tests):

- Only DLPFC in this loop is allowed to call `Brain.focus/2` for curiosity probes.
- Curiosity and Thalamus never mutate WM directly.
- WM items derived from curiosity have `payload[:reason] == :curiosity` for explainability.

See `Brain.CuriosityFlowTest` and Thalamus/DLPFC tests for the contract.

---

## Telemetry

Brain emits a rich telemetry surface, including (but not limited to):

- **LIFG**  
  - Stage-1 scoring and decisions (e.g. `[:brain, :lifg, :stage1, :score]`, `[:brain, :lifg, :stage1, :decision]`).  
  - Guard violations and audit events (e.g. `[:brain, :lifg, :stage1, :violation]` for char-grams / boundary issues).

- **Hippocampus**  
  - Episode writes and recalls (window size, Jaccard scores, recency/outcome effects).

- **Curiosity / Thalamus / BG / DLPFC**  
  - Curiosity proposals (`[:curiosity, :proposal]`).  
  - Thalamus decisions (`[:brain, :thalamus, :curiosity, :decision]`) with OFC/ACC/mood metadata.  
  - WM gating and admission events from Basal Ganglia and DLPFC.

Tests use `apps/brain/test/support/telemetry_helpers.exs` to assert shapes and invariants.  
The Phoenix UI (Symbrella Brain dashboard) also consumes these events to render region panels and reason traces.

---

## How Core and Web use Brain

- **Core** treats Brain as a service:
  - Calls into Brain for STM/WM and episodic interaction.
  - Does not live inside Brain or depend on Brain internals.
- **Web** (`symbrella_web`) interacts with Brain via:
  - `GenServer.call/2` for snapshots (e.g., `Brain.snapshot/0`),
  - PubSub/telemetry for live dashboards.

If you are working in Web or Core, donâ€™t import Brain internals; stick to the public APIs and structures.

---

## Running tests

From the umbrella root:

```bash
# Brain-only tests
mix test apps/brain/test

# LIFG stack
mix test apps/brain/test/brain/lifg_*_test.exs

# Curiosity / Thalamus / WM loop
mix test apps/brain/test/brain/curiosity_flow_test.exs
mix test apps/brain/test/brain/thalamus_*test.exs
```

You can also run individual files or focused tests via standard `ExUnit` patterns.

---

## Adding or changing a region

When you introduce a new Brain region or modify an existing one:

1. Keep the **dependency rule**: `db â† brain â† core â† web`.
2. Provide a small, pure core of logic that is testable without processes.
3. Wrap it in a minimal process module if you need concurrency/state.
4. Emit telemetry for:
   - key inputs,
   - decisions,
   - errors or invariant violations.
5. Add or update tests under `apps/brain/test/brain/â€¦`.
6. If the regionâ€™s behaviour impacts LIFG/WM/Hippocampus, update the guardrails document (`SYMBRELLA_PROJECT_GUARDRAILS.md`) so the contract stays in sync.

This keeps Brain explainable, testable, and honest about how it reaches decisions.
