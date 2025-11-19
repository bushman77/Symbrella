# Brain — Process Layer of Symbrella

This app hosts the **process-level “brain”** for Symbrella’s Neuro-Symbolic Synthetic Intelligence (NSSI).

Where `core` is mostly about **data structures and orchestration**, `brain` is about **running processes**:
OTP regions for things like LIFG, Hippocampus, Thalamus, Curiosity, Working Memory, and gating/valuation loops.

At a high level:

- Models multiple **brain-inspired regions** as OTP processes (LIFG, Hippocampus, ACC, OFC, DLPFC, Basal Ganglia, Thalamus, Curiosity, etc.).
- Maintains **working memory (WM)** as a normalized, capacity-limited list with decay and gating.
- Coordinates **episodic memory** (Hippocampus) and its windowed recall.
- Runs the **LIFG Stage-1 sense selection** over candidates prepared by `core` (no tokenization lives here).
- Implements the **Curiosity → Thalamus → BG/WM → DLPFC** loop that decides which small exploratory probes deserve focus.
- Exposes **telemetry events** that the UI (and tests) rely on to explain what the brain is doing.

> Design rule: `db ← brain ← core ← web`  
> Brain depends on `db`, **never** on `core` or `symbrella_web`. Core calls into Brain as a client.

---

## Responsibilities

### 1. Regions as OTP processes

Each major region is a module under `Brain.*` and typically `use Brain, region: :name`:

- `Brain.LIFG` — Stage-1 sense selection (competitive scoring over `si.sense_candidates`).
- `Brain.Hippocampus` — episodic memory entry point (writes/recall/telemetry).
- `Brain.Thalamus` — relay & arbitration layer for curiosity/value/conflict signals.
- `Brain.Curiosity` — proposes exploratory probes (does not write WM directly).
- `Brain.BasalGanglia` — pure WM gate; decides `:allow | :boost | :block`.
- `Brain.WorkingMemory` — normalizes items, applies decay, and keeps WM bounded.
- `Brain.DLPFC` — turns approved probes into **focus actions** via `Brain.focus/2`.
- `Brain.MoodCore` / `Brain.MoodPolicy` — mood vector calculation and modulation hooks.
- Additional regions (ACC, OFC, VMPFC, etc.) sit alongside these and communicate via telemetry and messages.

Most regions:

- Have a **pure core** of scoring / math utilities.
- Wrap that logic in a **GenServer** (or equivalent) for long-running behaviour.
- Expose a small public API (`nudge/1`, `snapshot/0`, etc.) plus telemetry.

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

### 4. LIFG Stage-1 (sense selection)

`Brain.LIFG` and `Brain.LIFG.Stage1` implement the **first pass** of sense selection. The main jobs:

- Read candidate senses from `si.sense_candidates[token_index]` (prepared by Core).
- Enforce LIFG path invariants:
  - **No char-grams** in the LIFG path.
  - **Boundary guard:** spans must align to word boundaries unless `mw: true`.
  - MWEs are always formed from **word-level n-grams**, injected in Core.
- Score candidates with a weighted mix of:
  - lexical fit,
  - prior/activation,
  - intent bias,
  - (eventually) episode-based boosts.
- Emit a **sense slate** plus telemetry describing winners, margins, and any drops.

Tests under `apps/brain/test/brain/lifg_*_test.exs` enforce these invariants.

### 5. Curiosity → Thalamus → BG/WM → DLPFC

This loop encodes “tiny exploratory acts”:

1. `Brain.Curiosity` emits a probe as telemetry (`[:curiosity, :proposal]`).
2. `Brain.Thalamus` listens, blends in OFC value, ACC conflict, and mood, and emits a decision (`:allow | :boost | :block`).
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

- LIFG Stage-1 scoring and invariant violations.
- Hippocampus writes/recalls.
- Curiosity proposals and Thalamus decisions.
- WM dynamics (eviction, decay, gating).

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

If you are working in Web or Core, don’t import Brain internals; stick to the public APIs and structures.

---

## Running tests

From the umbrella root:

```bash
# Brain-only tests
mix test apps/brain/test

# Specific group (e.g., LIFG)
mix test apps/brain/test/brain/lifg_*_test.exs

# Curiosity / Thalamus / WM loop
mix test apps/brain/test/brain/curiosity_flow_test.exs
mix test apps/brain/test/brain/thalamus_*_test.exs
```

---

## Adding or changing a region

When you introduce a new Brain region or modify an existing one:

1. Keep the **dependency rule**: `db ← brain ← core ← web`.
2. Provide a small, pure core of logic that is testable without processes.
3. Wrap it in a minimal process module if you need concurrency/state.
4. Emit telemetry for:
   - key inputs,
   - decisions,
   - errors or invariant violations.
5. Add or update tests under `apps/brain/test/brain/…`.
6. If the region’s behaviour impacts LIFG/WM/Hippocampus, update the guardrails document (`SYMBRELLA_PROJECT_GUARDRAILS.md`) so the contract stays in sync.

This keeps Brain explainable, testable, and honest about how it reaches decisions.
