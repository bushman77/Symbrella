# Symbrella Roadmap — Affective Appraisal & Real‑Time Mood Reactivity (“Minimum Viable Soul”)
**Date:** 2025-12-04 (America/Vancouver)  
**Owner:** Bradley (bushman77)  
**Purpose:** Make Symbrella’s affective and motivational state react immediately and transparently to user input, then let that state bias attention, curiosity, memory, and response style—without violating umbrella guardrails or drifting from plausible neuroscience.

---

## 0) Architectural Constraints (Non‑Negotiable)
These are explicitly reflected in this roadmap and must stay true throughout implementation:

- **Umbrella dependency direction stays acyclic:** `db <- brain <- core <- web`
- **LIFG stays in** `apps/brain`
- **Core orchestrates** the pipeline; Brain provides region machinery and stateful dynamics.
- **No new per‑app Application supervisors** (single umbrella‑root supervisor under `Symbrella.Application`).
- **Telemetry-first observability:** every substantive affective change is measurable and live‑viewable.
- **Tests are the source of truth** (invariants and tripwires must be covered by ExUnit).

---

## 1) Immediate Procedural Checklist (Do One Item at a Time)
This is the active “fix-first” workflow that we follow before layering new behavior.

- [ ] **(1) LIFG Guard invariants** — span normalization/recovery; telemetry meta correctness (incl. `count`); tripwire emission
- [ ] **(2) MWE unigram backfill POS canonicalization** — e.g. `"proper noun" → "proper_noun"`; synthesized `id`/`pos`
- [ ] **(3) Stage1/Guard char‑gram tripwire plumbing** — consistent overrideable events and expected metadata

**Current status (as of 2025‑12‑04):** Items (2) and (3) are passing locally; one remaining failure indicates item (1) still needs a targeted span recovery fix.

---

## 2) “Soul” Outcome Definition (What Success Looks Like)
Within a single input/response cycle:

1. **Appraisal is computed deterministically** from the resolved `SemanticInput` and surface text.
2. **Mood/neuromodulator levels shift immediately** (bounded deltas + decay model).
3. **The change is visible in telemetry + LiveView** (HUD and “region state” panels).
4. **Downstream behavior changes measurably** (not vibes):
   - curiosity proposal rate changes
   - attention/WM gating shifts
   - memory write likelihood changes
   - response style selection changes

---

## 3) Neuroscience‑Aligned Model Commitments (Practical + Plausible)
We will use a **coarse, engineering‑safe mapping** that is consistent with major findings, while explicitly acknowledging it is not a biological simulation.

### 3.1 Appraisal space
We will use a compact appraisal output:
- **Valence** `[-1.0..1.0]` (negative to positive affect)
- **Arousal** `[0.0..1.0]` (activation/alerting)
- **Dominance / Control** `[-1.0..1.0]` (controlled vs in-control)
- **Tags** `[:praise, :insult, :threat, :uncertainty, :question, :command, :self_reference, ...]`

Design note: this is explicitly an **engineering representation** intended to be stable, testable, and interpretable.

### 3.2 Neuromodulator correspondences (simplified but grounded)
We implement the following as **engineering correlates**, not one‑to‑one truths:

- **Dopamine (DA):** reward prediction error / learning signal; motivational salience (especially positive valence + novelty/praise tags).
- **Norepinephrine (NE; locus coeruleus):** arousal and alerting; “gain control” and interrupt-like behavior (especially high arousal, threat, surprise).
- **Acetylcholine (ACh):** attentional precision and uncertainty management (useful framing: expected uncertainty vs NE for unexpected uncertainty).
- **Serotonin (5‑HT):** behavioral inhibition/avoidance modulation; aversive processing and reward–punishment signaling patterns (coarse proxy, not a claim of biosim fidelity).

**Design implication:** we treat DA/NE/ACh/5‑HT as *bounded control knobs* for:
- learning rate / plasticity
- vigilance / interrupt threshold
- exploration vs exploitation bias
- response tone selection

---

## 4) Where This Lives (Fit to Symbrella’s Architecture)
### 4.1 Modules (proposed; names can be adjusted)
- `Brain.AffectiveAppraisal` (appraisal; returns a struct/map; deterministic + testable)
- `Brain.MoodCore.apply_appraisal/1` (state update + decay-aware bounded adjustments)
- `Core` pipeline hook (or routed through `Core.BrainAdapter` when you want runtime-only coupling)

### 4.2 No new umbrella app required
A separate `apps/affective_appraisal --sup` is **not aligned** with current guardrails. Prefer implementing these capabilities **inside `apps/brain`** (and wiring from `core`), supervised under the single umbrella root (`Symbrella.Application`).

---

## 5) Phase Plan (Fine‑Tuned to Symbrella)
### Phase 1 — Affective Appraisal Engine (rule-based, fast to ship)
**Goal:** deterministic appraisal with evidence, testability, and extensibility.

**Inputs:**
- `SemanticInput` (intent, tokens, senses, relations, optional mood snapshot)
- surface sentence (optional but preferred)

**Output (example):**
```elixir
%{
  valence: -1.0..1.0,
  arousal: 0.0..1.0,
  dominance: -1.0..1.0,
  tags: [...],
  evidence: %{
    hits: [%{term: "love", weight: 0.8, tag: :praise}, ...],
    negation_scopes: [...],
    target: :assistant | :self | :other | :unknown
  },
  v: 1
}
```

**Rule set (start minimal, expand safely):**
- affect lexicon (start with ~50 “strong” words; later expand)
- negation handling (“not good”, “never again”)
- intensifiers/diminishers (“very”, “kind of”)
- direct address and blame (“you are…”, “this sucks”)
- threat/urgency markers
- uncertainty markers (“maybe”, “I guess”, “not sure”)
- questions/commands (punctuation + intent signals)

**Deliverables:**
- `Brain.AffectiveAppraisal.appraise/1` + unit tests
- telemetry event `[:brain, :affect, :appraisal]` with meta `{valence, arousal, dominance, tags, v}`

---

### Phase 2 — MoodCore Instant Reactivity (bounded deltas + decay)
**Goal:** each input produces a measurable, bounded modulation.

**Key requirements:**
- apply deltas with clamps (no exploding state)
- decay continues to work as designed (appraisal is additive, not replacement)
- compute derived outputs (`exploration`, `vigilance`, `plasticity`, `inhibition`) from chemistry

**Illustrative mapping (tunable constants; treated as policy):**
- DA responds to positive valence + novelty/praise tags
- NE responds primarily to arousal + threat/urgency tags
- ACh responds to uncertainty markers/questions/conflicts
- 5‑HT responds to social threat/insult and dominance/control signals

**Deliverables:**
- `Brain.MoodCore.apply_appraisal/1`
- telemetry event `[:brain, :mood, :appraisal_applied]` including `delta_*` fields and `count: 1`

---

### Phase 3 — Telemetry + LiveView “It’s Alive” Feedback Loop
**Goal:** the “dance” is visible in real time.

**Telemetry events (minimum set):**
- `[:brain, :affect, :appraisal]` (raw appraisal + tags + evidence summary)
- `[:brain, :mood, :appraisal_applied]` (deltas + resulting levels)
- reuse existing mood update event `[:brain, :mood, :update]` as needed

**UI updates (minimum viable):**
- add an “Appraisal” mini panel beside current mood HUD:
  - rolling last‑N V/A/D values
  - tags chips
  - a short evidence preview (top 3 hits)

---

### Phase 4 — Behavior Coupling (where affect changes the brain)
**Goal:** mood becomes causally relevant, not cosmetic.

**Coupling points (aligned to existing regions):**
- **Curiosity loop:** increase proposal rate under higher DA and moderate ACh; reduce under high NE/low 5‑HT.
- **WM/attention gating:** high NE raises interrupt sensitivity and shortens persistence; higher ACh increases attention to “uncertainty‑reducing” cues.
- **Memory writes (Hippocampus):** increase write likelihood with salience (|valence| high) and novelty (DA proxy), throttled under overload (NE high).
- **Response planning:** route through `Core.Response` tone/mode selection using appraisal tags + mood snapshot.

**Deliverables:**
- small, explicit bias functions with unit tests
- telemetry counters for each influence point (so behavior coupling is measurable)

---

### Phase 5 — Optional Learning Loop (small ML, low risk)
**Goal:** personalize appraisal gradually without losing determinism.

1. Log samples:
   - embeddings (phrase, semantic)
   - rule appraisal (V/A/D + tags)
   - downstream outcomes (user feedback + response metrics when available)
2. Train a tiny MLP (Axon/Nx) offline or on-device when feasible.
3. Runtime blending:
   - start with `rules 0.9 / ml 0.1`
   - increase only after stability checks pass

**Deliverables:**
- `db` schema/table for `affective_samples`
- versioned predictor weights + safe hot‑swap strategy (no brain restart)

---

## 6) Validation & Safety
### 6.1 Validation
- Unit tests for:
  - negation scopes
  - address targeting (`:self` vs `:assistant` vs `:other`)
  - bounds/clamps always maintained
- Property tests:
  - appraisal output ranges always respected
  - telemetry meta always includes `count` and `v`

### 6.2 Safety commitments
- No claims of “sentience”; treat as *simulated affective control*
- Avoid manipulative reward loops; keep settings visible and tunable
- Provide a “mute affect” or “debug mode” toggle for testing

---

## 7) Minimal Weekly Plan (Practical Execution Order)
1. Finish Procedural Checklist item (1) (Guard spans + telemetry meta)
2. Implement Phase 1 (50-word appraisal + tests + telemetry)
3. Implement Phase 2 (`MoodCore.apply_appraisal/1` + clamps + telemetry)
4. Implement Phase 3 (HUD/telemetry wiring)
5. Add one coupling point (Curiosity bias) with measurable effect

---

## Appendix A — Pipeline Hook (Architecture‑safe pseudocode)
Placed after LIFG resolution and before response generation:

```elixir
with {:ok, resolved} <- Brain.LIFG.resolve(input) do
  appraisal = Brain.AffectiveAppraisal.appraise(resolved)
  :ok = Brain.MoodCore.apply_appraisal(appraisal)
  {:ok, resolved, appraisal}
end
```

If you prefer runtime-only coupling, route updates through your existing Core↔Brain adapter.
