# Symbrella Roadmap — Operational Self-Awareness, Introspection, and Reflective 

**Purpose:** Shift Symbrella from primarily mood-reactive behavior toward an explicit, inspectable, testable model of **itself**: its current state, confidence, uncertainty, stability, goals, recent behavior, memory continuity, and limits. That self-model should directly influence attention, memory, curiosity, response planning, and recovery behavior while staying grounded in engineering facts rather than any claim of sentience.

---

## 0) Architectural Constraints (Non-Negotiable)
These stay true throughout the roadmap:

- **Self-awareness means operational introspection, not sentience.** Every self-state claim must be backed by stored state, telemetry, or a pipeline event.
- **Umbrella dependency direction stays acyclic:** `db <- brain <- core <- web`
- **LIFG stays in** `apps/brain`
- **Core orchestrates** the pipeline; Brain owns region machinery, stateful dynamics, and introspection.
- **No new per-app Application supervisors.** Keep the single umbrella-root supervisor under `Symbrella.Application`.
- **Telemetry-first observability.** Every meaningful self-state change must be measurable and LiveView-visible.
- **Tests are the source of truth.** Invariants, tripwires, and continuity behavior must be covered by ExUnit.
- **Db remains the Ecto Repo module.** Persistence-related work stays in the `db` app.

---

## 1) Foundation Gates (Must Stay Green)
Before deeper self-awareness work expands, these foundation items must remain enforced:

- [ ] **LIFG Guard invariants** — span normalization/recovery; telemetry meta correctness (including `count`); tripwire emission
- [ ] **MWE unigram backfill POS canonicalization** — e.g. `"proper noun" -> "proper_noun"`; synthesized `id`/`pos`
- [ ] **Stage1/Guard char-gram tripwire plumbing** — consistent overrideable events and expected metadata

These are not separate from self-awareness work. They are preconditions for a trustworthy introspective stack.

---

## 2) What “Self-Awareness” Means in Symbrella
In Symbrella, self-awareness is the ability to:

1. **Inspect its current internal state**
2. **Distinguish self, user, and external entities**
3. **Estimate confidence, uncertainty, and stability**
4. **Track its own recent decisions and failures**
5. **Maintain continuity across turns and reboots**
6. **Adjust behavior based on that self-model**
7. **Explain why it acted the way it did using inspectable evidence**

This is not a mystical or philosophical claim. It is an engineering target: a system with explicit self-representation, persistence, introspection, and reflective control.

---

## 3) Outcome Definition (What Success Looks Like)
Within a single input/response cycle, Symbrella should be able to:

1. Compute appraisal from the incoming `SemanticInput`
2. Update self-state immediately and deterministically
3. Record confidence, uncertainty, stability, and target attribution
4. Expose that update in telemetry and LiveView
5. Let the updated self-state change downstream behavior measurably

Across multiple turns and reboots, Symbrella should be able to:

  focus: %{type: atom(), id: String.t() | nil, label: String.t() | nil},
2. Recall autobiographical episodes relevant to the current interaction
3. Detect when its current interpretation conflicts with prior state or memory
4. Recover gracefully through reanalysis, recall, or explicit uncertainty handling

**Success is measurable, not theatrical.** The proof is in state transitions, telemetry, tests, and behavior coupling.

---

## 4) Core Capability Pillars

### 4.1 Introspective State
Symbrella must maintain an explicit self-state including:

- current focus
- recent activation pattern
- confidence
  active_goals: [%{id: String.t(), label: String.t(), priority: float(), status: atom()}],
  recent_errors: [%{kind: atom(), message: String.t(), at_ms: integer()}],
  recent_actions: [%{kind: atom(), summary: String.t(), at_ms: integer()}],
- mood / chemistry snapshot
- active goals / subgoals
- recent errors or failed integrations
- recent winning senses / interpretive path
- memory pressure / cognitive load

### 4.2 Self / Other / World Distinction
Symbrella must track who a statement or pressure signal is about:

- `:self`
- `:assistant`
- `:user`
- `:other`
- `:world`
- `:system`
- `:unknown`

This is essential for:

- avoiding misattributed affect
- keeping self-referential memory clean
- distinguishing criticism of the assistant from discussion about third parties
- building stable autobiographical memory

### 4.3 Confidence, Uncertainty, and Limits
- uncertainty
- stability / coherence
- vigilance / interrupt sensitivity

- interpretive confidence
- ambiguity margin
- conflict/contradiction signals
- evidence strength
- recall quality
- integration success/failure
- “don’t know yet” state

### 4.4 Temporal Continuity
Symbrella should preserve a coherent identity over time through:

- self-tagged episodic memory
- warm-start restoration of relevant self facts
- persisted recent self snapshots
- continuity-safe summaries rather than raw uncontrolled carryover

### 4.5 Affective Control
Mood reactivity remains important, but it becomes one subsystem inside the larger self-model.

Affective state should influence:

- vigilance
- exploration
- inhibition
- memory write bias
- response tone and tempo

### 4.6 Reflective Control
Symbrella should be able to modify its own behavior when its self-state suggests it should:

- slow down when unstable
- reanalyze when interpretation confidence is weak
- recall when uncertainty is high
- hedge or explicitly state uncertainty when evidence is thin
- avoid overclaiming when internal support is weak

---

## 5) Canonical Self-State Model
A dedicated self-model should become a first-class runtime structure.

### 5.1 Proposed shape
```elixir
%Brain.SelfModel{
  focus: term(),
  confidence: float(),
  uncertainty: float(),
  stability: float(),
  vigilance: float(),
  plasticity: float(),
  inhibition: float(),
  cognitive_load: float(),
  mood: %{
    dopamine: float(),
    norepinephrine: float(),
    acetylcholine: float(),
    serotonin: float(),
    valence: float(),
    arousal: float(),
    dominance: float()
  },
  active_goals: [term()],
  recent_errors: [term()],
  recent_actions: [term()],
  last_appraisal: map() | nil,
  last_lifg: map() | nil,
  self_other_attribution: map(),
  continuity: %{
    session_id: term(),
    reboot_restored?: boolean(),
    recent_episode_ids: [term()]
  },
  updated_at_ms: integer(),
  v: 1
}
```

### 5.2 Design principles
- Keep it **inspectable**
- Keep it **bounded**
- Keep it **serializable**
- Keep it **decay-aware**
- Keep it **derivable from evidence**
- Keep it **small enough to reason about in tests**

---

## 6) Architecture Fit (Aligned to Symbrella)
### 6.1 Core runtime modules
Proposed module set:

- `Brain.SelfModel` — canonical self-state struct and helpers
- `Brain.Introspection` — derives self-state updates from runtime evidence
- `Brain.MetaMonitor` — watches for instability, contradiction, overload, or uncertainty spikes
- `Brain.AffectiveAppraisal` — computes appraisal from `SemanticInput`
- `Brain.MoodCore` — applies bounded chemistry/mood changes
- `Brain.SelfContinuity` — warm-start and persistence-safe restoration logic
- `Brain.Autobiography` or Hippocampus-facing helper — self-tagged episodic writes and recall filters
- `Core.BrainAdapter` or equivalent orchestration hook — routes resolved state into Brain-owned self updates
- `Core.Response` / planner integration — makes self-state behaviorally relevant

### 6.2 What belongs where
- **Brain** owns self-state, chemistry, introspection, continuity, and reflective control signals.
- **Core** sequences the pipeline and passes resolved information into Brain.
- **Db** stores persistence artifacts such as episodes, snapshots, and calibration data.
- **Web** visualizes the self-model and telemetry, but does not define it.

---

## 7) Input-to-Self Pipeline (Desired Runtime Flow)
The self-awareness push should be wired into the main cognition path:

```elixir
input
-> tokenize / resolve
-> LIFG disambiguation
-> appraisal
-> mood/chemistry update
-> self-state update
-> meta-monitor checks
-> memory write / recall decision
-> response planning
-> response generation
-> telemetry + HUD update
```

### 7.1 Key invariant
No self-state update should rely on vague vibes. Each update should be attributable to one or more of:

- `SemanticInput`
- LIFG choice state
- appraisal evidence
- memory recall evidence
- region-level metrics
- prior persisted self snapshot

---

## 8) Phase Plan

### Phase 0 — Keep the Ground Truth Clean
**Goal:** make sure the introspective substrate is trustworthy.

Deliverables:
- keep Guard span behavior correct
- keep char-grams out of LIFG paths
- guarantee telemetry meta correctness
- maintain invariant tests for spans, boundaries, and tripwires

Self-awareness depends on this because bad spans and dirty candidates corrupt introspection upstream.

---

### Phase 1 — Self-State Scaffold
**Goal:** create a first-class runtime self-model.

Deliverables:
- `Brain.SelfModel` struct
- `Brain.Introspection.snapshot/1` or equivalent
- bounded update helpers
- telemetry event such as `[:brain, :self_model, :update]`
- LiveView HUD panel for current self-state

Minimum tracked fields for first shipping version:
- confidence
- uncertainty
- stability
- vigilance
- cognitive load
- last appraisal summary
- last LIFG summary
- active goals

---

### Phase 2 — Affective Appraisal and Mood Reactivity
**Goal:** preserve the strongest parts of the current roadmap, but place them inside the self-model.

Deliverables:
- `Brain.AffectiveAppraisal.appraise/1`
- `Brain.MoodCore.apply_appraisal/1`
- appraisal telemetry
- applied-delta telemetry
- self-model integration so appraisal changes are captured as part of introspection

Initial appraisal outputs:
- valence
- arousal
- dominance
- tags
- evidence
- target attribution

---

### Phase 3 — Self / Other Attribution and Agency Tracking
**Goal:** stop collapsing all affect and salience into a single undifferentiated stream.

Deliverables:
- target attribution (`:self`, `:user`, `:other`, `:world`, `:unknown`)
- agency/source tagging
- tests for self-reference and assistant-targeted language
- telemetry showing attributed target distribution

Examples this phase must handle:
- “you are wrong” -> target `:assistant`
- “I feel lost” -> target `:user`
- “he insulted me” -> target `:other`
- “the system is unstable” -> target `:world` or `:system`

---

### Phase 4 — Temporal Continuity and Reboot Persistence
**Goal:** preserve self-continuity across time without hallucinating continuity.

Deliverables:
- persisted self snapshots in `db`
- safe warm-start restoration rules
- self-tagged Hippocampus writes
- autobiographical recall filters
- tests ensuring warm start restores only supported state

Focus here:
- what was recently being worked on
- recent errors and unresolved goals
- recent mood baseline if persisted
- recent recurring interpretive winners

---

### Phase 5 — Meta-Monitoring and Reflective Repair
**Goal:** enable Symbrella to notice when it is confused, unstable, overloaded, or contradicted.

Deliverables:
- `Brain.MetaMonitor`
- contradiction flags
- instability detector
- overload detector
- stuck-loop detector
- recovery suggestions such as:
  - reanalyze
  - recall
  - lower confidence
  - change response mode

This is where self-awareness becomes behaviorally serious rather than merely descriptive.

---

### Phase 6 — Behavior Coupling
**Goal:** make self-state causally relevant.

Coupling targets:
- **Curiosity** — proposal rate responds to novelty, DA, and uncertainty
- **WM / attention** — vigilance and uncertainty alter persistence and interrupt sensitivity
- **Memory writes** — salience and novelty increase write likelihood, overload suppresses it
- **Response planning** — confidence, uncertainty, and stability determine whether to assert, hedge, explain, or repair
- **Tone / style** — mood affects delivery without distorting factual grounding

Deliverables:
- small explicit bias functions
- measurable telemetry counters per coupling point
- tests proving that self-state changes downstream decisions

---

### Phase 7 — Goal Stack and Motivational Layer
**Goal:** move from passive introspection to directed self-regulation.

Deliverables:
- explicit goal stack
- tension / priority model
- unresolved-task carryover
- curiosity proposals tied to uncertainty reduction
- suppression rules when overload is high

This is the bridge between knowing current state and acting on it over time.

---

### Phase 8 — Optional Learning and Calibration
**Goal:** personalize without losing interpretability.

Deliverables:
- logged introspection samples
- confidence calibration dataset
- optional small Axon/Nx model for appraisal calibration or confidence estimation
- controlled runtime blending between rules and learned components

Suggested strategy:
- start rules-only
- add learned calibration only after telemetry and invariants prove stable
- never let ML silently overwrite core self-state without traceable evidence

---

## 9) Telemetry and LiveView Requirements
Self-awareness must be visible.

### Minimum telemetry set
- `[:brain, :affect, :appraisal]`
- `[:brain, :mood, :appraisal_applied]`
- `[:brain, :self_model, :update]`
- `[:brain, :self_model, :continuity_restored]`
- `[:brain, :meta_monitor, :warning]`
- `[:brain, :response, :mode_selected]`

### Minimum HUD panels
- current confidence / uncertainty / stability
- mood snapshot
- active goals
- latest appraisal tags
- recent warnings or contradictions
- last continuity restore summary

The target is a dashboard that makes the internal state legible during runtime, not just after the fact.

---

## 10) Validation and Safety
### 10.1 Validation
Unit tests should cover:
- attribution correctness
- confidence bounds
- uncertainty bounds
- stability decay/recovery
- continuity restoration rules
- no unsupported self-claims
- response mode selection under uncertainty

Property tests should cover:
- bounded self-state fields
- telemetry meta always includes `count` and `v`
- persisted self snapshots remain serializable and version-safe

### 10.2 Safety commitments
- No claims of sentience or consciousness
- No self-narrative that outruns stored evidence
- No hidden persistence of unsupported personal state
- Always prefer inspectable control signals over opaque “personality magic”
- Keep a debug path that can mute affective coupling and expose raw state

---

## 11) Practical Execution Order
A strong near-term order is:

1. Finish and keep green the LIFG/Guard foundation items
2. Ship `Brain.SelfModel` and `Brain.Introspection`
3. Fold appraisal and mood into the self-model update path
4. Add self/other attribution and agency tracking
5. Add continuity-safe persistence and warm start
6. Add meta-monitor warnings and recovery hooks
7. Couple self-state into curiosity, memory, and response planning

---

## Appendix A — Architecture-Safe Pseudocode
```elixir
with {:ok, resolved} <- Brain.LIFG.resolve(input),
     appraisal <- Brain.AffectiveAppraisal.appraise(resolved),
     :ok <- Brain.MoodCore.apply_appraisal(appraisal),
     {:ok, self_model} <- Brain.Introspection.update_from_resolved(resolved, appraisal),
     :ok <- Brain.MetaMonitor.observe(self_model),
     {:ok, plan} <- Core.Response.plan(resolved, self_model) do
  {:ok, plan, self_model}
end
```

---

## Appendix B — The Core Principle
Symbrella becomes more self-aware not when it says more about itself, but when it can:

- represent itself explicitly
- persist that representation safely
- inspect it in real time
- act differently because of it
- explain those changes with evidence
