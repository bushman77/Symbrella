# Symbrella Roadmap: Cognitive Control, Operational Self-Awareness, And ML Calibration

**Purpose:** Shift Symbrella from primarily mood-reactive behavior toward an explicit, inspectable, testable cognitive-control architecture: semantic evidence, bounded self-state, current goals, memory continuity, conflict, admission control, and response planning. The self-model should influence attention, memory, curiosity, response planning, and recovery behavior while staying grounded in engineering facts rather than any claim of sentience.

The ML track calibrates bounded self-state signals. It does not replace the symbolic self-model.

---

## 0) Architectural Constraints

These stay true throughout the roadmap:

- **Self-awareness means operational introspection, not sentience.** Every self-state claim must be backed by stored state, telemetry, or a pipeline event.
- **Umbrella dependency direction stays acyclic:** `db <- brain <- core <- web`
- **LIFG stays in** `apps/brain`.
- **Core orchestrates** the pipeline; Brain owns region machinery, stateful dynamics, appraisal, attribution, introspection, calibration, continuity, and reflective control.
- **No new per-app Application supervisors.** Keep the single umbrella-root supervisor under `Symbrella.Application`.
- **Telemetry-first observability.** Every meaningful self-state change must be measurable and LiveView-visible.
- **Tests are the source of truth.** Invariants, tripwires, attribution, calibration, continuity behavior, and persistence boundaries must be covered by ExUnit.
- **Db remains the Ecto Repo module.** Persistence-related work stays in the `db` app.
- **ML stays advisory until explicitly blended.** Learned output must not silently overwrite `Brain.SelfModel`.
- **Continuity must be evidence-backed.** Reboot restoration may only use persisted, version-checked, bounded state.
- **BasalGanglia is the canonical target for general WM admission.** Gate decisions such as `:allow`, `:boost`, and `:block` should converge there.
- **LIFG selects and finalizes linguistic evidence.** Stage1 performs competitive semantic interpretation; Stage2 should prepare post-selection evidence for downstream control rather than act as the final WM authority.
- **WorkingMemory maintains admitted active representations.** It normalizes, merges, activates, decays, trims, evicts, enforces capacity, and emits telemetry.
- **SelfModel modulates but does not gate.** Bounded self-state may bias cognitive control, but it must not bypass evidence, rule, or admission gates.
- **The architecture is recurrent.** WorkingMemory/PFC feedback may affect LIFG context, BasalGanglia context, retrieval, attention/control, and SelfModel evidence.

- [x] Guard span behavior remains correct.
- [x] Char-grams stay out of LIFG paths.
- [x] Telemetry meta correctness is guaranteed for implemented events.
- [x] Invariant tests cover spans, boundaries, and tripwires.
---

## 1) Current Green Checkpoint

Current status:

- [x] Umbrella compiles successfully.
- [ ] Eliminate remaining compiler/type warnings in existing `db`, `core`, and
  `symbrella_web` areas.
- [x] Core warning path fixed through `Core.BrainAdapter`.
- [x] `Brain.SelfModel` exists as the canonical runtime self-state struct.
- [x] `Brain.SelfModel` has bounded helper behavior for numeric state fields.
- [x] `Brain.Introspection.snapshot/0` builds a self-model from live Brain evidence.
- [x] `Brain.Introspection.update_from_resolved/2` emits `[:brain, :self_model, :update]`.
- [x] Core pipeline calls the Brain-owned self-model update hook.
- [x] LiveView HUD can display current self-model state.
- [x] `Brain.AffectiveAppraisal.appraise/1` emits appraisal telemetry.
- [x] `Brain.MoodCore.apply_appraisal/1` applies bounded mood deltas from appraisal.
- [x] `Brain.Attribution` exists as a first-class attribution policy module.
- [x] Attribution distinguishes `:self`, `:assistant`, `:user`, `:other`, `:system`, `:world`, and `:unknown`.
- [x] Attribution returns inspectable evidence.
- [x] `Brain.AffectiveAppraisal` carries attribution into result evidence and telemetry metadata.
- [x] `Brain.SelfCalibration` sample, feature, logger, dataset, tensor, prediction, and predictor contracts exist.
- [x] Nx is installed and exercised by tensor/predictor tests.
- [x] Runtime calibration samples can be logged from introspection.
- [x] Calibration prediction telemetry exists.
- [x] `Brain.SelfCalibration.Evaluator` compares predictions against labels.
- [x] Axon-backed calibration model, training artifact path, and advisory predictor exist.
- [x] Baseline/Axon comparison and explicit blend telemetry exist.
- [x] Initial SelfModel -> WM admission coupling exists for vigilance, uncertainty, inhibition, and cognitive load.
- [x] Explicit bounded self-state bias function exists.
- [x] Self-state gate contribution is telemetry-visible.
- [x] Neutral/default self-state preserves previous gate behavior.
- **Active frontier:** Phase 9 cognitive gating consolidation and behavior coupling.

---

## 2) Ground Truth Gates

Before deeper self-awareness, ML calibration, or persistence work expands, these foundation items must remain enforced:

- [x] **LIFG Guard invariants:** span normalization/recovery; telemetry meta correctness, including `count`; tripwire emission.
- [x] **MWE unigram backfill POS canonicalization:** e.g. `"proper noun" -> "proper_noun"`; synthesized `id` and `pos`.
- [x] **Stage1/Guard char-gram tripwire plumbing:** consistent overrideable events and expected metadata.
- [x] **Calibration feature schema stability:** feature order and label order must not change silently.
- [ ] **Telemetry metadata contract:** self-model, appraisal, attribution, calibration, and continuity events include version metadata.
  - [x] Self-model update telemetry includes `v`.
  - [x] Appraisal telemetry includes `v` and attribution metadata.
  - [x] Calibration sample telemetry includes `v` and `feature_schema_v`.
  - [x] Calibration prediction telemetry includes `v` and `feature_schema_v`.
  - [x] Calibration evaluation telemetry includes `v` and `feature_schema_v`.
  - [x] Calibration blend telemetry includes `v`.
  - [ ] Continuity telemetry includes `v`.
- [x] **Persistence version gates:** restored self-state must pass version and bounds checks before use.

These are preconditions for a trustworthy introspective, calibration, and continuity stack.

---

## 3) What Self-Awareness Means In Symbrella

In Symbrella, self-awareness means the system can:

1. Inspect its current internal state.
2. Distinguish self, assistant, user, other entities, system, and world references.
3. Estimate confidence, uncertainty, stability, vigilance, and cognitive load.
4. Track recent decisions, appraisal, attribution, LIFG state, calibration samples, and failures.
5. Maintain bounded continuity across turns and reboots.
6. Adjust behavior based on self-model state.
7. Explain behavior using inspectable evidence.

This is an engineering target, not a philosophical claim.

---

## 4) ML Contract

ML fits into Symbrella as a calibration layer:

```text
runtime evidence
-> rule-derived self-model
-> calibration sample
-> dataset rows
-> Nx tensors
-> baseline advisory prediction
-> Axon advisory prediction
-> baseline/candidate comparison
-> explicit blend decision
-> telemetry
```

Rules remain authoritative unless an explicit blend policy accepts model output.

Initial ML prediction targets:

- confidence
- uncertainty
- stability

Later prediction targets:

- recall needed?
- reanalysis needed?
- response mode
- memory write bias
- instability risk

Non-goals:

- ML does not define identity.
- ML does not invent self-narrative.
- ML does not silently overwrite `Brain.SelfModel`.
- ML does not make safety decisions without rule gates.
- ML does not persist unsupported personal state.

---

## 5) Core Runtime Models

### 5.1 `Brain.SelfModel`

Current shape:

```elixir
%Brain.SelfModel{
  confidence: float(),
  uncertainty: float(),
  stability: float(),
  vigilance: float(),
  plasticity: float(),
  inhibition: float(),
  cognitive_load: float(),
  mood: map(),
  active_goals: [term()],
  recent_errors: [term()],
  recent_actions: [term()],
  last_appraisal: map() | nil,
  last_lifg: map() | nil,
  self_other_attribution: map(),
  continuity: map(),
  updated_at_ms: integer() | nil,
  v: 1
}
```

Status:

- [x] Struct exists.
- [x] Runtime derivation exists.
- [x] Bounded numeric helper behavior exists.
- [x] Tests cover sparse runtime defaults and bounded values.
- [x] Add explicit focus field.
- [x] Add version-safe serialization helper.
- [x] Add continuity snapshot import/export helpers.
- [x] Initial bounded modulation affects WM admission scoring through current
  Phase 9 policy coupling.

Control rule:

```text
SelfModel
-> bounded modulation
-> BasalGanglia / cognitive gating
-> Working Memory update / hold
```

`Brain.SelfModel` is not itself the gate.

### 5.2 `Brain.Attribution`

Current shape:

```elixir
%{
  target: :self | :assistant | :user | :other | :system | :world | :unknown,
  source: :self_name | :second_person | :first_person | :third_person | :system_term | :world_term | :none,
  confidence: float(),
  evidence: [%{term: String.t(), source: atom(), confidence: float()}],
  version: 1
}
```

Status:

- [x] First-class module exists.
- [x] `target/2` compatibility API exists.
- [x] `classify/2` returns inspectable evidence.
- [x] Appraisal stores attribution in result evidence.
- [x] Appraisal telemetry includes target/source/confidence metadata.
- [ ] Add agency/source tagging from `SemanticInput` metadata, not just text.
- [ ] Add confidence calibration rules for conflicting evidence.

### 5.3 `Brain.SelfCalibration.Sample`

Current role:

```text
versioned runtime evidence + rule-derived labels
```

Status:

- [x] Sample struct exists.
- [x] Feature map exists.
- [x] Label map exists.
- [x] Source and version metadata exist.
- [x] Used by logger and dataset.

### 5.4 `Brain.SelfCalibration.Prediction`

Current role:

```text
advisory calibration output for confidence / uncertainty / stability
```

Status:

- [x] Prediction struct exists.
- [x] Baseline predictor exists.
- [x] Prediction telemetry exists.
- [x] Axon-backed predictor exists.
- [x] Runtime blend policy exists.

---

## 6) Architecture Fit

### 6.1 Runtime Modules

Implemented:

- [x] `Brain.SelfModel`: canonical self-state struct and helpers.
- [x] `Brain.Introspection`: derives self-state updates from runtime evidence.
- [x] `Brain.AffectiveAppraisal`: computes appraisal from SI-like input.
- [x] `Brain.MoodCore`: applies bounded chemistry/mood changes.
- [x] `Brain.Attribution`: classifies target with evidence.
- [x] `Brain.SelfCalibration.Sample`: versioned sample contract.
- [x] `Brain.SelfCalibration.Features`: feature and label extraction.
- [x] `Brain.SelfCalibration.Logger`: JSONL sample persistence.
- [x] `Brain.SelfCalibration.Dataset`: JSONL loading and row vectorization.
- [x] `Brain.SelfCalibration.Tensor`: Nx tensor conversion.
- [x] `Brain.SelfCalibration.Prediction`: advisory prediction struct.
- [x] `Brain.SelfCalibration.Predictor`: baseline prediction contract.
- [x] `Brain.SelfCalibration.Evaluator`: compare predictions to labels.
- [x] `Brain.SelfCalibration.Model`: Axon model definition.
- [x] `Brain.SelfCalibration.Training`: offline training task.
- [x] `Brain.SelfCalibration.AxonPredictor`: artifact-backed advisory prediction path.
- [x] `Brain.SelfCalibration.Comparison`: baseline/candidate evaluation comparison.
- [x] `Brain.SelfCalibration.Blend`: explicit advisory blend decision with telemetry.
- [x] `Core.BrainAdapter`: Core-side Brain facade.
- [x] `Core.Brain.Introspection`: orchestration hook into Brain self-model update path.
- [x] Hippocampus-facing self-memory helper for self-tagged episodic writes.

Continuity and behavior status:

- [x] `Brain.SelfContinuity`: warm-start and persistence-safe restoration.
- [x] `Brain.MetaMonitor`: instability, contradiction, overload, uncertainty spikes.
- [ ] Autobiographical recall filters.
- [x] `Core.Response` integration: self-state affects response mode and planning.

### 6.2 Ownership

- **Brain** owns self-state, chemistry, appraisal, attribution, introspection, calibration, continuity, and reflective control signals.
- **Core** sequences the pipeline and passes resolved information into Brain.
- **Db** stores durable persistence artifacts such as episodes, snapshots, and calibration data when promoted from JSONL.
- **Web** visualizes self-model, telemetry, and calibration state, but does not define them.

### 6.3 Cognitive-Control Ownership

Target responsibility boundaries:

```text
LIFG Stage1:
competitive semantic interpretation

LIFG Stage2:
post-selection linguistic evidence finalization

ACC / control context:
conflict, uncertainty, ambiguity, and task pressure

BasalGanglia:
canonical general Working Memory admission decision

Thalamus:
relay / arbitration / gating-control analogue

DLPFC / PFC:
executive and task-control maintenance

WorkingMemory:
active-representation mechanics

SelfModel:
bounded modulation and evidence summary, not the gate
```

Current overlap in `Brain.LIFG.Stage2`, `Brain.WM.Policy`, and
`Brain.BasalGanglia` is an architecture-consolidation task. `Brain.WM.Policy`
currently contains both admission and retention mechanics. The target split is:

```text
BasalGanglia:
general cognitive admission decision

WM policy / WorkingMemory:
retention, decay, duplicate/lemma constraints,
capacity, normalization, merge, and eviction
```

---

## 7) Input-To-Self Pipeline

Target flow:

```text
input
-> tokenize / resolve
-> LIFG disambiguation
-> LIFG evidence finalization
-> affective appraisal
-> target attribution
-> mood/chemistry update
-> self-state update
-> calibration sample logging
-> optional calibration prediction
-> optional model comparison / blend decision
-> meta-monitor checks
-> BasalGanglia/control gate for WM admission where routed
-> memory write / recall decision
-> response planning
-> response generation
-> telemetry + HUD update
```

Current implemented subset:

```text
input
-> Core pipeline
-> LIFG attach
-> Brain.AffectiveAppraisal.appraise/1
-> Brain.Attribution.classify/2
-> Brain.MoodCore.apply_appraisal/1
-> Brain.Introspection.update_from_resolved/2
-> Brain.SelfCalibration.Features.build_sample/2
-> Brain.SelfCalibration.Logger.log/1
-> JSONL dataset
-> Dataset rows
-> Nx tensors
-> baseline advisory prediction
-> Axon advisory prediction
-> evaluation
-> comparison
-> blend decision telemetry
-> initial bounded self-state contribution to WM admission scoring
```

Invariant: no self-state update should rely on vague vibes. Each update must be attributable to one or more of:

- `SemanticInput`
- attribution evidence
- LIFG choice state
- appraisal evidence
- mood snapshot
- working memory state
- calibration sample
- prediction metadata
- comparison metadata
- blend decision metadata
- region-level metrics
- prior persisted self snapshot
- canonical gate telemetry where a candidate affects WorkingMemory

---

## 8) Phase Plan

### Phase 0 - Ground Truth

**Goal:** keep the introspective substrate trustworthy.

Deliverables:

- [x] Guard span behavior remains correct.
- [x] Char-grams stay out of LIFG paths.
- [x] Telemetry meta correctness is guaranteed for implemented events.
- [x] Invariant tests cover spans, boundaries, and tripwires.

---

### Phase 1 - Self-State Scaffold

**Goal:** create a first-class runtime self-model.

Deliverables:

- [x] `Brain.SelfModel` struct.
- [x] `Brain.Introspection.snapshot/0`.
- [x] Bounded update helpers.
- [x] `[:brain, :self_model, :update]` telemetry event.
- [x] LiveView HUD panel for current self-state.
- [x] Tests for canonical fields and bounds.
- [x] Add explicit focus tracking.
- [x] Add version-safe serialization helper.

Minimum tracked fields:

- [x] confidence
- [x] uncertainty
- [x] stability
- [x] vigilance
- [x] cognitive load
- [x] last appraisal summary
- [x] last LIFG summary
- [x] active goals field

---

### Phase 2 - Affective Appraisal And Mood Reactivity

**Goal:** preserve mood reactivity, but place it inside the self-model path.

Deliverables:

- [x] `Brain.AffectiveAppraisal.appraise/1`.
- [x] `Brain.MoodCore.apply_appraisal/1`.
- [x] Appraisal telemetry.
- [x] Applied-delta telemetry.
- [x] Self-model integration captures appraisal through introspection.
- [x] Appraisal carries target attribution evidence.
- [ ] Add tests proving appraisal changes self-model fields over a full Core pipeline run.

Initial appraisal outputs:

- [x] valence
- [x] arousal
- [x] dominance
- [x] tags
- [x] evidence
- [x] target attribution

---

### Phase 3 - Self / Other Attribution And Agency Tracking

**Goal:** stop collapsing all affect and salience into a single undifferentiated stream.

Deliverables:

- [x] `Brain.Attribution` module.
- [x] Target attribution for `:self`, `:assistant`, `:user`, `:other`, `:system`, `:world`, and `:unknown`.
- [x] Self-name statements target `:self`.
- [x] Assistant-targeted language targets `:assistant`.
- [x] First-person language targets `:user`.
- [x] Third-person language targets `:other`.
- [x] System/world language targets `:system` or `:world`.
- [x] Attribution returns inspectable evidence.
- [x] Appraisal telemetry includes attribution metadata.
- [ ] Agency/source tagging from SI metadata.
- [ ] Tests for explicit speaker/source metadata.
- [ ] Conflict tests where multiple attribution signals appear in the same sentence.

Examples covered:

- [x] `"you are wrong"` -> target `:assistant`
- [x] `"I feel lost"` -> target `:user`
- [x] `"he insulted me"` -> target `:other`
- [x] `"the system is unstable"` -> target `:system` or `:world`
- [x] `"Symbrella is wrong"` -> target `:self`

Next target shape:

```elixir
%{
  speaker: :user | :assistant | :system | :unknown,
  target: :self | :assistant | :user | :other | :system | :world | :unknown,
  source: atom(),
  confidence: float(),
  evidence: [map()],
  version: 1
}
```

---

### Phase 4 - Self-Calibration Data Contract

**Goal:** create the data contract that future ML learns from.

Deliverables:

- [x] `Brain.SelfCalibration.Sample` struct.
- [x] Feature extraction from `Brain.SelfModel`.
- [x] Features include appraisal, attribution, LIFG, cognitive load, recent errors, and mood.
- [x] Labels include confidence, uncertainty, and stability.
- [x] Sparse evidence defaults are tested.
- [x] Runtime sample logging from introspection.
- [x] JSONL sample storage.
- [x] Logger telemetry.
- [x] Dataset loading from JSONL.
- [x] Dataset conversion to stable feature and label rows.
- [x] Tests for sample construction, logging, dataset loading, and row conversion.

Current feature order:

```elixir
[
  :appraisal_valence,
  :appraisal_arousal,
  :appraisal_dominance,
  :attribution_confidence,
  :lifg_choices_count,
  :cognitive_load,
  :recent_error_count,
  :mood_vigilance,
  :mood_plasticity,
  :mood_inhibition
]
```

Current label order:

```elixir
[
  :confidence,
  :uncertainty,
  :stability
]
```

Remaining:

- [ ] Add lexical DB-derived features.
- [ ] Add feature schema validation.
- [ ] Add sample pruning / rotation policy.
- [ ] Promote JSONL samples to Db storage if needed.

---

### Phase 5 - Tensor Boundary And Baseline Predictor

**Goal:** prove Symbrella can convert runtime self-state evidence into tensors and advisory calibration predictions.

Deliverables:

- [x] Add Nx dependency.
- [x] Convert feature and label rows to Nx tensors.
- [x] Tensor tests prove expected shapes.
- [x] `Brain.SelfCalibration.Prediction` struct.
- [x] `Brain.SelfCalibration.Predictor` runtime boundary.
- [x] Baseline predictor.
- [x] Prediction telemetry.
- [x] `Brain.SelfCalibration.Evaluator`.
- [x] Evaluation metrics for prediction vs labels.
- [x] Evaluation report comparing baseline output to labels.

Current tensor contract:

```text
Dataset.to_rows(samples)
-> Tensor.from_rows(rows)
-> %{x: Nx.Tensor.t(), y: Nx.Tensor.t()}
```

Current prediction/evaluation contract:

```text
tensor batch
-> Predictor.predict(batch)
-> Evaluator.evaluate(prediction, labels)
-> %Brain.SelfCalibration.Evaluation{}
```

---

### Phase 6 - Axon Calibration Model

**Goal:** add a small learned calibration model behind the existing predictor contract.

Deliverables:

- [x] Add Axon dependency.
- [x] Define small calibration model.
- [x] Offline training task.
- [x] Model artifact save/load path.
- [x] Evaluation against baseline predictor.
- [x] Advisory-only runtime prediction path.
- [x] Explicit rule/model blend with telemetry.
- [x] Tests proving model output stays bounded.

Current model shape:

```text
10 input features
-> dense 16
-> relu
-> dense 3
-> sigmoid
```

Current outputs:

- confidence
- uncertainty
- stability

Rules:

- Model output is advisory.
- Rule output remains visible.
- Model output remains visible.
- Blend policy must be explicit.
- No silent overwrite of `Brain.SelfModel`.

---

### Phase 7 - Temporal Continuity And Reboot Persistence

**Goal:** preserve continuity across time without hallucinating continuity.

Deliverables:

- [x] `Brain.SelfContinuity` warm-start boundary.
- [x] Persisted self snapshots in `db`.
- [x] Safe warm-start restoration rules.
- [x] Latest valid snapshot restore path.
- [x] Stale, missing, invalid, or unsupported snapshots degrade explicitly.
- [x] Self-tagged Hippocampus writes.
- [ ] Autobiographical recall filters.
- [x] Continuity telemetry.
- [x] Tests ensuring warm start restores only supported, bounded, version-checked state.
- [x] Persisted self snapshots in db.

Focus:

- current work
- recent errors
- unresolved goals
- recent mood baseline, if persisted
- recurring interpretive winners
- calibration model version, if active

---

### Phase 8 - Meta-Monitoring And Reflective Repair

**Goal:** let Symbrella notice confusion, instability, overload, and contradiction.

Deliverables:

- [x] `Brain.MetaMonitor`.
- [x] Contradiction flags.
- [x] Instability detector.
- [x] Overload detector.
- [x] Stuck-loop detector.
- [x] Recovery suggestions:
  - reanalyze
  - recall
  - lower confidence
  - change response mode
  - ask for clarification

This phase can consume both rule-derived self-state and advisory calibration predictions.

---

### Phase 9 - Behavior Coupling

**Current active frontier.**

**Goal:** make self-state causally relevant without letting self-state bypass
evidence, rule, or admission gates.

Coupling targets:

- [x] **Curiosity:** proposal score responds to novelty, exploration/dopamine, and uncertainty.
- [x] **WM / attention:** initial bounded vigilance, uncertainty, inhibition, and cognitive-load contribution exists in admission scoring.
- [ ] **Memory writes:** salience and novelty increase write likelihood; overload suppresses writes.
- [x] **Response planning:** confidence, uncertainty, and stability determine assert, hedge, explain, or repair.
- [ ] **Tone / style:** mood affects delivery without distorting factual grounding.
- [ ] **Overload suppression:** high load reduces write/amplification pressure before it becomes runaway behavior.

Deliverables:

- [x] Small explicit bias function for current self-state WM admission contribution.
- [x] Measurable telemetry counters per coupling point.
- [x] Tests proving self-state changes downstream decisions.
- [ ] Tests proving calibration predictions do not bypass rule gates.
- [ ] Calibration-rule gate tests prove model/advisory signals stay bounded by explicit rules.

### Phase 9A - Cognitive Gating Consolidation

Goal:

```text
Make Working Memory admission biologically inspired,
causally affected by bounded self-state, and governed
by one inspectable cognitive-control path.
```

Deliverables:

- [x] Initial SelfModel -> WM admission coupling exists for vigilance, uncertainty, inhibition, and cognitive load.
- [x] Explicit bounded self-state bias function exists.
- [x] Self-state gate contribution is telemetry-visible.
- [x] Neutral/default self-state preserves previous gate behavior.
- [ ] Consolidate final WM admission under `Brain.BasalGanglia`.
- [ ] Redefine `Brain.LIFG.Stage2` as linguistic evidence finalization rather than final WM admission.
- [ ] Route LIFG Stage2 candidates through the canonical cognitive gate.
- [ ] Reconcile `Brain.WM.Policy` admission responsibilities with `Brain.BasalGanglia`.
- [ ] Ensure all WM ingress paths obey the same canonical gate.
- [ ] Ensure Hippocampus/recall ingress obeys the canonical gate.
- [ ] Ensure Curiosity/Thalamus/DLPFC ingress obeys the canonical gate.
- [ ] Tests proving no path bypasses the canonical gate.
- [ ] Tests proving self-state modulation cannot bypass hard evidence/rule gates.
- [ ] Preserve bounded scores and telemetry across all gate paths.

Notes:

- Existing direct or commit-oriented behavior in `Brain.LIFG.Stage2` is an
  implementation area to consolidate, not the target architecture.
- Current `Brain.WM.Policy` behavior includes both admission and retention
  mechanics. The target is for BasalGanglia to own general admission while
  WM policy / WorkingMemory own retention, duplicate/lemma constraints,
  capacity, normalization, merge, decay, and eviction.
- If code or config still uses `lifg_min_score`, treat that name as
  implementation-specific. A future cleanup may generalize it to a concept such
  as `evidence_floor` or `min_input_score`.

### Phase 10 - Goal Stack And Motivational Layer

**Goal:** move from passive introspection to directed self-regulation.

Deliverables:

- [x] Explicit goal stack.
- [x] Tension / priority model.
- [x] Unresolved-task carryover through bounded self-continuity snapshots.
- [x] Curiosity proposals tied to uncertainty reduction.
- [ ] Suppression rules when overload is high.

---

## 9) Telemetry And LiveView Requirements

Minimum telemetry set:

- [x] `[:brain, :affect, :appraisal]`
- [x] `[:brain, :mood, :appraisal_applied]`
- [x] `[:brain, :self_model, :update]`
- [x] `[:brain, :self_calibration, :sample_logged]`
- [x] `[:brain, :self_calibration, :prediction]`
- [x] `[:brain, :self_calibration, :blend]`
- [x] `[:brain, :self_model, :continuity_restored]`
- [x] `[:brain, :meta_monitor, :warning]`
- [x] `[:brain, :response, :mode_selected]`
- [x] Gate/admission telemetry exposes current self-state contribution where
  the Phase 9 coupling is implemented.
- [ ] Canonical BasalGanglia gate telemetry covers every WM ingress path.

Minimum HUD panels:

- [x] current confidence / uncertainty / stability
- [x] mood snapshot
- [x] active goals count
- [x] latest appraisal target
- [x] last LIFG summary
- [ ] latest calibration prediction
- [ ] calibration model version
- [ ] latest blend decision
- [ ] recent warnings or contradictions
- [x] last continuity restore summary

---

## 10) Validation And Safety

### 10.1 Validation

Unit tests should cover:

- [x] attribution correctness
- [x] confidence bounds
- [x] uncertainty bounds
- [x] appraisal telemetry metadata
- [x] self-model update telemetry metadata
- [x] calibration sample construction
- [x] calibration logging
- [x] dataset vectorization
- [x] tensor conversion
- [x] baseline prediction
- [x] evaluator metrics
- [x] model predictions stay bounded
- [x] explicit blend policy
- [ ] stability decay/recovery
- [x] continuity restoration rules
- [ ] no unsupported self-claims
- [x] response mode selection under uncertainty

Property tests should cover:

- [x] bounded self-state fields
- [ ] telemetry meta always includes `count` and `v`
- [x] persisted self snapshots remain serializable and version-safe
- [ ] feature rows match declared schema width
- [ ] model predictions remain bounded across generated inputs

### 10.2 Safety Commitments

- No claims of sentience or consciousness.
- No self-narrative that outruns stored evidence.
- No hidden persistence of unsupported personal state.
- Prefer inspectable control signals over opaque personality behavior.
- ML output remains advisory unless accepted by an explicit blend policy.
- Keep rule output, model output, comparison, and blend decision visible in telemetry.
- Keep a debug path that can mute affective and ML coupling and expose raw state.

---

## 11) Practical Execution Order

Completed:

1. [x] Ship `Brain.SelfModel` and `Brain.Introspection`.
2. [x] Fold appraisal and mood into the self-model update path.
3. [x] Add first-class target attribution.
4. [x] Add inspectable attribution evidence.
5. [x] Build self-calibration sample contract.
6. [x] Log calibration samples.
7. [x] Convert samples to dataset rows.
8. [x] Convert rows to Nx tensors.
9. [x] Add baseline advisory prediction contract.
10. [x] Add evaluator and prediction error metrics.
11. [x] Add Axon model, offline training, and artifact-backed predictor.
12. [x] Compare Axon output against baseline.
13. [x] Add explicit blend policy with telemetry.
14. [x] Add `Brain.SelfContinuity` warm-start boundary.
15. [x] Persist and restore bounded, version-checked self snapshots.
16. [x] Add `Brain.MetaMonitor` warning and repair suggestions.
17. [x] Add `Brain.GoalStack` and uncertainty-reduction pressure.
18. [x] Couple response planning to bounded self-state.
19. [x] Begin SelfModel -> WM admission coupling for vigilance, uncertainty,
    inhibition, and cognitive load.

Next:

1. [ ] Consolidate final WM admission under `Brain.BasalGanglia`.
2. [ ] Route `Brain.LIFG.Stage2` candidates through the canonical cognitive gate.
3. [ ] Reconcile `Brain.WM.Policy` admission responsibilities with
   `Brain.BasalGanglia`.
4. [ ] Ensure Hippocampus/recall and Curiosity/Thalamus/DLPFC ingress obey the
   same gate.
5. [ ] Add bypass tests proving no WM ingress path mutates WorkingMemory outside
   the canonical gate.
6. [ ] Finish Phase 9 behavior coupling for memory writes, tone/style,
   overload suppression, and calibration-rule gate tests.

Recommended next implementation target:

```text
Phase 9A - Cognitive Gating Consolidation
```

Purpose:

```text
candidate evidence + bounded control context
-> BasalGanglia canonical gate
-> controlled PFC / WorkingMemory update
```

---

## Appendix A - Architecture-Safe Pseudocode

```elixir
resolved = Core.resolve_input(input)

appraisal = Brain.AffectiveAppraisal.appraise(resolved)
:ok = Brain.MoodCore.apply_appraisal(appraisal)

{:ok, self_model} =
  Brain.Introspection.update_from_resolved(resolved, appraisal)

sample =
  Brain.SelfCalibration.Features.build_sample(self_model,
    appraisal: appraisal,
    lifg: self_model.last_lifg,
    source: :runtime
  )

:ok = Brain.SelfCalibration.Logger.log(sample)

rows = Brain.SelfCalibration.Dataset.to_rows([sample])
{:ok, batch} = Brain.SelfCalibration.Tensor.from_rows(rows)

{:ok, baseline} = Brain.SelfCalibration.Predictor.predict(batch)
{:ok, evaluation} = Brain.SelfCalibration.Evaluator.evaluate(baseline, sample.labels)

{:ok, candidate} =
  Brain.SelfCalibration.AxonPredictor.predict(batch, artifact: artifact)

{:ok, comparison} =
  Brain.SelfCalibration.Comparison.compare(baseline, candidate, sample.labels)

{:ok, decision} =
  Brain.SelfCalibration.Blend.decide(baseline, candidate, comparison)

candidate =
  %{
    source: :lifg_stage2,
    winner: self_model.last_lifg,
    score: self_model.confidence,
    ambiguity: self_model.uncertainty
  }

gate_context =
  %{
    self_state: self_model,
    conflict: :from_acc,
    wm_state: :current_capacity_and_duplicates,
    goals: self_model.active_goals
  }

# Target architecture: route general admission through the canonical
# BasalGanglia/control gate before WorkingMemory mutation.
gate_decision = {:basal_ganglia_gate, candidate, gate_context}
```

---

## Appendix B - Core Principle

Symbrella becomes more self-aware not when it says more about itself, but when it can:

- represent itself explicitly
- inspect that representation in real time
- persist only supported state
- calibrate bounded signals from evidence
- act differently because of that state
- explain those changes with evidence
