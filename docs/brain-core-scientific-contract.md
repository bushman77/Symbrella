# Brain/Core Scientific Contract

This document defines what Symbrella can honestly guarantee about `apps/brain`
and `apps/core`.

The project is biologically inspired. It is not a biological brain simulation,
not a clinical model, and not evidence that the system has consciousness,
feelings, sentience, or human-like subjective state. Region names such as LIFG,
pMTG, Hippocampus, ACC, OFC, BasalGanglia, DLPFC, Thalamus, and Working Memory
are engineering abstractions for bounded software responsibilities.

When this project uses neuroscience language, the permitted claim is that a
module is a computational analogue or functional correlate for an engineering
role. The prohibited claim is biological equivalence.

The guarantee we can make is narrower and testable:

- every brain/core stage has a defined software contract;
- numeric state is bounded where it represents probabilities, confidence,
  mood indices, salience, activation, conflict, or uncertainty;
- token and span transforms preserve LIFG-safe invariants;
- memory and retrieval paths are bounded by explicit capacity, top-k, window,
  and timeout limits;
- region decisions are traceable through audit maps, telemetry, or returned
  metadata;
- scientific terminology must not be used to overclaim beyond the measured
  behavior.

## Cognitive-Control Contract

The intended control structure is recurrent cortical / basal-ganglia /
thalamic control expressed as software boundaries:

```text
Semantic / lexical representations
-> LIFG Stage1
   competitive semantic interpretation
-> LIFG Stage2
   linguistic evidence finalization
-> ACC / cognitive-control context
   conflict / uncertainty / task pressure
-> BasalGanglia
   canonical Working Memory admission gate
-> Thalamic relay / gating control
-> DLPFC / PFC control
-> WorkingMemory
   maintained active-representation store
```

Feedback is part of the design:

```text
WorkingMemory / PFC
-> LIFG context
-> BasalGanglia context
-> memory retrieval
-> attention/control
-> SelfModel evidence
```

Current code is still consolidating overlapping gate-like behavior in
`Brain.LIFG.Stage2`, `Brain.WM.Policy`, and `Brain.BasalGanglia`. The contract
target is one inspectable cognitive admission path before general WorkingMemory
mutation.

## Claim Classes

Use these labels when documenting, reviewing, or testing brain/core behavior.

### Contractual

A contractual claim is guaranteed by code and tests.

Examples:

- LIFG input must not contain char-grams.
- Non-MWE tokens must align to word boundaries.
- Working Memory is capacity-limited.
- Mood, confidence, salience, conflict, and probability values are clamped to
  finite ranges.
- LIFG Stage1 emits choices, margins, finalist rankings, and audit counts.

### Biologically Inspired

A biologically inspired claim is an analogy that guides architecture, not a
scientific proof.

Examples:

- `Brain.Hippocampus` is an episodic-memory-inspired recall and consolidation
  service.
- `Brain.ACC` is a conflict-monitoring correlate.
- `Brain.OFC` is a value-estimation correlate.
- `Brain.DLPFC` is an execution/focus-control correlate.
- `Brain.MoodCore` uses neuromodulator names as bounded control variables.

### Prohibited

The code, UI, prompts, docs, and logs must not claim these as facts:

- the system is scientifically equivalent to a human brain;
- region names prove biological fidelity;
- mood values are emotions;
- self-model values are consciousness, sentience, or subjective experience;
- outputs are medically or neurologically validated.

## Region Contract Map

| Area | Software role | Allowed scientific wording | Required validation |
| --- | --- | --- | --- |
| `Core.SemanticInput` | Canonical semantic carrier | Semantic state carrier | Struct field compatibility and no unknown pipeline mutation |
| `Core.Token`, `Core.LIFG.Input` | Tokenization and span construction | Linguistic preprocessing | Word boundary, span, and MWE invariants |
| `Core.Intent.*` | Intent selection and confidence | Intent heuristic/classifier | Known intent atoms only, confidence bounded |
| `Core.Recall.*` | Recall planning and episode merge | Memory cue planning | Top-k/window limits, safe fallbacks |
| `Brain.LIFG.Stage1` | Competitive sense selection | LIFG-inspired disambiguation | Softmax/probability bounds, margins, audit counters |
| `Brain.LIFG.Stage2` | Post-selection linguistic evidence finalization | LIFG-inspired evidence finalization | Winner identity preserved, score bounded, margin bounded/non-negative, traceable provenance, no unbounded processing, does not silently become an uncontrolled WM writer |
| `Brain.PMTG` | Controlled semantic retrieval | pMTG-inspired retrieval | Weak-choice detection, query limits, no unbounded reruns |
| `Brain.Hippocampus` | Episodic encode/recall | Hippocampus-inspired episodic memory | Window bounds, recency/Jaccard scoring, evidence provenance |
| `Brain.WorkingMemory` | Capacity-limited maintained active item set | Working-memory correlate | Capacity, normalization, activation, decay, duplicate merge, trimming, eviction, telemetry, payload shape |
| `Brain.BasalGanglia` | Canonical target gate for general WM admission | Basal-ganglia-inspired gating | `:allow | :boost | :block`, bounded scores, traceable evidence/context, no LIFG-only assumption |
| `Brain.Thalamus` | Arbitration/relay for curiosity/value/conflict | Thalamus-inspired relay | Score clamp, monotonic weighting tests |
| `Brain.ACC` | Conflict/uncertainty monitor | ACC-inspired conflict correlate | Conflict in bounded range, decay behavior |
| `Brain.OFC` / `Brain.VMPFC` | Proposal valuation | Value-estimation correlate | Bounded value, mood cap, risk/novelty weighting |
| `Brain.DLPFC` / `Brain.PFC` | Execution and policy control | Executive-control correlate | No direct uncontrolled WM writes |
| `Brain.MoodCore` | Bounded control vector | Neuromodulator-inspired control state | Level clamps, half-life decay, small deltas |
| `Brain.SelfModel` | Runtime evidence integration and bounded control modulation | Self-model correlate / engineering abstraction | Bounded fields, evidence provenance, advisory/modulatory only, no consciousness claims, cannot bypass gates |
| `Brain.Self*` | Continuity, calibration, introspection, and reflective helpers | Self-model-related engineering abstractions | Versioning, bounds, traceability, advisory-only ML unless explicitly blended |

## Pipeline Invariants

These are the invariants that should be treated as regression blockers.

### Core pipeline

- `Core.resolve_input/2` returns a `%Core.SemanticInput{}`.
- `tokens` are lists of token-like maps/structs.
- `sense_candidates` is keyed by token index.
- LIFG-bound tokens are word-boundary safe.
- MWEs are word n-grams, not character n-grams.
- Intent values are known atoms or `:unknown`.
- Confidence-like values are numeric and clamped to `0.0..1.0`.
- Recall attachment never crashes the pipeline.
- Response planning must not expose internal reasoning or hidden state as truth.

### Brain pipeline

- Region GenServers must tolerate absent optional processes in test/reduced mode.
- LIFG Stage1 must return either `{:ok, %{choices: ..., audit: ...}}` or
  `{:error, reason}`.
- Stage1 choices must include `token_index`, winner id, score/probability, and
  margin when available.
- LIFG Stage2 must preserve winner provenance and produce bounded linguistic
  evidence for downstream control.
- PMTG must only query bounded evidence and either boost/inhibit or rerun through
  an explicit mode.
- Architectural target: general WM admission must pass through the canonical
  BasalGanglia/control gate before WorkingMemory mutation. Transitional
  gate-like paths in Stage2 and WM policy are consolidation work, not the
  desired endpoint.
- Mood deltas must be small and clamped.
- Self-model predictions are advisory metadata, never asserted as subjective
  state.
- SelfModel modulation may bias cognitive control only through bounded,
  telemetry-visible contributions and must not bypass hard evidence or rule
  gates.

## Review Checklist

Use this checklist for every brain/core change.

- Does the change preserve the `db <- brain <- core <- web` dependency rule?
- Does it introduce any unbounded loop, unbounded memory growth, or unbounded
  recall query?
- Does every score/probability/confidence-like value stay finite and bounded?
- Does the trace/audit metadata explain why a decision changed?
- Does the code distinguish "biologically inspired" from "biologically proven"?
- Are tests focused on observable behavior rather than region-name mythology?
- Does `mix compile` pass?

## Current Status

The existing project already has useful invariant coverage around:

- token span safety;
- no char-grams in the LIFG path;
- MWE injection and ordering;
- LIFG Stage1 candidate coverage and missing-candidate auditing;
- synonym competition behavior;
- response policy profiles;
- bounded self-calibration prediction outputs.

Remaining work to move closer to a strong guarantee:

- add explicit bounded-value tests for all mood, salience, conflict, value, and
  activation outputs;
- add contract tests around full `Core.resolve_input/2` traces;
- add integration tests proving PMTG reruns cannot loop unboundedly;
- consolidate overlapping admission logic in `Brain.LIFG.Stage2`,
  `Brain.WM.Policy`, and `Brain.BasalGanglia`;
- add tests proving LIFG, Hippocampus/recall, curiosity/thalamus/DLPFC, and any
  other WM ingress path cannot write WM outside the canonical gate;
- add tests proving self-state modulation cannot bypass hard evidence/rule
  gates;
- add a static documentation check that prohibits overclaim language.
