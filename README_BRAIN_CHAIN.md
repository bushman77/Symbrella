# Brain Chain - Cognitive Control, Semantic Evidence, and Memory

This document explains Symbrella's current turn-processing chain and the
intended biologically-inspired cognitive-control structure that guides ongoing
work.

Symbrella uses region names as engineering abstractions and computational
analogues. They are grounded in broad functional neuroscience, but they are not
claims of biological equivalence. This project is not a literal brain
simulation, not a clinical neurological model, and not evidence of
consciousness, sentience, feelings, or subjective experience.

The umbrella dependency rule remains:

```text
db <- brain <- core <- web
```

Core may call Brain and Db. Brain may call Db. Brain must not depend on Core or
the web app.

## Canonical Control Loop

For documentation and implementation planning, Symbrella's target cognitive
control path is:

```text
Semantic / lexical representations
-> LIFG Stage1
   competitive semantic interpretation
   "Which interpretation is winning?"
-> LIFG Stage2
   linguistic evidence finalization
   winner / score / margin / ambiguity / conflict
   "How strong was the linguistic decision?"
-> ACC / cognitive-control context
   conflict / uncertainty / task pressure
-> BasalGanglia
   canonical Working Memory admission gate
   :allow | :boost | :block
   "Should this representation update WM?"
-> Thalamic relay / gating control
-> DLPFC / PFC control
   task-relevant maintenance / control
-> WorkingMemory
   maintain / normalize / merge / decay / evict
```

This is a software control model, not a claim that actual human cognition is a
simple linear assembly line. The inspiration is recurrent cortical /
basal-ganglia / thalamic control, so feedback is expected:

```text
WorkingMemory / PFC
-> LIFG context
-> BasalGanglia context
-> memory retrieval
-> attention/control
-> SelfModel evidence
```

## Current Implementation vs Target

Implemented pieces already exist across Core and Brain:

- Core builds `Core.SemanticInput`, token streams, MWE candidates, evidence,
  intent, recall cues, response plans, and Brain adapter calls.
- `Brain.LIFG.Stage1` performs competitive semantic interpretation.
- `Brain.LIFG.Stage2` exists and currently produces commit-shaped decisions.
- `Brain.ACC`, `Brain.OFC` / `Brain.VmPFC`, `Brain.Thalamus`,
  `Brain.BasalGanglia`, `Brain.DLPFC`, and `Brain.WorkingMemory` participate in
  control, value, relay, gating, focus, and active-memory behavior.
- `Brain.SelfModel` integrates distributed runtime evidence, and Phase 9 work
  has begun coupling bounded self-state fields into WM admission scoring.

The target is to consolidate all general Working Memory admission under the
canonical BasalGanglia/control gate:

```text
LIFG Stage1
-> LIFG Stage2 linguistic evidence
-> BasalGanglia canonical cognitive admission decision
-> PFC / WorkingMemory control
-> WorkingMemory mechanics
```

Known transitional overlap remains in:

- `Brain.LIFG.Stage2`
- `Brain.WM.Policy`
- `Brain.BasalGanglia`

This overlap is technical debt, not the target architecture.

## Semantic Interpretation

Core owns the early semantic pipeline:

- `Core.Pipeline.Perception` and `Core.Token` build word-level tokens with
  sentence-aware spans.
- Core keeps the LIFG path words-only. Character n-grams are not allowed into
  LIFG.
- `Core.MWE.Stage` and `Core.MWE.Injector` form multi-word expressions from
  word n-grams.
- `Core.SenseSlate` and candidate pipeline modules prepare
  `si.sense_candidates` keyed by token index.
- Db and Lexicon evidence are attached through Core-owned pipeline stages.

Core orchestrates; it does not own Brain region machinery.

## LIFG Stage1

`Brain.LIFG.Stage1` is the competitive semantic / word-sense selection stage.

Conceptual question:

```text
What interpretation is currently best supported?
```

It compares competing interpretations and emits:

- winner identity;
- confidence-like score;
- margins;
- finalists;
- feature and candidate audit information;
- control evidence for downstream regions.

Stage1 is not the final Working Memory gate.

## LIFG Stage2

`Brain.LIFG.Stage2` currently exists and produces commit-shaped decisions.
Architecturally, its intended long-term role is narrower:

- finalize LIFG linguistic evidence after Stage1 competition;
- preserve or extract winner identity;
- carry winner score and margin;
- expose ambiguity and conflict evidence when available;
- prepare a candidate representation for downstream cognitive gating.

Conceptual question:

```text
How strong and unambiguous was the linguistic decision?
```

Stage2 should not be the canonical final Working Memory admission authority.
Current direct commit-oriented behavior should be treated as an implementation
area to consolidate during Phase 9A.

## ACC / Control Context

`Brain.ACC` is a conflict and uncertainty monitoring analogue. It provides
bounded cognitive-control context such as:

- conflict;
- uncertainty;
- ambiguity pressure;
- task pressure;
- reanalysis or repair pressure.

ACC does not admit memories by itself. Its output should inform the canonical
gate.

## BasalGanglia Admission

`Brain.BasalGanglia` is the intended canonical general Working Memory admission
gate.

Conceptual question:

```text
Should this candidate update or reinforce Working Memory?
```

It owns or should eventually generalize decisions such as:

```elixir
:allow | :boost | :block
```

Signals it may consume include:

- candidate evidence strength;
- attention and salience;
- WM fullness;
- duplicates;
- cooldown;
- source preferences;
- goals and control context;
- conflict;
- bounded SelfModel modulation.

The BasalGanglia abstraction should not remain LIFG-specific. If documentation
or configuration mentions `lifg_min_score`, treat that as an implementation-
specific name that may later be generalized to `evidence_floor` or
`min_input_score`. P-300 does not rename runtime code.

## Thalamic Relay

`Brain.Thalamus` is a relay, arbitration, and gating-control analogue. It can
blend curiosity, ACC conflict, OFC/VmPFC value, mood, salience, and other
control inputs before downstream focus/control decisions.

It should not be documented as the semantic decision-maker or as the owner of
final WM admission.

The current curiosity path is:

```text
Curiosity
-> Thalamus relay/arbitration
-> BasalGanglia admission decision where routed
-> DLPFC/PFC focus control
-> WorkingMemory mechanics
```

Some legacy/transitional paths may still pass through WM policy behavior during
consolidation.

## DLPFC / PFC Control

`Brain.DLPFC` and `Brain.PFC` are executive/task-control and working-memory
maintenance/control analogues. Their role is to maintain task-relevant
representations, apply control decisions, and coordinate focus.

DLPFC/PFC should not become uncontrolled direct writers to WorkingMemory.

## WorkingMemory

`Brain.WorkingMemory` is the maintained active-representation store. Its core
mechanical responsibilities are:

- normalize;
- maintain;
- merge;
- activate;
- decay;
- trim;
- evict;
- enforce bounded capacity;
- emit telemetry.

The general cognitive decision "Should this candidate enter WM?" belongs
upstream in the cognitive gating/control layer, with BasalGanglia as the
canonical abstraction. `Brain.WM.Policy` currently contains both admission and
retention mechanics; the target split is:

```text
BasalGanglia:
general cognitive admission decision

WM policy / WorkingMemory:
retention, decay, duplicate/lemma constraints,
capacity, normalization, merge and eviction
```

## SelfModel Modulation

`Brain.SelfModel` is an engineering-level integration of distributed runtime
evidence. It is not a literal biological structure and not a claim of subjective
experience.

It carries bounded signals such as confidence, uncertainty, stability, focus,
vigilance, plasticity, inhibition, cognitive load, mood, active goals, recent
errors/actions, appraisal, LIFG summary, attribution, and continuity.

SelfModel can modulate cognitive control, but it is not itself the gate:

```text
SelfModel
-> bounded modulation
-> BasalGanglia / cognitive gating
-> Working Memory update / hold
```

Current Phase 9 work has begun coupling vigilance, uncertainty, inhibition, and
cognitive load into WM admission scoring. The architectural target is to
consolidate general admission and self-state modulation around the
BasalGanglia/control layer instead of letting multiple independent gates grow.

## Hippocampal Recall

`Brain.Hippocampus` is an episodic-memory-inspired service for encoding,
binding, recall, and persistence-backed memory interaction. It can provide
context back into Core and Brain as bounded evidence.

Recall ingress should eventually obey the same canonical admission gate before
it mutates WorkingMemory. Recall selection and evidence retrieval remain
bounded by top-k, window, scoring, and provenance contracts.

## Recurrent Feedback

WorkingMemory and PFC state are allowed to influence later interpretation and
control, provided the feedback remains explicit and bounded:

- active WM can inform LIFG context;
- PFC/control state can affect gate context;
- memory retrieval can add evidence for reinterpretation;
- attention/control can change salience;
- SelfModel can update from observed decisions and errors.

Feedback should improve inspectability rather than hide causality.

## Order of Operations

The production path starts in `Core.resolve_input/2` and currently includes:

1. Perception and tokenization.
2. MWE and candidate preparation.
3. STM / activation snapshot through Brain adapters.
4. LTM, lexical, and episode evidence attachment.
5. Lexicon enrichment when enabled.
6. Intent and recall planning.
7. LIFG Stage1 competitive interpretation.
8. LIFG Stage2 / post-selection integration.
9. ATL, pMTG, ACC, OFC/VmPFC, Hippocampus, Blackboard, and control-region
   updates.
10. Curiosity, Thalamus, BasalGanglia, DLPFC/PFC, and WorkingMemory interaction.
11. Response planning and optional LLM-backed synthesis.
12. Hippocampal encoding/persistence and activation telemetry.

## Introspection

- `Brain.snapshot/0` gives a broad Brain state snapshot.
- `Brain.Introspect` and `Brain.Introspection` expose region-oriented views.
- `Brain.Introspection` can publish the latest `%Brain.SelfModel{}` into the
  Brain coordinator for downstream consumers.
- Individual regions commonly expose `snapshot/0`, `status/0`, or both.
- The Phoenix `/brain` LiveView renders selected Brain state and telemetry.

## Useful Test Targets

```bash
mix test apps/core/test/core/pipeline_contract_test.exs
mix test apps/core/test/core/resolve_input_test.exs
mix test apps/brain/test/brain/lifg_stage1_invariants_test.exs
mix test apps/brain/test/brain/lifg_stage2_contract_test.exs
mix test apps/brain/test/brain/curiosity_flow_test.exs
mix test apps/brain/test/brain/thalamus_*test.exs
mix test apps/brain/test/brain/wm_*test.exs
```
