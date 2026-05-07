# Brain Chain - Semantic Disambiguation, Integration, and Memory

This document summarizes the current turn-processing chain and the
brain-inspired modules that receive signals after Core prepares a
`Core.SemanticInput`.

The rule of thumb is:

```text
db <- brain <- core <- web
```

Core may call Brain and Db. Brain may call Db. Brain must not depend on Core or
the web app.

## Order of Operations

1. **Perception and tokenization (Core)**
   - `Core.Pipeline.Perception` and `Core.Token` build word-level tokens with
     sentence-aware spans.
   - Core keeps the LIFG path words-only. Char-grams are not allowed into LIFG.

2. **MWE and candidate preparation (Core)**
   - `Core.MWE.Stage` and `Core.MWE.Injector` form multi-word expressions from
     word n-grams.
   - `Core.SenseSlate` and candidate pipeline modules prepare
     `si.sense_candidates` keyed by token index.

3. **STM / activation snapshot (Brain through Core adapters)**
   - Core asks Brain for short-term/working-memory context and activation
     summaries.
   - The result is folded back into `Core.SemanticInput`.

4. **LTM and evidence (Db + Core)**
   - Db returns known cells, episodes, and lexical rows.
   - `Core.Pipeline.Evidence` attaches evidence without letting storage logic
     leak into Brain scoring.

5. **Lexicon enrichment (Lexicon through Core)**
   - When enabled, Core can ask `Lexicon.enrich/2` for external dictionary
     evidence.
   - New HTTP work should prefer `Req`; the current adapter still has legacy
     `:hackney` usage.

6. **Intent and recall planning (Core)**
   - `Core.Intent.*` selects intent, keyword, and confidence.
   - `Core.Recall.*` decides whether Hippocampus recall should run, which cues
     to use, and how many results to attach.

7. **LIFG Stage-1 (Brain)**
   - `Brain.LIFG.Stage1` performs competitive sense selection over the Core
     sense slate.
   - Boundary, char-gram, MWE, margin, and telemetry contracts are enforced by
     Brain LIFG guard modules and tests.

8. **Post-LIFG integration (Brain)**
   - Brain regions receive the enriched semantic state and/or telemetry:
     - `Brain.ATL` for semantic hub integration.
     - `Brain.PMTG` for controlled retrieval and rerun pressure.
     - `Brain.ACC` for conflict monitoring.
     - `Brain.OFC` / `Brain.VmPFC` for value signals.
     - `Brain.Hippocampus` for episodic binding and recall.
     - `Brain.WorkingMemory` and `Brain.WM.*` for focus admission.
     - `Brain.Blackboard` for cross-region event state.

9. **Curiosity and focus loop (Brain)**
   - `Brain.Curiosity` proposes probes.
   - `Brain.Thalamus` blends curiosity, ACC, OFC, and mood.
   - `Brain.DLPFC` turns approved decisions into `Brain.focus/2`.
   - `Brain.BasalGanglia` and `Brain.WorkingMemory` gate and normalize the item.

10. **Response planning and synthesis (Core + Llm)**
    - `Core.Response.*` builds a safe response plan.
    - `Core.Response.LlmPrompt` and `Core.Response.LlmSynthesis` may call `Llm`
      for model-backed phrasing.

## Current Status

- LIFG Stage-1 is operational and heavily tested.
- LIFG Stage-2 / blackboard bridge exists for post-selection integration.
- MWE injection is word-level and Core-owned.
- Hippocampus recall, evidence attachment, and episode storage are active.
- WM gating, decay, duplicate handling, and capacity trimming are Brain-owned.
- Curiosity -> Thalamus -> DLPFC -> WM is implemented with telemetry and tests.
- Mood, self-model, cycle clock, and ML/self-calibration modules are present and
  should be treated as evolving Brain internals.

## Introspection

- `Brain.snapshot/0` gives a broad Brain state snapshot.
- `Brain.Introspect` and `Brain.Introspection` expose region-oriented views.
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
