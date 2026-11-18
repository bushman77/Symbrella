# Neural Atlas evidence layers (Phoenix LiveView)

The maintained interface is the Phoenix `/brain` route, built from
`SymbrellaWeb.BrainLive`, `SymbrellaWeb.Components.Brain.Panels`, and
`SymbrellaWeb.Components.Brain.NeuralAtlas`.

The older `docs/flowcharts/symbrella_architecture.html` is not used by this
LiveView. Do not update it to change the live atlas.

## Registry and evidence provenance

`SymbrellaWeb.Region.Registry` is the authority for registered region keys,
module membership, documented conceptual links and telemetry topic names.

The inspector offers four independent layers:

1. **Conceptual links:** registry-owned `:related` metadata. These are
   navigation aids, not verified connections or observed calls.
2. **BEAM references:** `AtlasEvidence.dependencies/1` reads the `imports`
   chunk from compiled registered module BEAM files and retains static
   references between different registered regions. This is direct remote
   call evidence at compile time, **not** runtime execution evidence.
   Dynamic dispatch, inter-process messages, references to unregistered
   modules, and source code that did not compile are outside its scope.
3. **Live telemetry:** `AtlasEvidence.capture/4` filters `brain:blackboard`
   messages to registered `:telemetry` event topics. Matching receipts
   show the topic, receiving region(s), and time received by the page.
   A registered topic need not have fired; an observed topic does not prove
   synaptic activation or a complete causal trace.
4. **Bus events:** `AtlasEvidence.capture_bus/4` records non-telemetry
   `brain:blackboard` message kinds and an explicitly claimed registered
   region when present. Unattributed messages remain unattributed.
   These receipts do **not** prove which module/function executed.

The two observation buffers are per connected LiveView and each holds up to
40 receipt summaries. No event payloads, user text, or neural cell state are
written to either buffer. Refreshing/reconnecting starts a new observation
window; no historical completeness is implied.

Existing Brain.Cell activation visualizations and snapshots remain separate
from this evidence model.

## Verification

From the umbrella root:

```sh
mix test apps/symbrella_web/test/symbrella_web/components/neural_atlas_test.exs \
  apps/symbrella_web/test/symbrella_web/components/atlas_evidence_test.exs
```

Then open `/brain`, choose **Neural Atlas**, and switch between layers.
A quiet live layer means **no matching receipt observed on this connected
page**, not that the cognitive subsystem was idle.
