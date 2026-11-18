# Neural Atlas evidence layers (Phoenix LiveView)

The maintained interface is the Phoenix `/brain` route, built from
`SymbrellaWeb.BrainLive`, `SymbrellaWeb.Components.Brain.Panels`, and
`SymbrellaWeb.Components.Brain.NeuralAtlas`.

The older `docs/flowcharts/symbrella_architecture.html` is not used by this
LiveView. Do not update it to change the live atlas.

## Graph-first interface

The default display on `/brain` is **Network graph**, implemented in
`SymbrellaWeb.Components.Brain.NeuralAtlasGraph`. The **Region list** toggle
keeps keyboard- and touch-friendly cards available as an alternative. Both
views use the same `select-region` event and selected-region inspector.

The graph has stable, presentation-only coordinates for known regions, plus a
deterministic fallback for newly registered keys. Coordinates are inspired by
brain anatomy but do not claim anatomical precision or biological neurons.
Curved conceptual edges are derived only from registry `:related` entries
and deduplicated as undirected pairs. Directed, dashed BEAM edges use
`AtlasEvidence.dependencies/1` results. Observed telemetry and Bus receipts
highlight nodes and show session counts; **neither produces inter-region edges**.

Every registered module is represented by one small violet satellite dot attached
to its region hub. The spoke means **registry membership**, not a function call,
runtime activation, or a separate biological neuron. Regions with zero registered
modules have no satellites, even if their underlying implementation may exist.

On BEAM, telemetry and Bus layers, the graph retains faint dotted conceptual
links for geographic context. They are explicitly labeled as registry navigation
metadata, **not** observed calls or inferred signal traffic. Telemetry and
Bus observations are marked by amber rings and counts but are not animated
continuously: a receipt observed earlier in the connected session does not
establish continuing activity.

The graph uses SVG for non-interactive edges and ordinary HTML buttons for
the selectable nodes. Buttons have accessible names, keyboard focus, and
explicit `aria-pressed` values. Narrow screens can scroll horizontally.
At very wide layouts, the selected-region inspector sits to the right.

The graph does **not** yet support zooming, panning, or timestamped execution
span correlation. Those are independent follow-up milestones.

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
  apps/symbrella_web/test/symbrella_web/components/neural_atlas_graph_test.exs \
  apps/symbrella_web/test/symbrella_web/components/atlas_evidence_test.exs
```

Then open `/brain`, choose **Neural Atlas**, and switch between layers.
A quiet live layer means **no matching receipt observed on this connected
page**, not that the cognitive subsystem was idle.
