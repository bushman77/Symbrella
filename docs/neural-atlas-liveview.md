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
Curved conceptual edges use registry `:related` entries, which now also
include separately documented source-backed adjacency, and are deduplicated
as undirected navigation edges. **Source topology** presents the exact
reviewed `Registry.source_relations/0` witnesses with grouping, configured
planner, explicit function-call, and telemetry-subscription types. These are
source declarations, not observed execution. Directed, dashed BEAM edges still
derive from compiled imports via `AtlasEvidence.dependencies/1`.
Observed telemetry and Bus receipts highlight nodes and show session counts;
**neither produces inter-region edges**.

Every registered module is represented by one small violet satellite dot attached
to its region hub. The spoke means **registry membership**, not a function call,
runtime activation, or a separate biological neuron. Regions with zero registered
modules have no satellites, even if their underlying implementation may exist.

On Source topology, BEAM, telemetry, Bus and Turn trace layers, the graph
retains faint dotted conceptual
links for geographic context. They are explicitly labeled as registry navigation
metadata, **not** observed calls or inferred signal traffic. Telemetry and
Bus observations are marked by amber rings and counts but are not animated
continuously: a receipt observed earlier in the connected session does not
establish continuing activity.

The graph uses SVG for non-interactive edges and ordinary HTML buttons for
the selectable nodes. Buttons have accessible names, keyboard focus, and
explicit `aria-pressed` values. At very wide layouts, the selected-region
inspector sits to the right.

### P-003: Mobile graph canvas

The graph's inner canvas has an explicit `height: 600px` and
`min-width: 1000px` in inline styles, independent of Tailwind arbitrary
utilities (such as `h-[600px]`). This prevents the absolutely positioned
SVG and region buttons from collapsing into a zero-height container when
an arbitrary utility is absent from compiled CSS.

The outer `atlas-network-viewport` remains bounded to the screen width
and horizontally scrollable on touch devices. It is a keyboard-focusable
`role="region"` landmark with an accessible label. A small-screen
instruction says to swipe sideways and tap a region. SVG positioning and
button dimensions also include inline fallbacks; graph coordinates, visual
evidence semantics, desktop sizing and the selected-region inspector are
unchanged.

Rendered-component tests assert the width/height contract, accessible
scrolling container, selectable nodes and all evidence layers. These are
markup regression tests; device/browser verification is still required.

The graph does **not** yet support zooming, panning, or verified propagation
along source-backed edges. Turn Trace v1 has correlated stage timings but
cannot infer cross-process causal routes from uninstrumented calls.

## Registry and evidence provenance

`SymbrellaWeb.Region.Registry` is the authority for registered region keys,
module membership, documented conceptual links and telemetry topic names.

The inspector offers six independent layers:

1. **Conceptual links:** registry-owned `:related` metadata, including
   source-backed navigational adjacency. Not proof of execution.
2. **Source topology:** reviewed source declarations from
   `Region.Registry.source_relations/0`, with precise relation kinds and
   implementation witnesses. Grouping and configured planner relationships
   are distinguished from direct calls and telemetry subscriptions.
3. **BEAM references:** `AtlasEvidence.dependencies/1` reads the `imports`
   chunk from compiled registered module BEAM files and retains static
   references between different registered regions. This is direct remote
   call evidence at compile time, **not** runtime execution evidence.
   Dynamic dispatch, inter-process messages, references to unregistered
   modules, and source code that did not compile are outside its scope.
4. **Live telemetry:** `AtlasEvidence.capture/4` filters `brain:blackboard`
   messages to registered `:telemetry` event topics. Matching receipts
   show the topic, receiving region(s), and time received by the page.
   A registered topic need not have fired; an observed topic does not prove
   synaptic activation or a complete causal trace.
5. **Bus events:** `AtlasEvidence.capture_bus/4` records non-telemetry
   `brain:blackboard` message kinds and an explicitly claimed registered
   region when present. Unattributed messages remain unattributed.
   These receipts do **not** prove which module/function executed.
6. **Turn trace:** `Core.CognitiveTrace` instruments real, ordered
   symbolic chat worker calls, and `CognitiveTraceStore` retains bounded,
   turn-correlated stage timings and evidence counters. The P-002 executive
   panel now shows Prefrontal planner call status, validated/merged controls,
   shadow-scoring usage and live-versus-shadow action comparison for the
   **selected turn**. The Prefrontal inspector summarizes all planners;
   selecting DLPFC, FPC, DMPFC, VMPFC or Salience reveals that region's
   recorded contribution. The graph counts explicitly observed planner calls,
   excluding unavailable modules. These observations never create new runtime
   edges or prove that shadow recommendations affected live execution.
   See [Cognitive Trace v1](cognitive-trace-v1.md) for contracts and tests.

Telemetry and Bus observation buffers are per connected LiveView and each holds
up to 40 receipt summaries. The separate CognitiveTraceStore is supervised,
shared in memory and bounded to 20 turn IDs, 40 receipts per turn, and ten
minutes of history. Trace records contain only allowlisted, sanitized metadata;
no user text, raw event payload, or neural cell state is stored in that buffer.
Refreshing/reconnecting resets LiveView observation windows but can still
display recent trace-store entries until expiration; historical completeness
is never implied.

Existing Brain.Cell activation visualizations and snapshots remain separate
from this evidence model.

## P-005: streamlined Brain dashboard

The bottom **All Regions — Live Status** grid has been removed from the
`/brain` page. The LiveView no longer collects
`BrainRuntime.all_region_status/0` on mount or every selected-region refresh.
The repeated Live State and Module Status process cards have also been removed
from the dashboard, and the selected-region configuration no longer displays
a PID/queue/current-function column.

The selected region's actual runtime configuration (including LIFG thresholds
and weights), region snapshot, working memory, blackboard, mood, neuronal
activity chart, Neural Atlas, and Cognitive Trace are preserved. The underlying
region processes, diagnostic component functions, and on-demand plain-text
Brain report are unchanged. The text report can still include OTP diagnostic
status when explicitly requested; the background UI no longer polls all
regions merely to render redundant process cards.

## Verification

From the umbrella root:

```sh
mix test apps/symbrella_web/test/symbrella_web/components/brain_report_copy_test.exs \
  apps/symbrella_web/test/symbrella_web/components/neural_atlas_test.exs \
  apps/symbrella_web/test/symbrella_web/components/neural_atlas_graph_test.exs \
  apps/symbrella_web/test/symbrella_web/components/executive_trace_panel_test.exs \
  apps/symbrella_web/test/symbrella_web/components/atlas_evidence_test.exs
```

Then open `/brain`, choose **Neural Atlas**, and switch between layers.
A quiet live layer means **no matching receipt observed on this connected
page**, not that the cognitive subsystem was idle.
