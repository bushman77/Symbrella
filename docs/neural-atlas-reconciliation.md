# Neural Atlas: source reconciliation

## Purpose

The Neural Atlas previously drew many region hubs without module satellites or
links. That was an **incomplete registry**, not evidence that those brain-inspired
components were unused. This pass reconciles known isolated region keys with
verified source modules and specifically identified architectural relationships.

The authoritative interactive page is `/brain`; the old
`docs/flowcharts/symbrella_architecture.html` is not part of the rendering path.

## Reconciled region ownership

| Region | Registered implementation | Evidence from reviewed source |
| --- | --- | --- |
| Frontal | `Brain.Frontal` | Pure grouping facade; `status/0` enumerates children |
| Prefrontal | `Brain.Prefrontal`, `Core.Brain.Prefrontal` | Pure aggregation of planner outputs and Core pipeline integration |
| FPC | `Brain.FPC` | `plan_branches/1` emits `[:brain, :fpc, :branch]` |
| DLPFC | `Brain.DLPFC`, `Brain.DLPFC.Policy` | Listener for Thalamus and curiosity telemetry; separate pure policy planner |
| DMPFC | `Brain.DmPFC` | `calibrate_confidence/1`, `[:brain, :dmpfc, :calibration]` |
| VMPFC | `Brain.VmPFC` | `compute_utility/1`, `[:brain, :vmpfc, :utility]` |
| OFC | `Brain.OFC` | Curiosity proposal valuation and mood observation, `[:brain, :ofc, :value]` |
| Cerebellum | `Brain.Cerebellum` | Score calibration and learning called from `Brain.LIFG.Stage1` |
| Temporal | `Brain.Temporal` | Context-window process with push/prune/status telemetry |
| ATL | `Brain.ATL`, `Core.Brain.ATL` | Region service and semantic integration stage |

All mapped modules are declarations of software ownership, **not** evidence
that a particular request invoked them.

## Verified source topology

`SymbrellaWeb.Region.Registry.source_relations/0` stores reviewed architecture
links, each with a relationship kind and a precise implementation witness.

- **Grouping:** Frontal → Prefrontal (`Brain.Frontal.status/0`); Prefrontal → OFC
  (`Brain.Prefrontal.status/0` children). These mean conceptual containment.
- **Configured planner references:** Prefrontal → DLPFC Policy, VMPFC, FPC,
  and DMPFC, drawn from `Brain.Prefrontal.@planners`. The aggregator calls
  loaded/exported planners through guarded dynamic dispatch; an individual
  dispatch is not guaranteed for every chat turn.
- **Direct source call:** LIFG → Cerebellum through
  `Brain.LIFG.Stage1.cereb_calibrate/4` and
  `Brain.Cerebellum.calibrate_scores/4`. This is a source-level call site,
  not proof it executed on a specific input.
- **Telemetry subscription:** DLPFC listens to Thalamus decisions via
  `Brain.DLPFC.init/1` and `[:brain, :thalamus, :curiosity, :decision]`.
  A subscription is not proof the event was emitted or received.

The **Conceptual links** layer now incorporates these as navigable adjacency;
the **Source topology** layer renders them distinctly in violet, labels their
kind, and exposes the witness in the selected region's inspector. **BEAM
references** continue to show compiled remote imports. **Live telemetry**,
**Bus events**, and **Turn trace** retain their original, separate evidence
semantics. No synthetic runtime edges were added.

## Verification and limitations

```bash
mix test apps/symbrella_web/test/symbrella_web/components/neural_atlas_test.exs \
  apps/symbrella_web/test/symbrella_web/components/neural_atlas_graph_test.exs
```

The tests check the module inventory, the validity of source-backed endpoints,
the relationship kinds, graph rendering, and that the previously isolated nodes
remain connected to registered regions.

This is a targeted reconciliation of the visually isolated region keys, **not**
an exhaustive inventory of every module in the umbrella. It does not imply
each module is supervised, enabled, called, or capable of performing the
behavior suggested by its brain-inspired label. Runtime participation requires
instrumented execution receipts or other appropriately attributed evidence.
