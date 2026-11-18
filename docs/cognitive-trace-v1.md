# Cognitive Trace v1 — symbolic chat execution observability

## Entry point and provenance

Production chat submits a turn in
`SymbrellaWeb.HomeLive.handle_event("send", ...)`. That already creates a
random `pending_user_turn.id`. The supervised worker now passes that same
ID to `Core.CognitiveTrace.with_trace/2`.

All instrumentation is **read-only** and opt-in to the traced worker. No
algorithmic stage is reordered, no lexical or semantic winners are changed,
and the existing response and archive behavior remain authoritative.

The worker emits one `[:symbrella, :cognitive_trace, :stage]` telemetry
receipt at the end of each instrumented call, plus turn start/end receipts.
Each completed receipt records a monotonic-clock duration in microseconds
and a wall-clock observation timestamp. It is *not* a neural firing event.

## Existing execution boundaries

- `chat_route`: executed chat response path (including possible memory fast path)
- Phase 1: input and token preparation
- Phase 2: context, discourse and working-memory recall
- Phase 3: lexical candidate preparation
- Phase 4: grammar and competition, with separately timed LIFG and ATL calls
- Phase 5: semantic composition
- Phase 6: comprehension summary and speech-act binding, each separately timed
- Phase 7: working memory and response planning
- Phase 8: experience and memory, including qualified Hippocampus persistence

Phase spans include nested operations; **summing phase and nested durations
double-counts time**. Stage status `:ok` means the operation returned without
an exception, **not** that Symbrella correctly understood the utterance.

The `comprehension_summary` receipt reports the actual
`Core.Comprehension.Summary` evidence: its `degraded?` flag and counts of
`understood`, `uncertain`, and `unresolved`. The speech-act receipt
reports only a whitelisted act or `not_detected`. The LIFG span reports
the count of choices; ATL reports whether a slate was attached. No selected
tokens, definitions, user text or untrusted values are retained.

On the legacy memory-fast-path route, the eight Core phases may not run.
That is a **real observed bypass**, not missing trace data to fill in.

## Storage and UI

`SymbrellaWeb.CognitiveTraceStore` is a supervised GenServer with a
Telemetry handler. It sanitizes and stores at most 20 recent turn IDs,
at most 40 receipts each, for at most ten minutes. Records disappear on
application restart and are never written to a database or chat history.
The process broadcasts only sanitized trace snapshots to
`brain:cognitive_trace` via `Brain.Bus`.

`SymbrellaWeb.BrainLive` subscribes on connection and supplies trace
snapshots to `NeuralAtlas.panel`. Select **Turn trace** in the existing
`/brain` page to see the latest turn or choose one of the recent turn IDs.
The detailed timeline shows observed ordered completion receipts, elapsed
time, status, and bounded evidence counters.

The network graph highlights **only** regions with specifically
instrumented, completed module calls: LIFG selection, ATL integration,
and Hippocampus persistence. Faint conceptual edges are still navigation
context; they do not claim that a signal traversed those paths.
More precise causal edges will require explicit parent/child span IDs and
instrumented message send/receive boundaries.

This is **not** a distributed tracer. Internally spawned, uninstrumented
processes are not automatically included. The dashboard never attributes
a Telemetry or Bus receipt to a chat turn without a matching turn-scoped span.

## Verify

From the umbrella root:

```sh
mix test apps/core/test/core/cognitive_trace_test.exs
mix test apps/symbrella_web/test/symbrella_web/cognitive_trace_store_test.exs \
  apps/symbrella_web/test/symbrella_web/components/cognitive_trace_panel_test.exs \
  apps/symbrella_web/test/symbrella_web/components/neural_atlas_test.exs \
  apps/symbrella_web/test/symbrella_web/components/neural_atlas_graph_test.exs
```

Start Phoenix, keep `/brain` open on **Turn trace**, and send a
chat utterance on `/`. Inspect the trace. A degraded summary and an
unresolved count indicate limited comprehension evidence, even when all
stage calls completed successfully.

For investigations, check the `speech_act_binding` result and
`comprehension_summary` counts before changing any heuristics. For example,
a compound greeting plus wellbeing question may legitimately not match
the current narrow single-act grammar; the trace should report
`not_detected` rather than invent a complete social interpretation.
