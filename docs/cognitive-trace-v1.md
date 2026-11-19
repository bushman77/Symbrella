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
- Phase 7: working memory, explicitly timed Prefrontal planner aggregation,
  ActionSelector execution (including read-only shadow comparison), and a
  separately timed post-response-planning decision-lineage receipt
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

The network graph highlights **only** regions with explicitly recorded
activity: LIFG selection, ATL integration, Hippocampus persistence,
Prefrontal aggregation, and actual planner-call reports returned by that
aggregation. A planner with status `:unavailable` is **not** counted as an
executed planner. Faint conceptual edges remain navigation context; they
do not claim that signals traveled along those links.

### P-002: Executive observability

`Core.CognitiveTrace.step/3` now times the real
`Core.Brain.Prefrontal.attach/2` and
`Core.Brain.ActionSelection.attach/2` invocations nested in Phase 7.
The results feed the **same chat turn ID** as the enclosing worker.
Only known planner region keys, statuses, merged control values (bounded
numbers/enumerated policies), shadow action names, candidate score comparisons,
and per-planner control-key provenance survive telemetry sanitization.
Planner exception details, user text, raw semantic candidates, memory contents,
and arbitrary action payloads are never stored by the trace store.

Select **Turn trace** on `/brain` to inspect:

- A Prefrontal-region summary of the reported planner calls
- Individual DLPFC, FPC, DMPFC, VMPFC and Salience planner statuses
- Which planner keys were computed and which survived the canonical merge
- Which keys affected only the shadow-scoring comparison
- Live action versus shadow recommendation and the bounded candidate scores

The turn-scoped report distinguishes `produced`, `no_opinion`,
`unavailable`, `failed`, and `rejected`. A merged value does not prove
it influenced action selection, and shadow-scoring use does not imply a change
to the authoritative live decision. Disabled and memory-fast-path turns
may legitimately contain no executive receipt.

The DMPFC and VMPFC calculations still use heuristic outcome estimates in
`Core.Brain.Prefrontal.context_from_si/2`; they are marked
`:heuristic_proxy`, not verified learning success. No planner behavior,
working-memory admission, permissions, lexical winners, or action selection
is changed by P-002. The instrumentation cannot infer distributed causal
routes or prove that a recommendation improved comprehension.
More precise causal edges will require explicit parent/child span IDs and
instrumented message send/receive boundaries.

### P-006: Initial-to-final decision lineage

`Core.Brain.ActionSelection.attach/2` now writes an optional
`SemanticInput.decision_lineage` map, retaining the initial selected action,
its safety gate, a bounded initial shadow recommendation, an initial agency
decision ID (internal only), and command-receipt statuses. The existing
`Core.Response.Attach` reconciliation path can replace action metadata on
recognized social turns. It now preserves the earlier summary while marking
the final selected action and whether the response planner reconciled it
(`:reconciled`), retained it (`:unchanged`), or was not completed
(`:not_planned`). The SemanticInput optional-field contract is v13.

`Core.CognitiveTrace` emits a `:response_planning` receipt from the
existing Phase 7 route under the **same turn ID**. Both the Core emitter and
`CognitiveTraceStore` independently allowlist action atoms, reconciliation
reason/status, shadow-control keys, safety-gate enums, and up to 12 command
status atoms. They never retain user text, reply text, candidate payloads,
internal decision IDs or command bodies. Missing/legacy turns remain
explicitly unobserved.

The executive panel now shows **Initial live selection → Final planned action**,
with the reconciliation reason and any initial/final command receipts.
A command receipt of `:executed` is a reported execution, **not proof of
the intended outcome**. A planned response is never presented as delivered;
delivery remains `:not_observed` without independent evidence.

P-006 changes **observability only**. It does not alter the selector's
live action scores, safety/permission enforcement, command dispatch,
response text, or memory admission. It also does **not** make a shadow
recommendation authoritative.

### P-007: Comprehension diagnosis and repair readiness (observe-only)

The Phase 6 `:comprehension_summary` receipt now includes an independently
allowlisted diagnosis generated from the already-computed SemanticInput:

- Actual degradation **reason codes** (`:no_lifg_choices`,
  `:weak_decision_rate_high`, `:fallback_rate_high`,
  `:unresolved_content_tokens`) and bounded LIFG choice/content counts
- Weak and fallback selection rates (0..1), without raw candidate details
- Number of **independently verified lexical constructions**, distinct from
  raw LIFG winners, structural interpretation and whole-message understanding
- Observed structural-competition, sentence-meaning and integration-audit
  status; unknown/unrecognized states explicitly remain `:not_observed`
- Existing ATL-to-LIFG feedback mode and whether that earlier stage
  **reported** applying a bounded revision
- A non-executing review hint and the prerequisites of
  `Core.Curiosity.ComprehensionGaps.from_completed_turn/2`

The current curiosity-gap projection conservatively requires degraded
comprehension, **both** uncertain and unresolved content, session context
and user-origin evidence, and a nonempty sentence. The downstream caller
still has to verify that the session identity matches. Thus an observed turn with
one uncertain term and zero unresolved terms reports
`:needs_unresolved_content`: this describes an existing review gate, not a
new question or a reason to promote any lexical interpretation.

Select **Neural Atlas → Turn trace** to see the new
**Comprehension diagnosis · selected turn** panel below the timed stages.
It shows reason labels, lexical selection rates, verified lexical count,
structural/integration statuses, existing feedback and diagnostic review hints.
Legacy traces without full diagnosis metadata fail closed to an explicit
no-detail state. The panel is bound to the selected turn, not the latest
turn in the background.

Both Core's producer and the Phoenix `CognitiveTraceStore` sanitize the
metadata. This observer never logs the utterance, token text, lexical IDs,
dictionary definitions, memory or response content; telemetry cannot turn
unknown statuses into recognized causes.

P-007 **does not retry, rerank, ask follow-up questions, modify ACC/FPC,
change memory, or authorize new lexical revisions**. Existing ATL/LIFG
feedback behavior is left untouched. A degraded summary reports evidence
quality, not a failed execution span or proof that no structural interpretation
exists.

### P-008: Mixed social check-in interpretation (provisional)

`Core.Semantic.SpeechActs` now recognizes a tightly bounded full-utterance
social-update question such as `What's new with you?` and the two-part
`hey whats new with you?` / `Hey, what's new with you?`.
The parser records `:social_update_query` and a typed, ordered component
sequence `[:greeting, :social_update_query]` for the compound case, or
`[:social_update_query]` for a standalone question. The question mark and
complete phrase are required. Quoted, reported, elaborated, non-question,
instruction-bearing, and other-directed examples do not authorize a
personal response.

This is a **surface-only, provisional speech-act interpretation**. It does
not change the original LIFG winners, ATP/ATL evidence, sentence signatures,
structural competition, sentence meaning or degraded-comprehension flags.
A mixed social turn may therefore still show
`no_complete_candidate`, `not_yet_interpreted` and weak lexical evidence,
while also having a verified *surface grammar match* for its conversational
function. The two observations answer different questions.

`Core.Response.SemanticStrategy` and `Core.Response.SymbolicGeneration`
reuse the existing exact same-turn recomputation guard to realize a
low-risk, limited response for a matched check-in, without fabricating
offline personal experiences or claiming to have verified a dictionary
sense. Existing response risk, action, addressee and permissions gates
remain in force; neither a high-risk context nor a foreign addressee
can authorize the social reply.

The Phase 6 `:speech_act_binding` trace records the allowlisted act and
ordered component atoms only. `CognitiveTraceStore` independently rejects
unrecognized component combinations. It does not store the user's input,
reply, lexical choices or quoted text. The Turn trace timeline renders
`social components: greeting + social_update_query` when it is actually
observed for the selected turn.

No existing substantive grammar is generalized; unsupported requests
continue to abstain rather than gaining fabricated semantic proof.

This is **not** a distributed tracer. Internally spawned, uninstrumented
processes are not automatically included. The dashboard never attributes
a Telemetry or Bus receipt to a chat turn without a matching turn-scoped span.

## Verify

From the umbrella root:

```sh
mix test apps/core/test/core/cognitive_trace_test.exs \
  apps/core/test/core/semantic/speech_acts_test.exs \
  apps/core/test/core/response/symbolic_generation_test.exs \
  apps/core/test/core/comprehension/diagnosis_test.exs \
  apps/core/test/core/comprehension/summary_test.exs \
  apps/core/test/core/brain/action_selection_test.exs \
  apps/core/test/core/response/attach_test.exs \
  apps/core/test/core/semantic_input_sense_candidates_test.exs
mix test apps/symbrella_web/test/symbrella_web/cognitive_trace_store_test.exs \
  apps/symbrella_web/test/symbrella_web/components/cognitive_trace_panel_test.exs \
  apps/symbrella_web/test/symbrella_web/components/executive_trace_panel_test.exs \
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
