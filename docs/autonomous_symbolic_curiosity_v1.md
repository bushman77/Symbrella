# Autonomous symbolic Curiosity, v1

## Scope

`Core.Curiosity.Investigator` is a supervised investigation worker.
It now auto-enables in development; test and production remain disabled by default. It revisits durable `Db.CognitiveIssue` entries whose
`kind` is `missing_reasoning_link` and whose `next_action` is
`find_linking_premise`.

It does not investigate contradictory evidence, fetch missing population
figures, call Qwen, generate conversation messages, change LIFG rules, write
new consolidated conclusions, or promote hippocampal recollections to facts.

Unlike the existing question-triggered `MemoryBoundary.recall/1`, this
worker can initiate a bounded internal review without an incoming user
question. Each selected issue provides the *typed question*; the hippocampus
provides opaque proof fingerprints or parent IDs; Core re-reads and
revalidates the relevant stored premises. A stale missing-link issue can be
closed by a newly available direct proof, but it is recorded as a *direct*
resolution, never as a transitive deduction.

## One permitted cycle

1. Confirm the investigator and the existing three symbolic feature flags
   are enabled. Otherwise no issue is inspected.
2. Read at most 20 prioritized durable unresolved issues; choose one
   `missing_reasoning_link` not recently attempted.
3. Reload the selected row by ID, verify its status, source action, year,
   two subject names, and canonical pair key.
4. Inspect the typed `Brain.Hippocampus.SemanticAssociations` cue:
   a two-hop candidate with two independently stored parent IDs, or a
   direct proof pointer. If no cue survives the bounded hippocampal
   window, check the durable direct-comparison store by canonical pair
   key instead. Neither cue nor mere database existence is proof.
5. Call `Core.Cognition.MemoryBoundary.recall/1` with a synthetic,
   **internal-only** typed population-comparison context.
6. For two-hop cues, `PopulationChain` revalidates every relevant proof
   and resolves the issue only after an authorized transitive composition.
   For direct cues, `Consolidation.recall/1` independently validates the
   source-backed direct proof. The investigator then requires the
   authoritative answer plan and uses
   `Db.CognitiveIssues.resolve_direct_reasoning_link/4` to check durable
   fingerprint, pair/year identity, and absence of competing direct proofs.
   Direct DB fallback must satisfy *the same* proof authorization gates.
7. Re-read the issue row. Two-hop resolutions retain both parent IDs;
   direct resolutions record one proof ID and fingerprint with
   `reasoning_rule: "direct_population_comparison"`. Otherwise leave
   uncertainty open.
8. Emit bounded telemetry `[:core, :curiosity, :symbolic_investigation]`
   (status, issue ID and proof identifiers only; no chat, names or source text).
   Retain a bounded runtime snapshot and apply a one-hour retry cooldown
   per unresolved issue.

In development a cycle is attempted every minute; production/test remain
inert unless explicitly configured, with a five-minute fallback interval.
Only one issue can be investigated per cycle and at most 32 recent attempt
timestamps are kept. The worker does not automatically resolve issues
that need new external evidence.

## Development opt-in and inspection

After running migrations, enable the three existing Core switches and
the separate Curiosity switch in the *running* BEAM process:

```elixir
Application.put_env(:core, :symbolic_consolidation_enabled?, true)
Application.put_env(:core, :symbolic_composition_enabled?, true)
Application.put_env(:core, :semantic_associative_recall_enabled?, true)
Application.put_env(:core, Core.Curiosity.Investigator, enabled?: true)
```

The server is supervised even when disabled. A separately supervised named
worker can opt in with `enabled?: true` in its **start options** while the
default worker remains globally disabled. Its scheduled tick and manual
`run_now/1` both honor that process-local choice; the public
`investigate_issue/1` still requires the global switch. All three
symbolic feature flags remain mandatory for every process. This is useful
for controlled unattended trials without enabling the default worker.

To request one supervised cycle and inspect the compact result:

```elixir
Core.Curiosity.UnresolvedIssues.reasoning_candidates(20)
Core.Curiosity.Investigator.run_now()
Core.Curiosity.Investigator.snapshot()
```

A successful two-hop investigation reports `%{status: :resolved, issue_id: id,
parent_proof_ids: [id_a, id_b], origin: :hippocampal_two_hop_cue, ...}`.
A directly resolved stale issue reports `%{status: :resolved, issue_id: id,
proof_id: direct_id, origin: :verified_direct_proof, ...}` when hippocampal
cueing succeeds, or `origin: :verified_direct_db_fallback` when the
proof is recovered only from durable storage.
Unresolved issues report `:unresolved`; unsupported issue types are not
selected. If `run_now/0` returns `:idle`, there are no eligible issues
outside their cooldown.

To disable autonomous investigation without affecting normal chat:

```elixir
Application.put_env(:core, Core.Curiosity.Investigator, enabled?: false)
```

The current `config/config.exs` sets these flags on development startup.
IEx overrides are temporary; configured values return after restart. Never enable
this worker as a way to assert a population result without independent
source-backed premises.

## Regression tests

```bash
mix test apps/core/test/core/curiosity/investigator_test.exs \
  apps/core/test/core/response/population_chain_reasoning_test.exs --trace
```

The tests use the existing database sandbox and fabricated example
subjects/sources to validate **control flow**, not to assert real-world
population facts. The runtime never treats the tests' synthetic figures
as evidence.

## V2 opt-in: source-backed missing-bridge acquisition

The new `Core.Curiosity.EvidenceAcquisition` runs only when the supervised
Investigator is enabled **and** `acquire_evidence?: true` is explicitly
configured. The reasoning-only default remains network-free. This does not
grant access to arbitrary web browsing, agent-selected URLs, Qwen, news,
personal data or unsolicited chat.

For an open A-C missing-link issue, the planner requires exactly one A-B
stored proof ID from the original issue. It revalidates that source-backed
parent through `Consolidation.recall/1`, and selects only B-C for the same
year. Missing, stale, conflicting or ambiguous parent pointers fail closed.

A single scheduled acquisition calls the existing fixed-provider geographic
population route through `WebRetrieval.enrich/2` for B-C, with two attempts
per municipality and a 3.5-second request timeout. `PopulationComparison`
must re-derive a source-supported B-C comparison. The B premise must match
the A-B parent in subject, value, year, URL, source type, measurement type
and table. `ResponseFrame`, `AuthoritativeAnswerPlan` and
`Consolidation.capture/2` apply their existing authorization and immutable
storage rules; failure does not store a new proof.

Capture alone **never** closes A-C. The Investigator retries only the
existing hippocampal cue + Core two-hop proof-validation path, once, with
acquisition disabled for that retry. Only that path can resolve the durable
issue. Cooldowns, one-issue-per-cycle scheduling and bounded telemetry remain.

### Controlled unattended trial

Leave the normal worker disabled. Start a second named supervised worker:

```elixir
Application.put_env(:core, Core.Curiosity.Investigator,
  enabled?: false, acquire_evidence?: false)

trial_name = :curiosity_acquisition_trial
spec = Supervisor.child_spec(
  {Core.Curiosity.Investigator,
   [name: trial_name, enabled?: true, acquire_evidence?: true,
    interval_ms: 60_000, retry_ms: 3_600_000]},
  id: trial_name)
Supervisor.start_child(Symbrella.Supervisor, spec)
```

The three existing symbolic gates must also be enabled. For an unattended
test, do not call `run_now/1` or `investigate_issue/1`. After a scheduled
cycle, inspect `Core.Curiosity.Investigator.snapshot(trial_name)` and the
durable `Db.CognitiveIssue`. A captured B-C proof is reported separately
from the final A-C reasoning resolution. To stop the trial:

```elixir
Supervisor.terminate_child(Symbrella.Supervisor, trial_name)
Supervisor.delete_child(Symbrella.Supervisor, trial_name)
```

Offline regression suite:

```bash
mix test apps/core/test/core/curiosity/evidence_acquisition_test.exs \
  apps/core/test/core/curiosity/investigator_test.exs \
  apps/core/test/core/response/population_chain_reasoning_test.exs --trace
```

Offline fixtures contain synthetic source-backed observations solely to
exercise authorization. Runtime facts must come from actual providers.

## V3: durable Curiosity investigation history

Each real issue attempt now creates an operational audit record in
`curiosity_investigations` **before** reasoning or source acquisition, then
completes the same row with its result. It is separate from knowledge
(`consolidated_conclusions`) and epistemic uncertainty (`cognitive_issues`).
An abrupt process crash can leave `status: "started"` and `completed_at: nil`,
making an incomplete attempt visible instead of claiming success. Database
audit unavailability does not grant facts or interrupt Core's reasoning.

The audit stores only issue ID, default/isolated worker class, status and
reason codes, acquisition outcome, accepted proof ID, two-hop proof IDs,
reasoning origin, duration and UTC times. Provider attempts are a capped
list of fixed provider identifiers and status codes. **Raw user messages,
place names, web excerpts, arbitrary provider URLs and rejected source
content are never copied into the audit.** Accepted source URLs are
available in the referenced source-backed proof rows.

Idle and disabled cycles still emit lightweight telemetry but **do not**
create a durable database row, avoiding an unbounded idle-history flood.
Telemetry measurements include `count` and `duration_ms`; metadata carries
issue ID, audit `history_id`, status/reason, acquisition result, and proof IDs.
The worker snapshot now includes `last_successful_result`, which survives
later idle ticks in the running process. The PostgreSQL audit survives restart.
Events from before this migration are **not backfilled or invented**.

### Schema migration

From the umbrella root, migrate the existing database without resetting it:

```bash
cd ~/Symbrella
mix db.migrate
```

`mix test` runs the test database migrations automatically through the
umbrella test alias. Do not use `db.reset` or `db.rebuild` here.

### Read-only IEx inspection

```elixir
Core.Curiosity.InvestigationHistory.recent(20)
Core.Curiosity.InvestigationHistory.for_issue(4, 20)
Core.Curiosity.InvestigationHistory.explain(4)
Core.Curiosity.Investigator.snapshot(:curiosity_acquisition_trial)
```

`explain/1` shows the current issue status and question subjects from
the **existing issue row**, all recorded attempts, and verified accepted
source URL provenance drawn only from persisted conclusion rows. Rejected
provider attempts include only provider names and generic outcome codes.
If an issue predates this history feature, its attempt list is empty.

### Regression tests

```bash
mix test apps/core/test/core/curiosity/investigation_history_test.exs \
  apps/core/test/core/curiosity/evidence_acquisition_test.exs \
  apps/core/test/core/curiosity/investigator_test.exs \
  apps/core/test/core/response/population_chain_reasoning_test.exs --trace
```


## V4: automatically enabled in development

`config/config.exs` sets the three symbolic feature gates and the
`Core.Curiosity.Investigator` switches using `config_env() == :dev`.
Consequently `iex -S mix phx.server` automatically starts the *existing*
supervised worker with `enabled?: true` and `acquire_evidence?: true`
in development. No additional `Supervisor.start_child/2` or
`Application.put_env/3` commands are necessary.

The development schedule starts its first investigation tick after 60
seconds, revisits at most one eligible issue each minute, and defers
retrying an unresolved issue for an hour. Idle cycles do not write durable
audit rows. The optional acquisition path uses only fixed public-place
population sources and remains gated by the full symbolic proof checks.

The flags remain **off in test and production** by default. Tests may
explicitly opt in as before; deployment requires a conscious production
configuration override. Changing the flags in IEx only affects the current
BEAM process and can override the configured values until restart.

Before first start with the durable history feature, migrate the existing
development database:

```bash
cd ~/Symbrella
mix db.migrate
iex -S mix phx.server
```

Then verify the automatically started default worker from IEx:

```elixir
Core.Curiosity.Investigator.snapshot()
Application.get_env(:core, Core.Curiosity.Investigator)
Core.Curiosity.InvestigationHistory.recent(10)
```

A successful snapshot contains `enabled?: true` and
`acquisition_enabled?: true` with `cycles: 0` until the first scheduled
tick. The earlier standalone trial worker is not automatically started
and should not be left active alongside the configured default worker.

## V5: evidence-grounded follow-up explanations

A verified population answer now records a **short-lived session-scoped
receipt** of the answer's identity, claim subjects/year, and persisted
direct or two-hop proof identifiers. It is a cue, **not new factual
authority**. The table belongs to the long-lived LlmChatHistoryOwner
GenServer and is capped at 256 sessions and a 10-minute time-to-live.
No migrations are required for this addition.

When the next chat turn asks `how do you know that?`, `what evidence
supports that?`, `what are your sources?`, or `where did you get those
figures?`, Core first checks that the receipt matches the latest actual
assistant reply in the **same** session. Core then independently reruns
`MemoryBoundary.recall/1` on the original typed population question;
the re-derived claims, year, source URLs and exact persisted parent proof
IDs must match. Any loss of verification fails closed with an honest
unavailability statement. Verified follow-up prose comes from Core and
cannot be rewritten by Qwen.

Direct comparisons cite the exact source URLs recorded in their accepted
proof. Two-hop comparisons explain each verified parent, the shared
intermediate population, and why the transitive deduction is justified.
These proof details are supplied **only** in response to an explicit
evidence follow-up; unrelated conversations do not receive proof traces.
A memory-only LiveView reply invalidates any older receipt, preventing
the follow-up from being misattributed to an earlier population answer.

The receipt is intentionally non-durable: after restarting Phoenix, ask
the comparison question again to create a new verified receipt. Existing
durable `consolidated_conclusions` still survive the restart.

### Regression checks

```bash
cd ~/Symbrella
mix test apps/core/test/core/response/population_chain_reasoning_test.exs \
  apps/core/test/core/response/llm_chat_history_owner_test.exs \
  apps/core/test/core/semantic/event_frames_test.exs --trace
```

In the UI, ask the original population question, followed immediately
by `how do you know that?`. Explain metadata should report
`response_source: :symbolic_verified_followup` for the second answer,
never `:llm`.


## V6: verified symbolic semantics in Brain working memory

Successful verified population answers and their immediate grounded
evidence-follow-ups now trigger **independently checked WM admission**.
\`Core.Brain.WM.VerifiedSemantics.admit/5\` re-runs
\`Core.Cognition.MemoryBoundary.recall/1\` against the durable
\`consolidated_conclusions\` records. Admission is refused if the
reference year, subject names, proof IDs, chain rule, or Core-authored
authoritative answer does not match the persisted source-backed proofs.

This is **not** a pathway for a conversation, LLM output, or a WM item
to become a verified fact. It stores only a typed, ephemeral semantic
*reference* to facts that Core must revalidate on every later use.
The separately admitted \`:comprehension_frame\` remains an honest report
of lexical and syntactic certainty; a successful verified response does
not overwrite its \`degraded?: true\` flag.

The new Brain WM item has:
- \`source: :verified_semantics\`
- \`kind: :verified_semantic_frame\`
- \`context.conversation_act: :verified_answer | :evidence_followup\`
- \`context.relation: :population_comparison\`
- \`context.subjects\`, \`entities\` (including the shared intermediary
  for a two-hop proof), \`reference_year\`
- \`context.parent_proof_ids\` and \`reasoning_rule\`
- \`context.verification: :source_supported\`,
  \`interpretation_status: :verified\`, and
  **\`factual_authority?: false\`**

No user text, model reply, inferred population counts or source-page
bodies enter the WM payload. The Brain gate is used exactly as for other
WM candidates with per-call admission settings; the global LIFG and
memory thresholds are unchanged. An isolated telemetry event
\`[:core, :wm, :verified_semantics]\` reports admission or rejection
without personal text.

The Brain process has a global WM, so \`Core.Response.RuntimeContext\`
strictly excludes both comprehension and verified-semantic session
items from the global LLM prompt snapshot. Each item has a hashed
session ID and an approximately 20-minute expiry for context inspection;
this is working memory, not long-term factual storage.

The feature starts automatically in development, not in production
or test. No migration or extra supervisor child is needed. In IEx,
after asking the population comparison and the evidence follow-up:

\`\`\`elixir
Application.get_env(:core, Core.Brain.WM.VerifiedSemantics)
# [enabled?: true]

Core.Brain.WM.VerifiedSemantics.snapshot("THE_CHAT_SESSION_ID")
# A bounded typed context, never a source of factual authorization.

Brain.snapshot_wm().wm
|> Enum.filter(&(&1.source == :verified_semantics))
# Only the new semantic references, one per active session.
\`\`\`

The actual LiveView session identifier may be obtained from a known
response trace; do not substitute an unrelated IEx session ID.

Targeted regression:

\`\`\`bash
mix test apps/core/test/core/response/population_chain_reasoning_test.exs \
  apps/core/test/core/brain/comprehension_wm_test.exs \
  apps/core/test/core/semantic/event_frames_test.exs --trace
\`\`\`

This work intentionally does **not** rewrite LIFG's uncertain sense
selection. The report's degraded comprehension frame may coexist with
the independently verified semantic object. Future work can connect
those references into general multi-turn discourse without promoting
them into authority.
