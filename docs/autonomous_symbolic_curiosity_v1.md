# Autonomous symbolic Curiosity, v1

## Scope

`Core.Curiosity.Investigator` is a supervised, **disabled-by-default**
investigation worker. It revisits durable `Db.CognitiveIssue` entries whose
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

By default a cycle is attempted at most once every five minutes.
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

The flags reset on restart unless explicitly configured. Never enable
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
