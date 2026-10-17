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
4. Require a typed `Brain.Hippocampus.SemanticAssociations` cue:
   either two independently stored parent proof IDs or a pointer to a
   newly available *direct* proof. A hint alone is never enough.
5. Call `Core.Cognition.MemoryBoundary.recall/1` with a synthetic,
   **internal-only** typed population-comparison context.
6. For two-hop cues, `PopulationChain` revalidates every relevant proof
   and resolves the issue only after an authorized transitive composition.
   For direct cues, `Consolidation.recall/1` independently validates the
   source-backed direct proof. The investigator then requires the
   authoritative answer plan and uses
   `Db.CognitiveIssues.resolve_direct_reasoning_link/4` to check durable
   fingerprint, pair/year identity, and absence of competing direct proofs.
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

The server is supervised even when disabled. To request one supervised
cycle and inspect the compact result:

```elixir
Core.Curiosity.UnresolvedIssues.reasoning_candidates(20)
Core.Curiosity.Investigator.run_now()
Core.Curiosity.Investigator.snapshot()
```

A successful two-hop investigation reports `%{status: :resolved, issue_id: id,
parent_proof_ids: [id_a, id_b], origin: :hippocampal_two_hop_cue, ...}`.
A directly resolved stale issue reports `%{status: :resolved, issue_id: id,
proof_id: direct_id, origin: :verified_direct_proof, ...}`.
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
