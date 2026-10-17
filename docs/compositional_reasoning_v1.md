# Compositional symbolic reasoning — two supported premises (experimental)

## Milestone

Symbrella can now reuse TWO previously consolidated comparisons when a direct
comparison is missing. If source-supported A > B and B > C were established
for the same reference year and comparable measurement, Core can infer A > C
without Qwen doing the arithmetic or supplying the factual result.

The same system can infer C > A when the two stored comparisons establish
A < B < C. It refuses to conclude anything from a nonmonotonic chain.

This is a **first compositional rule**, not general theorem proving.

## Evidence conditions and limitations

The rule reads `Db.ConsolidatedConclusion` rows, not chat history or
model-authored answers. Each parent proof must have valid:
- `source_supported_symbolic_derivation` provenance and a consistent
  original fingerprint;
- exactly two source-backed claims, with valid entities, counts, dates and URLs;
- independently re-derived winner, difference, measurement kind and year;
- matching intermediate subject B **and the exact same source claim for B**;
- one shared year and compatible measurement/source class for all three places;
- strict numeric monotonicity: A > B > C, or A < B < C.

Source URLs are retained, not re-fetched on every local inference. Source
support is **not** a claim that the source is still current, independent, or
correct. Current/latest questions bypass historical composition and use the
normal retrieval path. Fresh external revalidation is a separate project.

If multiple candidate chains disagree, the result is *not* selected on a
first/last basis. A `contradictory_premises` issue is created instead.

If only one side of a chain is present, years/sources differ, or strict
transitivity is not established, Symbrella records a `missing_reasoning_link`
issue rather than pretending an inference succeeded.

Each successful proof includes an inspectable `composition` map:
`rule`, `intermediate_subject`, `parent_proof_ids`,
`reference_year`, `origin`. The response reflection mirrors that map in
`symbolic_composition` and the system emits the event
`[:core, :reasoning, :composition]` without raw chat content.

## Curiosity and unresolved issues

The separate OTP Curiosity loop is **not modified to run unattended web
research**. Open compositional issues are available through
`Core.Curiosity.UnresolvedIssues.candidates/1`. The priority queue can later
feed a supervised investigation tool.

After a missing premise is added and a two-parent chain successfully passes
Core's authoritative answer gate, the corresponding
`missing_reasoning_link` issue is marked `resolved` with its two parent
proof IDs. Conflicting claims never self-resolve merely because a new answer
is available.

## Enabling after tests

The new feature is independent of and requires the existing consolidation
path. Both are disabled by default:

```elixir
config :core, :symbolic_consolidation_enabled?, false
config :core, :symbolic_composition_enabled?, false
```

No schema migration is added in this milestone. The earlier
`consolidated_conclusions` and `cognitive_issues` tables must already exist.

```bash
cd ~/Symbrella
git pull --ff-only origin development

mix test \
  apps/core/test/core/response/population_chain_reasoning_test.exs \
  apps/core/test/core/response/symbolic_consolidation_test.exs \
  apps/core/test/core/response/population_comparison_test.exs \
  --trace
```

Only after these tests pass, enable in your RUNNING `iex -S mix phx.server`
session for a controlled experiment:

```elixir
Application.put_env(:core, :symbolic_consolidation_enabled?, true)
Application.put_env(:core, :symbolic_composition_enabled?, true)
```

Revert with both flags set to `false`. This leaves stored historical proofs
and issues intact for audit.

Test fixtures use invented places and synthetic `example.invalid` URLs:
they test deduction plumbing, safeguards and the existing authorization
contract, **not** whether those place facts are true.

## Next work

- Extend from a bounded two-hop population relation to explicit, typed
  temporal and spatial relations with independent evidence gates.
- Use Core's actual sentence signatures/event frames to parse those claims;
  lexical co-occurrence alone must never create relation facts.
- Provide a budgeted curiosity investigation worker behind an explicit
  command/permission boundary (one issue per cycle, with trace and retry limits).
- Persist separately typed derivation DAG edges while preserving the parent
  source record identities and correction/revalidation semantics.
