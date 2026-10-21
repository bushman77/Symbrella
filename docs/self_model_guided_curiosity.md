# SelfModel-guided Curiosity and learning

## Purpose and boundaries

This feature learns bounded *investigative strategy preferences* from durable
investigation history, not factual knowledge. The independent symbolic proof
checks in Core remain the only authority for resolving cognitive issues.

This release deliberately limits autonomous investigation to the existing
missing_reasoning_link / find_linking_premise issue type. SelfModel anomalies,
low-confidence hippocampal episodes and disputed attribution events are not
automatically transformed into investigable facts. Other gap sources require
separate typed authorizations.

## Versioned success-to-SelfModel contract

Brain.CuriosityLearning.snapshot/0 derives its v1 projection from at most
100 recent completed Db.CuriosityInvestigation records and the corresponding
Db.CognitiveIssue rows. No new table or migration is necessary.

A durable audit receipt by itself cannot earn positive credit. It must match a
resolved CognitiveIssue whose direct or transitive reasoning rule and accepted
proof lineage agree with the audit outcome.

- verified_existing_direct: an accepted stored direct proof and positive proof ID
- verified_two_hop: independently authorized transitive result with two matching parent proof IDs
- verified_acquired_direct / verified_acquired_two_hop: an acquisition proof ID that is actually part of the final authorized resolution
- unresolved_conflict: a small negative bias for incompatible or unsafe evidence
- failed: a smaller negative bias for a failed attempt
- unresolved: neutral when supporting evidence simply does not exist
- ignored: incomplete, unsupported, duplicate or uncorroborated success claims

Only the most recent completed attempt per issue is considered, preventing
repeat rewards. A verified outcome credits the successful specific strategy
more than the general missing_link strategy; otherwise identical bias
increments cancel when ranking different candidates. Protective conflict
penalties are similarly more specific than broad. Strategy biases are limited
to [-0.12, 0.12] for missing_link, direct, two_hop and acquisition. A bounded
recent-history window lets older influence age out without inventing
synthetic events.

The v1 learning projection is embedded in Brain.SelfModel as the separately
versioned investigative_biases field, and is included in ordinary model
export/import and SelfContinuity persistence and warm-start. Older v1 SelfModel
snapshots without this optional field get neutral preferences. At runtime,
Brain.Introspection.snapshot/0 reconstructs the view from durable history.
A database failure yields neutral learning, not a fabricated result.

An autonomous outcome never directly increments global factual confidence:
success at one population investigation says nothing about unrelated claims.
It affects only proposal scoring and prioritization.

## Advisory scoring and execution boundaries

Brain.Curiosity no longer uses the legacy fixed 0.55 automatic score. It
combines uncertainty, novelty, exploration and dopamine with bounded
Brain.CuriosityPolicy adjustment. Explicit numeric caller scores remain
overrides. An optional kind can be missing_link, direct, two_hop or
acquisition; unknown kinds fall back to missing_link.

High vigilance mildly favours direct checks over speculative two-hop work.
Plasticity adds a small exploratory preference. High cognitive load and
inhibition lower priority and defer scheduled Brain proposals at 0.90.
Deferral is not a fact, an issue resolution, or permission to call tools.

The Core investigator reads the bounded durable missing-link queue.
It scores four *action hints*, each grounded in its available prerequisites:
- direct: an already stored, source-supported comparison exists for the exact
  subject key and reference year (a single bulk query, not an authority check)
- two_hop: the issue names exactly one positive original parent proof ID
- acquisition: neither of the above and the independently configured evidence
  acquisition gate is enabled
- missing_link: no stronger hint is supported, with acquisition disabled

Hints change **only selection order**. They do not force an investigation
outcome, guarantee a proof is compatible, or enable a disabled tool. Core
still validates the same source, contradiction, composition and issue gates.
Core.Curiosity.SelectionPolicy compares legacy queue selection against the
SelfModel-aware choice. Only a previously validated eligible issue can be
selected. In shadow mode the legacy choice is executed and the suggested
choice is reported separately. A policy_selection receipt in the live worker
last_result records the actual selected hint, score, learned bias, legacy
and hypothetical issue IDs (no subject names or raw evidence). The telemetry
event now distinguishes real selection from the shadow recommendation.
An end-to-end pure regression test projects a corroborated receipt into the
SelfModel preference, then proves the resulting bias changes the next active
selection while shadow mode preserves the legacy choice.

On development startup the Core self_model_policy? flag is enabled. Test and
production default to shadow/inert. Change it to false to use shadow mode in
development without disabling ordinary investigation. Active policy deferrals
leave the issue's cooldown intact, create no audit event and never bypass
acquisition, source authorization or the Core evidence checks.

The Core worker emits a sanitized policy_comparison telemetry event with
issue IDs, mode, score and a comparison flag. Once an actual investigation
has a committed audit completion, learning_outcome telemetry reports
the corroborated outcome and projection version. No chat, names or snippets
enter these telemetry messages. Audit retention remains independent of
knowledge authority.

## Inspect the system from IEx

    Brain.Introspection.snapshot().investigative_biases
    Brain.CuriosityLearning.snapshot()
    Brain.CuriosityPolicy.assess(:two_hop, Brain.Introspection.snapshot())
    Core.Curiosity.UnresolvedIssues.reasoning_candidates(10)
    Core.Curiosity.Investigator.snapshot()
    Core.Curiosity.InvestigationHistory.recent(10)

If the original Curiosity audit migration has not run, run mix db.migrate
before starting the development server. This change adds no migration.

To turn the Core decision policy back to shadow mode in a running process:

    cfg = Application.get_env(:core, Core.Curiosity.Investigator, [])
    Application.put_env(:core, Core.Curiosity.Investigator,
      Keyword.put(cfg, :self_model_policy?, false))

Existing workers read this setting each cycle. An isolated worker can also
supply self_model_policy?: false in its start options.

## Tests

    mix test apps/brain/test/brain/curiosity_learning_test.exs \
      apps/brain/test/brain/curiosity_policy_test.exs \
      apps/brain/test/brain/curiosity_self_state_test.exs \
      apps/core/test/core/curiosity/selection_policy_test.exs \
      apps/core/test/core/curiosity/investigation_history_test.exs \
      apps/core/test/core/curiosity/investigator_test.exs
    mix test

No mix format invocation is required. This is inspectable symbolic adaptation,
not Axon training, and makes no claim about subjective experience.
