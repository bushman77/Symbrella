# Reflective Agency — live lexical accountability (Phase 9B)

## Purpose

Symbrella's real production Core pipeline now **always** records a bounded,
read-only lexical-role accountability event. It is not an optional debugging
feature, and per-turn reflective_role_shadow? flags cannot turn it off.

This creates an inspectable record of what the current LIFG winner was
grammatically required to do, what Core could independently verify, whether
a scored dictionary alternative existed, and what remains unresolved.
It does NOT grant the audit a vote in cognition.

## Live pipeline placement

Phase 5 (after LIFG, structural competition, and typed composition):

    |> Core.Semantic.Composition.attach()
    |> Core.Agency.LexicalRoleShadow.attach(opts)

Always emits a :reflective_role_shadow event in SemanticInput.trace, including
explicit abstentions. The record includes bounded role conflict reasons and
expected/selected POS; candidate POS, score and selection basis; a SHA-256
fingerprint of the original bounded winner slate; structure/proof verification
flags; and explicit no-learning/no-factual-authority flags.

No sentence text, dictionary definitions, full candidate distribution, or
raw sense IDs appear in the accountability trace. Fingerprints are diagnostic
comparators, not signatures or cryptographic authenticity guarantees.

Phase 8 (after optional downstream synaptic feedback and discourse processing):

    |> Core.Agency.LexicalRoleAccountability.record(opts)

A production pass with a nonempty bounded session ID and the existing agency
ledger enabled writes a second, compact event to the **existing** agency_events
table. The receipt source is lexical_role_accountability, the action is
audit_lexical_role, and the source report is indexed under the session.

The saved payload includes the original and final LIFG winner fingerprints.
A mismatch means the chosen IDs changed after the audit; this is a detected
change, NOT proof that the change was safe, correct, or caused by reflection.
It includes the original role blockers and the evidence-gated proposals.

Storage errors are fail-open for conversation processing but **fail-closed for
accountability assertions**: the returned trace says :unavailable, not
:persisted. Passes without session scope or with the ledger disabled report
:skipped. No persistence success is claimed unless the database write returns
an actual saved row.

### Inspect production receipts

Given a session ID, from IEx:

    Core.Agency.LexicalRoleAccountability.recent("session-id", limit: 10)

For one pipeline result:

    Enum.find(si.trace, &match?(%{stage: :reflective_role_shadow}, &1))
    Enum.find(si.trace, &match?(%{stage: :reflective_accountability}, &1))

Trace entries are returned in newest-first order. A :persisted event includes
the database ledger ID. An :unavailable event must be handled as an audit
storage problem, not treated as verified cognition.

### Behavioral isolation

Existing Core.Response.AgencyMemory.Source.recent/2 now excludes events from
lexical_role_accountability **inside the database query, before its limit is
applied**. This prevents observational audit rows from diluting the finite
memory window or masquerading as repeated failures/successes in policy pressure.

The audit never mutates LIFG choices, synapses, BrainCell activation, working
memory admission, user responses, goals, or learning weights.

## What is verified versus provisional

- Core.Syntax.StructuralCompetition.verified/1 checks current structural
  competition against independently detected sentence signatures.
- Core.Semantic.Composition.Diagnosis.blocked_roles/1 identifies mismatches
  in the actual selected LIFG role decisions.
- Core.Semantic.Composition.RoleWitness.resolve/4 requires a scored and
  attested grammatical alternative on the current turn, including morphology.
- Core.Semantic.Composition.verified/1 checks any available formal derivation.

These gates can establish **grammatical role support and formal derivability**.
They cannot establish the true lexical sense, the speaker's meaning, real-world
facts, semantic correctness, or conscious experience. Unsupported cases
abstain, and the ledger must retain that uncertainty.

## Regression tests

    mix test \
      apps/core/test/core/agency/lexical_role_shadow_test.exs \
      apps/core/test/core/agency/lexical_role_accountability_test.exs \
      apps/core/test/core/pipeline_contract_test.exs \
      apps/core/test/core/semantic/composition_event_test.exs \
      --trace

Database-dependent tests require the standard Core/Db test setup and its
PostgreSQL sandbox.

## Next accountability milestone

Measure actual improvement on independently labeled, held-out lexical sense
decisions, retaining abstentions and negative changes. Only then propose a
separate, bounded, reversible adaptation mechanism. Audit rows are historical
observations, not independently corroborated improvement receipts.
