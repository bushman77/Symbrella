# Temporal Claim Reconciliation V1

## Purpose

Recognize when the same speaker supplies two opposite *claimed* temporal
orders for matching closed event descriptions. This is **not** a proof of
logical contradiction: actor kinds and event descriptions do not establish
that the speaker refers to the same individual events or event occurrences.

Example, in one session:

    The bird is sitting on the fence before the cat is sleeping under the tree.
    The bird is sitting on the fence after the cat is sleeping under the tree.

Both meanings were previously composed and replay-verified as
`:explicit_temporal_report`. The protected discourse ledger stores
`:reported_temporal_relation` records whose `:seq` values are report turn
positions, not actual event chronology.

## V1 detection

`Core.Semantic.TemporalClaimReconciliation.detect(session_id)`:

- Reads only the existing same-session protected ledger.
- Requires status `:provisional`, the original revalidated provenance,
  non-factual flags, the expected fixed event descriptor schema, and a
  well-formed proof fingerprint.
- Compares equal closed event descriptions in reversed claimed order,
  requiring two distinct descriptions.
- Returns one `{:ok, candidate}` only for a **unique** opposing report pair.
- Returns `:not_found`, `:ambiguous`, or `:window_incomplete` otherwise.
- Refuses to make a unique claim when more than 128 session entries remain,
  even if a pair is visible in part of that history.
- Does not modify ledger entries, assert speaker error, or create real-world
  identities, event occurrences, or external chronology.

The candidate includes both report sequence numbers and claimed event
descriptions. `:contradiction_verified?` and `:factual_authority?`
remain false; `:occurrence_identity` is unresolved.

The stored fingerprint is evidence of prior processing, not an independent
cryptographic proof that the speaker's real-world claim is true. This V1
trusts the existing ledger recording gate for the original lexical proof.

## Native question/answer

The sentence-meaning pipeline recognizes only these exact question forms
(case-insensitive, surrounding whitespace allowed):

    Did I describe conflicting temporal orders?
    Did I contradict myself about the order?

If exactly one candidate pair exists, `SentenceMeaning` attaches
`:temporal_claim_conflict_question`; `SemanticStrategy` selects
`:clarify_opposing_temporal_reports`; and `SymbolicGeneration` produces
a closed-vocabulary clarification. The answer names both report sequence
numbers, both claimed temporal orders, and explains that the descriptions
could concern different occurrences. It asks the user which order they
intended. No LLM is called.

Questions with zero, multiple, or incomplete candidate pairs cannot use
this native clarification. Ordinary declarative turns do **not**
spontaneously trigger a conflict warning.

## Verification and limits

Run:

    mix test apps/core/test/core/semantic/temporal_claim_reconciliation_test.exs \
      apps/core/test/core/discourse/temporal_ledger_test.exs \
      apps/core/test/core/semantic/temporal_sentence_test.exs \
      apps/core/test/core/semantic/temporal_event_composition_test.exs

The record store remains in-memory ETS, not durable across restarts.
There is no automatic preference for the last factual statement, nor any
resolution of real-world event identity. Temporal Clarification Resolution V1
now accepts the immediately following exact report-selection answer and
appends an independent, speaker-attributed preference record. It never edits
or retracts either original report. See temporal-clarification-resolution-v1.md.
