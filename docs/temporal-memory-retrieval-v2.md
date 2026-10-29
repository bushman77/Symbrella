# Temporal Memory Retrieval V2

## Goal

Answer a later explicit question about which temporal *description* the
speaker selected during a previously completed clarification. Preserve the
distinction between a stated intention, verified linguistic composition, and
the real-world occurrence or chronology of events.

A successful same-session example:

    The bird is sitting on the fence before the cat is sleeping under the tree.
    The bird is sitting on the fence after the cat is sleeping under the tree.
    Did I describe conflicting temporal orders?
    I meant report 1.
    [20 unrelated conversation turns]
    Which order did I finally mean?

Expected native answer:

    You clarified that you meant report 1: the bird sitting on the fence
    before the cat sleeping under the tree. That is your stated intended
    order, not verified event chronology.

Only the report's previously composed event properties and the speaker's
selected report sequence are asserted. There is no claim that these events
happened or refer to the same individual occurrences.

## Evidence flow

`Core.Semantic.TemporalMemoryRetrieval.recall/1` performs a read-only
same-session lookup. It first requires
`Core.Semantic.TemporalClaimReconciliation.detect/1` to return one
`{:resolved, ...}` result, retaining its unique-pair requirement and
128-entry completeness bound. It then independently retrieves the chosen
report and resolution by session sequence number, checks their provenance,
original proof fingerprints, selected claim descriptors, and all non-factual
flags. It neither reactivates a candidate pronoun referent nor guesses from
the most recent mention.

`question/1` recognizes precisely these English question forms
(case-insensitive, with required question mark):

    Which order did I finally mean?
    Which temporal order did I choose?

`verified/1` requires a full replay of the question and ledger evidence.
`Core.Semantic.SentenceMeaning` records
`:temporal_preference_recall_question`, and
`Core.Response.SemanticStrategy` chooses
`:answer_intended_temporal_order`. The native
`Core.Response.SymbolicGeneration` realizer verbalizes the closed event
vocabulary and includes an explicit uncertainty qualifier. No LLM is called.

## Invariants and limits

- The original two reports, pending clarification, and speaker preference
  remain append-only in the existing ETS discourse ledger.
- An ordinary recall question is read-only with respect to the evidence.
  The normal production pipeline still logs a conversation turn, but does
  not rewrite or consolidate the earlier records.
- An unresolved, ambiguous, cross-session, stale, missing, or truncated
  supporting record cannot authorize native preference recall.
- This increment handles one uniquely reconciled opposing pair; it does
  not handle multiple independent conflicts, paraphrases, persistent
  storage across restarts, or sessions with more than 128 retained turns.
- The returned preference is not real-world event confirmation.

## Focused regression

    mix test apps/core/test/core/semantic/temporal_memory_retrieval_test.exs \
      apps/core/test/core/semantic/temporal_clarification_resolution_test.exs \
      apps/core/test/core/semantic/temporal_claim_reconciliation_test.exs \
      apps/core/test/core/discourse/temporal_ledger_test.exs
