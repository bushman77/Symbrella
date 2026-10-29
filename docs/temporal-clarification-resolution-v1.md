# Temporal Clarification Resolution V1

## Purpose

Close one active, uniquely identified temporal-order disagreement without
deleting either speaker report or asserting any event took place.

A supported conversation in one session is:

    The bird is sitting on the fence before the cat is sleeping under the tree.
    The bird is sitting on the fence after the cat is sleeping under the tree.
    Did I describe conflicting temporal orders?
    I meant report 1.

The third turn can receive an existing native symbolic clarification question.
Its turn is now recorded in the discourse ledger as a
`:temporal_clarification_request`. The next turn can be the explicit answer:

    I meant report 1.
    I meant report 2.

The number is an **actual session report sequence**, not a generic ordinal.
The parser accepts only this exact, bounded answer grammar, including the final
period. It does not infer the answer from "the first one" or from pronouns.

## Evidence gate

`Core.Semantic.TemporalClarificationResolution.build(si)` requires:

- Eligible current-turn user-origin SI with a same-session identifier.
- The newest ledger entry is the pending request with the exact two report
  sequence numbers and original proof fingerprints.
- Recomputing the reconciliation still yields a single, unique opposing pair.
- The selected sequence is one of those two reports.
- The pending request immediately precedes this answer in the ledger.

`verified/1` reruns this check and compares the complete saved meaning
against the fresh result. A stale or forged correction cannot be recorded.

## Memory behavior

The existing `Core.Discourse.Ledger.record/2` stores only whitelisted
semantic fields. The request and resolution each consume one existing
session-scoped ledger turn. The resolution records the selected report number,
its descriptive event properties, the pending request, original proof
fingerprints and explicit uncertainty flags.

The original `:reported_temporal_relation` records are unchanged.
`TemporalClaimReconciliation.detect(session_id)` returns
`{:resolved, preference}` for a qualified resolution of the unique pair
rather than presenting the same disagreement as currently unresolved.

The selected report represents the speaker's **stated intended order**,
not a verified contradiction, real-world event identity, or actual chronology.
The LLM is not involved in the native acknowledgment.

## Limits

- No free-form correction, semantic paraphrase, persistent memory across
  restarts, or correction of an older unrelated conflict.
- No automatic priority given to the latest factual report.
- The bounded conflict detector continues to abstain on multiple possible
  pairs and on sessions with more than 128 retained entries.
- A clarification must be the next eligible user turn after the request.
  Intervening conversation turns invalidate it rather than leaving a stale
  pending request active.
- One selection closes only the exact fingerprint-matched conflict pair.

## Focused tests

    mix test apps/core/test/core/semantic/temporal_clarification_resolution_test.exs \
      apps/core/test/core/semantic/temporal_claim_reconciliation_test.exs \
      apps/core/test/core/discourse/temporal_ledger_test.exs
