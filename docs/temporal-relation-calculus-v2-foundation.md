# Temporal Relation Calculus — V2 Foundation

## Why this increment exists

Temporal Event Composition V1 orders reports by their ledger sequence numbers.
That does not tell us what "before" or "after" means in a described event.
The two notions must remain distinct.

This increment adds Core.Semantic.Composition.TemporalRelation as a pure
compositional operator, without changing V1 ledger sequence logic.

## Typed meaning

Core.Semantic.Composition.Event.compose/1 produces independently
replay-verified event predicates of type event -> truth.

Given independently checked P and Q in surface order, the result is a property
of two possible events:

- before(P,Q): λe1.λe2. P(e1) ∧ Q(e2) ∧ precedes(e1,e2)
- after(P,Q):  λe1.λe2. P(e1) ∧ Q(e2) ∧ precedes(e2,e1)

Both results have type event -> event -> truth.

The order of P and Q never changes; only the direction of precedes changes.
Beta reduction is deterministic and bounded. The entire proof is replayed
by Core.Semantic.Composition.Proof.

## Example

    alias Core.Semantic.Composition.{Event, Proof, TemporalRelation}

    {:ok, sitting} = Event.compose(%{
      predicate: "sitting", actor: "bird",
      preposition: "on", landmark: "fence"
    })

    {:ok, sleeping} = Event.compose(%{
      predicate: "sleeping", actor: "bird",
      preposition: "under", landmark: "tree"
    })

    {:ok, proof} = TemporalRelation.compose(:before, sitting, sleeping)

    :ok = Proof.verify(proof)
    :ok = TemporalRelation.verify(proof, :before, sitting, sleeping)

    proof.result_type
    # => {:arrow, :event, {:arrow, :event, :truth}}

Unknown connectives and invalid source proofs fail closed.

## Critical epistemic boundary

TemporalRelation.compose/3 accepts a connective argument, NOT a parsed
utterance. This alone cannot verify that a speaker used the connective:

- connective_witness_verified?: false
- event_occurrence: :not_established
- actual_event_chronology: :not_established
- referent_identity: :not_established
- factual_authority?: false

Even a mathematically correct derivation does not prove that either event
happened, that their actors are the same individual, or their real-world order.

## Next bridge

Admit before/after only through independently checked lexical and grammatical
witnesses. Then record a speaker-attributed temporal proposition in the ledger
without overwriting the original report sequence. Ambiguous scope must abstain.
