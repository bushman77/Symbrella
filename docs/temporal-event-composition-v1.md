# Temporal Event Composition V1

## Boundary

V1 composes **the order of two attributed, proof-qualified conversational
reports**. It does not prove event occurrence, actor identity, physical
chronology, or causation.

This extends the existing typed entity/event property join, which has type:

```elixir
{:arrow, :entity, {:arrow, :event, :truth}}
```

A report record includes a fingerprint of that verified typed property, and
the independent lexical witnesses were rechecked at ledger admission.

## Current admissible inputs

After a verified nominal description, two independently verified
anaphoric progressive locative reports can enter the same-session ledger:

```text
The small black bird.
It is sitting on the fence.
It is sleeping under the tree.
What did I describe before it was sleeping under the tree?
```

The first two reports must match one unique earlier nominal description, and
the target report must be unique. Any competing earlier candidate, repeated
target, missing evidence, unsupported source or other session abstains.

The question is deliberately restricted to a small, exact grammar:

`What did I describe before it was <sitting|sleeping>
<on|in|under|beside|above|below> the <fence|tree|table>?`

It returns a `:reported_event_order_question` and a checked
`:report_order_relation` with sequential indices. The response describes
the speaker's earlier and later reports and explicitly says this is not
verified event chronology.

## Limits

- Proven: the source reports' independently checked semantic properties, their
  ledger admission provenance, and their recorded sequence numbers.
- NOT proven: whether reported events happened, that the candidate description
  picks out one real-world individual, or that the events happened in the same
  order as they were reported.
- Not yet supported: `What was it doing before it flew?`, past tense,
  direct event-time ordering, explicit corrections, arbitrary temporal
  connectives, multiple actor candidates, or durable storage.
- Storage remains bounded, session-scoped, and in-memory. The previous
  conversation-history infrastructure remains unchanged.

## Inspect

```elixir
Core.Discourse.Ledger.entries(session_id)

Core.Semantic.TemporalEventComposition.pair(
  session_id,
  %{action: :sleep, relation: :under, landmark: :tree}
)
```

A successful result has `order: :reported_before` with
`actual_event_order: :not_established` and
`event_occurrence: :not_established`.

V2 should add independently witnessed temporal connectives and revisions,
then extend event predicates (including directional motion) without promoting
discourse order into real event chronology.
