# Symbrella — Discourse Ledger V1

## Purpose

Retain conversation-wide **semantic evidence** independently of the short LLM
prompt window and the currently active working-memory focus. The ledger is not
a source of real-world factual authority.

The existing `Core.Response.LlmChatHistory` continues to own recent text
history. `Core.Brain.WM.Comprehension` continues to own temporary attention.
`Core.Discourse.Ledger` stores a separate ordered timeline of compact,
independently checked interpretations.

## Runtime

- Supervised `Core.Discourse.Ledger.Owner` owns protected ETS.
- Key: the exact conversation `session_id`, max 128 bytes. No global
  fallback or cross-session search.
- Each nonempty user/prod/test turn advances a monotonic sequence.
- Nominal descriptions are admitted only after
  `Core.Semantic.Composition.verified/1` and revalidation through the existing
  `Core.Brain.WM.Comprehension` nominal-focus contract.
- Anaphoric spatial reports are admitted only after
  `Core.Semantic.DiscourseComposition.verified/1`.
- Uninterpreted turns retain **only** a sequence and status, not user text.
  Their inclusion ensures intervening turns do not vanish from the timeline.
- Every record has `factual_authority?: false`. A report never asserts
  `event_occurrence_verified?: true`.
- Default retention: **4096 entries per session**, configurable up to 16384.
  This is a bounded semantic history, **not** a complete verbatim conversation
  transcript and **not** persistent across application restarts.

## Production attachment

`Core.resolve_input/2`:

1. Recall existing WM context.
2. Recall explicit old nominal candidates from the ledger.
3. Apply discourse reference binding (including ambiguity gating).
4. Run the normal STM → LIFG → ATL → semantic proof → response pipeline.
5. After the completed pipeline, append a compact ledger record, including
   when `response: :off` is used.

A phrase such as "What about that black bird?" can retrieve a nominal
description mentioned many turns earlier. If multiple earlier descriptions
match, `discourse_context.status` is `:ambiguous` and no ledger reference
is bound. An isolated "it" **cannot** retrieve a distant inactive referent.

## Introspection

```elixir
Core.Discourse.Ledger.entries(session_id)
Core.Discourse.Ledger.resolve(session_id, "What about that black bird?")
Core.Discourse.Ledger.clear(session_id)
```

`entries/1` is newest-first; `resolve/2` returns
`{:ok, candidate} | {:ambiguous, candidates} | :not_found`.

Opt out in a probe with `discourse_ledger?: false`. Configure via
`config :core, Core.Discourse.Ledger, max_entries_per_session: 4096`.

## Future phases

1. **Durability** — a proper `Db` schema/migrations for turn and semantic
   evidence records, isolated by conversation identity. Add explicit
   retention/deletion controls and migrations before claiming full persistence.
2. **Structured event queries** — select past provisional spatial reports
   through explicit references, contradictions and revisions, without treating
   user reports as independently verified observations.
3. **Discourse confirmation** — user consent/clarification can bind a
   previously ambiguous referent, with provenance and revision history.
4. **Entity continuity and topic shifts** — support more than the closed
   three-noun vocabulary, plural references, reintroduction and retractions.

## Safety boundaries

The next compositional stage includes a beta-replay-verified **typed
entity/event property join** with a relational actor predicate. That proves
a well-typed conditional relation between possible entities and events, **not**
the identity of a particular real bird or the occurrence of any event.
The ledger itself does not establish facts or inspect all prior raw text.
Retrieval is explicit and conservative: unknown/ambiguous references abstain.
It leaves existing LIFG winners, dictionary cells, and weights untouched.
