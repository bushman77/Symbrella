# Grammar-backed generalized predicate episodes

Symbrella now projects simple third-person present SVO clauses from *three*
independent internal evidence products rather than plain text alone:

1. A full-clause :finite_present_svo_candidate syntax signature with all
   required grammatical evidence and exactly one span.
2. Selected LIFG noun/verb/noun sense IDs for subject, predicate and object.
3. ATL winners agreeing with those exact LIFG selections and token indices.

The input must be a user-asserted, single-clause utterance. Negation,
hedging, quotations, subordinate clauses, questions, absent or contradictory
winners, ambiguous signatures, and incomplete role coverage are rejected.

Example: "The artist draws a picture." with selected predicate lemma "draw"
creates user-reported event:

    entity: "the artist"
    relation: "verb_draw"
    value: "a picture"
    source_frame: :finite_present_svo

"What did the artist draw?" retrieves the event from Hippocampus, returning
the *original user sentence* with attribution rather than generating new
grammar from the relation name.

This establishes an extensible grammatical predicate pathway. It does not
claim a calibrated confidence score or independently verified truth.
The projection's fixed 0.8 grade is a structural admission grade, not a
probability the assertion is true. It intentionally covers only simple
third-person finite-present clauses. Past-tense and multi-clause events need
additional role grounding and temporal modeling.

The historical reconstruction audit remains read-only and **does not**
retroactively accept surface-only SVO patterns as source-supported events.

Verify:

    mix test apps/core/test/core/semantic/svo_event_test.exs
    mix test
