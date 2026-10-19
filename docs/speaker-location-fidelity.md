# Speaker / current-location fidelity at the response boundary

## Captured regression — 2026-10-18

A live Symbrella Brain/LIFG and Blackboard export showed a meaningful
difference between verifying *a city's coordinates* and knowing the
*physical location of either conversation participant*.

- `where is kamloops bc` -> Wikipedia-supported city identity.
- `how far from maple ridge is from kamloops` -> a Canadian GeoNames
  coordinate-based **228.8 km** straight-line answer. Core appropriately
  repaired an LLM draft by adding the source-backed method and citations.
- `where am i atm` -> **incorrect** LLM answer
  `I'm currently located in Kamloops, British Columbia, Canada.`;
  `ReflectionLoop` accepted the draft with no issues.
- `no you are not` -> the **same self-location claim** repeated and again
  accepted, despite the user denial being recognized as `:deny`.

The captured LIFG denial-turn audit chose `no|preposition|1`, with four
low-confidence token decisions, but the higher-level `:deny` intent was
correct. This response repair does **not** change LIFG, dictionary senses,
ATL, hippocampus or synapses.

## Correction

`Core.Response.SpeakerLocationFidelity` performs a narrowly scoped,
read-only review of generated responses.

1. For an explicit *user-current-location* question (`where am i atm`,
   `where am i`, etc.), reject affirmative location claims about
   the user **or** Symbrella when no verified participant-location evidence
   was supplied. A municipality's sourced coordinates do NOT prove a
   person's live location.
2. For an explicit *assistant-location* question (`where are you`),
   reject unsupported affirmative claims about the assistant's physical
   place. Symbrella's local runtime identity is not a physical-location
   sensor.
3. For an immediately adjacent *direct denial* (e.g.
   `no you are not`), when the prior same-session user question was
   about current physical location, reject any new assistant self-location
   assertion. When the previous assistant already asserted an unsupported
   city, report that this claim was not verified. The check also blocks
   a *new* unsupported self-location claim after the first reply was
   safely repaired. Previous assistant prose is a **correction target**,
   never verification.

The Core `ReflectionLoop` issues list includes
`:speaker_location_fidelity` when repair applies; the deterministic
fallback acknowledges uncertainty or the challenged prior claim rather
than inventing a physical location. The usual review continues to
inspect source-backed distances and city descriptions independently.

`Core.Response.LlmSynthesis` gives response review only the immediately
preceding complete same-session turn pair under `:review_history`.
The previous assistant's statement does not become a canonical fact,
and unrelated denial turns or public-city questions do not trigger
this correction.

This is a bounded *linguistic* guard, **not** a full semantic-proof engine
or a physical location API. More elaborate paraphrases can still evade
it; those should be measured and handled with independent provenance
rather than indiscriminately expanding regex patterns.

## Local validation

    git pull --ff-only origin development

    mix test apps/core/test/core/response/speaker_location_fidelity_test.exs \
      apps/core/test/core/response/llm_route_flow_test.exs \
      apps/core/test/core/response/reflection_loop_test.exs

    mix run scripts/probe_speaker_location_fidelity.exs

The probe is read-only, uses captured draft strings, and calls neither
LLM nor geographic providers. The fake-LLM integration test checks the
normal response path with prior session history. Finally use a fresh
chat session with `where am i atm`, then `no you are not`
as the user correction, and confirm Explain does not accept unsupported
self-location claims.

Do not confuse `:speaker_location_fidelity` with the existing
`:semantic_role_drift` guard. The latter is limited to structural
actor/predicate/patient substitution for reported events.
