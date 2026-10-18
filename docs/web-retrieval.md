# Symbrella web retrieval

This development iteration adds bounded web evidence for smaller local LLMs.

## When it searches

- Explicit requests starting "search the web for...", "look up online...", or "find online...".
- An unverified, structured location question about population or geographic proximity.

Other personal conversation is not automatically sent to search providers.
For non-test environments retrieval is on unless disabled.

## Providers

The default is the keyless English Wikipedia API. This is useful for encyclopedic
material, not broad current-event research or guaranteed official statistics.

Optional SearXNG (JSON search endpoint must be enabled):

    export SYMBRELLA_WEB_PROVIDER=searxng
    export SYMBRELLA_SEARXNG_URL=https://your-trusted-searxng.example

The configured SearXNG endpoint must use HTTPS, except localhost / 127.0.0.1.
Only returned snippets are used; result links are never automatically fetched.

Turn retrieval off:

    export SYMBRELLA_WEB_RETRIEVAL=false

## Response pipeline

1. Core creates the usual bounded semantic/response context.
2. Core.Tools.WebRetrieval selects a lookup only for eligible requests.
3. The provider returns at most three clipped, untrusted source excerpts.
4. Grounding extracts a Wikipedia population value only when the article title
   matches the requested location and the excerpt itself supports the figure.
5. The LLM receives snippets and source URLs with an explicit untrusted-content warning.
6. Reflection retains the original model draft, replaces unsupported geographic
   population guesses with sourced figures when available, or refuses unsupported
   geographic claims when the source is missing.
7. core/response/complete telemetry includes web_retrieval status and provenance.

Source-supported is not independently verified. No general factual verifier has
been implemented. Proximity measurements remain unverified in this initial
version unless they can be grounded by a future specific verifier. The SearXNG
provider supplies context but does not bypass reflection's geographic safeguard.

## Offline tests

    mix test apps/core/test/core/tools/web_retrieval_test.exs
    mix test apps/core/test/core/response/geographic_continuity_test.exs
    mix test

Restart Phoenix after pulling. Test the Maple Ridge three-turn conversation and
inspect web_retrieval in core/response/complete blackboard metadata.


## Province-qualified public place questions (Oct 2026)

A real live turn, `where is kamloops bc`, had the correct retrieved
Wikipedia article (`Kamloops`) and an irrelevant article about a school,
but the response still fell back to a reference-only link list.
`Core.Tools.WebRetrieval.PlaceLocation` previously required the normalized
search query (`kamloops bc`) to match the full Wikipedia article title
(`Kamloops`), which erroneously discarded the relevant city article.
This was a retrieval identity-admission issue, not a failure to find
the article, choose `:answer_user`, or choose a semantic cohort.

The location-reference matcher now accepts an unqualified Wikipedia article
title ONLY when a Canadian province-qualified question precisely names that
article's base place, the Wikipedia HTTPS article URL matches the title,
and the opening article sentence explicitly asserts the place lies
**in** the requested province. A mere mention such as "in Alberta, near
British Columbia" is NOT an identity witness for BC. Source leads can
contain bounded parenthetical pronunciations before "is", and trailing
encyclopedic detail can be shortened to a cited jurisdictional clause
without introducing new facts.

The normal `ReflectionLoop` check `:location_reference_unaligned`
uses this newly admitted source and replaces an uncited, unaligned local
LLM answer with a bounded citation, e.g.:

    According to Wikipedia, Kamloops is a city in south-central
    British Columbia, Canada. Source: https://en.wikipedia.org/wiki/Kamloops

Other pages, including the Kamloops Indian Residential School,
unmatched/wrong article URLs, non-Wikipedia domains, unsupported excerpts
and mismatching provinces remain unverified reference results. This
does not modify live LIFG noun/abbreviation decisions or claim that
dictionary selections alone establish an external geographic fact.

Offline validation with deterministic sample provider responses:

    mix test apps/core/test/core/tools/web_retrieval/place_location_test.exs \
      apps/core/test/core/tools/web_retrieval_test.exs \
      apps/core/test/core/response/llm_route_flow_test.exs

    mix run scripts/probe_place_location.exs

These checks do not access live Wikipedia. After pulling into the
Termux runtime, retry `where is kamloops bc` and inspect the
actual Wikipedia source excerpt plus the final response. If the live
provider does not supply a title- and province-matched lead, Core must
still abstain rather than invent location details.

The LIFG selection `bc|noun|1` (birth control) visible in the same
diagnostic is a separate POS/abbreviation disambiguation issue.
The citation-bound response fix intentionally does not override
the lexical selector, conflate lexical guesses with verified facts,
or silently promote a dictionary sense.
