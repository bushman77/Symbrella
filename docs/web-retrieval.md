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
