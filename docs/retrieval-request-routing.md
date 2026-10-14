# Deterministic retrieval-request routing (development)

Symbrella recognizes retrieval intent before asking Qwen to synthesize a reply.
`Core.Tools.WebRetrieval.RequestRouter.route/1` returns:

`%{query: String.t(), relation: atom() | nil, request_type: atom(), trigger: atom()}`

The legacy `WebRetrieval.query_for/1` API still returns just `{query, relation}`.
`WebRetrieval.route_for/1` exposes the complete decision. Actual results record
`request_type` and `routing_trigger` in `context.web_evidence` and retrieval
telemetry for inspection and training archives.

## Request categories

| Request type | Examples | Retrieval |
| --- | --- | --- |
| `:current_events` | "What is happening with the Ukraine war as of current?", "Search the web for latest news about Iran", "Any updates on that?" | Google News RSS, dated headlines **only** |
| `:general_search` | "Search for the Japanese railgun project", "Look into X" | SearXNG when configured; otherwise Wikipedia |
| `:technical_reference` | "Search for Elixir release notes", "Where can I find official API documentation?" | General reference/search excerpts |
| `:scientific_research` | "Research scientific papers on entorhinal cortex", "What do studies say about X?" | General reference/search excerpts, **not** scholarly database verification |
| `:historical_reference` | "Who invented the transistor?", "History of the Panzer III" | Wikipedia/reference excerpts |
| `:comparison` | "Compare sources on solar and wind power", "Differences between AC and DC" | General reference/search excerpts |
| `:fact_check` | "Verify that claim", "Fact-check X" | General search excerpts, **not** independently verified truth |
| `:local_information` | "Look up cafes near me" | General search excerpts, **not** live opening hours or local-business availability |
| `:geographic_population` | Supported symbolic `:location_question` population frame | Wikipedia + claim-specific census extraction |
| `:geographic_proximity` | Supported symbolic proximity frame | Wikipedia and Wikidata coordinate-based straight-line comparison |

General search needs a working SearXNG endpoint for web-wide results.
Without one, the configured Wikipedia provider offers only encyclopedic
coverage. The router does **not** fabricate specialized weather, sports,
shopping, scholarly citation, flight, maps, reservation, or pricing APIs.

## Trigger provenance

- `:explicit_search`: user explicitly requested external research
- `:temporal_question`: deterministic current-events cue plus event/question pattern
- `:structured_question`: narrow technical, historical, scientific or comparison pattern
- `:symbolic_frame`: a prior Core geographic frame marked unverified
- `:followup`: explicit temporal anaphora resolved from bounded prior **user** turns

Ordinary greetings, personal-memory questions, commands, and personal
statements do not automatically query external sources.

A retrieval follow-up such as **"No, search for it"** uses up to three recent
same-session conversation turns supplied by `LlmChatHistory`, selecting a
substantive USER topic and ignoring assistant output and short corrections.
Without a substantive antecedent, ambiguous pronouns do **not** generate
searches. History is used only transiently for routing, and does not become
a trusted retrieval source.

## Multi-turn topical continuity

An explicit "Search the web for the latest developments" contains a
retrieval **operation and recency preference**, not a subject. The router
now resolves it against a substantive same-session user question, such as
"What do you know about Japan's railgun development?". If there is no
antecedent, it produces no route rather than querying a generic news feed.

The follow-up "What do you think about those developments?" also resolves
to the same user-established subject and obtains dated headlines, allowing
Reflection to enforce source attribution and a qualified assessment.
Intermediate "search for the latest developments" requests, conversational
corrections, and social acknowledgements are not substituted as topics.

Narrow public-knowledge prompts ("What do you know about [public subject]?",
"Tell me about [public subject]?") can request reference excerpts, while
private memory questions ("Tell me about my sister") remain local.

News answers now require the actual retrieved headline text in addition to
the matching outlet, date and URL: listing only an outlet and link does not
demonstrate that the requested topic was addressed. A brief generic "I need
more information" deferral despite available reference excerpts is repaired
with cautious, attributed excerpts, not invented facts.

The routing policy deliberately cannot create evidence: if Wikipedia
or the configured SearXNG endpoint has no relevant sources, the assistant
must acknowledge that limitation rather than fabricate research results.

## Evidence strictness

- Dated RSS headlines have `resolution: :headlines_available` and **no**
  fact-verification claim. The existing ReflectionLoop news checks still
  require publisher, publication date, matching link, and separate assessment
  when requested.
- Non-geographic reference snippets have `resolution: :reference_sources`.
  Their text is untrusted and does not satisfy structured fact validators.
  Qwen must attribute and cite snippets, or admit they do not answer.
- Population/proximity retain their stricter `:source_supported` validators.
- When reference snippets were retrieved but Qwen only promises to search
  later, Reflection repairs the answer with bounded attributed excerpts.
- An absent provider or empty search does not create fake citations.
- The number of attempts, sources, and query length remain bounded by the
  evidence resolver; no arbitrary user URL is fetched by this feature.

## Tests

```sh
mix test apps/core/test/core/tools/retrieval_request_router_test.exs
mix test apps/core/test/core/tools/current_events_retrieval_test.exs
mix test apps/core/test/core/tools/web_retrieval_test.exs
mix test apps/core/test/core/response/evidence_revision_flow_test.exs
```

For direct introspection from IEx:

```elixir
Core.Tools.WebRetrieval.route_for(%{
  user_text: "What is happening with the Ukraine war as of current?"
})
```

**Do not** confuse successful routing with successful source retrieval or
a verified answer. Observe the separate status, resolution and provenance
fields before claiming the user request was answered.
