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

## Handling misleading search outcomes

Live report from October 14 identified two independent gaps after the
topic-continuity fix:

- A reference query for **Japan's railgun development** produced only
  generic "Railgun", "Plasma railgun" and fiction articles. For `general_search`
  requests with a capitalized possessive named focus (`Japan's` etc.),
  the resolver now retains only sources whose title or excerpt mentions
  that named focus. An empty result is **not** counted as a source-supported
  answer; Reflection gives a scoped, honest no-evidence response.
- The dated Google News RSS provider made two correctly scoped attempts
  with no qualifying results; Qwen nevertheless said that Symbrella lacked
  web-search capability and Reflection accepted it. Reflection now
  distinguishes `:not_found` (search succeeded but found no qualifying
  headlines) from `:unavailable` (provider could not be queried).
  It repairs only false capability claims when actual search attempts are
  present. No articles or citations are fabricated.

This does **not** increase the Google News window, add a specialised Japan
railgun source, or guarantee topical citations. Enabling a vetted SearXNG
endpoint can extend general-reference coverage; RSS is still a bounded
dated-headline discovery source.

## October 14 follow-up: source-selection and output validation

The follow-up Brain report exposed two failures that were invisible to routing
tests. The first reference lookup retained an anime/television-season entry
whose description happened to contain **Japanese** and **railgun**. The named
programme gate now uses whole words, requires an additional query subject
term, and rejects entertainment-dominant results when the original question
does not request entertainment. These are conservative lexical checks, not
semantic fact verification.

For typed reference responses that have retrieved material, Reflection now
requires a link to at least one retained source. If the draft gives a factual
narrative without any of those URLs, the reply is repaired to bounded,
attributed excerpts rather than silently accepting unsourced assertions.
Even a URL does *not* prove the model's surrounding claims; deeper
claim-level validation is still future work.

The news follow-up correctly retained the railgun subject and returned no
qualifying dated headlines, but Qwen replied "I can't assist with that."
A generic can't-assist refusal after a documented unsuccessful news lookup
now triggers the same honest empty-search fallback as a false claim that
web search capability is absent. The search remains subject-bound and no
headline is invented.

## Recorded search outcomes for conversational accountability

A routing decision alone does not establish that the requested sources were
found. Every dated-news lookup produces an authoritative status, query,
attempt count, resolution and qualifying-source count. An empty lookup
must not be described as "I found information", "there were recent
developments", or "I'll search" *after* Core has already searched.
Reflection replaces these unsupported claims with the actual outcome.

For immediate follow-up checks such as "Well did you?", the session's last
dated-news receipt is retained in an ETS table owned by
`LlmChatHistoryOwner`. The receipt contains only bounded provenance fields:
`query`, `relation`, `status`, `resolution`, `attempted_count`,
and `source_count`. It expires after five minutes, stays isolated by
the chat's session key, and does not store untrusted article text.
Reflection uses it to answer directly without inventing search success.
This receipt is not a durable memory and is not evidence of any factual
claim inside retrieved documents.

Regression coverage includes the complete search-then-"Well did you?"
exchange with a fake LLM and fake empty news provider, plus false-positive
tests for statements of retrieval success with zero returned sources.

## One-year dated-news lookback

Google News RSS discovery uses a rolling `when:1y` query rather than
`when:3d`. The RSS adapter and the evidence resolver now both accept
well-dated headlines no older than `365 * 24` hours. Older, undated,
non-HTTPS or unattributed headlines remain disallowed. A custom
`max_age_hours` option may narrow this window but cannot widen it
past one year. This gives specialized topics a chance to recover the
latest *documented* milestone even when nothing was published in
the last few days. Headline age and source attribution must remain
visible; older headlines do not imply new events occurred today.

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


## Repeated-"from" intercity distance questions (October 2026)

Production reproduction: after answering `where is kamloops bc`
from a correctly matched Kamloops Wikipedia lead, Symbrella
answered `how far from maple ridge is from kamloops` with an
**unsupported** 30 km (19 miles) estimate falsely attributed to
Wikipedia. The distance is about 229 km straight-line based on
Maple Ridge and Kamloops city-centre coordinates; this is NOT a
driving-route measurement. The failure arose because the first
turn was a static place-identity question, whereas the noncanonical
wording on the second turn was not classified as a two-place
proximity request. Without that frame, numeric geographic
claims were not subjected to the right source-verification gate.

`Core.Semantic.EventFrames.location_question/1` now recognizes:

    how far from maple ridge is from kamloops
    How far from Maple Ridge is it from Kamloops, BC?

It produces a `:location_question` /
`:proximity` frame with two explicitly named, separate subjects,
`:grounding_status => :unverified`, and no verified distance.
`Core.Tools.WebRetrieval.RequestRouter` routes that frame to the
coordinate evidence resolver, which asks the official Canadian
GeoNames provider and/or Wikidata for the two exact places.
Only provider-verified place identifiers and coordinates authorize
a *haversine straight-line* distance. No unverified driving
distance can be computed from straight-line coordinates.

The response grounding gate now also requires that both source
titles and support subject/target match the two bound locations
in the correct order; a source for Maple Ridge and **Vancouver**
must not authorize a distance query asking about Maple Ridge and
**Kamloops**, even if the provider reports a successful status.
A successfully grounded distance has an exact authorized response
sentence, not merely a valid number and citations embedded
somewhere in otherwise unconstrained LLM prose.

Without both verified coordinate records, reflection rejects
the unsupported number and reports missing distance evidence.
It must NOT publish the 30 km / 19 mile hallucination or imply
Wikipedia independently verified an intercity driving route.

### Local test and read-only diagnostic

    git pull --ff-only origin development

    mix test apps/core/test/core/semantic/event_frames_test.exs \
      apps/core/test/core/tools/evidence_resolver_test.exs \
      apps/core/test/core/tools/official_canadian_sources_test.exs \
      apps/core/test/core/tools/web_retrieval_test.exs

    mix run scripts/probe_distance_question_grounding.exs

The probe uses **synthetic** geographic-support fixtures and performs
no network calls, model calls, or cognitive writes. Its sample
229.6 km is a test assertion, not a newly retrieved live
measurement. Real chat must fetch and revalidate its own sources.
Test the two live turns consecutively and inspect Explain:
`:geographic_claim_unverified` signals corrected/withheld
distance; a supported answer names both places, gives a
straight-line distance and includes both coordinate citations.


### Live source rejection diagnosis and suffix normalization

A follow-up after the initial repeated-"from" parser fix still returned
insufficient verified distance evidence for both `maple ridge` ->
`kamloops` and `maple ridge bc` -> `kamloops bc`.

A definite second-stage failure was in `Grounding.supported_proximity`:
its source-title check treated `Maple Ridge` and `maple ridge bc`
as distinct place identities, rejecting some otherwise valid provider
proofs even after the provider checked the province. It now recognizes
Canadian province/territory suffixes in comma and whitespace formats
for **city-name equivalence only**. The provider must still establish
identity and jurisdiction; this change does not promote a search hit
or guess coordinates.

The resolver also retains a bounded `:reason` atom when a coordinate
provider returns `{:error, reason}`, e.g. `:ambiguous_place`,
`:place_not_verified`, or `:unavailable`. Numeric claims remain
blocked on errors. The no-support fallback now reflects `how far`
without changing it into an `is it close` question.

    mix test apps/core/test/core/tools/evidence_resolver_test.exs \
      apps/core/test/core/semantic/event_frames_test.exs \
      apps/core/test/core/tools/official_canadian_sources_test.exs

    mix run scripts/diagnose_live_geographic_distance.exs

Unlike the synthetic proof script, `diagnose_live_geographic_distance.exs`
makes real bounded HTTP calls. It prints the current non-secret retrieval
policy, both question frames, each search/coordinate attempt and failure
reason, whether a retrieved support object passes the final grounding
gate, and direct Canadian GeoNames/Wikidata errors for the qualified
city pair. **This diagnostic is read-only for cognitive state, but NOT
offline.** Its results identify whether the remaining blocker is the
external API, ambiguous entity matching, timeout, or local evidence
authorization. Do not infer a successful live calculation from a
synthetic 229.6 km test fixture.
