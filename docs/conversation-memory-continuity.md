# Conversation coherence and identity memory

## Contracts

1. Assistant identity is configuration-owned, not a user fact. Explicit assistant-name
   questions are resolved deterministically by Core.Response.Memory.
2. User facts are owned by the user subject, stored as hippocampal fact episodes and
   retrieved by exact key from Db.Episode.latest_user_fact/1. A partial PostgreSQL
   index accelerates this lookup across the full episode table. Neural scores must
   not replace these authoritative facts.
3. The LiveView's current conversation identifier is owned by ChatHistory. It is
   stable across LiveView remounts while the application is running. Clearing the
   chat starts a fresh conversation identifier. Bounded LLM prompt history retains
   six turn pairs by default. A separate ETS buffer retains up to 100 completed
   exchanges for evidence-based past-turn recall, never sent wholesale to the LLM.
   Explicit questions such as "Have I asked you what 1 plus 1 is?" check the
   recorded user turns before the semantic/LLM pipeline, returning a grounded
   answer or a limited-history miss.
4. Brain.Neural.MemoryRelevancePilot remains an offline, human-feedback-only
   experiment. Production factual recall does not invoke neural ranking.

## Evidence for future neural experiments

For identity or fact-read responses, Core.Response.Memory publishes:

    [:core, :response, :memory_recall]

Measurements contain count and hit (1 or 0). Metadata contains action,
identity_subject, memory_source and neural_ranker_used? (false).
No name, retrieved value or message text is included. This represents a
retrieval outcome, NOT a reviewed relevance label.

The existing opt-in Core.TrainingArchive provides turn observations and
hippocampal events. TrainingBuilder requires explicit human labels, and the
memory-relevance pilot must remain out of the live response path until an
independent evaluation justifies a separate rollout.

## Verification

Apply database migrations before starting the app after pulling:

    mix ecto.migrate

Run the complete suite:

    mix test

Or focused tests:

    mix test apps/core/test/core/response/identity_continuity_test.exs
    mix test apps/core/test/core/response/conversation_recall_test.exs
    mix test apps/core/test/core/response/memory_recall_telemetry_test.exs
    mix test apps/db/test/db/episode_exact_fact_test.exs
    mix test apps/core/test/core/response/durable_fact_acknowledgement_test.exs
    mix test apps/symbrella_web/test/symbrella_web/chat_session_continuity_test.exs

Then manually verify after a restart:

- "My name is Bradley." -> acknowledgement
- "What's my name?" -> Bradley
- "What's your name?" -> Symbrella
- "And yours?" after a name discussion -> Symbrella
- Restart the application, then ask "What's my name?" again.

## Boundaries and follow-up work

ChatHistory is deliberately in-memory: it does not write a full chat transcript
to disk. A process/application restart discards the conversational buffer, but
durable fact episodes remain available through the database.

Ordinary hippocampal episodes still use best-effort persistence. Explicit
user-name and user-fact claims instead use Brain.Hippocampus.encode_verified_fact/2:
the PostgreSQL insert must succeed before the memory window is updated and
before a durable acknowledgement is emitted. If persistence fails, the reply
states the limitation rather than claiming permanent recall.

## Current limits of conversational continuity

Explicit past-turn recall is evidence-based, but open-ended pronoun resolution,
topic transitions, and questions about facts not explicitly stored as user facts
still depend on the model and the semantic pipeline. The 100-turn buffer is
in-process and is lost on application restart. A verified name/fact survives
restart through PostgreSQL, but a full transcript does not.


## Geographic follow-up continuity (development)

The new session-scoped discourse binding links it/its to an explicitly named location in a recent user question, without treating a pronoun dictionary gloss as a verified antecedent. Grammatical MWE precedence suppresses incompatible is-it/interjection and close-to/adverb lexical promotions in the tested interrogative construction, retaining raw selections for diagnostics.

Core.Semantic.EventFrames now distinguishes population and proximity question roles and records grounding_status as unverified. Core.Response.ReflectionLoop can repair unsupported geographic assertions when the corresponding frame is present and comprehension is degraded. This does not add a geographic data source or fact verification. Response policy also suppresses generic closing probes on completed factual answers.

Regression command:

    mix test apps/core/test/core/response/geographic_continuity_test.exs

Live check: Where is Maple Ridge, BC?; What is its population?; Is it close to Vancouver? Inspect the symbolic frame, comprehension diagnostics, and reflection alongside the reply. Coverage is limited to these clear syntactic forms, six recent turns, and the current process.
