# Generalized episodic memory v1

Symbrella's hippocampal episodic memory has two representations:

- **Cognitive trace**: an ordinary Db.Episode containing the user's sentence,
  lexical winners, affect, decision metadata and provenance.
- **Typed user-reported event**: an additional Db.Episode with
  kind=user_reported_event, event_version=1, owner, entity, entity_key,
  relation, value, status, source session and provenance=user_statement.

Typed events are cross-session and persist across application restarts.
Retrieval is an exact owner+entity+relation DB lookup, independent of the
Hippocampus 200/300-episode warm-start window. Events do not verify real-world
facts or authorize external numerical/geographical answers.

## Extensible event contract

Call Core.Response.EpisodicMemory.remember_event/3 with:

    %{entity: "Aster", relation: "maintainer", value: "Rowan",
      status: :asserted}

Status may be :asserted or :retracted. Subsequent events for the same owner,
entity_key and relation supersede prior assertions for the current answer,
while previous entries remain available via
Brain.Hippocampus.user_event_history/3. Event order follows database insertion,
not a user-specified historical event time.

The conversational text adapter deliberately recognizes only unambiguous
assertions such as "Rowan lives in Victoria", "I met Rowan at the library",
"The project deadline is Monday", "We named the robot Aster", "The meeting
is on Tuesday", and relationship/preference forms. Other structured
processors should call remember_event/3; do not extend the database schema
or create separate recall engines for every relation.

Memory answers carry source=:user_reported_event and
response_source=:symbolic_conversation. The assistant's responses and
hypothetical or question-shaped utterances must not establish facts.

## Limits

The text adapter is **not** a general NLP parser. Pronouns, nuanced negation,
quoted speech, indirect speech, uncertain attribution, arbitrary event
extraction, contradictory sources, multi-user authentication and historical
backfill need more work. Do not promote existing untyped hippocampal episodes
into current claims without a provenance-preserving re-analysis pipeline.

The existing SpatialDialogue module remains a compatibility pathway for
previously stored spatial episodes. New typed events coexist with it.

The default owner "local-primary" is for single-user local deployments only;
multi-user deployments must supply a stable, authenticated owner per user
before enabling shared persistent recall.

Verify:

    mix ecto.migrate
    mix test apps/core/test/core/response/episodic_memory_test.exs
    mix test
