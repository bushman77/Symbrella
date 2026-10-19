# Comprehension-driven working memory

## Runtime loop

In Core's production pipeline:

1. Core.Brain.WM.Comprehension.recall/2 loads the most recent eligible same-session frame after intent selection and before discourse-reference binding.
2. Core runs STM, LIFG, MWE and ATL, then read-only EpisodicProjection before SentenceMeaning. A uniquely revalidated action or spatial event now yields a compositional `:participant_assertion` proposition with `:structurally_interpreted` status. Independent question frames retain their existing interpretation path.
3. Core.Comprehension.Summary.attach/2 retains that proposition while keeping lexical uncertainty independent. Core.Brain.WM.Comprehension.store/2 submits one bounded active-focus frame through Brain.focus/2 -> Brain.WM.Admission -> Brain.BasalGanglia -> Brain.WorkingMemory.
4. Prefrontal planning, action selection, legacy topic focusing, response planning, Hippocampus and activation continue normally.

The temporary frame is NOT a verified fact. It carries at most four explicitly identified entity names; up to six recognized topics, lexical constructions and uncertain terms; any interpreted relation; and degraded/interpretation status. Raw sentences, full candidate slates, and model-generated answers are not stored. The fixed candidate score is a temporary focus priority, not a truth probability.

This frame has a per-call permissive gate threshold of zero. Other LIFG, episodic and factual candidates retain their evidence gates. No global WM configuration is mutated.

## Validated participant events

The grammatical episodic projector runs before the comprehension summary and
WM admission. When a user-reported finite-present action or progressive-spatial
event has one revalidated participant frame, the session-scoped WM context now
also carries bounded actor, patient/location and predicate roles, the relation,
and a `:validated_projection` role status. These are derived from the event's
independent syntactic and lexical evidence; raw candidate injection is checked
by recomputing the projection. Ambiguous multiple candidates and assistant-
sourced inputs do not supply these roles.

Example: `The artist draws a picture.` yields active entities
`["the artist", "a picture"]`, relation `"verb_draw"`, and an
`event_roles` map containing actor, canonical predicate, verb surface,
patient, and tense. This is **conversation context**, not a verified external
fact and not proof that Hippocampus has persisted the event. Lexical
`understood`, `uncertain`, and `degraded?` remain independent, as does the
legacy sentence-meaning `interpretation_status`.

## Unified structural sentence meaning

`Core.Semantic.SentenceMeaning` now consumes exactly one revalidated
`Core.Semantic.EpisodicProjection` participant event when no resolved WH-topic
question frame takes precedence. Actions and progressive spatial states use the
same proposition interface: `type: :participant_assertion`,
`epistemic_status: :user_reported_unverified`, bounded semantic arguments,
grammatical modifiers, and the originating `source_frame`.

For the artist example the proposition has `event_type: :action`, canonical
predicate `draw`, surface predicate `draws`, and actor/patient arguments.
For the bird example it has `event_type: :spatial`, predicate `sitting`,
actor/location arguments, progressive aspect and the `on` position.

The representation says *what the user's sentence asserted*. It never says the
assertion was confirmed true, assigns new dictionary senses, or claims the
Hippocampus completed a write. `status: :structurally_interpreted` reports
composition success only. Existing weak LIFG lexical selections can still
make `comprehension.degraded?` true and leave lexical `understood` empty.
Stale, forged, ambiguous, or assistant-sourced participant candidates fail
closed to `:not_yet_interpreted` unless a separate supported frame applies.

Inspect a real production trace with:

    mix run scripts/trace_action_participants.exs

The `sentence_meaning`, `comprehension_status`, and
`working_memory_context` fields show separate interpretations and certainty
levels. This diagnostic is read-only.

## Session isolation and retention

Only inputs with a non-nil session_id are eligible. Each session has a stable hashed frame ID. A newer turn replaces/refreshes the same session's prior frame rather than admitting every word independently. Recall never falls back to another session's frame. Frames remain eligible for recall for twenty minutes after their most recent update, subject to the existing global WM capacity. This is not a dedicated per-session WM process.

Core.Response.RuntimeContext.snapshot/0 excludes session-scoped frames from unscoped global WM snapshots so their contents cannot leak into another session's LLM prompt. Authorized same-session context flows through si.wm_context.

Enabled in development/production via config/config.exs, disabled under MIX_ENV=test unless a call explicitly sets comprehension_wm?: true. To disable in normal runtime, set:

    config :core, Core.Brain.WM.Comprehension, enabled?: false

No CLI variable is required.

## Plural reference example

First turn: How far is Kamloops BC from Maple Ridge BC?

The existing geography event frame supplies the explicit subject and target. The comprehension frame stores them as context-only referents.

Follow-up: What are the populations of each?

Same-session recall binds "each" to both places before discourse-reference and semantic interpretation. The population event frame contains subjects ["Kamloops BC", "Maple Ridge BC"] and grounding_status :unverified. The response prompt names both and requires independent verification of any population numbers.

This is conversational continuity, not yet a two-city population-lookup provider. Grounding plural numerical facts is a separate milestone.

## Verification

    git pull origin development
    mix test apps/core/test/core/brain/comprehension_wm_test.exs
    mix test apps/core/test/core/response/llm_prompt_test.exs
    mix test

Run Phoenix, then ask the two questions in the same chat session. Check SI trace wm_comprehension (admitted) on the first turn and wm_context (recalled) on the second. Check discourse_reference.source :wm_comprehension and symbolic_frame.subjects. Brain.snapshot_wm() should include the session's comprehension item; Core.Response.RuntimeContext.snapshot().wm_items must not.

These new trace stages differ from the legacy topic-only wm_focus. If Brain is unavailable, the pipeline carries on without crashing.
