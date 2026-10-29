# Attention-Guided Comprehension Repair V1

## Purpose

When no authorized symbolic, lexical, or semantic strategy can produce a
response and LLM inference is intentionally disabled, Symbrella should attempt
a **specific, evidence-bounded comprehension repair** before the generic
unavailable message.

The goal is to surface what is independently understood or currently noticed,
identify **one** actual unresolved surface term, and ask for clarification.
This is a conversational repair request, **not** a claim that its interpretation
was corrected or that the user supplied a new dictionary sense.

## Execution order

Existing `Core.Response.plan/2` is unchanged in priority:

1. Memory answers / policy / verified symbolic response strategy
2. Symbolic generation
3. Lexical definitions
4. `Core.Response.AttentionFallback.generate/4`
5. LLM-disabled or otherwise unavailable response

The fourth step now attempts partial comprehension repair **before** its
existing generic focus-continuation replies. No LLM is started by repair.

`Core.Brain.WM.Comprehension.store/2` already admits a compact current
same-session frame through `Brain.focus/2`. When no explicit topic or
projected entity exists, the focus goal list can now use the *current turn's*
bounded understood/uncertain lexical anchors. Salience is a **focus priority**,
not a dictionary-sense probability or factual confidence.

Response gating requires all of these:

- Low-risk request and a supported non-safety-blocking response action,
  including a cautious `:ask_first` for targeted repair only.
- Same exact current sentence and allowed user/prod/test source.
- Identical current-turn SI WM context and independently admitted,
  non-expired same-session Brain working-memory frame, including a SHA-256
  fingerprint of the exact current utterance (no raw text stored in WM).
- Positive attentional salience, a nonempty current goal, no released focus.
- An unresolved term reported by `Core.Comprehension.Summary` and matching
  the admitted WM's `:uncertain` projection. The term must actually appear
  on the current input surface.
- No known already-interpreted temporal meaning that would make an unknown
  explanation inaccurate.

A weak or uncertain lexical label is not automatically a clarification
target. If that same term is already part of both the recognized conversational
topic and a recognized-topic anchor (for example, "ocean" in a request about
"the Pacific Ocean"), the repair selector excludes it. A *different* unknown
term in the same question remains eligible. This preserves the legacy
topic-continuation behavior and prevents spurious "what do you mean by
ocean?" questions.

Attentional focus prioritizes among the eligible uncertain terms. It never
proves the term's meaning. The repair reports the specific unresolved surface
term and records it as `:unresolved_term` in provenance metadata.

## Wording and evidence tiers

- **Independently dictionary-verified lexical construction:** if a matching
  `lexical_constructions` witness exists, the phrase is present on the
  current input surface and in the admitted WM's `:understood` list,
  Symbrella may say: `I understand "bird", but I'm not sure how you're
  using "flarped" here. Could you clarify that part?`
- **Recognized attentional topic/entity only:** says `I'm following "bird"...`,
  explicitly avoiding the stronger understanding claim.
- **Unresolved term only:** says `I'm not sure how you're using
  "flarped" here. Could you clarify what you mean?` without inventing
  any recognized referent.

All three variants have `factual_claims?: false`,
`semantic_verified?: false`, `llm_called?: false`,
`status: :attention_comprehension_repair`, and
`provenance: :admitted_same_session_comprehension`.
The original generic WM continuation remains available when repair lacks
proper evidence but the focus is still sufficient.

`Core.Response.Attach` and the LiveView backup planner now forward the
relevant comprehension / WM / lexical construction witnesses so the response
layer cannot silently lose the evidence needed for more specific wording.

## Deliberate boundaries

- A lexical uncertainty is **not** proof of a misunderstood complete
  sentence. Fully interpreted temporal claims should be handled by their
  own semantic realization, not a fake repair question.
- V1 asks one focused question but **does not yet bind a later correction to
  the pending unknown**, update a dictionary sense, or train synapses.
- A strong LIFG candidate alone is not the independently verified lexical
  construction required for `I understand`.
- If the same-session context is missing/stale or the uncertain term cannot
  be tied to the current surface, existing abstention remains in effect.
- Does not override safety policy, definition requests, or higher-priority
  verified responses.

## Tests

```bash
mix test apps/core/test/core/response/attention_fallback_test.exs
```

The suite exercises WM admission from comprehension-only lexical goals,
verified-vs-recognized wording, unknown-only repair, live response planner
integration, stale/safety exclusions and unchanged legacy focused-topic
continuation.


## 2026-10-29: evidenced parts versus genuinely unknown parts

The first live test of a bird/fence/cat/tree report returned a weak
attention continuation about `sleeping`. The diagnostic snapshot reported:

- Working memory admitted one session frame (salience 1.0) with goals such
  as `bird`, `sitting`, `before`, `cat`;
- 7 weak LIFG decisions and 16 low-confidence decisions;
- structural competition `:no_complete_candidate`;
- the general composition stage `:no_admissible_composition`;
- intent classification keyword `sleeping` and domain `:health_sleep`.

These observations are not proof that `sleeping` was the subject of the
sentence. The general composition stage is also separate from the
closed, proof-revalidating `Core.Semantic.TemporalSentence` path.

The response fallback now distinguishes three evidence levels:

1. **Reverified temporal speaker report.** `TemporalSentence.verified/1`
   replays the two event properties and connective. The fallback describes
   the *reported* bird/cat order, then names what is still unresolved
   (exact dictionary senses, referent identities, event occurrence, and
   actual real-world chronology). It does **not** suggest the temporal
   connective is unknown, or assert the events happened.
2. **Actually unresolved lexical term.** A specific clarification requires
   `Comprehension.Summary.unresolved`, not merely `:uncertain`. The
   unresolved list is carried into admitted session WM and rechecked against
   the current SI, sentence fingerprint, and surface token.
3. **Weak lexical senses without missing vocabulary.** A generic report
   identifies at most three surface terms with uncertain sense selection.
   No clarification about the meaning of familiar words is demanded.
   Stronger `I understand ...` wording additionally requires a
   current-turn dictionary-verified `:lexical_constructions` witness.

If none applies, the existing topic-attention continuation or guarded
unavailable response remains. The LLM is not invoked; no word senses,
synapses, database rows, or beliefs are modified.

The diagnostics still do not establish that the complete chat response
will work until a user pulls, compiles, runs tests, and exercises LiveView.


## Context-guided fuzzy clarification pilot

The original statement's text and token evidence remain unchanged. Only when
the same-turn admitted comprehension identifies an actually unresolved word
does \`Core.Response.AttentionFallback\` ask \`Core.Text.Fuzzy.suggest_word/3\`
for a non-mutating hypothesis. The spelling matcher uses existing
Damerau–Levenshtein and DB candidate lookup infrastructure with additional
guards: a single occurrence of the unresolved word, distance exactly one,
an attested bounded context frame or strong contextual candidate score, and
an unambiguous lead over other context-supported candidates. No suggestion
is treated as an exact lexical sense or a verified event.

The first deliberately narrow frame recognizes **bird is [unknown action]**
and permits **flarping → flapping** as a question only. This is a curated
contextual compatibility pilot, *not* generalized ATL semantic inference.
Context-free or tied candidates abstain, retaining the existing targeted
clarification. This pilot does not persist definitions, alter the original
utterance, resolve the meaning of a novel word, or implement a multi-turn
confirmation/learning loop. Future iterations can replace curated frames
with explicitly revalidated ATL/lexical and corpus evidence.

To validate on the Elixir runtime:

\`\`\`bash
mix test apps/core/test/core/text/fuzzy_test.exs \
  apps/core/test/core/response/attention_fallback_test.exs
\`\`\`


## Immediate fuzzy confirmation V1

LiveView verified that “The bird is flarping on the fence.” could ask
“Did you mean ‘flapping’?”, but a following “yes flapping” was previously
misclassified as fresh lexical uncertainty. This was not a fuzzy-candidate
failure: it was a missing **pending clarification state**.

After an actual low-risk \`wm_attention_fallback\` response is finalized,
\`Core.Discourse.Ledger.record/2\` now admits a bounded
\`:fuzzy_clarification_request\` record only if the response visibly contains
the corresponding “Did you mean …?” question and the current SI still
revalidates the candidate. It retains original and proposed tokens, a hash
of the source sentence, session sequence and timestamp; never the full
source utterance or a validated dictionary sense.

\`Ledger.pending_fuzzy_confirmation/2\` admits an explicit “yes”, “yes
flapping”, or similarly bounded affirmative **only when** that request is
the latest ledger record in the same session and less than five minutes
old. \`AttentionFallback\` handles this ahead of ordinary lexical-gap
reporting, with a provisional speaker-intent acknowledgment. The finalized
confirmation is itself stored as
\`:fuzzy_clarification_resolution\` so the question cannot be answered
twice. Unmatched replies, interrupted conversations, stale requests,
unknown sessions, and negative answers abstain.

This does not modify the source sentence, rerun a corrected semantic parse,
assert event occurrence, or write a learned dictionary definition or
synaptic correction. Those are separate future capabilities that need
independent verification and explicit user intent.

Focused regression command:

\`\`\`bash
mix test apps/core/test/core/response/attention_fallback_test.exs \
  apps/core/test/core/text/fuzzy_test.exs
\`\`\`


## Same-session fuzzy action recall V1

A confirmed fuzzy substitution is not automatically reflected in the
owner-wide Hippocampus. The question `What is the bird doing?` previously
returned `Sitting.` from a separate, older stored `spatial_state` event even
after the current session had confirmed `flarping → flapping`. That older
stored event was not evidence for the newly clarified utterance.

The discourse ledger now attaches only a bounded surface actor to an *issued*
fuzzy question when the original utterance matches a strict progressive,
locative clause. Confirmation retains that actor without copying or
silently rewriting the source utterance. For the very next exact
`What is the <actor> doing?` question, `confirmed_fuzzy_activity/2`
revalidates the adjacent request/resolution entries, session, actor,
original, candidate, source-turn digest, and nonfactual provenance.

`Core.Response.Memory` consults that revalidated same-session correction
before the owner-wide episodic activity lookup, answering in the form
`In your recent description, you confirmed that "flarping" meant "flapping"
for the bird. That is your intended wording, not a verified event.`

An intervening ledger turn, other actor, or different session prevents this
narrow recall. It is not a generalized semantic reparse, an assertion about
real-world bird activity, a database correction, or a permanent definition.
The existing older owner-wide memory remains unchanged.


## Speaker-confirmed event recomposition (focused V1)

The preceding activity-recall implementation correctly retrieved the
clarification, but returned a *description of the clarification* rather than
answering the requested role. This revision adds
\`Core.Semantic.ClarifiedEvent\`:

- The issued clarification's discourse record retains seven bounded source
  lexemes plus the original utterance's SHA-256 fingerprint. They are
  source evidence, not a substituted or overwritten sentence.
- Before use, \`Ledger.confirmed_fuzzy_activity/2\` rebuilds the source
  progressive clause, checks the stored original hash, and links the
  adjacent, same-session question and explicit user confirmation.
- \`ParticipantQuestions.parse/1\` independently selects the actor and
  requested \`:predicate\` role of the *current* question. It must match the
  reconstructed source actor.
- The resulting provisional frame has explicit actor, predicate, aspect,
  locative relation, and landmark, with a separate
  \`:speaker_confirmed_substitution\` provenance for the predicate.
  The exact dictionary sense, external event occurrence and referent identity
  remain unverified.
- \`Core.Response.Memory\` uses that frame's predicate role to answer
  **"In your description, the bird is flapping."** before it considers older
  owner-wide activity episodes. Older Hippocampal records remain untouched.

This is a bounded source-grammar recomposition, **not** a full new LIFG/ATL
parse or claim that an actual bird moved. Any unrelated actor, other session,
unmatched question, source hash mismatch, broken request linkage, or
intervening turn without that active pair abstains.

Run the focused tests after pulling:

\`\`\`bash
mix test apps/core/test/core/text/fuzzy_test.exs \
  apps/core/test/core/response/attention_fallback_test.exs \
  apps/core/test/core/semantic/clarified_event_test.exs \
  apps/core/test/core/response/episodic_memory_test.exs
\`\`\`


## Surgery 1 — independent grammatical-gap observation

\`Core.Syntax.Signatures\` now produces
\`:progressive_lexical_gap_candidate\` for a seven-slot progressive
locative clause when the predicate has an \`-ing\` surface form **but no
dictionary POS row**, and all surrounding noun, determiner, and
preposition slots have independent POS evidence. This signature is an
observational pre-LIFG role hypothesis, not an inferred verb sense,
verified event, or LIFG/ATL winner. With complete participle evidence,
the existing \`:progressive_locative_candidate\` is retained instead.

When the actual response asks a fuzzy clarification question,
\`Core.Discourse.Ledger\` attempts to independently replay this
\`Signatures.detect/1\` witness against the original SemanticInput,
then retains a bounded \`source_syntax\` witness together with the source
lexemes and SHA-256 fingerprint. On a subsequent speaker-confirmed
correction, it revalidates the source clause and carries
\`role_evidence: :attested_core_syntax_gap\` if the witness is present,
or \`:surface_clause_only\` for the existing narrow compatibility path.

\`Core.Semantic.ClarifiedEvent\` exposes the distinction as
\`frame.evidence.role_binding\` and
\`frame.evidence.pre_lifg_gap_observed?\`. Neither variant changes
the original utterance, the lexicon, LIFG/ATL choices, or Hippocampal
event records. Both preserve
\`frame.evidence.event_occurrence_verified? == false\`.

This does **not yet** generalize fuzzy lexical hypothesis generation:
the current \`Fuzzy.suggest_word/3\` still requires contextual candidate
evidence, with a curated pilot frame for the bird example. Nor is the
new gap signature a completed ATL semantic integration. Later stages
should wire candidate scoring and semantic competition to these
explicit evidence records rather than assuming a matching \`-ing\`
suffix is sufficient.

Focused regression command:

\`\`\`bash
mix test apps/core/test/core/syntax/signatures_test.exs \
  apps/core/test/core/semantic/clarified_event_test.exs \
  apps/core/test/core/response/attention_fallback_test.exs
\`\`\`

## Surgery 2 — LIFG unresolved-role observation

`Brain.LIFG.RoleCompetition.resolve/1` now independently rechecks the pre-LIFG `:progressive_lexical_gap_candidate` before emitting an `:unresolved_roles` map. This requires an ordered seven-token progressive locative clause, dictionary POS witnesses for the surrounding determiners, nouns and preposition, and no attested dictionary POS for the predicate. It remains `status: :unresolved`, with no POS or sense ID.

`role_competition.roles` remains reserved for supported existing dictionary alternatives, and Stage-1 scoring still reads only those supported roles. No score boost, invented sense, ATL constituent, or factual event is produced by this observation. `Core.Syntax.RoleCompetition` reports an `:unresolved` diagnostic with `unresolved_grammar_count` separately from supported count. The next surgery can bridge this into ATL without promoting it to semantic truth.

Focused validation:

```bash
mix test apps/brain/test/brain/lifg/role_competition_test.exs \
  apps/core/test/core/syntax/role_competition_test.exs \
  apps/core/test/core/syntax/signatures_test.exs \
  apps/core/test/core/semantic/clarified_event_test.exs
```


## Surgery 3 — read-only LIFG → ATL unresolved grammatical roles

\`Brain.ATL.UnresolvedGrammar.attach/2\` adds a distinct
\`:unresolved_grammar\` observation list and
\`:unresolved_grammar_count\` to the local ATL slate. The observations
are not constituents, concept counts, dictionary senses, or events.
They retain \`:status => :unresolved\` and explicit false values for
lexical POS verification, exact sense, referent identity, event
occurrence, and factual authority.

The bridge only accepts LIFG's \`:unresolved_roles\` entries that
match an independently replayed
\`Brain.LIFG.RoleCompetition.resolve/1\` result. An actual selected
dictionary sense at that predicate token index suppresses the
observation, while a nonlexical fallback cannot become a sense.
The Core ATL adapter attaches observations to its existing
reduced slate without replacing \`:winners\`, \`:by_id\`, \`:by_norm\`,
or \`:constituents\`. It adds the observation count to the ATL trace.

This is intentionally not yet ATL event composition or lexical
interpretation: the semantic hub can inspect the unresolved gap
while all existing sense-selection and fact-generation boundaries
remain intact.

Focused regression command:

\`\`\`bash
mix test apps/brain/test/brain/atl/unresolved_grammar_test.exs \
  apps/core/test/core/brain/atl_constituents_test.exs \
  apps/brain/test/brain/lifg/role_competition_test.exs \
  apps/core/test/core/syntax/role_competition_test.exs
\`\`\`


## Surgery 4 — semantic hypothesis from audited syntax, LIFG and ATL gap

\`Core.Semantic.ProvisionalInterpretation.detect/1\` now produces the
\`:unresolved_progressive_predicate\` frame for a complete, bounded
progressive-locative statement *only* after replaying:

- the original seven-token clause and its exact surface punctuation;
- the \`Core.Syntax.Signatures\` gap's SHA-256 source-span fingerprint;
- the LIFG unresolved-role entry, through \`Brain.ATL.UnresolvedGrammar.attach/2\`;
- the ATL slate's single unresolved predicate observation, with no
  genuine dictionary winner in that slot.

The frame describes actor surface, unresolved predicate surface and
locative phrase; it carries \`:operation: :clarify_unresolved_predicate\`,
\`:status: :provisional\`, and explicitly unresolved lexical sense,
referent identity, speaker clarification and event occurrence. It is
an **observation for future clarification**, not a verified event and
not a response or memory write. It does not replace established
sentence meanings or LIFG/ATL winners. A forged frame is rejected
by \`ProvisionalInterpretation.verified/1\`.

The previous fuzzy clarification pathway remains independent.
An additional later step would need to bind a speaker-confirmed
substitution to this original-turn provisional frame *without*
confusing earlier missing-role recognition with verification.

Run:

\`\`\`bash
mix test apps/core/test/core/semantic/provisional_interpretation_test.exs \
  apps/core/test/core/brain/atl_constituents_test.exs \
  apps/brain/test/brain/atl/unresolved_grammar_test.exs \
  apps/brain/test/brain/lifg/role_competition_test.exs
\`\`\`


## Surgery 5 — source provisional interpretation → confirmed lexical substitution

The actual clarification request can now carry a bounded
\`source_provisional\` witness. \`Core.Discourse.Ledger\` only builds this
witness when \`ProvisionalInterpretation.verified/1\` independently
reconstructs the original missing-predicate frame and its source-span
SHA-256 digest matches the replayed seven-token clause. It retains only
the original actor/predicate/locative role surfaces, frame identifier,
digest, and negative epistemic markers—not a wholesale sentence
rewrite, fabricated dictionary cell, or factual event.

At follow-up, the existing exact-session, adjacent request/resolution
sequence and original source-utterance fingerprint are still required.
A nonnil source witness is replayed against its original source
tokens and the fingerprinted Core syntax signature. **Every role**
must agree: actor, missing predicate surface, preposition, location.
A corrupted witness causes abstention instead of falling back to
surface-only correction. Records with no original provisional frame
retain the previous conservative \`:attested_core_syntax_gap\` or
\`:surface_clause_only\` pathway, so older dialogue does not break.

\`Core.Semantic.ClarifiedEvent.build/2\` now explicitly distinguishes
\`:validated_original_provisional_frame\` in
\`frame.evidence.role_binding\`, sets
\`original_semantic_frame_replayed?: true\` and preserves a compact
\`original_interpretation\` reference. It still answers the question
by projecting the speaker-confirmed replacement into the requested
predicate slot, not by claiming an observed event. In particular,
\`exact_dictionary_sense?: false\`,
\`event_occurrence_verified?: false\`, and
\`factual_authority?: false\` remain unchanged.

The previous focused response and semantic tests now include an
actual response-selected clarification with a full Core
syntax→LIFG→ATL→provisional source frame and negative tests for
mismatched actor, predicate, position, landmark, and source hashes.
This is intentionally a **bounded same-session semantic repair**;
generalized grammar repair and long-lived lexical learning remain
separate milestones.

Focused validation:

\`\`\`bash
mix test apps/core/test/core/response/attention_fallback_test.exs \
  apps/core/test/core/semantic/clarified_event_test.exs \
  apps/core/test/core/semantic/provisional_interpretation_test.exs \
  apps/core/test/core/response/episodic_memory_test.exs
\`\`\`
