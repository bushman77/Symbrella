# Symbolic language development — Phase 3: grounded discourse

This is the **symbolic-language development workstream**, not the eight-stage
`Core.resolve_input/2` runtime pipeline and not the eight-phase operational
self-awareness roadmap. Keep their numbering and completion criteria separate.

## Baseline carried forward from symbolic Phase 2

- `FinitePastLocativeIntegration` is default-on in the existing Core pipeline.
- `sit/sat` and `sleep/slept` have typed, replayable conditional event-property
  proofs and an adversarial validation matrix.
- The proof establishes **possible-event structure**, not event occurrence,
  referent identity, speaker commitment, or factual authority.
- The original LIFG winner remains untouched (including `sat|noun|1`).
- Unsupported constructions must abstain; no LLM is involved.

## Phase 3 target

Enable safe links between independently verified sentence interpretations
and future same-session conversational questions, without treating a
speaker-attributed description as an independently verified world fact.

### Gate 1 — pure speaker-description candidate (implemented, opt-in)

`Core.Semantic.FinitePastDiscourse.project/2`:

- Requires a complete bounded declarative-looking surface and a user/prod
  turn with an explicit session scope.
- Requires an attached finite-past proof, successfully replayed by
  `FinitePastLocative.verify/1`, then **independently re-derived** from the
  current `SemanticInput` by `FinitePastLocativeIntegration.derive/2`.
- Projects only bounded action, actor-kind, and location-kind properties,
  with source and proof SHA-256 fingerprints.
- Sets `status: :provisional`, `event_existence: :not_asserted`,
  `event_occurrence_verified?: false`, `referent_identity_verified?: false`,
  and `factual_authority?: false`.
- Writes nothing and does not attach a new field to the production pipeline.
  It is intentionally **not yet an enabled memory or question-answering path**.
- Rejects questions, negation, quotes, reported material, modal constructions,
  changed current-turn evidence, invalid session scope, missing dictionary
  evidence, and malformed proofs.

Gate 1 exit test:

```bash
mix test apps/core/test/core/semantic/finite_past_discourse_test.exs
```

### Gate 2 — explicit opt-in provisional Ledger admission (verified in targeted tests)

- `Core.Discourse.Ledger.record/2` accepts
  `finite_past_discourse_ledger?: true` to *explicitly* opt in. Existing
  production behavior stays unchanged by default.
- Under the opt-in, normal fuzzy, temporal, progressive, nominal and
  cross-turn composition priorities run before finite-past admission.
- `Core.Semantic.FinitePastDiscourse.project/2` must independently revalidate
  the attached proof against this turn's evidence and a canonical lemma
  dictionary lookup. For deterministic tests only, the record call accepts
  `finite_past_lemma_lookup: fun`.
- Reject missing session, invalid origin, contradictory question/command intent,
  stale or tampered proofs, unsupported or ambiguous structure, and unavailable
  dictionary evidence. Such user turns remain ordinary `:uninterpreted` records.
- An admitted record carries `:provisional`, `:declarative_candidate`,
  `:conversation_context_only`, `:not_asserted` event existence and
  `factual_authority?: false`. This is a **speaker-attributed description**,
  not proof of speaker commitment or that an event occurred.
- Existing Ledger Owner supplies strictly same-session, bounded ETS retention,
  sequential turn IDs, and reset. No sentence text, session identifier, full
  lambda AST or LIFG slate is stored inside this new record.
- **No new recall or answer path is enabled.** `Ledger.resolve/2` continues
  to ignore finite-past candidates; Gate 3 must establish identity ambiguity,
  corrective turns and tense-aligned question binding before any retrieval.
- Tests: `apps/core/test/core/semantic/finite_past_discourse_test.exs`.
  Targeted tests and the pre-existing Ledger regression suite passed in the
  user's local environment. Full umbrella regression remains a separate gate.

### Gate 3 — opt-in read-only past-question projection (implemented, testing pending)

- `Core.Semantic.FinitePastDiscourseQuestion.project/2` is a **separate,
  explicitly invoked read-only API**, not a planner hook, Ledger.resolve
  extension, or generated answer. Its inputs are a session ID and an exact
  past-tense question.
- It recognizes only `What was the bird doing?` / `Where was the bird?`
  (also `cat` and `dog` as closed actor kinds). Present tense, bare pronouns,
  quoted speech, vague questions and compound forms abstain.
- The newest Ledger entry must be an eligible, validated, same-actor
  `:reported_finite_past_locative_candidate`. It must carry the expected
  admission-time provenance flags, hashes, and explicit **nonfactual** status.
- `bird` denotes a *kind*, not a proven unique individual. If multiple
  finite-past reports for the same actor kind survive in the session, return
  `{:ambiguous, sources}` without recency guessing. Another known mention
  of that actor-kind without a valid report is an abstention barrier.
- Even a single match returns **only** a typed reported predicate or location
  descriptor, `:speaker_description_candidate`, with a source sequence and
  proof digest. It does not render a response string or assert event occurrence.
- Any later unrelated turn blocks retrieval until a future explicit continuity
  policy is proven. The Ledger remains in-memory and session-scoped.
- **Proof fingerprints identify admitted evidence but are not cryptographic
  proof of trustworthy writes.** Readback depends on the trusted Ledger
  admission path; no original SI/proof is retained to replay at read time.
  This is why the module remains unconnected to live responses.
- Run the Phase 3 tests and prior Ledger regressions:

```bash
mix test apps/core/test/core/semantic/finite_past_discourse_test.exs apps/core/test/core/discourse/ledger_test.exs
```

- Before enabling user-visible answers, audit stable speaker-commitment evidence,
  correction/negation barriers, repeated mentions, safe reference identity,
  and whether ledger admissions can be trusted by all internal callers.

### Gate 4 — production and regression gate (not implemented)

- Compare the new path against current progressive and nominal recall.
- Verify no altered event/fact authority or LIFG sense winners.
- Run targeted tests, the full umbrella suite, cold/warm-session tests, and
  representative production probes with negative and ambiguous cases.
- Record attachment, ledger and retrieval latency separately.
- Promote the path only after every admission and abstention invariant passes.

## LIFG sense-resolution workstream

LIFG dictionary-sense selection remains an independent, measurable issue.
Use finite-past proofs as **shadow evidence** for candidate/competition audits;
never back-propagate a proof into the same turn's sense winner and then use that
winner to validate its own proof. Any scoring change needs a separate controlled
A/B evaluation and tests.
