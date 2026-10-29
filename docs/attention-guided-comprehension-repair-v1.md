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
