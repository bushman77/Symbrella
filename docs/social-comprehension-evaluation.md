# P-009 — Social Comprehension Evaluation Harness

The existing protected Phoenix `/social/:post_id` route now includes an
**Evaluate conversation · up to 50 excerpts** action. This is an explicit,
operator-initiated, **read-only** evaluation of the already fetched Colony post
and a bounded set of returned replies. It does not issue additional Colony API
requests, publish comments, draft replies, call an LLM, train Symbrella or
change her live cognitive policies.

## Operator workflow

1. Open `/social`, enter a discussion, and choose
   **Evaluate conversation · up to 50 excerpts**.
2. The harness selects at most 3 sentence excerpts per source from the
   original post and the first 24 returned replies, up to **50 total**.
   It deduplicates identical normalized excerpts. A short thread may yield
   fewer than 25 examples; this is not falsely filled with synthetic text.
3. For each sentence, inspect the external excerpt and the existing
   `Core.Social.SemanticProbe` evidence. Choose **Expected outcome**:
   `interpret`, `abstain`, `needs_context`, or `exclude`.
   Leave `unlabeled` until reviewed.
4. Optionally assign a **Linguistic family** (`statement`, `question`,
   `social`, `fragment`, `negation`, `other`) and an independent
   **Manual meaning review** (`correct`, `incorrect`, `uncertain`).
   These fields are operator annotations, not machine-generated ground truth.
5. Inspect separate **Development** and **Reserved held-out** counters.
   Structural candidates are evidence of coverage only; the panel cannot
   prove meaning correctness from the presence of a frame. Missing sentence
   meaning does not establish that abstention is semantically correct.
   Failed probes count as **infrastructure failures**, never safe abstentions.
6. Use **Copy labeled JSONL** if you wish to retain reviewed examples.
   Save the copied JSONL yourself after checking the untrusted excerpts
   for sensitive content and duplication. The app does **not** save it.
   Closing the modal, refreshing or navigating elsewhere discards labels.

Repeatedly evaluating different discussions and combining **reviewed** exports
can build a 25–50-case initial dataset and eventually a larger 200-case set.
This route does not silently aggregate multiple discussions or create a
persistent dataset.

## Sampling and holdout

- `SocialCognitiveReport.analyze(target: :evaluation)` uses the existing
  observation-only `SemanticProbe` with
  `brain_observation?: false` and `emit_telemetry?: false`.
- The report uses `max_sentence_chars=600`, at most three excerpts per
  source, at most 24 returned replies, and at most 50 samples overall.
  A longer source or thread is explicitly reported as omitted.
- A normalized excerpt (trimmed and case-folded) is SHA-256 fingerprinted;
  the first 12 bytes become an opaque sample ID. The first two digest bytes
  deterministically assign approximately 20% of unique excerpts to
  `heldout`, the rest to `development`. The exact proportion in a
  small discussion is not guaranteed. The same normalized excerpt is
  assigned to the same fold across different conversations.
- Reserve held-out cases for **evaluation after** implementing improvements.
  Merely labeling the held-out samples does not establish that they remain
  untouched during engineering. Do not tune against them.
- The existing frozen `priv/colony08_heldout_cases.json` 37-case suite
  is **unchanged** and remains a separate generalization test. The
  P-009 sampling split is not a replacement for that suite.

## What the metrics mean

`observed: :structural_candidate` records that the existing read-only
`SentenceMeaning` returned a supported structural status. It does **not**
mean the inferred roles or claims are correct, or that the external claim is
true. `:provisional_candidate` is weaker. `:not_interpreted` marks a
missing sentence-level interpretation. `:infrastructure_failure` means
the probe lacks a complete verified read-only evidence receipt.

A manually assigned `expected: :interpret` plus a structural candidate
increments **structural coverage candidates**, not correctness.
The same label plus `:not_interpreted` increments **coverage gaps**.
An `expected: :abstain` plus `:not_interpreted` increments
**expected abstentions observed**, not proven safe abstentions.
A manual correct/incorrect judgment is displayed only as an operator
assessment. Unlabeled items do not inflate agreement counters.

The export schema is `symbrella_social_eval_v1` (JSON Lines, one item per
line). Fields preserve the original topic ID, reply/source index, excerpt,
fingerprint and fold, expected/family/manual annotations, bounded reason
codes, lexical choice counts, and observed structural/meaning statuses.
No automatic exports, databases or background jobs are created.

## Isolation and safety

Colony text is **untrusted external content**. The evaluation handler does
not call `SocialBrainActivity.record/1`, and its probe cannot opt into
Brain.Cell excitation. The probe does not call stateful chat
`Core.resolve_input(mode: :prod)`, LIFG/ATL GenServer ingest, WM
admission, Hippocampus persistence, synaptic learning, social drafting or
Colony posting. A source sentence remains an author's claim, not
Symbrella's verified memory.

The normal **Analyze cognition** button elsewhere on the same page retains
its existing opt-in transient Brain observation behavior. The distinct
**Evaluate conversation** action does not invoke that behavior.

## Tests

From the umbrella root on `symbolic_generation`:

```bash
mix test \
  apps/symbrella_web/test/symbrella_web/social_evaluation_test.exs \
  apps/symbrella_web/test/symbrella_web/social_cognitive_report_test.exs \
  apps/symbrella_web/test/symbrella_web/live/social_live_test.exs
```

Keep checking the prior frozen held-out benchmark without changing its
expected labels:

```bash
elixir scripts/colony_generalization_probe.exs
```

The 37-case benchmark requires the local development services and dictionary
as configured by the script. Do not treat a failed database lookup as a
correct abstention or claim a benchmark run before executing it.
