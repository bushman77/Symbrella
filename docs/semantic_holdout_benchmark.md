# Symbrella: sealed, human-reviewed semantic holdout benchmark

## Status

The benchmark **harness** exists. No independently reviewed lexical-sense
holdout dataset is included, and no real-world accuracy measurement can
truthfully be reported until external reviews and dictionary IDs are supplied.

The synthetic cases in SemanticHoldoutTest validate evaluation logic only.

## Why this exists

Previous tests established:
1. LIFG decisions and current-turn scored alternative POS/verb evidence can
   be captured read-only in the real Core pipeline.
2. A semantic comparison can count exact-ID agreement with a supplied label.
3. Normal live traffic has **no trusted label** and cannot self-certify success.

Core.Agency.SemanticHoldout adds a *separate* bounded benchmark runner that:
- rejects edited manifests unless their separately saved digest matches;
- requires two distinct reviewer declarations agreeing on the same exact
  current-dictionary sense ID;
- requires an explicitly declared development-utterance hash list;
- rejects repeated case IDs, repeated normalized utterances, and exact
  whitespace/case-normalized overlap with the declared development set;
- supplies the **sentence only** to the resolver, never the gold label;
- reuses SemanticCorrectionEvaluator.compare/2 and its live RoleWitness gates;
- reports **whole-cohort original LIFG baseline agreement** separately from
  correction-subset improvement/regression/abstention metrics, avoiding
  selection bias from scoring only examples with eligible alternatives;
- never trains, modifies LIFG winners, authorizes policy adjustments, or
  supplies "verified" success to working memory or autonomy.

**Limitations:** manifest hashes are tamper-detection checks, not digital
signatures. Reviewer IDs and training-set hashes are declarations and cannot
be authenticated by this code. Whitespace/case normalization does NOT detect
paraphrases, source overlap, LLM pretraining contamination, or dictionary
version changes. A nominal "holdout" split declared by a file does NOT
establish a genuinely held-out evaluation.

A useful published result would additionally require independent reviewer
records, a frozen dictionary inventory, release/version provenance, an
externally maintained development corpus, and no benchmark-based tuning.

## Preparing the first real dataset

1. Review current live dictionary inventories; never assume numbered IDs.
   For each ambiguous token, examine full definitions, examples and POS in
   Db.brain_cells_for_norm/1. The older
   docs/lifg-word-sense-evaluation.md describes the procedure.
2. Have two separate human reviewers assign exact dictionary IDs without
   seeing Symbrella's decisions or the other reviewer's proposed answer.
   Disagreements remain unresolved; do not force labels.
3. Build a development dataset and record normalized utterance digests. The
   normalized digest is computed by
   Core.Agency.SemanticHoldout.utterance_digest/1, which trims edges,
   downcases and collapses whitespace. This does not catch paraphrases.
4. Copy benchmarks/semantic_holdout/manifest.template.json to a file under
   tmp/ (never commit private examples). Replace ALL placeholders with real
   data. The shipped template intentionally cannot pass validation.
5. Freeze, review and archive the original manifest. Record its canonical
   content seal separately from the file and before evaluating any models:

       mix run scripts/benchmark_semantic_holdout.exs \
         --manifest tmp/semantic_holdout.json --print-seal

6. Only after review/freeze, evaluate the unchanged manifest:

       mix run scripts/benchmark_semantic_holdout.exs \
         --manifest tmp/semantic_holdout.json \
         --seal YOUR_PREVIOUSLY_SAVED_64_CHARACTER_SHA256

The script calls Core.resolve_input/2 in production mode with response and
episode persistence disabled. It supplies only a sentence to the resolver.
The production cognitive stack may nevertheless emit telemetry or transient
state; it is not a proven process-isolated evaluator. For a rigorous blinded
study, replay frozen inputs in independent runtime instances.

Raw benchmark output is displayed in the terminal; avoid sharing private
sentences, reviewer IDs or session data inadvertently. The summary claims
agreement with **supplied** labels only. The harness itself will always
report external_reviewer_identity_verified?: false,
actual_training_isolation_verified?: false, and
semantic_truth_verified?: false.

## Manifest schema (v1)

- version: integer 1
- dataset_id: nonempty unique string (max 128 bytes)
- development_utterance_sha256: nonempty distinct array of valid lower-case
  SHA-256 digests of known development utterances, max 4096 entries
- cases: 1..128 cases; each has:
  - case_id: unique bounded string
  - sentence: exact input string
  - target_index: original zero-based unigram index
  - gold_sense_id: reviewed current-dictionary sense ID
  - split: "holdout" only
  - reviewers: exactly two maps with distinct reviewer_id values and
    equal sense_id values matching gold_sense_id

The harness binds labels to the returned unchanged sentence using
SemanticCorrectionEvaluator.utterance_sha256/1. A valid reviewer-declared
label is still rejected if it is not attested in that turn's current
dictionary cells. Grammar support without a uniquely attested alternative
does not count as a correction and is scored as abstention.

The baseline cohort counts label agreement whenever both the original LIFG
winner and the reviewed label are attested for that token in the current
dictionary. The separate correction-subset report uses only examples with
an eligible exact-sense alternative; all other cases count as abstentions
there. If no correction is comparable, status is :no_comparable_corrections,
not a success claim. The two denominators must never be conflated.

The initial correction benchmark evaluates only the narrow progressive
constructions already recognized by LexicalRoleShadow. Most general
word-sense cases will abstain on correction comparison; that is an honest
coverage limitation.

## Tests

    mix test \
      apps/core/test/core/agency/semantic_holdout_test.exs \
      apps/core/test/core/agency/semantic_correction_evaluator_test.exs \
      apps/core/test/core/agency/lexical_role_shadow_test.exs \
      --trace

## Future trustworthy metrics

After collecting a real dataset, publish:
- dataset and dictionary version digests, run commit and runtime environment;
- label coverage and unresolved reviews;
- original LIFG correct/incorrect/abstain counts on all labeled examples;
- proposed alternative correct/incorrect/abstain counts, including cases
  in which the original decision was already correct;
- wrong-to-right, right-to-wrong, both-wrong, net improvement;
- uncertainty estimates / repeated-run variance where relevant;
- full unchanged benchmark results after any attempted strategy adaptation.

**Learning remains prohibited at this stage.** A human-reviewed dataset
can support an engineering decision; it is not a machine consciousness test.
