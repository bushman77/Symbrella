# Human-reviewed lexical sense evaluation — collecting real dictionary evidence

## Status

The real-dictionary review workflow is implemented, but no independent
reviewers have yet provided sense labels for this repository's current
database. Existing ExUnit fixtures remain synthetic. This workflow does not
turn those fixture results into real accuracy estimates.

Use the **current** dictionary, not guessed numerical suffixes. The
pre-existing \`scripts/benchmark_lifg_senses.exs\` examples (bank, bat,
draw, light) have already been used for development and must NOT be
represented as unseen test cases.

## 1. Author a new private batch of evaluation sentences

Copy the deliberately unfilled template:

    mkdir -p tmp
    cp benchmarks/semantic_holdout/review_cases.template.json \
      tmp/semantic_cases.json
    vim tmp/semantic_cases.json

Replace all placeholder values and create several genuinely unfamiliar
contrast cases. Each input is:

    {
      "dataset_id": "human-review-batch-001",
      "cases": [
        {
          "case_id": "unique-case-001",
          "sentence": "A new, untrained example sentence.",
          "target_index": 2,
          "target_word": "untrained"
        }
      ]
    }

target_index is **zero-based**; for initial preparation it is checked
against the whitespace-separated token at that position. The later runtime
benchmark separately checks the actual Core token index and will abstain if
the evidence does not match. Avoid MWE, quotation and unusual punctuation
when designing the first review cohort.

Do not write the correct sense in the case input. Do not show Symbrella's
LIFG predictions or diagnostic scores to reviewers. Avoid adding evaluation
sentences to the git repository or any development/training/test fixture.

## 2. Export the full dictionary inventory for blind review

    mix run scripts/prepare_semantic_reviews.exs \
      --cases tmp/semantic_cases.json \
      --out tmp/semantic_review_packet.json

This reads \`Db.brain_cells_for_norm/1\` for each target word and creates:
- \`tmp/semantic_review_packet.json\` — exact current sense IDs, POS,
  dictionary definitions, examples and status, sorted deterministically;
- \`tmp/semantic_review_packet.reviewer_a.blank.json\` — first empty sheet;
- \`tmp/semantic_review_packet.reviewer_b.blank.json\` — second empty sheet.

Output files are created with restricted permissions (0600) and are never
overwritten by these scripts. The packet includes no LIFG decisions,
candidate probabilities or correct-answer labels. Inventories with fewer
than two senses, duplicate IDs or more than 512 rows are rejected instead
of quietly truncating possible correct answers.

## 3. Collect two genuinely independent human reviews

Give the same original packet to two separate reviewers. Give each
reviewer **only their own** blank review sheet. Ask each to inspect the
sentence and all supplied dictionary definitions and write:
- their own distinct reviewer_id;
- the exact sense_id for *every* case, copied from the packet.

A filled sheet looks like:

    {
      "packet_sha256": "<EXACT_VALUE_IN_BLANK_SHEET>",
      "reviewer_id": "reviewer-a",
      "votes": [
        {"case_id": "unique-case-001", "sense_id": "<REAL_DICTIONARY_ID>"}
      ]
    }

The two reviewers should not see each other's votes or Symbrella's answers
before both submit. Save them locally as:
- \`tmp/reviewer_a.completed.json\`
- \`tmp/reviewer_b.completed.json\`

A reviewer must not invent a sense ID. If the sentence is truly ambiguous
or no sense fits, flag that issue for revision **before freezing**.
The v1 protocol rejects a batch containing missing or disagreeing votes
rather than silently removing difficult items and inflating accuracy.
Maintain an external attrition log and arrange resolution without
selecting cases based on Symbrella's predicted answer.

The code cannot establish that two identifiers correspond to two real
independent humans. That requires an external review process.

## 4. Declare previously used development utterances

Create \`tmp/semantic_development_hashes.json\` with the normalized SHA-256
digests of ALL known development/training utterances in scope. For each
one, calculate the digest in IEx or using:

    mix run -e 'IO.puts(Core.Agency.SemanticHoldout.utterance_digest("YOUR EXACT DEVELOPMENT SENTENCE"))'

The JSON file is:

    {
      "development_utterance_sha256": ["<64-character-lowercase-SHA256>"]
    }

This list cannot be empty. The code rejects identical normalized test
sentences appearing in the declared development set. It cannot detect
paraphrase leakage, incomplete source lists, shared prompt origins or
training-set contamination. Preserve a separate audit of dataset origins.

## 5. Freeze reviews and the precise dictionary snapshot

    mix run scripts/finalize_semantic_reviews.exs \
      --packet tmp/semantic_review_packet.json \
      --review-a tmp/reviewer_a.completed.json \
      --review-b tmp/reviewer_b.completed.json \
      --development tmp/semantic_development_hashes.json \
      --out tmp/semantic_holdout.json

This command checks that both signed-*by-declaration* sheets refer to
the original packet; checks completeness, agreement, and valid sense IDs;
rejects declared development overlap; and checks that the current
dictionary inventory is unchanged since the review packet was prepared.

It writes a version-1 holdout manifest accepted by SemanticHoldout. The
manifest includes:
- source review packet SHA-256;
- a per-case SHA-256 of the reviewed dictionary sense inventory;
- the two reviewer identifiers and agreeing exact sense IDs;
- development utterance digests and original cases.

Save the manifest's printed SHA-256 separately **before evaluation**.
These SHA-256 values are alteration detectors, not cryptographic reviewer
signatures, and cannot establish dataset authenticity.

## 6. Evaluate with unchanged LIFG

    mix run scripts/benchmark_semantic_holdout.exs \
      --manifest tmp/semantic_holdout.json \
      --seal YOUR_PREVIOUSLY_SAVED_MANIFEST_SHA256

The production benchmark script now **requires a frozen review_inventory**
and checks it against Db.BrainCell data *before* invoking the resolver. Any
definition/sense change aborts the run. The resolver gets only the sentence.

Read the two different denominators carefully:

1. cohort_baseline: original LIFG agreement with supplied labels across
   all cases whose current token/sense inventory is comparable.
2. correction_subset: comparison of exact, attested lexical alternatives
   for the narrow grammatical constructions currently supported by
   LexicalRoleShadow, including wrong-to-right, right-to-wrong,
   both-wrong and abstentions.

Most ordinary same-POS word-sense cases may have no supported alternative
and will therefore count as correction abstentions. The script explicitly
reports \`:no_comparable_corrections\` when appropriate, while retaining
the whole-cohort baseline metrics.

The process never trains from reviewer votes, changes LIFG selections,
modifies cognition or certifies semantic truth. It is evaluation, not
self-correction activation.

## Regression testing

    mix test \
      apps/core/test/core/agency/semantic_review_test.exs \
      apps/core/test/core/agency/semantic_holdout_test.exs \
      apps/core/test/core/agency/semantic_correction_evaluator_test.exs \
      apps/core/test/core/agency/lexical_role_accountability_test.exs \
      --trace

A successful software test run only checks the workflow's behavior.
A meaningful measured accuracy result requires independently sourced
and reviewed real examples, an immutable benchmark, and no test-driven
retuning before final measurement.
