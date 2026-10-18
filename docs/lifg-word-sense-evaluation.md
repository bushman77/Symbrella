# Evaluating live LIFG word-sense selection

Unit tests passing does **not** establish word-sense disambiguation accuracy.
A correct POS (noun/verb/etc.) is not a verified numbered dictionary sense,
and a high Stage1 softmax probability is not a calibrated correctness estimate.

## 1. Run current production selection on contrasting contexts

    git switch development
    git pull origin development
    mkdir -p tmp
    mix run scripts/benchmark_lifg_senses.exs \
      | tee tmp/lifg-sense-baseline.txt

The built-in contrast corpus covers **bank**, **bat**, **draw**, and **light**.
Each pair intentionally uses a different meaning of the same word. The script
runs `Core.resolve_input/2` in `:prod` mode with the usual lexicon stages
and the *configured* production grammar weight by default, without response
planning, user-episode writes, or session-scoped comprehension WM admission.
The benchmark sets `scores: :all` for observation only. The original version
incorrectly forced `grammar_weight: 1.0` without disclosing the difference.
To run that **separate experiment** for comparison, use:

    mix run scripts/benchmark_lifg_senses.exs --grammar-weight 1.0 \
      | tee tmp/lifg-sense-grammar1.txt

For one new sentence:

    mix run scripts/benchmark_lifg_senses.exs \
      --sentence "She went to the bank to deposit cash." --word bank

Compare:

- `stage1_winner`: Brain's raw selected dictionary sense
- `final_winner`: Core's final choice after grammar/tie-break/MWE routing
- `final_winner_definition`: definition from the **current DB**, not a guess
- `stage1_top_probability`, `stage1_margin`, `stage1_pos_mass`: per-token
  competitive scores, **not** probability that the choice is correct
- `Top scored dictionary senses`: real candidate IDs, definitions and
  separate deterministic ContextFit values, including large (>64) sense slates
  using a bounded, original Stage1 audit rather than Core's compact choice
- `full_score_map?`: whether the full distribution was stored in Core's final
  choice; **false does not mean candidates were unscored**
- `grammar_required_pos`: any signature-derived grammatical override;
  `mwe_covering_target` and `mwe_decisions`: why a unigram final choice may
  be absent
- `pos_accuracy_among_selected`: POS precision for actual final unigram
  choices; `all_cases_pos_and_selection_rate`: overall coverage plus correctness

A missing unigram final choice (e.g. MWE shadowing) is **ungradable** until
the specific token coverage is explained.

## Structural role competition (development)

`Core.Syntax.RoleCompetition.attach/1` runs immediately after sentence
signatures and before Stage1. The pure
`Brain.LIFG.RoleCompetition.resolve/1` adjudicates two carefully bounded
patterns using *independent* token, morphology, and dictionary POS witnesses:

- A complete determiner–adjective–noun group (`a light suitcase`) supports
  the **adjective** modifier of the full NP over incompatible overlapping
  shorter nominal hypotheses.
- A determiner–noun subject immediately preceding an attested finite-past
  predicate (`A bat flew`) supports the **noun** subject over a speculative
  separated verb-particle parse that mistakenly assigns `flew` as its object.

`Brain.LIFG.GrammarEvidence.apply_role_competition/3` adds bounded positive
support (`0.30` in pre-softmax score units) to existing dictionary candidates
in that POS only. It does not delete alternatives, invent senses, change
a dictionary record, or claim an exact lexical meaning is verified.
When no complete independent structural witness exists, it abstains.

The production benchmark prints `adjudicated_structural_pos`. Compare it
with `stage1_winner`, `final_winner`, and the actual dictionary definition.
The latter may remain wrong even after POS improves: e.g., grammar cannot on
its own distinguish the flying mammal sense of *bat* from its other noun senses,
or the low-weight sense of *light* from its other adjective senses.

Verify on your local Elixir runtime:

    mix test apps/brain/test/brain/lifg/role_competition_test.exs
    mix test apps/core/test/core/syntax/role_competition_test.exs
    mix test
    mix run scripts/benchmark_lifg_senses.exs \
      | tee tmp/lifg-sense-role-competition.txt

This only supports two witness patterns initially. Extend by writing a
contrasting structural counterexample first, not by adding unconditional
POS biases or treating generic signatures as verified parses.

## Two-tier sense computation (development)

A *supported* `role_competition` result can focus costly exact-sense
`ContextFit.score/3` evaluation on the corresponding dictionary POS.
Stage1 still computes basic numeric features, Cerebellum calibration,
grammar support, and a **single unchanged full-slate softmax** across
**every** original candidate. No unused candidate is removed, no
probabilities are renormalized across a smaller selection, and no
dictionary sense is marked verified merely because its POS matches.

`Brain.LIFG.SenseFocus` is fail-open:
- Only the previously audited full determiner–adjective–noun and
  determiner–noun-plus-attested-past-predicate frames can focus a token.
- Missing or conflicting roles, missing candidate POS, MWE tokens,
  same-POS-only slates and explicit opt-out run the original broad pass.
- During a focused pass, `SemanticControl` compares the *supported POS*
  instead of relying on a provisional top candidate of potentially wrong POS.
  Weak or tied context-fit signals continue to abstain.

The Stage1 decision and Core choice expose `sense_focus`, including
`mode`, `reason`, `focus_pos`, `context_evaluations` and
`context_skipped`. Stage1's audit aggregates `focused_tokens`,
`context_evaluations` and `context_skipped`. Counts represent context
function invocations avoided, **not** measured latency, memory savings,
or accuracy gains.

Evaluate focused selection against broad selection on the same corpus:

    mkdir -p tmp
    mix test apps/brain/test/brain/lifg/sense_focus_test.exs
    mix test
    mix run scripts/benchmark_lifg_senses.exs \
      | tee tmp/lifg-focused.txt
    mix run scripts/benchmark_lifg_senses.exs --no-sense-focus \
      | tee tmp/lifg-broad.txt

Compare the `final_winner`, numbered-sense definitions,
`adjudicated_structural_pos`, `context_focus`, and evaluation counts.
The returned probability distribution still covers all original senses.
An improvement in POS alone does not establish exact-sense accuracy;
assign independent human-reviewed `expected_id` labels before making
any such claim.

## MWE shadow boundary correction

The 2026-10-17 **broad** benchmark (focused computation disabled)
showed the financial `bank` sense before shadowing but no `bank`
afterwards, despite no accepted semantic MWE replacing it.
`Core.LIFG.Attach` had been discarding constituent unigrams when a
virtual phrase received a Stage1 lexical choice, **before**
`Core.MWE.Decisions` applied its source, span, grammatical-precedence,
and sentence-competition acceptance gates.

The LIFG attachment now retains all original `lifg_choices`. Only
`Core.MWE.Decisions.attach/1` decides which words an **accepted** phrase
replaces in its derived `semantic_choices` view. Rejected MWEs cannot
erase the original lexical words merely because they were proposed.
The diagnostic field `retained_words` makes this visible; legacy
`after_shadow` trace data is retained only for existing consumers.

Run:

    mix test apps/core/test/core/mwe/decisions_test.exs
    mix run scripts/benchmark_lifg_senses.exs \
      --sentence "I deposited cash at the bank." --word bank

The fix preserves grammatical provenance; it **does not itself prove**
that financial `bank|noun|1` was selected for the right semantic reason.

## 2. Establish human-reviewed labels before scoring exact-sense accuracy

Get real current sense IDs by reviewing the **full** dictionary inventory
for the target word and POS. For example, in IEx:

    Db.brain_cells_for_norm("bank")
    |> Enum.filter(&(&1.pos == "noun"))
    |> Enum.map(&{&1.id, &1.definition})
    |> IO.inspect(limit: :infinity, printable_limit: :infinity)

Write `tmp/lifg_gold.jsonl` locally. Each line is one JSON record with
`sentence`, `word`, `expected_pos`, `expected_id`, and optional `meaning`:

    {"sentence":"I deposited cash at the bank.","word":"bank","expected_pos":"noun","expected_id":"REPLACE_WITH_VERIFIED_DB_ID","meaning":"financial institution"}

Select the ID by **reviewing its actual definition**, never by assuming the
numbering from an earlier version, a random example, or a model-generated
label. If no unique dictionary sense fits, do not enter an `expected_id`;
report that example as unresolved rather than forcing a ground-truth label.

    mix run scripts/benchmark_lifg_senses.exs --gold tmp/lifg_gold.jsonl \
      | tee tmp/lifg-reviewed-baseline.txt

Only records with reviewed, existing IDs contribute to exact-sense accuracy.
Invalid IDs are explicitly flagged, and cases without gold IDs are **not
labeled**, not failures. POS accuracy is counted separately.

## 3. Test generalization and avoid circular evaluation

Use multiple contexts for each meaning; vary subject, object, punctuation,
and nearby words. Add negative controls where expected senses should change.
Repeat the same corpus after code changes and compare exact ID selection,
gold rank, candidate coverage, and POS. Keep the independently reviewed gold
file unchanged across versions.

Separate three questions:

1. **POS**: correct syntactic category?
2. **Sense**: correct dictionary ID *within that POS*?
3. **Provenance**: did Stage1, Core's tie-break, grammar, ATL, or another
   selection step change the winner? Inspect the raw/final difference.

A schema/projection test answers none of these by itself. A context-fit audit
measures the available lexical evidence but does not measure real Stage1
selection. The new benchmark observes *actual* runtime choices, but should
not be mistaken for a blinded test while human labels are unavailable.

This probe does not request LLM responses, store user episodes, or feed
supervised correctness labels to Cerebellum. It still exercises production
Brain runtime code and may emit ordinary telemetry/transient state.
