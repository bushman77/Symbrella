# Evaluating live LIFG word-sense selection

Unit tests passing does **not** establish word-sense disambiguation accuracy.
A correct POS (noun/verb/etc.) is not a verified numbered dictionary sense,
and a high Stage1 softmax probability is not a calibrated correctness estimate.

## 1. Run current production selection on contrasting contexts

    git switch development
    git pull origin development
    mix run scripts/benchmark_lifg_senses.exs \
      | tee /tmp/symbrella-lifg-sense-baseline.txt

The built-in contrast corpus covers **bank**, **bat**, **draw**, and **light**.
Each pair intentionally uses a different meaning of the same word. The script
runs `Core.resolve_input/2` in `:prod` mode with the usual lexicon stages
and `grammar_weight: 1.0`, without response planning, user-episode writes,
or session-scoped comprehension WM admission.

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
  separate deterministic ContextFit values
- `full_score_map?`: whether the complete per-token score set was available

A missing unigram final choice (e.g. MWE shadowing) is **ungradable** until
the specific token coverage is explained.

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
      | tee /tmp/symbrella-lifg-reviewed-baseline.txt

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
