# LIFG lexical sense contrast baseline — 2026-10-17

Origin: manual production-equivalent Termux output from
`scripts/benchmark_lifg_senses.exs`, **before any semantic scoring changes**.
The updated benchmark's original diagnostics did not show impacting grammar
signatures or accepted MWE spans; a later observation-only revision adds both.

## Execution

    mix run scripts/benchmark_lifg_senses.exs | tee tmp/lifg-sense-baseline.txt
    mix run scripts/benchmark_lifg_senses.exs --grammar-weight 1.0 \
      | tee tmp/lifg-sense-grammar1.txt

Both runs used `scores: :all`, `trace_word_scores?: true`, `mode: :prod`,
`persist_episodes: false`, and `comprehension_wm?: false`. The first used
configured grammar defaults; the second explicitly overrode grammar weight.
The tests were sequential, not randomized or repeated across run order.

## Observed winners

| Target sentence | Intended meaning | Configured default final | Grammar weight 1.0 final |
| --- | --- | --- | --- |
| I deposited cash at the bank. | financial institution | NO unigram; MWE `at the bank|phrase|1` appears | NO unigram; same MWE |
| We sat on the bank of the river. | river edge | `bank|noun|10` | `bank|noun|10` |
| A bat flew through the cave. | flying mammal | `bat|noun|20` (obsolete child's shoe) | `bat|verb|3` (to strike) |
| He swung the bat at the ball. | sporting equipment | `bat|noun|4` | `bat|noun|4` |
| They draw a picture. | produce a picture | `draw|verb|129` | `draw|verb|129` |
| They draw water from a well. | extract water | `draw|verb|113` (takes up water) | `draw|noun|13` (dry stream bed) |
| She carried a light suitcase. | not heavy | `light|noun|7` (facts, information) | `light|noun|7` |
| The light filled the room. | illumination | `light|noun|1` | `light|noun|1` |

### Raw numbers

Configured default: POS correct 6/7 final unigrams, one missing (75% across
all eight). Grammar weight 1.0: POS correct 4/7 final unigrams, one missing
(50% across all eight). **Neither is reviewed exact dictionary-sense accuracy**.
A correct POS does not imply the expected meaning; e.g. configured-default
`bat|noun|20` and `light|noun|7` are clearly wrong despite being nouns.

## Diagnostic clues requiring causal verification

1. The `bat` grammar override reported `grammar_required_pos: "verb"`
   under grammar weight 1.0. `Core.LIFG.SignatureRoles` can derive that
   from a separated `:verb_particle_construction` candidate when the
   target is the putative predicate; a speculative source dictionary POS
   alternative is **not proof** that the pattern is a complete true parse.
   Inspect the emitted signature before changing any gate.
2. `bank` has an MWE candidate `at the bank|phrase|1` which coexists
   with missing final `bank` unigram. The old
   `mwe_covering_target: []` read the post-run base tokens and could
   miss virtual MWE spans. Inspect `semantic_choices` and MWE promotion
   audit before claiming whether the MWE was accepted or what it means.
3. `draw` has 156 scored senses. Low individual top probability (~0.007)
   does not mean the word is unfamiliar; the configured run assigns about
   89% of aggregate probability to verb candidates and still has very
   weak separation among individual senses. A structural object-source
   compatibility audit may help select the actual extraction sense.
4. Multiple proper-noun and noun IDs have equal/near-equal competitive
   probabilities with zero context fit. The chosen numbered sense can be
   a tie artifact and should **not be promoted to verified meaning**.
5. `light suitcase` gives no discriminating `ContextFit` for weight;
   POS and lexical-sense certainty should be handled separately.

## Follow-up

- Observation-only benchmark now prints `grammar_signatures_for_target`
  and `mwe_semantic_choices_covering_target`, allowing direct diagnosis of
  specific override and MWE spans without further production change.
- Run each failure with and without `--grammar-weight 1.0`, inspect the
  exact source signature before constraining it.
- Obtain human-reviewed actual dictionary IDs and use `--gold` to compute
  real exact-sense accuracy. Do not use model-selected IDs as gold.
- Preserve existing correct `bank|noun|10`, `bat|noun|4`,
  `draw|verb|129`, `light|noun|1` behavior in future regression tests.

Nothing in this document constitutes a source-of-truth sense annotation,
new source evidence, or authorization to update dictionary rows.
