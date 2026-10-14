# Proving why a dictionary MWE becomes canonical

This diagnostic examines the real production pipeline without changing
the lexical database, scoring weights, grammar signatures, or selection.

Target input: What do you know about Japan's railgun development?

## Run in Termux

From the repo root:

    mix run scripts/trace_question_mwe_promotion.exs

Compare with explicit grammar weight 1.0 (optional second production run):

    MWE_COMPARE_GRAMMAR=1 mix run scripts/trace_question_mwe_promotion.exs

These call Core.resolve_input/2 in production mode. Normal cognitive
state, episodic memory, or mood side effects are possible. The session ID
is diagnostic; no LLM, physical device operation, or web query is invoked.

## Evidence supplied

- Token cover: whether a dictionary-backed phrase was chosen over base tokens.
- Exact lexicon senses: real definitions and independent ContextFit.explain/3
  scores; these are not the same as LIFG's final selection score.
- mwe_lifg_score_audit: raw Brain.LIFG winner, available raw candidate
  scores, and Core.LIFG.Attach's post-selection winner.
- mwe_promotion_audit: exact lexical gate booleans, grammar-conflict
  booleans, and actual promotion into semantic_choices.
- Sentence signatures: all hypotheses available before LIFG.
- ATL: grammar roles resolved or unresolved after MWE promotion.
- Final canonical choice: whether the phrase survived all stages.

If lexical_gate_passed? is true, question_conflict? false, and
promoted? true, yet an unrelated ATL hypothesis remains unresolved,
that proves the promotion is not currently revised based on ATL's outcome.
It does not prove the ATL hypothesis itself is correct.

## Scope

Trace is opt-in through lifg_opts: [trace_mwe_promotion?: true].
No scoring or semantic decisions use these trace events. No iteration
or recursive reconsideration is introduced. The evidence is bounded.

Run characterization tests:

    mix test apps/core/test/core/mwe/decisions_test.exs

The synthetic tests expose the mechanism; the production script verifies
which real dictionary senses and LIFG scores are actually selected on Termux.
