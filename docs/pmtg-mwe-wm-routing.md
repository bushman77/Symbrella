# PMTG MWE overlap and working-memory routing diagnostic

## PMTG: eliminate redundant MWE constituents

The October 16 Brain Diagnostics file showed nine PMTG queries for a distance
question: `how`, `how far`, `far`, `kamloops`, first `bc`, `maple`,
`maple ridge`, `ridge`, second `bc`.

`Brain.PMTG.Overlap` now prunes *only the query plan*: it keeps a recognized
multiword phrase with a real, matching lexical winner, and suppresses the
individual queried unigrams whose byte spans sit wholly inside that phrase.
It does **not** change LIFG interpretation, candidate scores, or content words
outside the phrase. Separate occurrences, including both `bc` tokens, remain
independent. Explicitly vetoed or otherwise disputed choices still get their
own retrieval request. Fallback/MWE guesses without a matching winner or valid
span never suppress components.

For that recorded distance question, the expected plan is five lookups:
`how far`, `kamloops`, first `bc`, `maple ridge`, second `bc`.
The planner trace includes `suppressed_overlap_count` and
`suppressed_token_indices`. Treat this as a test expectation pending an
independent runtime diagnostics export, **not** a measured performance claim.

## Why geographic questions leave working memory empty

The normal Core pipeline invokes `Core.LIFG.Attach`, which runs LIFG to
interpret the user's sentence and attaches `lifg_choices` to SemanticInput.
That is **not** equivalent to submitting those choices through
`Brain.focus/2`.

At the end of the Core pipeline, `Core.Brain.WM.focus_prompt_topics/2`
currently selects only a small fixed set of brain-related topics such as
`working memory`, `lifg`, `hippocampus`, and `symbrella`. Population and
distance questions do not match this list, so **no admission is attempted**.
The observed `wm: []`, `wm_updates: 0`, and `wm_last_ms: nil` are therefore
consistent with the current routing; they do not demonstrate that
`lifg_min_score: 0.35` rejected the LIFG winners.

The `:wm_focus` SemanticInput trace now distinguishes:

- `:none` / `:no_configured_prompt_topic`: no candidate submitted, no gate
- `:focused` / `:prompt_topic_focus`: `Brain.focus/2` returned WM contents
- `:unavailable`: focus was attempted but Brain did not return a WM list
- `:disabled`: the caller opted out of topic focusing

The trace includes `lifg_choices_available`, `candidate_count`,
`lifg_winners_submitted?` (currently false for the topic-only bridge),
`focus_attempted?`, and `wm_size_after` where available.
Telemetry `[:core, :wm, :focus]` records aggregate counts and routing reason;
it does not copy private question text.

These changes **do not** weaken the BasalGanglia evidence floor or auto-admit
all low-confidence LIFG winners. A future explicit WM-admission milestone
should select bounded, relevant content candidates, submit them to the
canonical `Brain.WM.Admission` gate, and test both admission and decay.
Changing the score threshold first would be treating a symptom without a
demonstrated rejection.

## Verification

```sh
git pull origin development
mix test apps/brain/test/brain/pmtg_overlap_test.exs
mix test apps/core/test/core/brain/wm_trace_test.exs
mix test
```

Ask the same distance/population questions after restarting Phoenix and export
a fresh Brain Diagnostics report. Verify PMTG's current query list and whether
Core's `:wm_focus` trace says `:none` versus `:focused`. A snapshot of
`Brain.snapshot_wm()` alone cannot establish the cause of empty WM.
