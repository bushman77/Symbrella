# Context continuity and verified population reasoning

## Observed failure

The three-turn Kamloops / Maple Ridge diagnostic showed correct first-turn
distance and second-turn dual-population retrieval, but the third turn
("Which of those cities has the larger population?") lost both entities in
the WM comprehension frame and answered from prior LLM chat text without
structured evidence. This change closes both gaps.

## Changes

- The comprehension WM reference binder recognizes unambiguous plural
  demonstratives ("those cities", "these cities", "these places") and
  comparative follow-ups in addition to "each" and "both". Its frame
  builder reads event subjects as well as scalar subject and target.
  Unrelated new turns still displace the prior entities.
- The semantic event frame represents the two-place question as a
  population question with comparison :greater, two ordered subjects, and
  grounding_status :unverified. Recognition alone asserts no statistic.
- Core.Response.VerifiedPopulation records only provider-validated, dated,
  separately attributed population statements from the existing
  population_group retrieval result. It does not retain model replies or
  promote unsourced facts. Its owner-managed ETS table is scoped by an
  explicit session ID, holds at most four subjects for up to ten minutes,
  and is limited to 256 session records in this process.
- At response synthesis, Core.Response.PopulationComparison.recall looks
  for an exact same-session set of dated, source-supported subjects.
  A hit avoids redundant retrieval; missing or expired claims use the
  existing WebRetrieval path, not previous assistant messages.
- PopulationComparison.attach checks that both subjects are supported
  and have the same reporting year before computing greater-than,
  equal, and difference. Different years and missing figures are
  explicitly not comparable by this strict metric.
- ResponseFrame carries a typed comparison and separately sourced claims.
  Grounding recomputes the arithmetic and validates the claim names,
  years, figures, URLs, and derived winner before permitting an
  affirmative comparison. Reflection can replace an ungrounded model
  comparison with the deterministic, cited answer.

This is the first narrow evidence-backed comparison capability, not a
general-purpose inference engine. Historical dated counts are not presented
as current population estimates. Retained evidence and transient WM
comprehension are deliberately separate structures with different lifetimes.

## Validate on development

    cd ~/Symbrella
    git pull origin development
    mix test apps/core/test/core/response/population_comparison_test.exs
    mix test apps/core/test/core/brain/comprehension_wm_test.exs
    mix test apps/core/test/core/tools/plural_population_grounding_test.exs
    mix test

Then ask all three questions in the same live chat:

1. How far is Kamloops BC from Maple Ridge BC?
2. What are the populations of each?
3. Which of those cities has the larger population?

Expected third turn: the two active entities remain in WM, the event frame
contains comparison: :greater, the comparison has two sourced dated claims,
and the response identifies Kamloops as having the larger recorded
2021 population by 6,912 with both original Wikidata citations.

Inspect whether the third turn's evidence_origin is
:prior_verified_population and attempted_count is 0. If not, confirm
that WebRetrieval attempted fresh source retrieval and distinguish this
fallback from a cache hit. Absence of a current verified claim must never
be silently replaced by conversation history.
