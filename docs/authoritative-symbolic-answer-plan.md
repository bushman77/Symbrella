# Authoritative symbolic answer planning

## Why this change

Live diagnostics after the three-turn Kamloops / Maple Ridge conversation
showed correct comprehension, verified evidence retention and a correct
symbolic population comparison. The free-form LLM nevertheless initially
identified Maple Ridge as the winner, contradicting the verified figures.
Reflection repaired the output. Verification should determine the final
answer *before*, not only *after*, language generation.

## Authority boundary

1. The previous population retrieval stores separately attributed dated
   claims in the same-session, bounded VerifiedPopulation ledger.
2. PopulationComparison restores the claims and produces a typed,
   source-supported comparison. It checks subject identity, same-year
   values, and the arithmetic.
3. ResponseFrame carries the verified conclusion. Grounding independently
   revalidates the source links, dates, winner, loser and numerical
   difference. It produces two natural-language renderings, **brief**
   and **detailed**, with the required figures and citations.
4. AuthoritativeAnswerPlan builds only if both typed evidence and the
   Grounding verification succeed. The plan is immutable per turn from the
   perspective of the LLM.
5. LlmSynthesis invokes this plan path *before* normal free-form drafting.
   In the default configuration, Core immediately chooses the brief
   rendering with no LLM call. Optional LLM involvement is restricted
   to choosing one of two exact style identifiers. A model output
   containing any other text, numbers or instructions is ignored.
6. ReflectionLoop still checks the prepared reply. If a downstream
   operation changes it outside the exact allowlist, Core publishes the
   authorized detailed fallback instead. The public Response planner
   records response_source: :symbolic_verified and cannot append a
   curiosity prompt to that conclusion.
7. Response completion telemetry includes
   reflection.authoritative_plan: status, outcome, style, style source,
   source count and fallback indicator.

## Optional style selector

By default the verified brief response requires **no LLM generation**.
If the model should choose a presentation style, enable the option inside
the existing :core/:llm_synthesis configuration:

    authoritative_style_selection_enabled?: true

The model can output only BRIEF or DETAILED. It never writes the
numerical answer or its citations. Invalid model output selects Core's
brief default. Even if the model process is offline, a fully verified
symbolic comparison can still be answered; other questions keep their
original model availability behavior.

## Safety scope

This authoritativeness is deliberately limited to two-place population
comparisons with individually source-supported counts for the same
measurement year. The semantic question frame alone is NOT evidence.

No authoritative plan is produced for:
- unverified or source-mismatched figures;
- missing one of the two required counts;
- different reference years without a comparable time-aligned result;
- a comparison conclusion whose winner or difference disagrees with
  the stored numbers; or
- unrelated general conversation.

For those cases, the existing evidence and response paths still apply.
The new renderer is a closed collection of Core-owned natural phrasings,
not a general unconstrained paraphrase verifier.

## Test commands

    cd ~/Symbrella
    git pull origin development
    mix test apps/core/test/core/response/authoritative_answer_plan_test.exs
    mix test apps/core/test/core/response/population_comparison_test.exs
    mix test apps/core/test/core/response/response_frame_test.exs
    mix test apps/core/test/core/response/llm_route_flow_test.exs
    mix test

In a single live session, repeat:
1. How far is Kamloops BC from Maple Ridge BC?
2. What are the populations of each?
3. Which of those cities has the larger population?

The third response should be Core-authored with a verified winner,
difference, reporting year and both source citations, without a free-form
factual LLM draft or a model-contradiction repair. Inspect the final
reflection.authoritative_plan in the exported response-complete event.
