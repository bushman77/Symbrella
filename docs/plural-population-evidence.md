# Plural population evidence from comprehension-driven WM

## Motivation

A live Brain Diagnostics export showed that the follow-up
"What are the populations of each?" was successfully resolved by the previous
same-session working-memory frame to Kamloops BC and Maple Ridge BC. However,
the response stage still used a scalar subject, produced no sourced evidence,
and the reflection pass answered "Which place are you referring to?".

This change extends the existing narrow population evidence path. It does
not alter WM admission, LIFG meanings, or the source-validation rules for
single-subject population questions.

## Retrieval and response contract

- Core.Semantic.EventFrames emits a population location question with
  subjects: ["Kamloops BC", "Maple Ridge BC"] and grounding_status: :unverified.
- Core.Tools.WebRetrieval.RequestRouter recognizes the plural population
  frame as geographic_population, without requiring a scalar subject.
- Core.Tools.EvidenceResolver independently calls its existing single-place
  population flow for each named place (up to four), preserving every attempt.
  The structured dated Wikidata statement remains the first strategy, followed
  by the existing Wikipedia search fallback when needed.
- The group support kind is :population_group and has an ordered results
  array. Every entry has its own subject and :source_supported or :unavailable
  status; supported entries carry their own positive integer value, source URL,
  and date. Missing and undated values are not promoted. A provider response
  whose subject conflicts with the requested place is rejected.
- Group result resolution is :source_supported for all successes,
  :partial_support for partial success, and :exhausted if none are supported.
  Group provenance does not imply independent corroboration between Wikimedia
  sites. Each population claim is an individually dated attributed statement.
- Core.Response.ResponseFrame retains subjects and renders each verified
  claim, its own year, and its own citation, plus unavailable subjects.
- Core.Tools.WebRetrieval.Grounding produces a deterministic answer from the
  group contract and rejects competing/invented values and dates. The reflection
  stage uses it to repair unsupported LLM drafts, even if the draft mistakenly
  asks which city was meant. Without retrieval, it can still name the known
  referents rather than asking a redundant clarification.

This is a limited group-answer pathway for population, not arbitrary
multi-entity research or general natural-language inference. Population counts
must not be described as necessarily current or assumed to use the same
reporting year. Partial results never borrow a value or citation from another
subject.

## Verification

    cd ~/Symbrella
    git pull origin development
    mix test apps/core/test/core/tools/plural_population_grounding_test.exs
    mix test apps/core/test/core/tools/evidence_resolver_test.exs
    mix test apps/core/test/core/response/response_frame_test.exs
    mix test

In the same web chat session:

1. How far is Kamloops BC from Maple Ridge BC?
2. What are the populations of each?

Expected: no "Which place?" fallback. Each supported place gets its own
dated figure and source URL; unsuccessful lookups are reported by name.

Export a Brain Diagnostics report and confirm
web_retrieval.support.kind == :population_group and the ordered results
match the two referenced places. Check the response_frame evidence and
reflection outcome separately. The prior WM session frame should still
record both places; this change leaves comprehension-WM admission untouched.

The original wm_last_ms discrepancy remains a separate instrumentation task.
