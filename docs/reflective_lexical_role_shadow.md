# Reflective Agency: lexical-role reconsideration (shadow)

## Goal

Move the formal-repair experiment toward actual LIFG outputs without allowing
an introspective hypothesis to replace a word sense or assert an event.

Core.Agency.LexicalRoleShadow is a diagnostic observer, not a reranker.
It consumes the original SemanticInput *after* Core's structural competition
and typed composition stages.

## Evidence requirements

- StructuralCompetition.verified/1 must revalidate the stored preferred
  construction against the current original tokens and sentence signatures.
- Only the existing progressive directional and progressive locative
  constructions are in scope.
- Diagnosis.blocked_roles/1 identifies the specific selected lexical role
  mismatch from real LIFG choices.
- RoleWitness.resolve/4 may return an alternative only when the candidate
  was independently scored on this turn, attested in the current active
  dictionary rows, and morphology-compatible for the role.
- An alternative is **provisional role/POS compatibility only**. It does not
  establish a dictionary sense, lexical meaning, event occurrence, speaker
  intent, or world truth. Tied sense alternatives abstain.
- Composition.verified/1 independently checks any already-attached typed
  formal derivation. Formal proof validity does not establish lexical truth.

Reports distinguish:
- :no_conflict_detected — the selected roles have no detected blockers.
- :provisional_role_alternatives — every blocked role has a scored, attested
  construction-role witness, but the original LIFG winner is unchanged.
- :partial_role_alternatives — at least one role has a witness and at least
  one remains unresolved.
- :abstained — missing or untrustworthy evidence, or no admissible witness.

## Pipeline integration

The Core phase-five pipeline now has a disabled-by-default diagnostic hook:

    |> Core.Semantic.Composition.attach()
    |> Core.Agency.LexicalRoleShadow.attach(opts)

To enable this for a specific production or test call:

    Core.resolve_input(sentence, reflective_role_shadow?: true)

The hook appends only a bounded :reflective_role_shadow trace event with role
positions, candidate POS, selection basis, and verification booleans. Neither
sentence text nor dictionary IDs go into that trace.

Calling LexicalRoleShadow.evaluate(si) directly provides a fuller in-memory
diagnostic report for troubleshooting, including candidate IDs and scores.
There is no persistence, weight update, LIFG re-selection, command execution,
working-memory admission, or new response authorization.

## Run tests

    mix test \
      apps/core/test/core/agency/lexical_role_shadow_test.exs \
      apps/core/test/core/semantic/composition_event_test.exs \
      apps/core/test/core/agency/reflective_correction_test.exs \
      --trace

## Limits and next experiment

This is a role-evidence audit: it can show that the winning lexical POS conflicts
with a supported grammatical construction while an alternative POS has
independently scored and attested support. That is not proof the LIFG
interpretation was factually wrong or that the alternative sense is correct.

Next, prepare independently labeled, held-out utterances and compare an
unchanged baseline against hypothetical candidate strategies using evidence
outside the candidate's self-report. Count abstentions and regressions, not
just cases that become formally composable. Keep actual behavior untouched
until verified generalization is demonstrated.
