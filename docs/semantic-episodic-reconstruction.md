# Semantic projection and historical reconstruction audit

The generalized event contract now has a limited read-only semantic bridge:
Core.Semantic.EpisodicProjection runs after sentence-level interpretation and
projects a *resolved* :obligation_event into a user-reported
entity=me/relation=has_obligation event. Projection requires speaker ownership,
recognized first-person surface form, no unresolved complement roles,
confidence >= 0.8 and no interrogative text. The existing
:health_support_event and geography question frames are deliberately excluded.

The projection is attached to SemanticInput, carried through
Core.Response.Attach and persisted by SideEffects.record_turn/4. The write
boundary re-derives each projection from the original SI and confirms that its
sentence matches the user's original turn. Stored meta includes source_frame
and frame_confidence. The episode is still **user-reported**, not verified.

This is a narrow first bridge from existing symbolic grammar. It does **not**
mean arbitrary sentence meanings, predictions or hidden LLM conclusions can
be persisted as event facts.

## Historical audit: no database writes

    mix run scripts/audit_episodic_reconstruction.exs --limit 100
    mix run scripts/audit_episodic_reconstruction.exs --limit 1000 --include-legacy --output tmp/episodic_audit.jsonl

Rows with role=user and suitable source have attribution=:attributed_user.
Old episodes where role is null are marked :unattributed_legacy and omitted
unless --include-legacy is explicitly provided; even then they are review
candidates only. Assistant/tool rows and already structured memories are
excluded from extraction.

The audit emits episode ID, original timestamp, original session, source
attribution, candidate relation/value and write_authorized?=false. It does not
infer actor identity from the text, automatically trust legacy role=nil rows,
backdate claims or let a historical reprocessing insert supersede a newer
live memory. A separate human-reviewed backfill design is required first.

## Verification

    mix ecto.migrate
    mix test apps/core/test/core/semantic/episodic_projection_test.exs
    mix test apps/core/test/core/episodic/reconstruction_test.exs
    mix test
