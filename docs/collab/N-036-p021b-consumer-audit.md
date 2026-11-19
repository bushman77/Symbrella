# N-036 — P-021b downstream consumer audit (READ-ONLY)

FROM: Claude · TO: Bradley, GPT6 · 2026-11-19 · Branch `symbolic_generation` @ `6f608d1e`
Evidence tags: **[V]** read in source this session · **[H]** hypothesis · **[U]** unknown.
Nothing here was executed (no Elixir runtime). No production code, test or fixture touched.

Purpose: N-035 §4.6 required an audit *before* a new proof kind is admitted. This closes it, and
**corrects N-035**: my §4.6 list named four consumers; a repo-wide search finds ~19 non-test files that
touch `Composition.verified/derive`, `compositional_evidence` or the two proof kinds. Most are harmless
closed matchers. Two are not, and one Colony path would *regress* if I had followed N-035 literally.

## 1. Method

`grep -rnE "Composition\.(verified|derive)|compositional_(evidence|derivation)|:nominal_property|:progressive_event_property|SentenceMeaning\.build"` over `apps/**` excluding tests, then read every hit in context, then read the Colony/Web consumers of `reported_proposition`.

## 2. Findings by class

### Class A — closed `kind` matchers (an unknown kind falls through) [V]
| File:line | Behaviour on a new proof kind |
|---|---|
| `response/symbolic_generation.ex:224-236, 645-661` | realizers require `kind: :nominal_property` / `:progressive_event_property` → `:not_applicable` |
| `response/semantic_strategy.ex:259-303` | plan clauses match the two kinds → no plan |
| `discourse/ledger.ex:923-1033` (`ordinary_semantic_record`) | two kind clauses, then `maybe_finite_past_record` → `core` = `:uninterpreted` |
| `brain/wm/comprehension.ex:321` (`nominal_focus_from`) | requires `:nominal_property` + `:stacked_adjective_noun` |
| `response/attention_fallback.ex:238`, `response/projected_expression.ex`, `response/attach.ex:169` | key on specific meaning kinds or merely copy the SI field |

Verdict: safe by construction. **Required test:** one table test feeding a synthetic new-kind proof through each matcher and asserting no plan, no focus, no ledger record beyond `:uninterpreted`.

### Class B — any-kind consumers (need an explicit decision) [V]
1. **`semantic/sentence_meaning.ex:137-150` `attach_compositional_evidence/2`** copies *any* `{:ok, proof}` from `Composition.verified/1` onto `meaning.compositional_evidence` (`kind, span, resulting_type, proof_steps, event_semantics (nil default), formally_verified?: true, factual_authority?: false`). A new kind would be annotated with `formally_verified?: true`. Inert today (all readers match the two known kinds), but "verified" on an external-source meaning is a label future code could trust. **Decision for P-021b:** do not widen it. Either (a) keep Colony proofs out of `Composition.verified/1` and expose them via a separate `AttributedPredication.verified/1` (my recommendation; keeps SentenceMeaning unchanged exactly as N-035 §4.5), or (b) add `source`/`provenance` to the evidence map. Option (a) removes this class entirely.
2. **`agency/lexical_role_shadow.ex:121`** `formal_proof? = match?({:ok, _}, Composition.verified(si))` → `formal_derivation_verified?: true` for *any* kind. **Pre-existing imprecision [H→V on reading]:** the flag is true if *any* composition verifies, regardless of whether it concerns the construction being evaluated. Only attached in chat (`core.ex:249`), which the `:colony_external` gate never reaches, so no exposure from P-021a/b. Recommend a one-line tightening (`proof.kind in [..known..]`) in a separate ticket; **not** required for P-021.

### Class C — rebuild-equality guards (safe, but impose determinism) [V]
`integration/audit.ex:83` (`meaning == SentenceMeaning.build(si)`), `brain/synaptic_experience.ex:22-23`, `brain/semantic_propagation.ex:116`, `intent/structural_statement.ex:26`, `response/projected_expression.ex:35` all re-run `SentenceMeaning.build/1` and then match `kind: :participant_assertion` / `:wh_object_question`. A new kind cannot satisfy those matches → `:ineligible`. **Constraint inherited:** any new derivation must be pure and deterministic, with no timestamps, refs or map-order dependence, because `==` on the rebuilt map is the guard.

### Class D — Colony consumers (the important one) [V]
- **`social/reported_proposition.ex:77-79`**: `nil -> DeclarativeProposition.interpret(si)` is the extension point; a third fallback (`AttributedPredication`) belongs after Declarative abstains, as in N-035 §4.5.
- **`social/colony_cognition.ex:35-37`** `interpret/2` runs `claim_for_title/2` before building any frame. For any title that is not the legacy counted-link regex or one of seven closed constructions, `claim_for_title` returns `{:error, :unresolved_colony_evidence_relation}` → **no frame, no draft, no prompt**. So a new proposition is **observability-only** until `ColonyCognition` is deliberately re-specified. That is the correct boundary ("understood" ≠ "reply") and P-021 must not widen it.
- **`colony_cognition.ex:180-206` `reconcile_proposition/2` — regression hazard.** Its second clause matches *any* `status: :attributed_structural_interpretation` whose `embedded_relation` ≠ claim relation and returns `{:error, :colony_proposition_mismatch}`. Today a legacy counted title that Reported/Declarative abstain on yields `:not_interpreted` → `_ -> :ok` and proceeds. If the new fallback interprets such a title and reuses that status atom, `embedded_relation` is `nil` → **mismatch → a draft that works today would be refused.** N-035 §4.5 did not say this. **Rule:** the new proposition must use a **distinct status** (suggest `:attributed_predication`), and a regression test must assert the legacy counted-title flow is byte-identical.
- **`colony_cognition.ex:76-79` `prompt_frame/1`** interpolates `inspect(Map.take(reported_proposition, [:subject, :predicate, :complement, :object, ...]))` into the **model-expression prompt**. Not reachable for a new kind today (gate above), but if it ever is, fields must be witnessed lemma binaries passing a `safe_lemma?`-style check (`\A\p{L}[\p{L}\p{M}'-]*\z`, ≤40 bytes, like `brain/wm/comprehension.ex:359`), never raw spans. Record as an invariant now.
- **`social/colony_draft_policy.ex:173+`** guards match closed `construction` atoms → ignore new kinds.
- **Web (read-only displays):** `social_understanding.ex:73-76` counts `status == :attributed_structural_interpretation` (a distinct status will *not* be counted → needs an intentional counter if wanted); `social_evaluation.ex:253-275` uses an allow-list of meaning statuses (unknown → `:unclassified`); `social_discourse.ex:19-40` keys on `sentence_meaning.status`; `social_live.ex:1085` `inspect`s the proposition (display only). Telemetry in `colony_conversation.ex:455,487,519` copies `proposition_status` — a new atom is fine.

### Class E — chat path (must stay byte-identical) [V]
`Signatures.detect/1` is called from chat (`core.ex:184`) and the probe (`semantic_probe.ex:89`). P-021a's `source: :colony_external` gate inside `detect/1` therefore protects every Class A/B/C consumer; the chat differential test in N-035 §3.7 is the load-bearing proof and must run first.

## 3. Updated admission checklist for P-021b (supersedes N-035 §4.6)
1. New status atom, not `:attributed_structural_interpretation`; test that the legacy counted-title Colony flow is unchanged (Class D hazard).
2. Proof exposed via its own `verified/1`; `Composition.verified/1`, `Composition.derive/1` and `SentenceMeaning` untouched (kills B1).
3. Fields restricted to witnessed lemma binaries; no atoms from external text; no raw spans.
4. Pure/deterministic output (Class C equality).
5. Table test: synthetic new-kind proof → no plan/focus/ledger/realization in Class A.
6. `ColonyCognition`, `ColonyDraftPolicy` and the model prompt remain unreachable for the new kind; widening them is a separate, separately approved ticket.
7. If Bradley wants the improvement *visible*, a separate tiny UI ticket adds a counter for the new status (`social_understanding.ex`); otherwise it appears only in the probe result.

## 4. Where I disagree with my own N-035
- §4.5 under-specified the status atom (Class D hazard). Corrected above.
- §4.6 listed 4 consumers; there are 19 files. Corrected above.
- N-035 §4.2 proposed a new `{:kind_predication, ...}` Expression. That still holds, but item 2 above means it should live behind `AttributedPredication`, not inside `Composition.derive/1`, so none of the Class A/B consumers can observe it.

## 5. Unverified
Everything is source reading. I did not run the umbrella, did not confirm that no *test* asserts the exact set of proof kinds (13 test files reference these symbols; not read), and did not trace `Core.Pipeline` for other implicit consumers beyond the grep. [U]
