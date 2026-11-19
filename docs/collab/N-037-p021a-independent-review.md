# N-037 — P-021a independent implementation review (READ-ONLY)

FROM: Claude (independent reviewer) · TO: Bradley, GPT6 · 2026-11-19 · Branch `symbolic_generation` @ `4fca72ed`
Reviewed: diff `55d4823d..4fca72ed` — `syntax/chart.ex` (570 lines), `syntax/grammar.ex` (74), `syntax/signatures.ex` (`detect/1`, `chart_nodes/1`), `chart_test.exs`, `chart_dev_corpus_test.exs`; cross-checked against unchanged `Brain.LIFG.StructuralCompetition`, `Core.Syntax.StructuralCompetition`, `Brain.ATL.Constituents`, `SentenceMeaning`, `SemanticProbe`.

**Evidence status.** Everything below is **READ-ONLY source review. I ran no Elixir (no runtime here).** The only executed evidence is Bradley's local run: 53 passed / 0 failed for the two chart test files, exit 0. Tags: **[V]** read in source, **[H]** hypothesis, **[U]** unknown. Every counterexample below is a *reasoned trace through the code*, not an observed failure; each ships as a proposed test so the claim can be proved or refuted by running it.
I did not read, request or use the sealed set; nothing below is derived from it.

## 0. Verdict: CONDITIONAL GO

The architecture is the right one and is implemented with real discipline. It is **not yet safe to run the sealed set**, because I found two classes of *unsupported-promotion* defects that the visible DEV corpus cannot expose, plus an integration limit and a vacuous isolation test.

> *Has P-021a established a safe, deterministic, generalizable foundation, or a parser that fits the visible corpus?* — **Structurally the former, empirically not yet proven.** Determinism, budget fail-closure, no-atom-creation, Brain integration and attachment-ambiguity handling hold up under independent reading. But the 36 DEV rows pass against a lexicon that GPT6 built from the DEV words, and the failures I found live precisely in what DEV does not sample: lexical negation, determiner/quantifier agreement, polysemous real-LTM words, and a long-sentence truncation inside Brain.

## 1. Verified strengths [V]

1. **Brain integration is real.** The emitted signature satisfies `complete_clause?/2` second clause (`brain/.../structural_competition.ex:122-135`): id `:copular_predication` is not a constituent id, evidence non-empty, all roles integers ≤ last, `:predicate` + `:subject_head` present. `chart_test.exs` drives the *unchanged* `resolve/1` and gets `:preferred`.
2. **Token alignment matches Brain exactly**, including the `n: nil` divergence I flagged in N-033: `chart.ex:150-168` reimplements `base_indices/1` (`brain:76-89`) and falls to `:coverage_gap` on any mismatch.
3. **Fail-closed budgets.** Per-span (`close/2`, `chart.ex:256-273`), total (`:241`), result (`:86`) and token caps (`:125`) all return *total abstention*, never a truncated parse. Unary closure is depth-bounded (≤2 real; loop cap 6) and checked each pass. Join work is bounded because the 400-item total aborts first. [H on wall-clock; no benchmark.]
4. **Determinism.** Items sorted by a total key (`item_key/1`), signatures by `signature_sort_key/1`, no map-order dependence in anything that is emitted. [V]
5. **No atom creation.** `grep to_atom|binary_to_atom|to_existing` over `chart.ex`/`grammar.ex`: no matches. Role/feature keys are fixed module attributes; indices only.
6. **Output carries no surface text.** `signature/1` (`chart.ex:537-560`) emits id, integer roles, feature atoms, evidence atoms, `factual_authority?: false`. No agent/action/tool field; raw tokens never enter the signature, which keeps the model-prompt path (N-036 §D) clean for any later consumer.
7. **Attachment ambiguity survives.** Nested vs flat PP attachment produce distinct role keys (`chart.ex:481-535`). Both parses cover the same tokens, so Brain's `lexical_fit` is identical and the margin is 0 < 0.12 → `:unresolved` (`brain:223-233`). The test uses the real `resolve/1`.
8. **No other consumer mishandles the new id.** `Brain.ATL.Constituents.valid_signature?` is a closed `id in @ids` allow-list (ignored); `SentenceCompetition` filters `complete_topic_question?`; `RoleCompetition` does not read signatures. [V]
9. **Chat isolation by construction.** One gate, `signatures.ex:112`, `existing` computed exactly as before. [V static] — but see M3: the *executable* proof is vacuous.
10. **Rejection-precedence fix (`ee57cd3a`) does not weaken rejection.** In `diagnose/4` a non-nil reason from either `rejection_reason/4` or `valid_original_tokens?` abstains (`chart.ex:55-69`); reordering only changes *which label* a rejected input gets. No path exists from a rejection reason to emission. [V]

## 2. Findings

### BLOCKER — fix before any sealed run

**B1. Lexical negation is a closed surface list; polarity is then stamped positive.**
- `grammar.ex:12` `@negative_words` lists `not never no` and contractions only. `chart.ex:138` rejects only those; `chart.ex:541` unconditionally adds evidence `:positive_polarity`.
- Counterexample (traced, unrun): LTM rows `nothing`/noun, `is`/auxiliary, `ready`/adjective. `"Nothing is ready."` → terminal `noun` → `nominal` → `subject` (singular) + copula + complement → `:sentence`, no cue fires → `:copular_predication` with `:positive_polarity`. Same shape for `nobody`, and for `none` where LTM gives it a noun row; via `predicate_adverb` for `hardly/barely/scarcely/rarely/seldom`. [V code path; H on whether the live LTM carries those POS rows — the Wiktionary-derived rows very likely do.]
- Why it matters: a *negated* sentence is promoted as L1 with an explicit false polarity label. This is the exact failure the "polarity" requirement (N-035 §3.4, brief Task 2.6) exists to prevent, and P-021b would inherit the label.
- Smallest correction: extend the closed cue list (`none nothing nobody nowhere neither nor hardly barely scarcely rarely seldom without`) and rename the evidence atom to what is actually established, `:no_negation_cue_in_closed_list`.
- FileScope: fits (`grammar.ex`, `chart.ex`, tests).

**B2. Agreement is incomplete in three places; each promotes an ungrammatical clause.**
- (a) `grammar.ex:17,19` put `i` in `@singular_pronouns` and `"is" => :singular`, so `"I is ready."` passes `agreement?/2` (`chart.ex:479`).
- (b) Determiner–noun number is never checked: `combine(:determined_subject)` (`chart.ex:391-394`) takes the noun's number only. `"A pages are ready."`, `"These site is ready."`, `"This pages are open."` all emit.
- (c) Quantifier–noun number is never checked, and the quantifier terminal requires **no dictionary POS at all** (`chart.ex:309`, list membership only): `"Each pages are ready."`, `"Several site is ready."` emit. The quantifier role also has no `required_features` entry, so Brain never examines it (`chart.ex:405-407`).
- Why it matters: brief Task 2.6 names subject–verb agreement; (a)–(c) are unsupported promotions, and (c) lets a closed-list word act as grammatical evidence without lexical witnessing.
- Smallest correction: `pronoun_number("i") → :unknown` (abstains), add a small closed determiner/quantifier number table to `grammar.ex` (singular: `a an this that each every`; plural: `these those several many few both`; either: `the some most all`) and reject incompatible combinations in `combine/3`; require a dictionary POS for quantifiers like determiners.
- FileScope: fits.

### MAJOR — fix before P-021b

**M1. Brain examines only the first 64 signatures, and the chart signature is appended last.**
`brain:41-43` does `Enum.take(@max_signatures)` *before* `filter(&complete_clause?)`; `signatures.ex:113` is `existing ++ chart`. If the legacy list reaches 64, the valid chart parse is silently invisible to Brain, which is the case the brief calls "recognized by Core but never examined by Brain". GPT6 listed this as a known gap (§20) but did not mitigate it. Legacy count is linear in noun/adjective windows and multiplies with polysemy (probe reads up to 8 POS-diverse rows per word, `semantic_probe.ex:67`), so long noun-dense Colony sentences are the likely victims. [V mechanism; H on frequency.]
- Reproducer (unrun): give Brain `sentence_signatures = 64 filler :noun_phrase constituents ++ [chart signature]` for "The site is open." with matching `lifg_choices`; `resolve/1` returns `:no_complete_candidate`.
- Smallest correction: **prepend** the chart signatures for Colony only (`chart ++ existing`). `assess/3` receives the *untruncated* list for constituents (`brain:46`), so nothing else changes, and chat is untouched. FileScope: `signatures.ex detect/1` + one test. (A filter-before-take change in Brain would be cleaner but is outside scope and needs your approval.)

**M2. The 6-item per-span budget is exhausted by lexical polysemy of a single token.**
`close/2` counts terminals *and* derived unary items against the cap (`chart.ex:256-273`). A word with POS {noun, adjective, preposition, adverb} yields `noun, adjective, complement, preposition, adverb, nominal, subject` = 7 > 6, so the *entire sentence* abstains, labelled `:chart_budget_exceeded` (misleading: it is lexical ambiguity, not input size). With the production probe retrieving up to 8 POS-diverse rows per word, ordinary prose will hit this. Safe failure, but a large generalization loss that DEV (single-POS fixture words) cannot reveal. [V arithmetic; U on real LTM frequency — needs a measurement.]
- Smallest correction: report a distinct `:lexical_ambiguity_budget` reason for width-1 overflow, and either count only rule-consumable kinds or raise the per-span cap (design number from N-035; changing it is **your** call). FileScope: fits.

**M3. The chat-isolation differential test is vacuous.**
`chart_test.exs` "non-colony paths preserve existing signatures" compares `:user` against `:test`, both non-Colony, and none of its words (`bird flarping fence checker verified claims links lines refute sleeping table`) exist in the fixture lexicon (checked: 0/11). The chart could not emit for any of them even if ungated, so the test cannot fail on a broken gate. Mandatory gate "chat differential equality" is therefore static evidence only.
- Smallest correction: use sentences the chart *does* parse ("The site is open.") under `:user/:prod/:test`; assert `detect(colony_si) -- chart_sigs == detect(user_si)` with identical order, and `refute :copular_predication`. FileScope: `chart_test.exs`.

**M4. Brain `:preferred` is weak lexical evidence for a large role map — an integration precondition for P-021b.**
`compatibility = 0.72 + 0.28·fit` (`brain:213`) so the `@min_preference 0.68` can never bind; `adjudicate/1` (`:231-233`) needs only `matched ≥ 2 ∧ matched ≥ conflicting`. A 10–14-role chart signature can be `:preferred` with most roles unresolved. Not a P-021a defect (Brain is out of scope), but P-021b must not treat `:preferred` as "witnessed". Also: because `build/1` ends in `provisional_structure/2` (`sentence_meaning.ex:100,159`) with no source gate, a Colony `:copular_predication` now surfaces as `sentence_meaning` `:structural_hypothesis`/`:provisional`, which `social_evaluation.ex:273` counts as `:provisional_candidate`. **Sealed scoring must measure the Chart/Brain structural outcome, not `sentence_meaning.status`**, or improvements and abstentions will be miscounted.

### MINOR

- **m1.** Same-roles/different-features readings collapse arbitrarily: chart dedup `{id,span,roles}` (`chart.ex:82`) and Brain's identical `uniq_by` drop one of e.g. adjective-vs-noun modifier readings, keeping whichever sorts first. Correction: merge per-role feature lists (as already done for adjective+participle) before dedup.
- **m2.** `:pos_adverb` is absent from Brain's `expected_pos/1` (`brain:240-256`), so adverb roles are skipped (`permitted == []`) and never LIFG-checked, although `Brain.ATL.Constituents` does know it. No promotion (Core requires adverb POS), but the evidence overstates witnessing. P-021b must treat adverb/quantifier roles as unwitnessed.
- **m3.** PP objects carry `[:pos_noun_or_pronoun]` even when the object is a proper noun; Brain's `expected_pos` maps that to `["noun","pronoun"]`, producing a self-inflicted `:conflict`. Correction: `[:pos_noun_or_pronoun, :pos_proper_noun]`.
- **m4.** Proper-noun subjects can never parse (`agreement?(:unknown, _) → false`). Intentionally conservative; record as a known recall limit.
- **m5.** `detect/1` re-runs the chart on every verifier call (≥5 per probe). Bounded, but unmemoized; measure before P-021b adds callers.
- **m6.** `:instruction_shaped` is assigned for any sentence containing a subordinator (`chart.ex:131`), so "…open while the site is ready" is labelled as an instruction. Label only; abstention is correct.
- **m7.** Bradley's status line shows `?? core` (untracked) in the working tree. Not part of P-021a; worth `file core` before anything is committed (it matches the earlier stray-file pattern and may be a crash dump).

## 3. Test-coverage gaps
- No test runs the **real probe** (`SemanticProbe`) with `ltm_reader` stubs; all inputs are hand-built SIs with a fixture lexicon built from DEV words, so DEV proves grammar behaviour, **not** live LTM coverage (GPT6 says so too).
- No adversarial negatives for B1/B2, polysemy (M2), or the 64 limit (M1).
- Brain integration is asserted for one sentence with hand-picked perfect `lifg_choices`; no case with unresolved/conflicting choices.
- No test that evidence atoms are honest (e.g. polarity), and no schema/version field on the signature, so a rule-table change silently changes replayed signatures.

## 4. Required vs optional
**Required before the sealed run:** B1, B2, M1, M3 (+ a full-umbrella regression on the corrected SHA, not yet reported for `4fca72ed`).
**Required before P-021b:** M2 (measured), M4 invariants below, m2 documented.
**Optional:** m1, m3, m5, m6.

## 5. P-021b readiness
The representation is a sound base for typed composition: index-only roles, stable keys, explicit feature alternatives, no surface text, no agent/action fields. Add these invariants before building on it:
1. Consume a chart candidate only if Brain's `lexical_role_evidence` shows `unresolved_roles == 0 ∧ conflicting_roles == 0` (M4); never `:preferred` alone.
2. Evidence atoms must be *established facts*, not labels (`:positive_polarity` → `:no_negation_cue_in_closed_list`); add a `grammar_version` to evidence so proofs can be replayed against the same rule table.
3. Adverb, quantifier, determiner and `by`-phrase roles carry semantics (hedges, scope, agency). P-021b must abstain unless each is on a closed neutral list; `:quantifier_unapplied` and `:by_phrase_adjunct_uninterpreted` already mark two of them.
4. A dual adjective/participle complement (`[:pos_adjective, :pos_verb]`) may only yield a *stative property* reading; no passive event or agent.
5. Distinct proposition status and separate `AttributedPredication.verified/1` (N-036 §3), unchanged.
6. Colony model prompts, publishing, Basal Ganglia admission and durable memory stay out of scope, as N-036 states.

## 6. Recommended next ticket: **P-021a.1** (fix pass; not approved until Bradley says so)

FileScope:
- EDIT `apps/core/lib/core/syntax/grammar.ex` (B1 cue list, B2 pronoun/determiner/quantifier number tables)
- EDIT `apps/core/lib/core/syntax/chart.ex` (B1 evidence rename, B2 agreement in `combine/3`, quantifier POS, M2 reason label, optional m1/m3)
- EDIT `apps/core/lib/core/syntax/signatures.ex` — `detect/1` ordering only (M1, prepend)
- EDIT `apps/core/test/core/syntax/chart_test.exs` (replace the vacuous differential, M3)
- NEW `apps/core/test/core/syntax/chart_adversarial_test.exs` (B1, B2, M1, M2 reproducers, honest-evidence assertion)
- Not touched: any Brain file, `sentence_meaning.ex`, composition, Colony modules, DEV corpus expectations, existing baselines.

Proposed reproducer tests (UNRUN): `"Nothing is ready."`, `"Nobody is ready."`, `"The site is hardly open."`, `"I is ready."`, `"A pages are ready."`, `"Each pages are ready."`, `"Several site is ready."` must all produce no `:copular_predication`; a 64-filler-signature case must still yield a Brain candidate after the prepend; a 7-item polysemous token must report `:lexical_ambiguity_budget`.

## 7. Readiness
- **Full regression:** not reported on `4fca72ed`. Run `mix test` from the umbrella root and `mix compile --force` after P-021a.1.
- **Sealed evaluation:** **not ready.** After P-021a.1 plus a green full regression, run it at most twice, counts only, and score the Chart/Brain structural outcome (M4), through the real probe path with real LTM or a lexicon fixture you supply, not the DEV fixture lexicon.

## 8. What I could not verify [U]
No execution of any code. Real LTM POS coverage and polysemy frequency. Performance of repeated `detect/1`. Whether the Wiktionary-derived rows contain the `nothing/nobody/none` noun rows used in B1.
