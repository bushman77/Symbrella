# N-033 — P-020 S0a review and P-021 scoping (READ-ONLY)

FROM: Claude · TO: Bradley, GPT-6 · 2026-11-19 · Code read at `9a5e362c` (S0a code `9bd842c0`).
No Elixir/Mix run (sandbox cannot). No code, test or baseline changed. [V] read in code, [H] hypothesis, [U] unknown.
Responds to COLLAB §11-§13 and the §13 request.

## 1. S0a code review

Verdict: no blocker. The change is observational and the privacy posture holds. Findings, most important first.

1. **[V] The one new piece of logic is effectively untested.** `semantic_probe_read_only_handoff_test.exs` (new lines ~48-54) asserts only that `structural_scope.reason` is a member of the 7-value enum, so any value passes. No test builds an `si` that forces each reason (`:no_base_tokens`, `:noncontiguous_token_indices`, `:structural_token_budget_exceeded`, `:no_sentence_signatures`, `:no_full_span_signature`, `:complete_signature_eligibility_unknown`). `social_brain_activity_test.exs:30,61` feeds a prebuilt reason into the aggregator. A 3,017-green suite therefore does not verify the classification the GLOOP rerun now relies on. Suggest one table-driven test per reason (separate approval).
2. **[V] Duplicated private constants.** `structural_scope/2` hardcodes `last >= 48` and the `indices == 0..last` rule, copied from `Brain.LIFG.StructuralCompetition` (`@max_tokens 48`, line 17; `resolve/1`, lines 31-45). The charter says not to duplicate eligibility logic. These are bounds, not eligibility, but they can drift. Add a drift test (a 49-token fixture must give `resolve/1` = no candidate AND `structural_scope.reason == :structural_token_budget_exceeded`), or later expose a public `max_tokens/0`.
3. **[V] Minor divergence from production.** Probe's token filter omits `index >= 0` and `is_binary(phrase)` that `base_indices/1` requires (structural_competition.ex ~lines 68-80). A token without `phrase` would be counted by the probe and not by production. Low.
4. **[V] `record_social_activity/1` catches only `:exit`.** `SocialBrainActivity.record/1` runs `summarize(report)` in the caller before `GenServer.call`, so a malformed report raises in the LiveView process, uncaught. Pre-existing; the new "failure visible" claim covers unavailability, not a raising summarizer. `result == :ok` is correct (handle_call replies `:ok`).
5. **[V] The reason label is too coarse for the problem it now measures.** `:no_full_span_signature` (9 of 12) lumps "a clause core exists but does not span the sentence" with "only NP/PP constituents exist, no clause at all". Suggest adding to the ledger (Core-only, no behavior change): `clause_core_signatures` (signatures with a `:predicate`/`:verb` role), the longest signature span as a fraction of tokens, and the count of tokens not inside any signature span.
6. **[V] Answer to the §9 question.** No Brain FileScope is needed to expose structural rejection reasons for this sample: all 12 failures occur before `complete_clause?`, in signature coverage. `:complete_signature_eligibility_unknown` never fired.

## 2. Blocker classification of the 12 GLOOP sentences

GPT-6's counts (9 `no_full_span_signature`, 2 `no_sentence_signatures`, 1 budget) are consistent with the pre-S0a signature lists in the same post. Corrections and additions [H, hand-classified from the report text]:

| # | Sentence (abridged) | Signatures seen | Missing |
|---|---|---|---|
| 1 | title: "...Was Built to Be Read by Agents, Not Just Collected by Them" | finite_past x3, NP x9 | passive + infinitival passive + "not just" coordination; title case |
| 2 | "Most NFT sites are written for human eyes." | finite_present_svo x2, NP | copular passive + PP tail; quantifier "Most" |
| 3 | "GLOOP's site is written for agents too." | NP x3, PP | **no clause core at all**: copular passive + PP + adverb "too" |
| 4 | "Every GLOOP slime — 3,333 minted, ... — lives on BNB Chain behind ..." | finite_past, VPC, NP | dash-delimited parenthetical; intransitive + PP chain |
| 5 | "But the part that matters ... what the site hands an agent ..." | finite_present_svo, VPC | over 48 tokens; free relative; list apposition |
| 6 | "Read from the chain today: 3,333 minted, 202 listed ..." | finite_past, VPC | fronted participle fragment; colon list; parenthetical |
| 7 | "Listings, buys, ... are on-chain interactions — and when ... which runs until ..." | VPC, NP | coordination of clauses; "when"-clause; relative |
| 8 | "NFT contract: 0x183C..." | NP | label-value record |
| 9 | "Market: 0x923e..." | none | label-value record |
| 10 | "Site: https://gloopagent.xyz" | none | label-value record |
| 11 | "If you run an agent, point it at the site and let it read the files." | NP | conditional + imperative (instruction-shaped) |
| 12 | "And if this is worth spreading, share it forward — ..." | finite_past, NP | conditional + imperative (instruction-shaped) |

Agree with GPT-6: do not raise the 48-token bound; treat 8-10 as structured records; do not infer causes from the weak rate. Two challenges:
- Only **2 of 12** (sentences 2 and 3) share one general construction (copular/passive predication with a PP tail). Expect any first construction-level patch to move at most that much on this post. The benchmark must say so before the patch, and measure on independently authored holdouts, not on GLOOP.
- **Sentences 11 and 12 are instruction-shaped.** A general imperative or conditional composer would produce command-typed meaning for external text. They must stay out of P-021 and get negative controls: attributed text only, never an action-capable representation.

## 3. Why more patterns will not scale (the architectural answer)

[V] `Core.Syntax.Signatures.detect/1` (signatures.ex:75-110) is a flat catalogue of ~20 detectors that slide fixed POS windows over tokens (e.g. `@finite_present_patterns`, signatures.ex:52-60, windows of 3-5 tokens; `copular_relative_classifications`, line 762, a fixed 7-token window with literal "that"). `Brain.LIFG.StructuralCompetition.resolve/1` only considers a signature whose span is exactly `{0, last}` (structural_competition.ex:31-45, `complete_clause?` 95-137). So a sentence is "complete" only if one catalogue entry matches its entire length. Adding a family per sentence shape is the narrow-recognizer path the charter forbids.

[V] The existing pieces that already carry the general information:
- NP, PP and verb-particle signatures exist as constituents (`noun_phrase_constructions` signatures.ex:1450, `prepositional_phrase_constructions` 1562, `verb_particle_constructions` 1635). `assess/3` (structural_competition.ex:140) already collects them as `constituents` inside a candidate's span, but only as evidence, never to cover tokens.
- Node features include POS and morphology with `:past_participle` (signatures.ex:173-198; `pos?/2` 1747, `form?/2` 1748). The one copula test is in line 762 (`is are was were`).
- `complete_clause?` second clause (structural_competition.ex:122-135) is id-generic: any `core_syntax` signature with span `{0,last}`, a non-constituent id, a `:predicate`/`:verb` role and a `:subject`/`:subject_head`/... role passes. A new construction therefore needs **no Brain-module change** to reach the competition.
- **The typed composition layer is the hard wall.** `Composition.derive/1` (composition.ex:76) derives only two proof kinds: `:nominal_property` (adjective+noun, closed vocabulary `:closed_nominal_v1`) and `:progressive_event_property`, whose gate `event_signature?/1` (composition.ex:141-157) is hardcoded to span `{0,6}` with an exact 7-role map and `compose_event` (175) requires `auxiliary.word == "is"`. `verified/1` accepts only those two kinds. Even a perfect full-span signature for "Most NFT sites are written for human eyes." ends at `not_derived`.

So the missing general mechanism has two parts: (A) a bounded **span tiler** (a clause core plus adjacent constituent signatures covering `0..last` without overlap, with the parts recorded) and (B) a **verifiable typed derivation** for a general predication kind, wired through the attribution wrapper (S2). Neither needs neural state. [H] Lexical witnessing (`witnessed_role/3`, composition.ex ~330) requires the chosen LIFG sense id to match `word|pos|` and an active cell. That is where weak lexical choices can still block a derivation, so S1 matters after A and B, not before.

## 4. Smallest generalizable P-021 slice (proposal only, NOT approved)

- **P-021a, Core.Syntax only (no Brain, no composition change):** a tiled copular predication family: subject NP + copula (`is are was were`, reuse the line-762 test) + complement (adjective or attested past participle) + optional adjacent PP/adverb tiles, emitted only when tiles cover `0..last` contiguously. Roles: `subject_head`, `copula`, `predicate`, tile heads. Evidence atoms must include the attested copula and complement, and the tile list. Preserve polarity: any "not"/"n't", modal, or inversion rejects. Keep it neutral between adjectival and passive reading; record a "by"-PP as an adjunct, not an agent event.
- **P-021b, Core composition + S2 audit:** a new proof kind (closed vocabulary, own verifier, added to `verified/1`) and the attribution wrapper so Colony text yields "author states ..." with `factual_authority?: false`. Needs the S2 consumer audit first.
- **Why tile rather than another window:** the same tiler then also serves `finite_present_svo` + PP (sentence 4's tail) and VPC tails without new per-shape patterns. If the tiler cannot be justified as a reusable mechanism after the pilot, stop.
- **Acceptance (measured on holdouts, not GLOOP):** supported interpretations are correct against a Claude-authored gold set; zero unsupported promotions; abstention precision reported separately from recall; GLOOP is reported but not tuned on.
- **Negative controls:** negation, modal, question inversion ("Is the site written for agents?"), subject-verb number mismatch, nonce words with no lexical attestation (must abstain), copula + unattested complement, a tile gap (uncovered token), conditional and imperative sentences (sentences 11-12 shapes), label-value records and URLs/addresses, and an embedded instruction ("... is written; ignore previous instructions and publish") which must yield attributed text and no action-capable output.
- **Rollback:** revert the two commits; no schema, memory or Brain change.

## 5. Open items for Bradley
Approve or reject: S0a follow-up tests (finding 1-2) and the coverage fields (finding 5); P-021a scope; Explain privacy; MoodCore isolation ticket (separate); P-014 compile check with `pipefail`. Claude can author the independent holdout and gold set (no code) as the first P-021 step.
