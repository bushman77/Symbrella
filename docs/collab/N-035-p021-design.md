# N-035 — P-021 architectural review and design (READ-ONLY)

FROM: Claude · TO: Bradley, GPT-6 · 2026-11-19 · Code read at `e9e95e09`. No Elixir/Mix run (sandbox cannot). No production code, test, fixture or baseline changed. [V] read in code, [H] hypothesis, [U] unknown.
Responds to Bradley's P-021 brief (Tasks 1-5) and GPT-6's N-034. **No implementation is approved or implied.**

## 0. Short answer to the question

Symbrella has three narrow-recognizer layers stacked on a sound lexical core: (1) a catalogue of fixed POS-window detectors, (2) a typed composition layer whose vocabulary is three nouns, three adjectives and three verbs, and (3) a Colony wrapper that is itself regex- and token-count-specific. A sentence reaches "supported" only if all three happen to match one shape. The evolution that avoids a growing pile of recognizers is:

1. **A bounded chart over a rule table** (data, not per-sentence code) that builds hierarchical constituents with exact spans, attachment and agreement, and emits a full-span signature only when one parse covers every token.
2. **A typed skeleton for open vocabulary**: composition over *witnessed lexical constants* (dictionary-attested word|POS|sense references) rather than a closed list of meanings, so the proof certifies structure and role assignment, not word meaning or truth.
3. **A thin attribution wrapper** that turns a verified skeleton into "the author states ...", with `external_unverified`, and retires the bespoke Colony regexes as regression oracles.

Honest expectation: on the GLOOP post this reaches at most sentences 2 and 3 (about 2 of 12). The value is the mechanism and its measured precision, not the GLOOP count.

## 1. Independent verdict (Task 1)

### 1.1 Verified [V]

- **Signature catalogue.** `Core.Syntax.Signatures.detect/1` (signatures.ex:75-110) appends ~20 detectors. Each slides a fixed POS window over the tokens (`@finite_present_patterns` 52-60, 3-5 tokens; `copular_relative_classifications` 762, a fixed 7-token window with literal "that").
- **NP coverage is six windows, not a grammar.** `@noun_patterns` (signatures.ex:~14-21) has D-A-A-N, D-A-N, A-A-N, D-N, A-N, N-N. There is **no D-N-N, N-N-N or Q-N-N**. "Most NFT sites" and "the market files" cannot form a subject NP. This supports GPT-6's N-034 [H]: sentence 2 may fail on NP coverage before any clause rule matters.
- **Structural competition is span-exact.** `Brain.LIFG.StructuralCompetition.resolve/1` (structural_competition.ex:31-45) considers only signatures with span `{0,last}` over contiguous base indices, `last < 48`. `complete_clause?/2` second clause (122-135) is **id-generic**: any `core_syntax` signature, non-constituent id, non-empty evidence, at least two integer roles `<= last`, a `:predicate`/`:verb` role and one of `:subject`/`:subject_head`/`:wh`/`:object_head`/`:lexical_target`. A new construction therefore reaches the competition **without any Brain change**, and any new full-span id is adopted automatically, so ids and evidence must be tightly controlled.
- **Scoring and abstention.** `assess/3` (140-) compares each `required_features` role with the selected LIFG POS (`expected_pos/1` accepts a list, so a role may allow `[:pos_adjective, :pos_verb]`). `adjudicate/1` (215-) prefers a candidate only with `matched_roles >= 2`, compatibility `>= 0.68` and margin `>= 0.12`; otherwise `:unresolved`. Equal-roles candidates are merged by `Enum.uniq_by(&{&1.id, &1.span, &1.roles})` (resolve/1), so **attachment ambiguity must be encoded in role keys or it is silently collapsed**.
- **Every verifier re-runs `detect/1`.** `Core.Syntax.StructuralCompetition.verified/1` (core/syntax/structural_competition.ex ~62-80) and `Composition.derive/1` (composition.ex:84) require `Enum.all?(signatures, &(&1 in detected))`. Consequence: a new signature must be reproducible from `si` alone by `detect/1`. A parser run only inside the Colony probe and appended to `sentence_signatures` would fail every downstream verification. It also means `detect/1` runs several times per sentence (ledger.ex:752, discourse_composition.ex:140, passage.ex:126, composition.ex:84, structural_competition.ex:19,73), so the new code must be bounded and deterministic.
- **Chat shares the detector.** `Core.Syntax.Signatures.attach/1` is called in the chat pipeline (core.ex:184) and in the probe (semantic_probe.ex:89). Adding a family to `detect/1` unconditionally changes chat. It must be gated.
- **The signature type is flat.** `Core.SemanticInput.sentence_signature()` (semantic_input.ex:173-187): `roles` maps atom to a non-negative integer; no tree field.
- **Composition is closed and tiny.** `Expression` (composition/expression.ex:11-17): nouns `bird cat dog`, adjectives `black white small`, a fixed predicate set, actions `sit sleep fly`, landmarks `fence tree table`. `Lexical` (composition/lexical.ex:18-21) repeats the closed tables. `Composition.derive/1` (composition.ex:76) derives only `:nominal_property` and `:progressive_event_property`; `event_signature?/1` (141-157) is hard-wired to span `{0,6}`, an exact role map and (in `compose_event`, 175) `auxiliary.word == "is"`. `verified/1` accepts only those two kinds. There is no way, today, to produce a typed derivation for "site", "written" or "eyes".
- **A real typed core exists and is reusable.** `Expression` (typed lambda calculus), `Reducer` (bounded beta reduction), `Proof.verify/1` (replay with type preservation, composition/proof.ex) and `RoleWitness.resolve/4` (composition/role_witness.ex:17) are sound and general in form. Their vocabulary, not their machinery, is the limit.
- **SentenceMeaning does not consume proofs.** `build/1` (sentence_meaning.ex:40-110) is a priority `||` chain of specialised builders; `attach_compositional_evidence/2` (126-157) only *annotates* the chosen meaning with `Composition.verified/1` metadata. A verified proof does not yet produce a meaning.
- **The Colony layer is bespoke.** `ReportedProposition` (social/reported_proposition.ex:17) is one regex for one headline ("N of my checker's M links were lines that refute the claim"). `DeclarativeProposition` (social/declarative_proposition.ex:~34) admits 4-8 tokens and refuses `:`/`;`. The probe calls them after `SentenceMeaning` (semantic_probe.ex:~119-151).
- **Consumers of proofs reach chat.** `response/symbolic_generation.ex:224-236` and `645-661` pattern-match `compositional_evidence`/`Composition.verified`; `discourse/ledger.ex:923,966,1033`; `response/attach.ex:169`. A new proof kind must be audited against all of them (§4.4).

### 1.2 Hypotheses [H] / unknowns [U]

- [H] "Most" may not have a determiner POS in LTM; sentence 2 could also fail at lexical witnessing. [U] LTM rows were not read.
- [U] How the UI and `Core.Comprehension.Summary` count "supported interpretations" (probable: `attributed_proposition.status`); must be read before P-021b.
- [U] Whether `SemanticInput` rejects unknown `si` keys (relevant only if a new gating key is chosen; §3.6 avoids it).
- [H] With 198/198 weak LIFG choices, `matched_roles >= 2` may fail for correct parses; this is where S1 matters, after P-021a/b exist.

### 1.3 Where I disagree with GPT-6 / my own N-033

1. **"Tiling" is too weak.** Sibling tiles cannot express a PP inside a VP, a possessive inside an NP, or two attachments of one PP. A **bottom-up chart with children** subsumes tiling and gives nesting, attachment ambiguity and agreement for free. I withdraw "tiler" as the mechanism name.
2. **P-021a as framed cannot be verified end to end.** Without an open-vocabulary typed layer, P-021b will return `not_derived` for every real sentence. The design must include the witnessed-constants skeleton (§4), not just a larger closed vocabulary.
3. **Retire, do not extend, the bespoke Colony interpreters** once the general path covers their cases. Their tests become regression oracles.
4. **GPT-6's "do not count sentence 4" is right.** It needs an intransitive predicate row and a parenthetical rule; it is a later extension row, not a P-021a goal.

### 1.4 Three levels that must stay distinct

| Level | Artifact | Gate | Means | Does not mean | Failure mode |
|---|---|---|---|---|---|
| L1 grammatical hypothesis | full-span signature + `structural_competition.status == :preferred` | re-derivable by `detect/1`; LIFG POS agreement | "one parse covers every token" | any meaning | `:unresolved` / no candidate |
| L2 verified typed derivation | `compositional_derivation` proof | witnesses + type check + beta replay | "the parts compose type-correctly over attested words" | word meaning, truth, an event | `abstained` with a reason |
| L3 attributed statement | `attributed_proposition` with `source: :colony_external` | wrapper rejects unless L1 and L2 hold; source stays attached | "the author appears to state X" | X is true; any action | `not_interpreted` |

UI and telemetry should label these three names, never "understood".

## 2. Reusable mechanisms and missing abstractions

Reusable as is: lexical nodes (`base_nodes/2`, signatures.ex:235, POS/morphology sets, `plural_noun?`), `valid_surface?/1` (MWE boundary check, 1730), `pos?/form?`, the copula test in line 762, the existing NP/PP/possessive/VPC signatures as constituent seeds, `Expression`/`Reducer`/`Proof`, `RoleWitness`, `StructuralCompetition` (unchanged), `Trace`.

Missing: (a) a constituent chart with children and bounded ambiguity; (b) an open-vocabulary lexical constant bound to a dictionary witness; (c) a proof kind for predication; (d) an attribution wrapper that consumes L2; (e) a gate so chat is unchanged.

## 3. P-021a design: constituent chart (Task 2)

### 3.1 Algorithm (pure, deterministic, bounded)

Module `Core.Syntax.Chart`, input: the node list from `base_nodes/2`; output: full-span clause signatures. Steps:
1. **Items.** One terminal item per (token, attested category): `Det Q Adj N Pron PropN V(be) Vpp(past participle) Prep Adv Neg Modal Conj`. Categories come only from LTM POS/morphology (never from the surface), except the closed function-word lists in §3.3. An ambiguous word yields several items.
2. **Rules** (a data table in `Core.Syntax.Grammar`, each row `{lhs, rhs, head, roles, checks, evidence}`). Initial rows:
   - `NP -> Det? Q? (Adj | N){0,2} N` (bounded Kleene, replaces the six windows for subject use; the old detectors stay untouched);
   - `NP -> PossNP` (seeded from the existing possessive signatures, signatures.ex:1488);
   - `NP -> Pron | PropN`;
   - `PP -> Prep NP | Prep Pron` (seeded from signatures.ex:1562 plus the chart's NP);
   - `AdvP -> Adv`;
   - `Comp -> Adj | Vpp`;
   - `VP -> Cop Comp`;
   - `S -> NP VP PP* AdvP*` (adjuncts attach to the VP; an NP-internal `NP -> NP PP` row yields the competing attachment).
3. **Chart fill.** Standard bottom-up span combination over `0..last`, `last < 48`, with limits: at most 6 items per span, at most 400 items total, at most 12 full-span results. Exceeding a limit yields `{:abstain, :chart_budget_exceeded}`; nothing is truncated silently.
4. **Checks** run during combination: contiguity (children cover the parent span exactly, no gaps, no overlap between sibling children; nesting is allowed); polarity (no token in the span is a `Neg` item or negative quantifier "no"); modality and finiteness (`VP -> Cop Comp` requires the copula to be one of `is are was were`; modal, base `be`, `been`, `being` reject); agreement (§3.4); word-order (copula after the subject NP only, which rejects inversion and questions); clause boundary (an internal `, ; : -` or dash, read from `si.sentence` because punctuation tokens are not chart items, or a subordinator `if when unless because although while`, rejects with `:multiple_clauses`; P-021a does no clause segmentation). [U] How the tokenizer splits contractions (`aren't`) and whether punctuation consumes token indices was not verified; the negation list covers both whole and split forms.
5. **Emit.** For each full-span `S` parse emit one flat signature: `id: :copular_predication`, `span: {0, last}`, `source: :core_syntax`, `roles` integers only, `required_features`, `evidence`. Determinism: parses are sorted by `{fewer constituents, rule priority, lexicographic spans}`; no `Map` iteration order is relied on.

### 3.2 Encoding hierarchy and ambiguity in a flat signature

`roles` must stay `atom => non_neg_integer` (semantic_input.ex:173-187) and `resolve/1` merges equal `{id, span, roles}`. So:
- base roles: `subject_head`, `copula`, `predicate` (the complement head), plus `subject_determiner`, `subject_modifier_1..2`, `subject_quantifier` when present;
- adjuncts by **attachment-specific role keys**: `predicate_adjunct_1_preposition`, `predicate_adjunct_1_object`, `predicate_adjunct_1_adverb` versus `subject_adjunct_1_preposition`. Two parses that differ only in attachment therefore have different `roles` and stay separate candidates, so `adjudicate/1` sees margin 0 and returns `:unresolved` (an honest abstention) instead of picking one;
- complement reading in `evidence`: `:attested_adjective`, `:attested_past_participle` or both. "Written" with both attestations is one candidate carrying both atoms (reading `:adjectival_or_participial`), **not** two candidates and **not** a passive. A `by`-PP adds `:by_phrase_adjunct_uninterpreted`; no agent role exists in P-021a.
- `required_features`: `subject_head: [:pos_noun]` (or noun/pronoun), `predicate: [:pos_adjective, :pos_verb]` as attested, `copula: [:pos_auxiliary]`.
- The full tree is **not** stored in the signature; it is recomputed from `si` by the pure chart when needed (the verifiers already recompute `detect/1`).

### 3.3 Closed function-word lists (data, versioned)

`be` forms `is are was were`; negatives `not never no n't` (and the contractions `isn't aren't wasn't weren't`); modals `can could may might must shall should will would`; subordinators as above; quantifiers `most every each all both several some many few` (recorded, never applied). These are function words, not world vocabulary. Content words (noun, adjective, participle, preposition, adverb) come **only** from LTM attestation.

### 3.4 POS, morphology and agreement

- Past participle only when `:past_participle` is in the node's morphology set (signatures.ex:173-198); a surface "-ed" is not enough. Adjective only with attested adjective POS.
- Number: plural only with an attested plural form-of (`plural_noun?/1`, signatures.ex:220); otherwise singular, the same convention `finite_present_svo` already uses (signatures.ex:1280-1283). `is/was` require a singular-or-unknown head; `are/were` require an attested plural head; a mismatch rejects with `:agreement_conflict`. Known limitation: mass and irregular nouns ("data", "people") may misclassify; they must be negatives in the holdout, not special-cased.
- Possessives and modified subjects use the same NP rule and the existing possessive evidence; the possessor is recorded as an ungrounded role, never resolved.

### 3.5 Integration with structural competition (no bypass)

`Core.Syntax.Signatures.detect/1` appends `Chart.clause_signatures/2` output. `Core.Syntax.StructuralCompetition.attach/verified` and `Brain.LIFG.StructuralCompetition.resolve/1` run **unchanged**: the new signature passes `complete_clause?` only if it is truly full-span with a predicate and subject role, and still has to win `assess/adjudicate` on LIFG POS agreement. No gate is relaxed; no Brain file is touched.

### 3.6 Gating (chat unchanged)

`detect/1` enables the chart only when `Map.get(si, :source) in chart_sources`, with `chart_sources` defaulting to `[:colony_external]` (module attribute, overridable by app env for tests). No new `si` key, so no `SemanticInput` change. Enabling it for chat is a separate, later approval. A differential test (§3.9) must prove `detect/1` is byte-identical for non-Colony sources.

### 3.7 Exact FileScope proposal (P-021a, not approved)

- NEW `apps/core/lib/core/syntax/chart.ex`
- NEW `apps/core/lib/core/syntax/grammar.ex` (rule table and function-word lists)
- EDIT `apps/core/lib/core/syntax/signatures.ex`: `detect/1` only (gate plus one append) and a `@doc false` accessor for the existing node builder
- NEW tests: `apps/core/test/core/syntax/chart_test.exs`, `apps/core/test/core/syntax/chart_dev_corpus_test.exs` (loads the visible corpus as new fixtures)
- Not touched: any Brain file, `composition*.ex`, `sentence_meaning.ex`, `semantic_input.ex`, chat pipeline, existing tests and baselines.

### 3.8 Risks and rollback

- Cost: `detect/1` is re-run per verifier; the chart is bounded (§3.1) and gated to Colony only.
- Determinism: any nondeterministic ordering breaks `Enum.all?(signatures, &(&1 in detected))` (all verifiers fail closed). Mitigation: sorted lists and a repeat-run equality test.
- `complete_clause?` adopts any new full-span id: keep one id, strict `evidence`, no tolerant fallbacks.
- Lexical witnessing may still fail on weak LIFG POS (§1.2); that is a measured outcome, not a bug.
- Rollback: revert the commit; no schema, memory or Brain state is touched.

### 3.9 Tests (deterministic)

Chart unit tests per rule row; the DEV corpus (docs/collab/p021/dev_corpus.md) as table-driven cases; budget tests (49 tokens, item caps); repeat-run determinism; **differential test**: `detect/1` for `source != :colony_external` equals the pre-change output on a fixed list of existing test sentences (read from current tests, not edited); attachment-ambiguity test (two parses with different role keys survive `resolve/1` and produce `:unresolved`); a test that no atom is created from token text.

### 3.10 How this extends without a detector per sentence

New constructions are **rule rows plus word-class lists plus corpus cases**, not new matchers. Examples that reuse the same chart: `VP -> Vintrans PP*` (sentence 4 "lives on ..."), `VP -> V NP` (subsumes today's `finite_present_svo`/`finite_past_svo` detectors, which can then be retired), `X -> X Conj X` bounded coordination, a parenthetical rule that absorbs a dash-delimited span as an uninterpreted adjunct, and later relative clauses. Each addition is bounded by the same item caps and must ship with its own positives and negatives.

## 4. P-021b design boundary: verified semantic composition (Task 3)

### 4.1 Representation for open vocabulary

Extend the typed calculus with **witnessed lexical constants**: `{:lexical, kind, witness_id}` where `kind` is `:noun | :adjective | :participle | :preposition` and `witness_id` is the binary dictionary id already used by `RoleWitness` (`word|pos|sense`). Type: `e -> t` for noun/adjective/participle; a PP modifier is `(e -> t) -> (e -> t)`. **Never create an atom from external text** (atom-table exhaustion); constants are binaries, validated against `~r/\A[^|]{1,64}\|[a-z_]+\|\d+\z/`. Add one closed operator `{:kind_predication, tense, S, C}` of type `truth` (the kind S is described by property C), with tense in `:present | :past`. This is a **typed skeleton**: it certifies composition and role assignment, it does not interpret "site" or "written".

### 4.2 Proof structure (sketch)

```
%{version: 3, status: :derived, kind: :copular_predication, construction: :copular_predication,
  span: {0, last},
  premises: %{signature: Map.take(sig, [:id, :span, :roles, :required_features, :evidence]),
              lexical_witnesses: [%{role:, index:, word:, pos:, sense_id:}],
              vocabulary: :witnessed_lexical_constants_v1},
  initial: <Expression>, steps: [...], result: <Expression>, result_type: :truth,
  subject: %{head: i, determiner: i|nil, modifiers: [i], possessor: i|nil,
             quantifier: %{surface: "most", applied?: false}|nil},
  complement: %{index: i, reading: :adjectival | :participial | :adjectival_or_participial},
  modifiers: [%{attach: :predicate | :subject, kind: :prepositional | :adverbial, ...}],
  tense: :present | :past, polarity: :positive,
  denotation: :uninterpreted_witnessed_constants,
  epistemic_status: :conditional_on_lexical_and_structural_premises,
  quantifier_applied?: false, factual_authority?: false}
```

### 4.3 Verifier invariants

1. The saved proof equals `derive(si)` recomputed (as `verified/1` does today, composition.ex ~123-139).
2. `StructuralCompetition.verified(si)` is `:preferred` and its construction, span and roles equal the proof's signature (the `event_derivation` pattern, composition.ex:159-170).
3. Every witness re-resolves through `RoleWitness.resolve/4` against the current `lifg_choices` and `active_cells` (a POS witness is not a sense decision).
4. `Expression.type_of` equals `result_type`; `Proof.verify` replays every beta step.
5. No negation, modal, question or subordinator token inside the span (re-read from `si.tokens`); `polarity == :positive`.
6. Every base token is covered by the signature (no uncovered token).
7. `quantifier_applied? == false`; `factual_authority? == false`; `denotation == :uninterpreted_witnessed_constants`.
8. No atom derived from token text; all lexical constants are bounded binaries.

### 4.4 Abstention

Derive nothing (status `:abstained`, closed reason) when: structural status is not `:preferred` (including unresolved attachment); any witness fails; number or tense is unattested; a quantifier appears **and** the consumer requires application (otherwise `SUP+q` carries `unresolved: [:quantifier_scope]`); pronoun referents are required (they stay unresolved, never coreferred).

### 4.5 Consumption (SentenceMeaning stays unchanged)

Do **not** edit the SentenceMeaning builder chain. Extend the Colony wrapper: `ReportedProposition.interpret/1` already falls through to `DeclarativeProposition.interpret/1` when its regex does not match; add `Core.Social.AttributedPredication.interpret/1` as a third fallback. It requires `source: :colony_external`, L1 preferred and L2 verified, and returns the **existing result shape**: `status: :attributed_structural_interpretation`, `trust: :untrusted_external`, `verification: :author_claim_not_independently_verified`, `source: :colony_external`, plus the roles, reading, tense, polarity, quantifier and `unresolved` list. Existing UI/summary counters then work unchanged.

### 4.6 Consumers to audit before a new proof kind is admitted

- `response/symbolic_generation.ex:224-236, 645-661` (chat generation reads `compositional_evidence`/`Composition.verified`): confirm a new kind falls through safely and cannot generate text from external-source data.
- `discourse/ledger.ex:752, 923, 966, 1033` (writes compositional evidence into the session ledger): confirm the source gate excludes Colony; add a regression test that no Colony proof reaches the Ledger.
- `response/attach.ex:169`; `semantic/discourse_composition.ex`; `comprehension/summary.ex` and `diagnosis.ex`; `social_cognitive_report` and `social_live.ex` counters; `home_live/explain_evidence.ex` (privacy: term disclosure still undecided); `SocialBrainActivity`.
- Verify `Composition.verified/1` returns `:not_applicable` for the new kind outside Colony so chat behaviour is byte-identical.

### 4.7 FileScope (P-021b, not approved, after §4.6 audit)

`composition/expression.ex` (new term, type rule, binary-constant validation), `composition/lexical.ex` (witness-bound constants), NEW `composition/predication.ex` (compose and verify), `composition.ex` (dispatch and `verified/1`), NEW `social/attributed_predication.ex`, `social/reported_proposition.ex` (one fallback clause), tests. Rollback: revert; Colony falls back to today's abstentions.

## 5. S0a hardening (Task 5, separately scoped)

`structural_scope/2` is private (`defp`), so tests need either `@doc false def` or extraction to a pure module (`Core.Social.StructuralScope.observe/2`), recommended. Proposed deterministic tests:

| Reason | Fixture | Expectation |
|---|---|---|
| `:not_applicable` | structural status `:preferred` or `:unresolved` | reason `:not_applicable` |
| `:no_base_tokens` | tokens `[]`, or only punctuation | `:no_base_tokens` |
| `:structural_token_budget_exceeded` | 49 base tokens (indices 0..48) vs 48 tokens (0..47) | 49 → exceeded; 48 → not exceeded |
| `:noncontiguous_token_indices` | indices `[0,1,3]` | noncontiguous |
| `:no_sentence_signatures` | contiguous tokens, `sentence_signatures: []` | `:no_sentence_signatures` |
| `:no_full_span_signature` | signature `{0,2}` over `last = 4` | `:no_full_span_signature` |
| `:complete_signature_eligibility_unknown` | full-span signature that Brain rejects (e.g. no `:predicate` role) | unknown (documents the honest limit) |

**Counterfactual tests against the real Brain module** kill drift: for a valid full-span clause fixture, `Brain.LIFG.StructuralCompetition.resolve/1` returns a candidate at 48 tokens and none at 49 *by the real code*, and the diagnostic class must agree in both. **Source-token validity**: tokens with missing `phrase`, `index < 0`, `n: 2` or `kind: :punct` are ignored by Brain's `base_indices/1` (structural_competition.ex ~68-81) but counted by the probe's filter (it omits `phrase` and `index >= 0` checks): the test for a token lacking `phrase` is expected to **fail today** and defines the fix (align the filter). **No-behavior-change test**: `resolve/1` output is identical with and without the ledger.

Useful extra fields (Core-only, observational): `clause_core_signatures` (signatures with a `:predicate`/`:verb` role), `longest_span_tokens` and `covered_fraction`, `uncovered_token_count`, `constituent_only?`. They separate "only NP/PP, no clause" (sentence 3) from "clause core present but not spanning" (sentences 2, 4). Each needs a pure-function test on constructed signatures.

## 6. Holdout evaluation plan (Task 4)

- **DEV partition (visible):** `docs/collab/p021/dev_corpus.md`, 36 rows with expected grammar, expected semantic outcome (SUP / SUP+q / ABSTAIN with a closed reason), phase and rationale, covering every category in the brief.
- **SEALED partition (not in the repo):** 40 rows, different vocabulary and shapes (nested NP in PP, attachment ambiguity, pronoun subjects and PP objects, existential, coordinated complement and object, mass-noun-free agreement cases). **Commitment:** SHA-256 `dc6ad890cbcf842da58f27cd6eef70b480a9f36dbea53e2efbce159152447366` of `p021_sealed_corpus_v1.md`. Bradley holds the file. It is released only after Bradley approves the frozen P-021a design; it may be run **at most twice** (P-021a candidate, P-021b), reporting **counts only**, never per-item feedback to the implementer. Revealing the file later lets anyone verify the hash.
- **Metrics (proposed, to freeze):** (1) unsupported promotions on negatives, hard gate **= 0**; (2) precision of supported outputs (roles, tense, polarity, reading all correct) **>= 0.95**; (3) recall on phase-a positives, reported (target >= 0.6, informational); (4) abstention-reason accuracy, reported (target >= 0.8); (5) **zero action-capable fields** on every injection and instruction row (output kind must be one of the closed set); (6) later-phase rows must abstain, and any non-abstention counts as an unsupported promotion; (7) chat differential equality (§3.9).
- **Anti-tuning rules:** GLOOP is reported, never tuned on; no example is added to the sealed set after the hash; a failing sealed row is not "fixed" by a surface rule; a new construction is a rule row plus new DEV cases plus a new SEALED partition (v2).

## 7. Recommended order and smallest measurable improvement

1. **S0a hardening** (separately scoped): the counterfactual and drift tests, aligned token filter, optional coverage fields. Cheap, and it makes every later number trustworthy.
2. **Freeze the P-021a contract** with the DEV corpus and the SEALED hash; Bradley decides the Explain privacy question.
3. **P-021a** (chart plus copular rows, Core only, Colony-gated). Measure L1 only: full-span candidates and `:preferred` rate on DEV, then SEALED once. **This is the smallest approvable patch.** Success is "correct L1 on positives, zero promotions on negatives", not meaning.
4. **P-021b audit** (read-only): §4.6 consumer audit and the UI-counter read; then P-021b.
5. **S1** lexical A/B afterwards, because witnessing and `matched_roles >= 2` depend on LIFG POS.
6. Retire `ReportedProposition`/`DeclarativeProposition` once the general path matches them; use their tests as oracles.

## 8. What I could not verify

No code was run. Lexicon coverage of the corpus words, the real LIFG POS selections for these sentences, the UI counter logic and `SemanticInput`'s key validation are unread. Corpus outcomes are grammar-level expectations that must be checked against LTM attestation before they are frozen.
