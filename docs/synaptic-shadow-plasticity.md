# Experience-driven synaptic plasticity — durable shadow v4

This milestone is **not a live cognitive learning circuit**. It adds reversible
experience-weighted connections to a supervised GenServer, with no database
changes and no effect on LIFG, Axon, activation, response policy or memory facts.

## Pipeline

`Core.resolve_input/2` completes ordinary comprehension and
`Core.Brain.Activation.notify/2`; then
`Core.Brain.SynapticExperience.attach/2` checks the current SemanticInput.

Only an explicit human-origin user turn and a uniquely revalidated
finite-present SVO assertion can be eligible. It must pass the same grammar,
lexical/ATL role agreement, and participant projection gates used by
`Core.Semantic.SentenceMeaning` and
`Core.Brain.SemanticPropagation`.

If eligible, two typed *provisional* connections are proposed:

- predicate-surface sense ID -> actor noun sense ID, type `:actor`
- predicate-surface sense ID -> patient noun sense ID, type `:patient`

Neither ID's **meaning** becomes verified by observing a grammatical role.
The observed event is user-attributed, not externally verified.

Normal chat marks provenance using the internal `experience_origin: :user_turn`
option. Arbitrary production diagnostics without that internal marker do not
write to the shadow. No language-model reply is an eligible experience.

## Weight updates

A new distinct normalized SVO triple and sense-ID tuple produces a weight
contribution of **0.01 per edge**, capped at 0.08 across receipts. The exact
same event is deduplicated across sessions: another repetition is *not* another
independent experience, even when its session ID or turn ID changes.

Distinct contextual reports may reinforce shared edges, but are **not proof**
of independent witnesses. We deliberately do not claim that the archive
establishes source independence or objective event truth.

`Brain.Synapses.Shadow` retains a bounded number of keyed receipts, each
with its own two typed edges and a reversible review history. In development,
a stable private local HMAC key allows receipts to survive BEAM restarts
without storing raw statements or reviewer names. Source sense IDs and
the existing provenance classification are retained, but do not verify truth.

## Manual correction and evidence grades

`Core.Brain.SynapticExperience.review(si, :reject, "local-reviewer")` removes
**only that receipt's** two weight contributions and retains the receipt
for replay prevention. Independent supporting receipts still count.

`review(si, :confirm, "local-reviewer")` changes that receipt's contribution
to 0.03. A second identical review is a no-op; a later explicit review can
reverse a previous rejection or confirmation. Weight recomputation uses the
active receipt set, not blindly subtracting from saturated weights.

This is an **internal developer review API**, not a chat command or an
authenticated review UI. The reviewer string is only a label; trusted call
site authorization is NOT implemented. Natural-language statements such as
"actually..." do **not** automatically invoke it. A correction needs the
original, independently revalidated SemanticInput to locate a receipt.

Human review confirms/rejects the support recorded in the local observation;
it does NOT establish an externally verified fact or exact lexical sense.

## Read-only evaluation

`Core.Brain.SynapticExperience.preview(si, ids)` requires the same validated
SVO input plus candidate IDs present in the original lexical candidate slate.
It returns per-candidate shadow support for the currently selected patient
and does not alter candidates or ranking. The Brain-level `preview/3` is a
lower-level read-only probe, **not** proof of improved word-sense accuracy.

## Limits

- Development mode restores private receipts across BEAM restarts; tests default to ephemeral. No reviewed weight feeds live cognition.
- At most 512 experience receipts and 1,024 concurrent edges in memory.
- No reward model, automatic contradiction understanding, connection decay,
  automatic consolidation into authoritative knowledge, or learned activation spreading.
- No model-generated content qualifies for a new receipt through chat.
- Review calls are intentionally manual and programmatic.
- The graph uses current selected *inflected* surface sense IDs, not a verified
  cross-lemma sense equivalence. The separate semantic-lemma pulse remains
  an epistemically weaker routing hint.
- A shadow preview establishes only different **stored weights**, not
  downstream improvement.

## Validation

```sh
cd ~/Symbrella
git pull --ff-only origin development

mix test apps/core/test/core/brain/synaptic_experience_test.exs --trace

mix run scripts/probe_synaptic_experience.exs
mix run scripts/probe_synaptic_correction.exs
```

After a correction, the second probe should show: recorded weight 0.01,
reviewed rejection with no remaining patient edge, duplicate replay unable
to recreate it, and explicit confirmation restoring weight 0.03.

To opt out of experience recording on a Core call, use
`synapse_shadow?: false`. No existing live cognition uses those weights,
regardless of this setting.

Before any live cognitive rollout, require authenticated review attribution,
independent evidence lineage, contradiction adjudication and held-out counterfactual improvements
against an activation-only baseline.


## Explicit patient-correction proposals (shadow v3)

The production Core pipeline now also runs
`Core.Brain.SynapticCorrection.attach/2` after ordinary experience admission.

This phase recognizes a **very narrow** correction construction:

```text
Actually, the artist draws a diagram, not a picture.
```

Requirements:

1. The source must be marked as a real human-origin user turn.
2. The utterance must match an anchored, bounded, five-token positive SVO
   correction. Questions, quoted material, compound predicates, hypotheses,
   multiple objects and most natural-language variants are not supported.
3. Reconstruct the prior and replacement as separate positive clauses and
   independently comprehend both through real Core with response, persistence,
   neural recruitment and synaptic writes disabled. This uses the caller's LIFG
   options but is not a formally pure reconstruction: other transient region
   state may still be touched.
4. Each independent interpretation must pass `SynapticExperience.extract/1`.
   Their actor and predicate **sense IDs** must match, their patients differ,
   and the prior's exact experience receipt must already exist.
5. The prior must not already be rejected, and the new event must not already
   have an independently recorded receipt.

On success, `Brain.Synapses.Shadow.propose_correction/2` produces a read-only
`:review_proposed` report, attached as a
`:synaptic_correction` trace event. The trace contains bounded sense IDs
and review status, not original sentence text or evidence binaries.

The proposal **never** rejects the old receipt, registers the new event,
changes an edge weight, updates LIFG, or alters Hippocampus facts. Two real
events may have the same actor and predicate but different patients; even an
explicit "actually" is not enough to prove that an earlier event was false.

The manual `SynapticExperience.review/3` remains the only route for modifying
review verdicts. There is no authenticated end-user approval UI and no
automatic contradiction adjudication yet. The proposal itself is ephemeral:
durable storage contains only accepted experiences and their review decisions,
not unapproved correction proposals.

```sh
mix test apps/core/test/core/brain/synaptic_correction_test.exs --trace
mix run scripts/probe_synaptic_contradiction.exs
```

Disable with `synapse_correction?: false` or
`synapse_shadow?: false` in a Core call. Before enabling automatic
corrections or long-lived weights, add attributable event-instance identity,
verified provenance across sessions, authenticated human approval and
counterfactual evaluation against unchanged weights.


## Durable receipt consolidation (shadow v4)

Development config:

```elixir
config :brain, Brain.Synapses.Shadow,
  durable?: true,
  dir: "tmp/synapses"
```

Test mode defaults to `durable?: false`. Production is deliberately disabled
by default until deployment and operational controls are reviewed. This
feature *persists shadow observations*, not conclusions or model weights.

Private local files in the git-ignored directory:

- `secret.key`: stable random 32-byte receipt/snapshot HMAC key, mode 0600.
- `receipts.json`: authenticated, atomically replaced snapshot containing a
  bounded **receipt per distinct user-reported experience**. Each receipt
  contains its HMAC ID, two typed lexicon-cell edges, provenance class,
  first-observation timestamp, a keyed first-session identifier when available,
  current verdict, and up to eight **review events** with timestamp and
  pseudonymized reviewer digest. These provenance fields do not establish
  independent sources. No original utterance, gloss or reviewer name
  is stored. The directory is permission mode 0700.

**Restore these two files together.** A missing secret with an existing
snapshot, invalid HMAC, malformed receipt, oversized data or unsupported
version is treated as unavailable. Synaptic mutations then fail closed;
ordinary chat/lexical comprehension continues, but the shadow cannot
claim to be learning. Do not delete the secret or silently start afresh.

On startup `Brain.Synapses.Shadow` authenticates and structurally validates
all receipts; it reconstructs **every edge weight** by combining only
non-rejected receipts. Neither computed weight nor LIFG scoring is read
from disk. Duplicate receipt HMACs remain stable across BEAM restarts.
A rejected receipt remains in the ledger, so replay never restores it.

Updates are **write-before-acknowledge**: the complete receipt snapshot is
written to a private temporary file, synced, and renamed over the original.
If the write fails, the GenServer keeps its previous in-memory state.
This provides single-writer, crash-resistant atomic replacement, not a
guarantee of durability after sudden power loss on every filesystem.
There is no distributed write lock, remote backup, reviewer authentication,
or unlimited audit retention. No automatic compaction or truncation of the
audit trail occurs; after eight review changes on a receipt, further changes
are rejected with `:review_capacity` pending a future migration.

The two evidence classes remain separate:

- Unreviewed reports contribute 0.01 each to
  `provisional_weight` and `provisional_experiences`.
- Human-reviewed confirmations contribute 0.03 each to
  `reviewed_weight` and `reviewed_experiences`.
- Rejected receipts contribute **zero**. The combined `weight` is capped at
  0.08 for observation and read-only preview only.

Neither group certifies a dictionary sense or independent event truth.
Reviewers are attributed with a stable keyed digest; a caller-supplied
reviewer string is not identity verification. Only trusted in-process code
may call the review API.

### Validate persistence on the production dictionary

```sh
cd ~/Symbrella
git pull --ff-only origin development

mix test apps/brain/test/brain/synapses/receipt_store_test.exs \
  apps/core/test/core/brain/synaptic_experience_test.exs \
  apps/core/test/core/brain/synaptic_correction_test.exs --trace

mix run scripts/probe_synaptic_consolidation.exs
```

The standalone probe uses real Core for the SVO experience, then a separate
private shadow instance to test process restart, reviewed rejection,
duplicate suppression and reversible confirmation. It never injects new
weights into the supervised chat shadow. It removes its probe files.

To inspect the **live supervised shadow** after starting Phoenix:

```elixir
Brain.Synapses.Shadow.snapshot()
```

Expected in development: `persistence_status: :ready` and
`persisted?: true`. A warning status of `:unavailable` means the shadow
is read-only until local storage is repaired or recovered.

Existing snapshots must never be committed to Git or uploaded with bug
reports. Keep backups local and secured. Clearing `tmp/` will intentionally
erase this experimental synaptic archive.


## One-hop, role-conditioned synaptic propagation (shadow v5)

After comprehension and neuron feedback, **before** an experience is recorded,
`Core.Brain.SynapticPropagation.attach/2` inspects existing LIFG candidate
slates and the independently validated SVO patient. The order matters: an
experience must not provide artificial evidence for *its own* interpretation.

`Brain.Synapses.Propagation.compare/4` models a **read-only reverse traversal**
of an existing typed connection `{verb_sense_id, noun_sense_id, :patient}`:
the currently established noun context supplies small candidate-specific
support to the exact verb sense ID already present in this turn's lexical
slate. Actor relationships are supported at the Brain-level comparator too.
There is no multi-hop spread, inferred noun or verb, or cross-lemma mapping.

The shadow comparison has two branches with the **same** eligible candidate
set, ordered as supplied by LIFG:

- **Propagation off:** zero projected support for every candidate.
- **Propagation on:** compute an independently bounded contribution for
  matching role and *exact ID* only.

The counterfactual signal rule:

```text
shadow_signal = min(
  0.03,
  0.25 * provisional_weight + 1.00 * reviewed_weight
)
```

`provisional_weight` and `reviewed_weight` already come from separate,
capped experience receipts and their review histories. A rejected receipt
contributes zero. Large ambiguous slates up to **256 senses** are supported
without arbitrarily priming all the senses of a verb.

Importantly, the **signal-only order** is NOT a second LIFG decision; no
lexical scores are modified, no cell receives stimulation, no new experience
is stored by the propagation stage, and no claim of improved comprehension
has been established. All existing dictionary meanings remain unverified
until their usual evidence gates are independently satisfied.

Supported comparisons require the complete positive SVO sentence, matching
LIFG and ATL selected role cells, and a real candidate slate including
the selected predicate sense. Questions with an unbound object, stale
frames, unsupported language variants and most ordinary conversational
sentences remain ineligible.

For inspection:

```sh
mix test apps/brain/test/brain/synapses/receipt_store_test.exs \
  apps/brain/test/brain/synapses/propagation_test.exs \
  apps/core/test/core/brain/synaptic_propagation_test.exs \
  apps/core/test/core/brain/synaptic_experience_test.exs \
  apps/core/test/core/brain/synaptic_correction_test.exs --trace

mix run scripts/probe_synaptic_consolidation.exs
mix run scripts/probe_synaptic_propagation.exs
```

The propagation probe uses real production-dictionary comprehension for the
initial observation and held-out next-turn sentences, plus an isolated in-memory
synaptic store. It compares on versus off against a matching patient and an
unrelated patient, and checks for zero mutation.

To disable the new comparison on a Core call, specify
`synapse_propagation?: false`. The `synapse_shadow?: false` switch disables
both recording and automatic trace comparison, though the explicit
`SynapticPropagation.compare/2` developer probe remains read-only.

### Required next gate before using live synaptic signals

Establish an evaluation set with independent contexts, true ambiguity in
existing verb sense slates, review-grade outcomes, no training/test leakage,
and comparable baseline LIFG scores. Measure held-out accuracy, abstention,
contradiction handling and error regressions against disabled propagation.
Only then consider a bounded opt-in perturbation of candidate scores.


## Counterfactual experience-sensitive sense evaluation (shadow v6)

`Core.Brain.SynapticDisambiguation.evaluate/2` analyzes a previously
interpreted human-origin finite SVO without changing it. It uses the **actual**
`candidate_probs` associated with its selected verb's LIFG token, not
synthetic "winner" scores. Only evaluate when:

1. `Core.Brain.SynapticExperience.extract/1` independently revalidates the
   current utterance, role identities and ATL/LIFG agreement.
2. The exact experience has **never appeared** in the shadow receipt ledger
   (including rejected receipts). This is a narrow replay-leakage check;
   it cannot prove that a semantically equivalent paraphrase was unseen.
3. `sense_candidates` and `candidate_probs` contain exactly the same
   scored lexical candidate IDs, with no duplicate IDs. All probabilities
   must be finite, in `[0,1]`, and sum approximately to one.
4. The candidate slate has **at least two distinct real verb sense IDs** and
   at most 256 total candidates when `synapse_eval_full_scores?: true` is
   explicitly enabled in LIFG options. Stage1 currently omits large `candidate_probs`
   maps; cases with more than 256 candidates are marked insufficient rather than
   silently evaluating a top-two subset.
5. There is an available read-only propagation result for the currently
   validated patient role. No other lexical role is implicitly inferred.

This comparator carries the complete original Stage1 probability
distribution through two hypothetical branches: propagation-off uses original
scores, and propagation-on adds the bounded role signal to the same verb
sense ID before renormalizing to one. The additive probability-space
operation is a **hypothesis**, not a calibrated model, LIFG logit, or actual
LIFG rerun. The report separately retains the **real final LIFG winner**,
the baseline Stage1 decision, the projected decision, the raw gap and
abstention flag. Default abstention margin: 0.02 (diagnostic-only, configurable).

A separate `aggregate/1` reports total eligible cases, missing labels,
skips, projected flips, abstention transitions and, only with independently
supplied labels, right-to-wrong versus wrong-to-right changes.

**No accuracy figure is inferred from Symbrella's own LIFG selection**, from
the previously learned sense, or from the presence of a positive signal.
A `gold_id` is accepted only when it exists in the current scored dictionary
slate AND the caller explicitly supplies `split: :holdout` and
`label_source: :human_curated | :external_gold`. This is a caller-asserted
label origin, not machine authentication of the label.

To inspect actual candidates and honestly determine whether a competitive
evaluation is possible, run:

```sh
cd ~/Symbrella
git pull --ff-only origin development

mix test apps/brain/test/brain/synapses/propagation_test.exs \
  apps/core/test/core/brain/synaptic_propagation_test.exs \
  apps/core/test/core/brain/synaptic_disambiguation_test.exs --trace

mix run scripts/probe_synaptic_disambiguation.exs
```

The real-dictionary probe attempts eight separate **training sentence /
held-out sentence** pairs in its own isolated shadow store. It records
eligible training sentences there, **never** the held-out turns. It reports
actual score-map size, skip reason, comparison status, projected flips and
full summary. If it finds no complete 2+-verb-sense slate, it prints
`INSUFFICIENT_COMPETITIVE_EVIDENCE` and makes **no positive claim**.
Its default output is unlabeled. An unlabelled flip is not progress.

For independent manual gold review, BEFORE looking at projected winners,
optionally create a local, git-ignored JSONL file at
`tmp/eval/synaptic_disambiguation_gold.jsonl`. Example shape
(**the following ID is a placeholder, not a gold annotation**):

```json
{"example":1,"gold_id":"REPLACE_WITH_ACTUAL_DICTIONARY_VERB_SENSE_ID","source":"human_curated","basis":"Explicit dictionary sense definition checked independently"}
```

The file is strictly validated and errors on malformed/duplicate entries.
A label is accepted only for a complete competitive slate containing
that exact sense ID. Human-curated labels are NOT automatically verified;
reviewed sense definitions, independent adjudicators, truly unseen events,
time-based data splits and sufficient sample size remain future work.

No production LIFG scores, BrainCells, durable receipts or predictions are
modified by this evaluation. Do **not** enable live learned priors based only
on a single positive synthetic fixture or an unlabeled projected flip.


## Why the first real-dictionary disambiguation evaluation abstained

The first eight real LIFG held-out examples each had a scored candidate
map (two to four entries), but only **one surface verb-sense candidate**.
The score maps were not evidence of competition between multiple verb
meanings. Training observations were recorded successfully, but **0/8**
examples met the two-verb-sense evidence gate. This is an evaluation
coverage finding, not a failed synapse, nor proof that synaptic evidence
can or cannot improve sense choice.

An inflected surface like `draws|verb|1` is not the same sense namespace
as the base lexeme `draw|verb|N`. `Core.Semantic.VerbLemma.canonical/2`
can establish the base *form* `draw` from explicit dictionary morphology
tags and a matching gloss. That does **not** establish which `draw`
sense the experience exemplifies. Do not automatically distribute the
`draws|verb|1` synaptic weight among all the base verb senses.

To inspect the actual gap, run:

```sh
mix test apps/core/test/core/brain/synaptic_slate_audit_test.exs --trace
mix run scripts/audit_synaptic_verb_slates.exs
```

The audit reports original LIFG scores and POS counts, exact surface
verb competitors, whether the dictionary attests a base form, and
read-only counts/samples of verb senses for that base lexeme in
`Db.brain_cells`. A base dictionary lookup failure is labeled
`:database_unavailable`; missing morphology proof stays
`:no_proven_base_form`.

**Next gate:** establish independent contextual evidence that maps a
specific inflected form to a specific base sense, compare full same-POS
slates without training/test leakage, and collect independently reviewed
gold-sense labels. Until then, propagation, review, and disambiguation
remain conservative read-only diagnostics; no learned weight affects
LIFG or BrainCell activation.


## Evidence-gated surface → base-sense inventory (read-only bridge)

The real-dictionary lexical coverage audit passed its regression tests
(4/4) and found across eight independent finite-present SVO examples:

- **8/8** complete LIFG score slates, all containing *one* verb sense
  plus nouns and/or proper nouns, not competing verb meanings.
- **8/8** dictionary-attested third-person singular inflections leading
  to a base lexeme with at least two real verb senses.
- **0/8** verified mappings to a particular base verb sense.

For example, `draws|verb|1` attests the lexeme `draw`, whose database
contains 138 `draw|verb|N` senses. It does not identify which N.

`Core.Brain.InflectedVerbSenseBridge.project/2` now creates an isolated,
read-only base **candidate inventory** for a structurally validated
human-origin positive SVO. It requires `VerbLemma.canonical/2` to
provide a base DIFFERENT from the inflected surface (using attested tags
and the exact dictionary form-of gloss); retrieves existing `Db.BrainCell`
rows with base-matching norm, `pos == "verb"`, nonempty sense gloss and
a real dictionary ID; bounds the inventory to 256 unique entries; and
compares every candidate's own definition, examples and lexical evidence
with the *original sentence* through `Brain.LIFG.ContextFit.explain/4`.

An evidence-supported top candidate and score gap can produce a
`lexical_hypothesis`; this is deliberately NOT a verified sense mapping,
not a replacement of LIFG's actual winner, not a scored LIFG slate, and
not permission to copy `draws|verb|1`'s learned weight to any
`draw|verb|N`. A zero or tied evidence gap yields no proposal.

The new read-only production probe and the isolated tests:

```sh
git pull --ff-only origin development
mix test apps/core/test/core/brain/inflected_verb_sense_bridge_test.exs \
  apps/core/test/core/brain/synaptic_slate_audit_test.exs --trace
mix run scripts/probe_inflected_verb_sense_bridge.exs
```

The probe prints actual base sense IDs, evidence overlaps and top
dictionary glosses across eight sentences; no independent gold labels
are inferred. The meaning accuracy question is still open. A responsible
next milestone is independently reviewing a representative set of base
sense labels, checking contextual ranking against those labels, and only
then considering a bounded second-pass sense selection. The live
synaptic propagation and LIFG scoring remain unchanged.


## Evidence-gated semantic sense validation (read-only v7)

The production base-verb sense bridge found eight complete polysemous
inventories and four cases with lexical support. Importantly, the original
context matcher proposed `throw|verb|5` for "The child throws a ball"
because *ball* appears in the specialized cricket-bowling definition.
This is an **unsupported specialized reading**, not an adequate semantic
selection. The bridge correctly left its hypotheses unverified.

`Core.Brain.SemanticSenseValidator.evaluate/2` is a read-only second pass.
It re-runs the existing strict `SynapticExperience.extract` / SVO /
ATL/LIFG agreement, morphology attestation and complete DB sense inventory
through `InflectedVerbSenseBridge.project/2` with
`validation_evidence?: true`. No fabricated base IDs, form-to-sense
links, training receipts or final LIFG scores are introduced.

For **each existing dictionary candidate**, the validator:

1. Checks overt-object SVO compatibility with explicit `gram_function`
   and leading dictionary gloss labels. A sense marked *intransitive-only*
   is excluded from making a positive hypothesis in this finite SVO frame.
   Missing grammar annotations remain `:unspecified`, not a fabricated
   endorsement or a rejection. `ambitransitive`, `transitive` and
   `ditransitive` are compatible with an object.
2. Examines a deliberately narrow set of *lexically attested specialized
   requirements*: explicit dictionary domain/topic tags or gloss
   qualifiers such as **cricket bowling**, electric chair, radar,
   jazz, grenade, computing and graphical user interface. The famous
   "straightening the bowling arm" sense requires a context witness
   such as `cricket`, `bowling` or `bowler`. `ball` **alone** does
   not satisfy that restricted sense. Absence means
   `:missing_restriction_witness`, **not** proof the sense is impossible.
   The list is intentionally high precision, not an exhaustive model
   of lexical semantics.
3. Retains the original deterministic `ContextFit` scores for the
   remaining eligible candidates. A tentative hypothesis requires
   **nonzero substantive lexical overlap** (`min_context_fit: 0.10`
   by default) AND a top-versus-next gap of at least `0.10`.
   Otherwise the validator abstains with an explicit reason.
   These are heuristic diagnostic thresholds, not calibrated
   probabilities.

The result carries `baseline_id` (the earlier lexical hypothesis),
`proposed_id` (if any), excluded dictionary IDs with typed reasons,
the eligible top entries, and flags explicitly denying sense verification,
live reranking, synaptic transfer, and BrainCell stimulation.

The regression set covers:

- "The child throws a ball" — the unsupported special cricket
  interpretation `throw|verb|5` must be withheld; this is an
  **abstention**, not an automatic selection of `throw|verb|1`.
- "The artist draws a picture" — an exact dictionary picture gloss
  may remain a **provisional** hypothesis.
- "The author writes a letter" — intransitive-only word senses cannot
  be proposed solely because `author` matches their definitions.
- Explicit specialist cues, missing morphology proof, forged roles,
  assistant-origin utterances, incomplete inventories, and no-mutation
  behavior.

### Run and independently annotate

```sh
cd ~/Symbrella
git pull --ff-only origin development
mix test apps/core/test/core/brain/semantic_sense_validator_test.exs \
  apps/core/test/core/brain/inflected_verb_sense_bridge_test.exs --trace
mix run scripts/probe_semantic_sense_validator.exs
```

The probe uses eight real sentences and outputs baseline hypotheses,
validated hypotheses/abstentions, restricted senses, and the unmodified
dictionary evidence. It **never** derives the correct sense ID from the
scorer. To evaluate accuracy, an independent annotator should review
actual dictionary definitions *before* seeing the program's proposed
sense IDs, and optionally create:

`tmp/eval/semantic_sense_gold.jsonl` (git-ignored), JSONL records like:

```json
{"example":1,"gold_id":"REPLACE_WITH_REVIEWED_BASE_VERB_SENSE_ID","source":"human_curated","basis":"Specific dictionary definition independently reviewed"}
```

This is a **shape example only**, not an adjudicated gold label. The file
is validated for duplicate IDs and nonempty rationale. A gold ID is
accepted only if it belongs to the actual base dictionary candidate
inventory in a `:holdout` evaluation with declared label provenance.
Caller declarations are **not independently authenticated**; summary
counts such as `:wrong_to_abstention`, `:right_to_wrong`,
`:wrong_to_right`, and baseline/validator correct decisions are
conditional on the supplied labels and do not constitute a validation
study without external adjudication and a genuinely unseen corpus.

No proposed sense in this module is a verified synonym for a
`surface|verb|1` form or grounds transferring synaptic weights.
Live LIFG and the durable synaptic ledger remain unchanged.


## Read-only role-conditioned one-hop noun context (v8)

The evidence-gated sense validator passed **9/9** current regression tests
and the real dictionary audit continued to abstain on seven of eight
examples. The high-precision specialist exclusions held, but baseline
dictionary `ContextFit` missed associations such as `meal → food`,
`tree → plant`, and `ship → vessel`.

`Core.Brain.RoleSemanticContext.extract/2` now offers an explicitly
bounded **one-hop diagnostic**. It retrieves ZERO additional dictionary
senses. Only the *current* full-SVO subject and object noun choices can
supply supporting evidence, and each must have all of the following:

- A revalidated human-origin finite-present SVO with agreeing LIFG,
  ATL, active BrainCells, and episodic roles, using the existing
  `SynapticExperience.extract/1` gate.
- An exact real nonfallback noun-sense ID already selected for the
  **actor** or **patient** and an active BrainCell with the same ID,
  norm, POS and the **same literal dictionary definition** as LIFG
  selected. No alternative noun senses may donate definitions.
- A finite complete-enough per-token LIFG probability map including
  that noun ID. If multiple same-surface noun senses were scored,
  the selected one must exceed the runner-up by at least `0.08`.
  The bounded score distribution must also be finite and sum within
  0.98–1.02. Missing/incomplete score maps and close competitors produce
  NO expansion.
- A nonempty, bounded, single-clause dictionary gloss that yields
  up to 12 useful words after removing grammatical/scaffolding terms
  and the noun/predicate surfaces. No transitive relation is stored.

`RoleSemanticContext.support/2` compares those selected noun-gloss
terms with each **genuine existing base-verb dictionary sense** using
`Brain.LIFG.ContextFit.explain/4`. The direct sentence overlap remains
a separate field: `lexical_score`. One-hop patient evidence is
multiplied by 0.35 (cap 0.105); actor evidence by 0.15 (cap 0.045).
Combined role support is capped at `0.13`. The tuple of selected noun
sense ID, noun selection probability, matched terms and role-specific
score remains in the diagnostic report. Neither these heuristics nor
their coefficients are trained or calibrated probabilities.

**Crucial order:** The original specialist restrictions and explicit
transitivity validation run BEFORE adding role evidence. A noun gloss
mentioning `cricket` CANNOT supply the missing original-sentence
`cricket` cue. In a finite-present SVO, actor-only weak support
cannot license a new sense interpretation: to emit a *provisional*
hypothesis, the normal direct evidence criterion or separately adequate
patient support is required, with the existing total evidence and
runner-up gap thresholds. Near ties and zero-context matches abstain.

The production validator defaults to **role expansion disabled**.
For this offline read-only diagnostic, callers must explicitly specify
`role_context?: true`. No automatic change to the production LIFG,
BrainCells, synapses, or response generation occurs.

### Run the regression and real dictionary A/B

```sh
cd ~/Symbrella
git pull --ff-only origin development
mix test apps/core/test/core/brain/role_semantic_context_test.exs \
  apps/core/test/core/brain/semantic_sense_validator_test.exs \
  apps/core/test/core/brain/inflected_verb_sense_bridge_test.exs --trace

mix run scripts/probe_role_semantic_context.exs
```

The A/B probe interprets each sentence **once** and compares unchanged
baseline (`role_context?: false`) with the same true dictionary
and bounded one-hop role evidence (`role_context?: true`).
It reports selected noun sense IDs, rejected/uncertain noun contexts,
direct versus role scores, new provisional candidates, abstentions,
candidate exclusions and aggregate changes. It intentionally displays
warnings rather than claiming accuracy if no grounded noun glossary
meets the strict admission rules.

Independent gold-sense labels can be supplied in the already documented
`tmp/eval/semantic_sense_gold.jsonl`, prepared **before viewing**
system hypotheses and adjudicated against actual dictionary sense IDs.
The evaluator accepts caller-declared labels, but cannot authenticate
who reviewed them. A new proposal, even a linguistically plausible one,
is not proof of improved comprehension until matched against
independently verified holdout annotations.

The full unlabeled candidate inventories can also be exported for
independent human adjudication, WITHOUT predictions, via:

```sh
mix run scripts/export_semantic_sense_review.exs
```

This creates `tmp/eval/semantic_sense_review.jsonl` (one sentence per
JSONL record with its existing dictionary sense IDs, original definitions,
examples and grammatical tags). The separate
`tmp/eval/semantic_sense_gold.jsonl` is NEVER written automatically.
A reviewer should examine the complete inventory independently and
only then provide a justified gold ID. This reduces but does not erase
confirmation bias if program predictions were already seen.

No learned synaptic edge transfers to a base-verb sense through this
path, and no previous semantic-restriction behavior is weakened.


## Live-chat integration: semantic sense shadow Explain trace (v9)

The earlier inflected-form bridge, evidence-gated sense validator and
one-hop noun context were *diagnostic-only*. Live production Core now
evaluates an evidence-gated shadow semantic hypothesis immediately
**after `Core.Semantic.SentenceMeaning.attach/1`**, after LIFG/ATL
have resolved selected dictionary candidates, and **before**
comprehension, response planning, hippocampal capture, and new
synaptic experience admission.

`Core.Brain.SemanticSenseShadow.attach/2` executes
`SemanticSenseValidator.evaluate(si, role_context?: true)` by
default for production pipeline inputs. The underlying validator
performs its strict human-origin finite-present SVO, original lexical
choice, morphology and full base-dictionary inventory gates. Other
turns produce a compact `:skipped` diagnostic, not invented meaning.
No label or gold ID is presumed. Unexpected diagnostic exceptions
fail closed to `:diagnostic_unavailable` rather than interrupting chat.

A single, bounded `Core.Pipeline.Trace` entry with stage
`:semantic_sense_shadow` carries its exact scope:

- source surface sense, attested base lexeme, base sense count
- earlier lexical baseline vs provisional semantic hypothesis (if any)
- original and expanded score margins (heuristics, not probabilities)
- up to two already-selected noun-role gloss witnesses, three ranked
  candidate summaries, eight exclusions, and the total excluded count
- flags explicitly saying meaning NOT verified, no gold supplied,
  LIFG NOT modified, BrainCells NOT stimulated, synapses NOT
  transferred, and response NOT changed.

Restricted specialist cues (cricket, radar, etc.) STILL use only
the original user utterance, never the one-hop noun expansion.

`SymbrellaWeb.HomeLive.ExplainEvidence.from_si/1` projects only
that actual Core trace event; `EvidenceSections.build/1` renders a
"Semantic sense (shadow only)" section in the normal chat **Explain**
modal. The section is preserved in newly saved message payloads and
in the normal saved-payload reconstruction path. Existing history
without the new trace cannot fabricate a sense result.

**No new cognitive input fields** are added to SemanticInput. The
complete SemanticInput is unchanged except for one append-only trace
entry. LIFG choices, ATL winners, comprehension, response text,
BrainCells, hippocampal records and synaptic weights receive no
semantic-sense feedback from this stage. It is diagnostic and
opt-out; set `semantic_sense_shadow?: false` when calling
`Core.resolve_input/2` to skip the stage altogether. It does NOT
change `RoleSemanticContext`'s general opt-in default outside
this explicit live shadow call.

### Validate the live integration

```sh
cd ~/Symbrella
git pull --ff-only origin development

mix test apps/core/test/core/brain/semantic_sense_shadow_test.exs \
  apps/core/test/core/brain/role_semantic_context_test.exs \
  apps/core/test/core/pipeline_contract_test.exs \
  apps/symbrella_web/test/symbrella_web/live/explain_evidence_test.exs \
  --trace
```

Then start the usual Phoenix chat and submit an independently
interpretable finite-present SVO such as
"The painter draws a picture." Open **Explain** for the reply.
Look for the dedicated shadow section with trace reason, candidate
evidence, restrictions and explicit no-mutation flags. The live
production dictionary may produce a conservative abstention when
selected noun glosses lack sufficient independent evidence.

This wiring is *not* a verified improvement in comprehension or
answer accuracy. Changing actual sense selection remains dependent
on independent holdout gold-sense evaluation.


## Live-chat observation: plain SVO is not a memory request (v10)

The first live user test said "The baker cooks a meal." The semantic
shadow correctly appeared in Explain but returned
`:no_grounded_role_gloss` for actor/patient. Separately,
`Core.Response.Memory.acknowledge_projected/5` had automatically
replaced the reply with "I’ve recorded what you told me" merely
because an ordinary statement produced a valid episodic candidate.
This incorrectly confounded a stored, attributed observation with
an explicit request for a memory acknowledgment.

Corrections:

- The response-memory acknowledgment now requires **both** a
  `:memory_write` intent AND an overt `remember` instruction. A
  normal SVO declarative is not such an instruction. Existing
  `SideEffects.record_turn/4` still independently handles qualified
  user-report experiences during ordinary conversation; the shadow
  semantic validator still does not affect the response.
- `Core.Intent.StructuralStatement.attach/1` runs just after
  `SentenceMeaning.attach/1`. It upgrades **only** `:unknown`
  to `:statement` when the already-built complete finite-present
  participant assertion is revalidated by independently rebuilding
  SentenceMeaning and checking EpisodicProjection. It never invents
  an exact verb sense, touches LIFG winners, or calls memory writes.
  Questions, commands, nonuser sources and forged meaning frames
  cannot use this reconciliation.
- `RoleSemanticContext` now reports *why* a selected noun was
  rejected: missing probability slate, incomplete normalization,
  low same-surface noun-sense margin, missing/mismatched selected
  BrainCell definition, etc. Admission thresholds remain unchanged.
  Explain's Semantic sense section prints the typed reason for
  each noun role rather than a generic ungrounded status.

To inspect the exact production noun score distributions, run:

```sh
mix run scripts/probe_live_noun_admission.exs
```

This read-only probe prints the selected actor and patient noun IDs,
matching active-cell definitions, complete slate sum, same-surface
noun competitors and typed admission reason. It does not promote
the noun sense or modify synapses/LIFG.

Regression commands:

```sh
mix test apps/core/test/core/intent/structural_statement_test.exs \
  apps/core/test/core/response/projected_memory_reply_policy_test.exs \
  apps/core/test/core/brain/role_semantic_context_test.exs \
  apps/core/test/core/pipeline_contract_test.exs \
  apps/symbrella_web/test/symbrella_web/live/explain_evidence_test.exs \
  --trace
```


### Regression correction: environment-dependent SVO admission

The initial two regression modules incorrectly asserted that a
`Core.resolve_input/2` call with a stripped-down offline lexical
configuration **must** produce a full episodic assertion. In the
user's local test run, that setup produced `episodic_candidates: []`.
As a consequence, 5 of 20 Core tests failed *before* exercising
either memory acknowledgment or structural-intent reconciliation.
The earlier role-context, Core pipeline and web Explain tests passed.

Tests now distinguish:

- **Deterministic policy coverage:** explicit full finite-present SVO
  signatures, actual agreeing LIFG/ATL senses, an attested inflected
  verb BrainCell and revalidated EpisodicProjection/SentenceMeaning.
  These exercise the memory acknowledgment gate and the speech-act
  reconciliation with complete independent provenance.
- **Production coverage:** the live noun-admission probe now mirrors
  the HomeLive lexical settings (`fuzzy: true`,
  `enrich_lexicon?: true`, `lexicon_stage?: true`) while keeping
  response synthesis, durable event writes and model side effects
  disabled. It reports available SVO signatures, projected event
  count, sentence meaning and exact noun probability refusal reasons.

The existing SVO regression tests were updated to verify that
*attributed* user reports can still be stored and recalled without
forcing "I've recorded..." as the conversational reply. This is
different from proving the user intended a memory operation.
Neither meaning verification nor role-confidence requirements were
relaxed. Successful offline fixture tests alone cannot guarantee
that all live messages will have complete dictionary evidence.


## Live noun-score tie audit and unanimous-gloss shadow support (v11)

A live Maple Ridge independent SVO probe for
"The baker cooks a meal." confirmed the structural statement path and
returned one validated episodic candidate. The noun-role restriction
rejected BOTH selected noun senses not because dictionary glosses
were missing, but because **their full LIFG probability slates had
exact ties within the same noun surface**: four competing
`baker|noun|N` entries and nine `meal|noun|N` entries had equal
scores. This is real uncalibrated sense ambiguity, not proof that
`baker|noun|1` and `meal|noun|1` are the meanings intended.

The role context now has a **second, tightly bounded, read-only
fallback** specifically for exact ties. The existing unique-noun
admission, transitivity and specialist restriction gates remain
unchanged.

- It first checks that the entire LIFG probability map is valid and
  normalized, the selected noun ID is highest-scored among its
  same-surface noun alternatives, and at least two IDs are tied
  within `1.0e-6`. This is NOT a confidence measure.
- It reads only existing dictionary BrainCells for that noun norm,
  bounded to 256 retrieved rows and **at most 16 tied noun senses**.
  Each tied ID must occur *exactly once*, have correct noun POS/norm,
  a nonempty bounded definition, and the selected ID's definition
  must agree exactly with its already-active LIFG/BrainCell evidence.
  Missing, duplicated, stale or inconsistent rows fail closed.
- It extracts a tiny number of non-scaffolding words from *every*
  tied sense definition and retains only their **intersection**.
  The intersection can be used as a weak, provenance-tagged
  one-hop role signal. **No particular noun sense is picked or
  verified.** A lack of unanimous words gives
  `:no_unanimous_noun_gloss_terms` and no expanded score.
  A near-tie that isn't exact also fails closed.
- The evidence appears in live Explain with
  `:grounded_tie_consensus`, original tied noun IDs, glossary terms,
  and provenance `:unanimous_tied_dictionary_glosses`. Only original
  utterance words may satisfy specialized domain requirements.
  No new assertion or persistent event is created by the fallback.
- The validator can emit a **provisional** verb hypothesis if this
  unanimity-derived patient evidence meets its **existing**
  thresholds; otherwise it abstains. No literal dictionary
  synonymy or human gold answer is inferred. This remains shadow
  diagnostics; production LIFG, responses, ATL, BrainCells and
  synapses are untouched.

New regression cases verify a shared `food` gloss across tied
`meal` senses, conflicting meal definitions causing abstention,
missing/duplicate noun cells, a nonmaximal nominal winner, and
`cricket` appearing only in noun glosses NOT overriding the
explicit cricket-bowling restriction.

Run:

```sh
cd ~/Symbrella
git pull --ff-only origin development
mix test apps/core/test/core/brain/role_semantic_context_test.exs \
  apps/core/test/core/brain/semantic_sense_validator_test.exs \
  apps/core/test/core/brain/semantic_sense_shadow_test.exs \
  apps/symbrella_web/test/symbrella_web/live/explain_evidence_test.exs \
  --trace
mix run scripts/probe_live_noun_admission.exs
mix run scripts/probe_role_semantic_context.exs
```

The noun-admission probe now prints all exact tied IDs **and their
original dictionary definitions** to reveal whether the real
`baker` and `meal` inventories actually share any usable terms.
Synthetic test glosses establish correctness of the mechanism, not
real-world coverage: the actual nine `meal` definitions may
disagree substantially and produce a justified abstention.


## Score ties vs shared semantic evidence (v12 audit-only)

A subsequent 8-sentence real dictionary A/B run found **1 noun-role
consensus** (the two tied `tree` senses shared *plant/shrub*), but
**0 changes in the final verb-sense hypothesis**: both
`plant|verb|1` and `plant|verb|2` received identical `0.1015`
patient evidence, so the validator correctly abstained. Other noun
roles mostly lacked unanimous content terms across their tied senses.
The A/B summary previously undercounted grounded roles because it
ignored `:tie_consensus_only`; that counter now recognizes it and
separately reports unique-sense and tied-consensus evidence.

A tie in LIFG probabilities **does not imply** the competing senses
have common semantic content. To distinguish these cases, the
read-only `Core.Brain.NounTieConceptAudit.inspect/2` inspects real
dictionary definitions for *every* exact top-scored noun in a bounded
tie (at most 16). It verifies exact dictionary ID membership, selected
gloss concordance and independent structural SVO proof, then reports:

- **Unanimous gloss tokens** found in all tied definitions
- **Partial/recurrent tokens** found in at least two but not all
  definitions, with their supporting dictionary sense IDs
- An explicit `:ambiguous_with_no_unanimous_terms` when there is
  disagreement; no single sense is selected

These are transparent token associations, **not** verified concept
equivalence, entailment, generalization, calibrated likelihoods or
an authorization to use partial agreement in live LIFG.

```sh
mix test apps/core/test/core/brain/noun_tie_concept_audit_test.exs \
  apps/core/test/core/brain/role_semantic_context_test.exs --trace

mix run scripts/audit_noun_tie_concepts.exs
mix run scripts/probe_role_semantic_context.exs
```

The new audit never changes sense scores, episodic assertions,
hippocampal memories, LIFG/ATL, synapses, or response text. It is
separate from the existing fail-closed unanimous-gloss validator.


## Sentence-conditioned semantic cohorts — shadow-only v1

The first production A/B after unanimous noun-gloss expansion was
**8/8 evaluated**, **1** case with actual grounded patient-role
consensus (`tree|noun|1/2` → `plant`), and **zero changes** to
specific verb-sense hypotheses. Both `plant|verb|1` and
`plant|verb|2` received the same patient witness and score,
so exact-sense selection correctly abstained.

`Core.Brain.SemanticCohort.evaluate/2` now groups eligible **base-verb
dictionary senses** by *shared sentence-conditioned lexical anchors*
without forcing an exact dictionary sense winner. The evaluator uses
the full eligible slate from the existing validated projection:

- `SemanticSenseValidator` only exposes full `:cohort_evidence_rows`
  when explicitly called with `cohort_evidence?: true`. Normal
  validator clients are unchanged. The rows have already passed the
  morphology, full-SVO, dictionary inventory, grammatical valency and
  original-sentence specialist-domain gates.
- For every eligible sense, its genuine dictionary definition must
  independently contain the lexical term, and its **own**
  ContextFit/direct sentence match or **bounded already-admitted
  actor/patient role witness** must contain the same term.
- At least two senses sharing the **same evidence origin and term**
  form a tentative cohort (for instance,
  `{:patient_gloss, "plant"}`). Multiple cohorts may overlap.
  **Similarity is NOT transitive:** a group sharing `plant` and a
  group sharing `soil` are not automatically one larger group.
- Per group we report the IDs of its member senses, terms all group
  definitions share, definition terms that distinguish the members,
  their unchanged direct/actor/patient diagnostic scores and
  the **minimum** member support. This minimum is an evidence floor,
  not a sum or calibrated probability. Neither the cohort nor any
  member is labeled independently correct.
- If no independently witnessed shared anchor exists, report
  `:no_supported_cohort`. If the original user SVO and complete
  trusted eligible dictionary slate are unavailable or malformed,
  abstain with `:skipped`.
- Bounded output: maximum 256 eligible dictionary senses, 32 observed
  anchors considered, 8 cohorts retained, 12 detailed members per
  cohort. Group labels are descriptive dictionary-token anchors,
  **not** newly asserted ontology categories.

Live `Core.Brain.SemanticSenseShadow` requests the optional complete
cohort evidence **during its existing single validator evaluation**,
then attaches a bounded `:semantic_cohort` subreport to the real
`:semantic_sense_shadow` Trace event. This avoids a second
dictionary lookup. `ExplainEvidence` and `EvidenceSections`
show a separate **Semantic cohorts (shadow only)** section only
when that actual Trace subreport exists, including saved chat history.
Unprocessed/older messages cannot acquire made-up cohort results.

Controls: `semantic_sense_shadow?: false` disables the entire
shadow validator (and cohorts); `semantic_cohort_shadow?: false`
disables only cohort evaluation and keeps the existing scalar sense
diagnostic. Both default to enabled for normal eligible live chat.
A cohort is **never** allowed to update a sense winner, ATL,
BrainCells, working memory or hippocampal events, synaptic weights,
the action selector or the assistant's reply. This is still
*weak lexical evidence* against a structured SVO, not a general
semantic entailment model or a completed verb argument ontology.

### Test and inspect genuine dictionary examples

```sh
cd ~/Symbrella
git pull --ff-only origin development

mix test apps/core/test/core/brain/semantic_cohort_test.exs \
  apps/core/test/core/brain/semantic_sense_validator_test.exs \
  apps/core/test/core/brain/semantic_sense_shadow_test.exs \
  apps/core/test/core/pipeline_contract_test.exs \
  apps/symbrella_web/test/symbrella_web/live/explain_evidence_test.exs \
  --trace

mix run scripts/probe_semantic_cohorts.exs
```

Use live chat "The gardener plants a tree." and inspect **Explain**
for the existing per-sense abstention (if still ambiguous) followed by
the new **Semantic cohorts (shadow only)** section. A cohort may show
shared patient evidence while no individual sense is chosen.

Do not promote this into authoritative comprehension until an
independently adjudicated holdout set demonstrates improved behavior
without harmful sense conflation, invalid specialty readings or
misleading evidence attribution.


### Live cohort observation: action selector statement routing

The first live chat with "The gardener plants a tree." confirmed
`SemanticCohort` formed the group
`["plant|verb|1", "plant|verb|2"]` using the independently
grounded tied-tree patient cue `plant`. Its minimum diagnostic support
was `0.1015`, and individual verb-sense selection correctly
abstained. This establishes that the live Explain path works, NOT
that any dictionary sense or cohort has been independently verified.

An adjacent routing issue was exposed: Core's intent was correctly
`:statement` (0.68), but `Brain.ActionSelector` did not list
`:statement` among its recognized conversational intents. The
unknown-intent fallback then offered a clarification action,
which mood modulation ranked above responding normally. This is
independent of semantic cohort scoring.

`Brain.ActionSelector.conversational_intent?/1` now explicitly
accepts `:statement`, routing such utterances to
`:answer_user` with `:conversation_continuation` reason. Existing
`:memory_write`, health/safety, explicit question and command
paths remain higher-priority. The change does NOT generate a
new semantic meaning or automatically write memories.

```sh
mix test apps/brain/test/brain/action_selector_test.exs \
  apps/core/test/core/brain/action_selection_test.exs \
  apps/core/test/core/brain/semantic_cohort_test.exs \
  apps/symbrella_web/test/symbrella_web/live/explain_evidence_test.exs \
  --trace
```

The preceding live response echoed the statement verbatim; the
routing fix is not a guarantee that an LLM will stop parroting.
That requires a separate prompt/response-quality evaluation.


## Expressing projected comprehension in live responses (v13)

Live chat showed that Symbrella correctly constructed a patient-conditioned
`plant` cohort while the local model returned the user's entire
"The gardener plants a tree." sentence verbatim. This is not a
failure of the grammatical or cohort shadow stages; the response
generator lacked a source-grounded natural-language expression contract.

`Core.Response.ProjectedExpression.from_si/1` now builds a bounded
expression frame **only** for fully revalidated user-reported
positive finite-present SVO assertions whose intent is already
`:statement`. It independently requires existing
`SynapticExperience` proof, revalidates `EpisodicProjection`, and
rebuilds `SentenceMeaning` exactly. It carries the original actor,
predicate surface, independently attested base verb lemma and
patient. If an actual semantic shadow cohort exists, only its
short shared lexical cue(s) and a private bounded set of
**distinguishing** glossary terms are retained, with explicit
nonverification flags. No particular dictionary sense or asserted
external event is promoted.

`Core.Response.Attach.build_response_si_like/1` passes this frame
to `Core.Response.plan/2`, and `LlmPrompt` places the bounded
statement guidance after general response-posture/uncertainty
instructions. The local LLM is instructed to give a natural,
nonverbatim conversational acknowledgment of the user-described
action without inventing the occurrence, instrument, purpose,
location, outcome, or exact sense of the action. A normal bare
assertion is not a memory request or an obligation to ask another
question. The frame is never inserted for unvalidated SVO,
unknown intents, questions, assistant output, or forged ATL roles.

After existing model-generation, source-grounding and reflection
steps, `LlmSynthesis` invokes the narrow
`ProjectedExpression.review_draft/3` post-pass. It substitutes a
deterministic, role-only paraphrase **only** if:

1. The model returned exactly the user sentence (possibly twice
   with harmless whitespace/punctuation differences); or
2. A real unresolved semantic cohort supplies a distinction term
   from an exact competing dictionary gloss, the user did NOT
   mention that term, but the model's output did. This avoids
   asserting, for example, that the gardener planted "in soil"
   when only the patient role/tree had been observed.

The conservative repair uses the attested base-verb morphology
("plant" → "planting") in a reported-speech construction such as:

    I understand you're describing the gardener planting a tree.

This does not assert the planting happened. Unknown or unsuitable
inflections fall back to explicit actor/patient wording. The
replacement passes the existing reflection reviewer again.
Final response metadata contains a genuine `:repair` state, an
issue of `:projected_statement_echo` or
`:unverified_sense_specificity`, original/final text hashes, and
a bounded `:projected_expression_repair` trace with
`:promoted_sense?: false` and `:stored_memory?: false`.
This prevents the Explain review from falsely presenting an
automatic repair as acceptance of the model's first draft.

The natural-language route never feeds generated text back to
LIFG/ATL, sense scores, cohort formation, BrainCells, or synaptic
weights. Normal response persistence via the preexisting
`SideEffects.record_turn` policy remains separate; this
expression feature itself performs no cognitive writes.

### Test, probe, and live chat

```sh
cd ~/Symbrella
git pull --ff-only origin development

mix test apps/core/test/core/response/projected_expression_test.exs \
  apps/core/test/core/response/llm_prompt_test.exs \
  apps/core/test/core/response/llm_route_flow_test.exs \
  apps/core/test/core/brain/semantic_cohort_test.exs \
  apps/core/test/core/brain/semantic_sense_shadow_test.exs \
  apps/symbrella_web/test/symbrella_web/live/explain_evidence_test.exs \
  --trace

mix run scripts/probe_projected_expression.exs
```

Then retry *The gardener plants a tree.* in Phoenix chat.
The sentence should stay `:statement` and the semantic
cohort remain the same. The LLM can respond freely within the
grounded constraints; if it echoes verbatim or invents a
discriminating dictionary detail, the post-pass substitutes
the conservative reported-action paraphrase.

This is a narrow, auditable first step toward natural language
generation from projected comprehension. It is NOT full compositional
semantic generation, formal entailment checking, a general
hallucination detector, or independent truth verification.
More complex syntax and non-SVO utterances intentionally use the
existing response path until evidence-specific controls are developed.


### Live speech-act fidelity: prevent simple present → completed action

The first production chat after the expression module recognized both
`The gardener plants a tree.` and `The baker cooks a meal.` as
`:statement`, chose `:answer_user`, and retained correct shadow
cohorts; however, the LLM changed the utterances into
`The gardener has planted a tree.` and
`The baker has cooked a meal.` with a false review
`:accept`. The present-perfect statements imply a past completed
action with present relevance, while the user provided only a
finite simple-present assertion.

This is a **temporal/aspect drift**, not an exact echo or a supported
choice of verb dictionary sense. `ProjectedExpression.prompt/1`
now explicitly instructs the generator to preserve source
simple-present viewpoint. `ProjectedExpression.review_draft/3`
now checks the output for **strict standalone clause** transformations
with the identical independently bound actor and patient and
conservatively recognized participial / past / progressive / future
morphology. This acts only on eligible, revalidated positive
simple-present SVO user reports and does not apply to open questions,
unvalidated projected expressions, long explanations, or quotations.
Unrecognized inflections fail closed, rather than assuming a shifted
tense merely because `has` occurs.

When the model drafts one of the disallowed transformations, the
existing short reported-action fallback is substituted:
`I understand you're describing the gardener planting a tree.`
This does not claim an actual planting event or a chosen exact
dictionary sense. The reflected final output gets status `:repair`,
issue `:projected_tense_aspect_drift`, and bounded source-only
`:projected_expression_repair` metadata with trigger
`:unsupported_temporal_shift`. Existing echo and disputed-sense
guards remain active. No BrainCell, LIFG, synapse or memory action
is introduced by the new check.

New real-chat-draft regression scenarios verify both gardener and
baker examples via direct review and the actual mocked LLM +
`Core.Response.Attach` pipeline, plus structurally aligned
child/ball irregular participles and narrow false-positive exclusions.

```sh
cd ~/Symbrella
git pull --ff-only origin development
mix test apps/core/test/core/response/projected_expression_test.exs \
  apps/core/test/core/response/llm_prompt_test.exs \
  apps/core/test/core/response/llm_route_flow_test.exs \
  apps/core/test/core/pipeline_contract_test.exs \
  apps/symbrella_web/test/symbrella_web/live/explain_evidence_test.exs

mix run scripts/probe_projected_expression.exs
```

The probe prints both the exact echo fallback and the completed-action
correction for four source assertions, without sending LLM requests or
changing live response text. This is not a full natural-language
inference or full semantic entailment checker; it targets demonstrable
unlicensed shifts with explicit evidence gates.
