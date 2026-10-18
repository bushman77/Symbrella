# Experience-driven synaptic plasticity — durable shadow v4

This milestone is **not a live cognitive learning circuit**. It adds reversible
experience-weighted connections to a supervised GenServer, with no database
changes and no effect on LIFG, Axon, activation, response policy or memory facts.

## Pipeline

\`Core.resolve_input/2\` completes ordinary comprehension and
\`Core.Brain.Activation.notify/2\`; then
\`Core.Brain.SynapticExperience.attach/2\` checks the current SemanticInput.

Only an explicit human-origin user turn and a uniquely revalidated
finite-present SVO assertion can be eligible. It must pass the same grammar,
lexical/ATL role agreement, and participant projection gates used by
\`Core.Semantic.SentenceMeaning\` and
\`Core.Brain.SemanticPropagation\`.

If eligible, two typed *provisional* connections are proposed:

- predicate-surface sense ID -> actor noun sense ID, type \`:actor\`
- predicate-surface sense ID -> patient noun sense ID, type \`:patient\`

Neither ID's **meaning** becomes verified by observing a grammatical role.
The observed event is user-attributed, not externally verified.

Normal chat marks provenance using the internal \`experience_origin: :user_turn\`
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

\`Brain.Synapses.Shadow\` retains a bounded number of keyed receipts, each
with its own two typed edges and a reversible review history. In development,
a stable private local HMAC key allows receipts to survive BEAM restarts
without storing raw statements or reviewer names. Source sense IDs and
the existing provenance classification are retained, but do not verify truth.

## Manual correction and evidence grades

\`Core.Brain.SynapticExperience.review(si, :reject, "local-reviewer")\` removes
**only that receipt's** two weight contributions and retains the receipt
for replay prevention. Independent supporting receipts still count.

\`review(si, :confirm, "local-reviewer")\` changes that receipt's contribution
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

\`Core.Brain.SynapticExperience.preview(si, ids)\` requires the same validated
SVO input plus candidate IDs present in the original lexical candidate slate.
It returns per-candidate shadow support for the currently selected patient
and does not alter candidates or ranking. The Brain-level \`preview/3\` is a
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

\`\`\`sh
cd ~/Symbrella
git pull --ff-only origin development

mix test apps/core/test/core/brain/synaptic_experience_test.exs --trace

mix run scripts/probe_synaptic_experience.exs
mix run scripts/probe_synaptic_correction.exs
\`\`\`

After a correction, the second probe should show: recorded weight 0.01,
reviewed rejection with no remaining patient edge, duplicate replay unable
to recreate it, and explicit confirmation restoring weight 0.03.

To opt out of experience recording on a Core call, use
\`synapse_shadow?: false\`. No existing live cognition uses those weights,
regardless of this setting.

Before any live cognitive rollout, require authenticated review attribution,
independent evidence lineage, contradiction adjudication and held-out counterfactual improvements
against an activation-only baseline.


## Explicit patient-correction proposals (shadow v3)

The production Core pipeline now also runs
\`Core.Brain.SynapticCorrection.attach/2\` after ordinary experience admission.

This phase recognizes a **very narrow** correction construction:

\`\`\`text
Actually, the artist draws a diagram, not a picture.
\`\`\`

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
4. Each independent interpretation must pass \`SynapticExperience.extract/1\`.
   Their actor and predicate **sense IDs** must match, their patients differ,
   and the prior's exact experience receipt must already exist.
5. The prior must not already be rejected, and the new event must not already
   have an independently recorded receipt.

On success, \`Brain.Synapses.Shadow.propose_correction/2\` produces a read-only
\`:review_proposed\` report, attached as a
\`:synaptic_correction\` trace event. The trace contains bounded sense IDs
and review status, not original sentence text or evidence binaries.

The proposal **never** rejects the old receipt, registers the new event,
changes an edge weight, updates LIFG, or alters Hippocampus facts. Two real
events may have the same actor and predicate but different patients; even an
explicit "actually" is not enough to prove that an earlier event was false.

The manual \`SynapticExperience.review/3\` remains the only route for modifying
review verdicts. There is no authenticated end-user approval UI and no
automatic contradiction adjudication yet. The proposal itself is ephemeral:
durable storage contains only accepted experiences and their review decisions,
not unapproved correction proposals.

\`\`\`sh
mix test apps/core/test/core/brain/synaptic_correction_test.exs --trace
mix run scripts/probe_synaptic_contradiction.exs
\`\`\`

Disable with \`synapse_correction?: false\` or
\`synapse_shadow?: false\` in a Core call. Before enabling automatic
corrections or long-lived weights, add attributable event-instance identity,
verified provenance across sessions, authenticated human approval and
counterfactual evaluation against unchanged weights.


## Durable receipt consolidation (shadow v4)

Development config:

\`\`\`elixir
config :brain, Brain.Synapses.Shadow,
  durable?: true,
  dir: "tmp/synapses"
\`\`\`

Test mode defaults to \`durable?: false\`. Production is deliberately disabled
by default until deployment and operational controls are reviewed. This
feature *persists shadow observations*, not conclusions or model weights.

Private local files in the git-ignored directory:

- \`secret.key\`: stable random 32-byte receipt/snapshot HMAC key, mode 0600.
- \`receipts.json\`: authenticated, atomically replaced snapshot containing a
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

On startup \`Brain.Synapses.Shadow\` authenticates and structurally validates
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
are rejected with \`:review_capacity\` pending a future migration.

The two evidence classes remain separate:

- Unreviewed reports contribute 0.01 each to
  \`provisional_weight\` and \`provisional_experiences\`.
- Human-reviewed confirmations contribute 0.03 each to
  \`reviewed_weight\` and \`reviewed_experiences\`.
- Rejected receipts contribute **zero**. The combined \`weight\` is capped at
  0.08 for observation and read-only preview only.

Neither group certifies a dictionary sense or independent event truth.
Reviewers are attributed with a stable keyed digest; a caller-supplied
reviewer string is not identity verification. Only trusted in-process code
may call the review API.

### Validate persistence on the production dictionary

\`\`\`sh
cd ~/Symbrella
git pull --ff-only origin development

mix test apps/brain/test/brain/synapses/receipt_store_test.exs \
  apps/core/test/core/brain/synaptic_experience_test.exs \
  apps/core/test/core/brain/synaptic_correction_test.exs --trace

mix run scripts/probe_synaptic_consolidation.exs
\`\`\`

The standalone probe uses real Core for the SVO experience, then a separate
private shadow instance to test process restart, reviewed rejection,
duplicate suppression and reversible confirmation. It never injects new
weights into the supervised chat shadow. It removes its probe files.

To inspect the **live supervised shadow** after starting Phoenix:

\`\`\`elixir
Brain.Synapses.Shadow.snapshot()
\`\`\`

Expected in development: \`persistence_status: :ready\` and
\`persisted?: true\`. A warning status of \`:unavailable\` means the shadow
is read-only until local storage is repaired or recovered.

Existing snapshots must never be committed to Git or uploaded with bug
reports. Keep backups local and secured. Clearing \`tmp/\` will intentionally
erase this experimental synaptic archive.


## One-hop, role-conditioned synaptic propagation (shadow v5)

After comprehension and neuron feedback, **before** an experience is recorded,
\`Core.Brain.SynapticPropagation.attach/2\` inspects existing LIFG candidate
slates and the independently validated SVO patient. The order matters: an
experience must not provide artificial evidence for *its own* interpretation.

\`Brain.Synapses.Propagation.compare/4\` models a **read-only reverse traversal**
of an existing typed connection \`{verb_sense_id, noun_sense_id, :patient}\`:
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

\`\`\`text
shadow_signal = min(
  0.03,
  0.25 * provisional_weight + 1.00 * reviewed_weight
)
\`\`\`

\`provisional_weight\` and \`reviewed_weight\` already come from separate,
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

\`\`\`sh
mix test apps/brain/test/brain/synapses/receipt_store_test.exs \
  apps/brain/test/brain/synapses/propagation_test.exs \
  apps/core/test/core/brain/synaptic_propagation_test.exs \
  apps/core/test/core/brain/synaptic_experience_test.exs \
  apps/core/test/core/brain/synaptic_correction_test.exs --trace

mix run scripts/probe_synaptic_consolidation.exs
mix run scripts/probe_synaptic_propagation.exs
\`\`\`

The propagation probe uses real production-dictionary comprehension for the
initial observation and held-out next-turn sentences, plus an isolated in-memory
synaptic store. It compares on versus off against a matching patient and an
unrelated patient, and checks for zero mutation.

To disable the new comparison on a Core call, specify
\`synapse_propagation?: false\`. The \`synapse_shadow?: false\` switch disables
both recording and automatic trace comparison, though the explicit
\`SynapticPropagation.compare/2\` developer probe remains read-only.

### Required next gate before using live synaptic signals

Establish an evaluation set with independent contexts, true ambiguity in
existing verb sense slates, review-grade outcomes, no training/test leakage,
and comparable baseline LIFG scores. Measure held-out accuracy, abstention,
contradiction handling and error regressions against disabled propagation.
Only then consider a bounded opt-in perturbation of candidate scores.


## Counterfactual experience-sensitive sense evaluation (shadow v6)

\`Core.Brain.SynapticDisambiguation.evaluate/2\` analyzes a previously
interpreted human-origin finite SVO without changing it. It uses the **actual**
\`candidate_probs\` associated with its selected verb's LIFG token, not
synthetic "winner" scores. Only evaluate when:

1. \`Core.Brain.SynapticExperience.extract/1\` independently revalidates the
   current utterance, role identities and ATL/LIFG agreement.
2. The exact experience has **never appeared** in the shadow receipt ledger
   (including rejected receipts). This is a narrow replay-leakage check;
   it cannot prove that a semantically equivalent paraphrase was unseen.
3. \`sense_candidates\` and \`candidate_probs\` contain exactly the same
   scored lexical candidate IDs, with no duplicate IDs. All probabilities
   must be finite, in \`[0,1]\`, and sum approximately to one.
4. The candidate slate has **at least two distinct real verb sense IDs** and
   at most 256 total candidates when `synapse_eval_full_scores?: true` is
   explicitly enabled in LIFG options. Stage1 currently omits large \`candidate_probs\`
   maps; cases with 65+ candidates are marked insufficient rather than
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

A separate \`aggregate/1\` reports total eligible cases, missing labels,
skips, projected flips, abstention transitions and, only with independently
supplied labels, right-to-wrong versus wrong-to-right changes.

**No accuracy figure is inferred from Synbrella's own LIFG selection**, from
the previously learned sense, or from the presence of a positive signal.
A \`gold_id\` is accepted only when it exists in the current scored dictionary
slate AND the caller explicitly supplies \`split: :holdout\` and
\`label_source: :human_curated | :external_gold\`. This is a caller-asserted
label origin, not machine authentication of the label.

To inspect actual candidates and honestly determine whether a competitive
evaluation is possible, run:

\`\`\`sh
cd ~/Symbrella
git pull --ff-only origin development

mix test apps/brain/test/brain/synapses/propagation_test.exs \
  apps/core/test/core/brain/synaptic_propagation_test.exs \
  apps/core/test/core/brain/synaptic_disambiguation_test.exs --trace

mix run scripts/probe_synaptic_disambiguation.exs
\`\`\`

The real-dictionary probe attempts eight separate **training sentence /
held-out sentence** pairs in its own isolated shadow store. It records
eligible training sentences there, **never** the held-out turns. It reports
actual score-map size, skip reason, comparison status, projected flips and
full summary. If it finds no complete 2+-verb-sense slate, it prints
\`INSUFFICIENT_COMPETITIVE_EVIDENCE\` and makes **no positive claim**.
Its default output is unlabeled. An unlabelled flip is not progress.

For independent manual gold review, BEFORE looking at projected winners,
optionally create a local, git-ignored JSONL file at
\`tmp/eval/synaptic_disambiguation_gold.jsonl\`. Example shape
(**the following ID is a placeholder, not a gold annotation**):

\`\`\`json
{"example":1,"gold_id":"REPLACE_WITH_ACTUAL_DICTIONARY_VERB_SENSE_ID","source":"human_curated","basis":"Explicit dictionary sense definition checked independently"}
\`\`\`

The file is strictly validated and errors on malformed/duplicate entries.
A label is accepted only for a complete competitive slate containing
that exact sense ID. Human-curated labels are NOT automatically verified;
reviewed sense definitions, independent adjudicators, truly unseen events,
time-based data splits and sufficient sample size remain future work.

No production LIFG scores, BrainCells, durable receipts or predictions are
modified by this evaluation. Do **not** enable live learned priors based only
on a single positive synthetic fixture or an unlabeled projected flip.
