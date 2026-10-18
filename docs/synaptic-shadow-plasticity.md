# Experience-driven synaptic plasticity — shadow v2

This milestone is **not a live cognitive learning circuit**. It adds reversible
experience-weighted connections to an ephemeral GenServer, with no database
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
with its own two edges and review verdict. An in-process random HMAC key
hides the original event string from the ledger. No raw utterance, reviewer
name or original patient/actor phrase is persisted; lexical sense IDs remain
in the provisional edge graph.

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

- No durable synapses; the GenServer starts empty after each BEAM restart.
- At most 512 experience receipts and 1,024 concurrent edges in memory.
- No reward model, automatic contradiction understanding, connection decay,
  consolidation, or learned activation spreading.
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

Before any durable or live rollout, require authenticated review attribution,
independent evidence lineage, contradiction adjudication, a schema for
bounded reversible receipts, and held-out counterfactual improvements
against an activation-only baseline.
