# Synaptic feedback: guarded lexical integration

This experiment closes the **read-only observation path** between durable
synaptic receipts and a proposed second-pass lexical choice. The default
remains shadow-only. It does not train the LIFG model, change BrainCell weights,
or treat word co-occurrence as verified meaning.

Pipeline stage: \`Core.Brain.SynapticFeedback.attach/2\` runs immediately after
\`Core.Semantic.SentenceMeaning.attach/1\`, before downstream comprehension,
working-memory writes, and synaptic admission. The existing independent
\`SynapticPropagation.attach/2\` A/B trace is retained.

## Preconditions for a candidate revision

* Human-origin, structurally revalidated finite-present SVO.
* Previous experience must be in the shadow receipt store; the present exact
  user report is excluded from held-out comparison by receipt HMAC.
* Complete scored post-softmax LIFG candidate slate, with at least two genuine
  **same-surface verb** senses. Nouns, fallback IDs and manufactured base lemma
  sense identities do not compete.
* A close original baseline (at most 0.02 raw probability gap), reviewed
  signal at least 0.01, and a counterfactual margin at least 0.02.
* Original selected LIFG sense must be the baseline top ID; existing MWE
  ownership and prior ATL corrections are not overridden.
* Target sense must exist in both the original candidate bucket and live
  active-cell IDs. Rebuild ATL via the **pure reducer**, recompute episode and
  sentence meaning, and independently verify the typed SVO relation.
* A revised sense remains **lexically uncertain**. Original per-token
  probabilities are preserved as source estimates, never passed off as
  calibrated learned logits.

## Two explicit live gates

Runtime config (defaults false in \`config/config.exs\`):

\`\`\`elixir
config :core, :synaptic_feedback_live_enabled?, true
\`\`\`

Caller option:

\`\`\`elixir
synaptic_feedback_live?: true
\`\`\`

Both must be true for the experimental revision path. Neither is recommended
for normal deployment until independently curated held-out cases show positive
net improvement and zero unacceptable right-to-wrong regressions.
Do not activate live adaptation merely because a synthetic test passes.

Ordinary diagnostic \`Core.resolve_input/2\` carries a compact
\`:synaptic_feedback\` trace entry reporting the baseline and projected
counterfactual, the guard outcome and any accepted change. It is safe to keep
\`synaptic_feedback_live?: false\` while collecting evidence.

Unit tests:

\`\`\`sh
cd ~/Symbrella
mix test apps/core/test/core/brain/synaptic_feedback_test.exs apps/core/test/core/brain/synaptic_disambiguation_test.exs apps/core/test/core/brain/synaptic_propagation_test.exs
\`\`\`

Run these and existing umbrella tests locally before considering both gates.
The GitHub connector cannot run the laptop's Elixir runtime.
