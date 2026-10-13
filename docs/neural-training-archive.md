# Neural training archive (development milestone 1)

This is the first **observation and feedback recorder** for Symbrella's
proposed Nx/Axon nervous-system development. It does **not** train Axon, change
Hippocampus retrieval, or change how Core chooses responses.

## Enable for a local Termux session

From the umbrella root, after updating the development branch:

```sh
SYMBRELLA_TRAINING_ARCHIVE=on iex -S mix phx.server
```

The recorder is **off by default**. Set
`SYMBRELLA_TRAINING_DIR=/your/private/directory` to move the archive.
Relative paths are resolved from the application's current working directory.

```sh
tail -f tmp/training/turns.jsonl
tail -f tmp/training/memory_events.jsonl
```

The second file is created only when there is an archived memory event.

## Records

Each file is UTF-8 JSONL: **one complete JSON object per line**.

- `turns.jsonl`: one completed *human -> assistant* exchange with a randomly
  generated turn ID, session ID, both message texts, response metadata,
  versioned cognitive features/evidence and an explicitly **unreviewed** label.
  The record retains LIFG/semantic choices, alternatives, grammar signatures,
  comprehension, mood and other selected `SemanticInput` fields when present.
- `memory_events.jsonl`: a separate record per *attached* hippocampal episode
  and per recognized fact-memory reply operation. Every event references the
  same `turn_id` and `session_id` as its completed turn. Episodic events
  preserve the retrieval score, timestamp, rank and episode contents.
  `episode_fingerprint` is a content fingerprint, **not** an authoritative,
  database-stable episode identifier. `used_in_response: null` means unknown,
  not false.
- `feedback.jsonl`: manually reviewed corrections referring to prior
  turn IDs. The original observation remains unchanged.

The recorder runs when a Phoenix LiveView chat request completes successfully.
Cancelled tasks, crashed tasks and unsolicited curiosity messages do not become
completed human/assistant turns. Direct API calls outside HomeLive are **not**
recorded by this hook. Not all hippocampal operations appear in this version:
`memory_events.jsonl` describes attached evidence and fact-memory replies,
not every encode, query attempt or admission decision. Those will require
explicit instrumentation and stable episode IDs in a subsequent milestone.

Memory-only fast paths may have less cognitive evidence than the ordinary
semantic pipeline. Missing fields are not guessed.

The existing `SYMBRELLA_STATE_DUMP=all` file remains an independent,
human-readable debugging facility, **not** a JSONL training data source.

## Human-reviewed feedback

Identify the `turn_id` in `turns.jsonl`, then, from IEx, explicitly supply
a label. Example (replace IDs and accepted sense with independently verified
values that actually exist in your dictionary):

```elixir
Core.TrainingArchive.record_feedback(%{
  session_id: "s-example",
  turn_id: "turn-example",
  reviewed_by: "developer",
  label: %{
    kind: :word_sense,
    token_index: 2,
    accepted_id: "replace-with-verified-dictionary-id"
  },
  note: "This was a country abbreviation, not a pronoun."
})
```

This only **stores** feedback. No model weights are changed. Subsequent
training tools must validate that the referenced turn exists, the accepted ID
is available in its candidate slate, and train/evaluation groups do not leak.

## Privacy, durability, and limits

- `tmp/` is git-ignored. Files are chmod `0600` and the training directory
  `0700`. Never push these files to GitHub, or include them in bug reports.
- Turns can contain private user text, facts, and memories; use intentionally
  authorized conversations and establish retention/deletion practices.
- The archive is append-only, with no automatic rotation, compression, or
  retention limits. Consider space consumption on Termux.
- Appends are synchronized per local directory, but writing a turn and its
  separate memory-event file is **not an atomic transaction**. A disk write
  failure can leave a completed turn without all associated memory events.
  Check application warnings if you suspect storage errors.
- A turn ID is random; its session ID is the currently supplied chat-session
  value. IDs should be used as joins, not proof of a specific user identity.
- These records contain raw observations. A user message, recalled episode,
  LIFG winner, or assistant answer is not automatically a verified label.

## Tests

```sh
mix test apps/core/test/core/training_archive_test.exs
mix test
```

Check that both `turns.jsonl` and `memory_events.jsonl` appear after a
completed chat turn with episodic evidence, and that records share a turn ID.


## Reviewed training-data builder

The read-only builder at `apps/brain/lib/brain/neural/training_builder.ex`
joins the archive's turn, memory-event and human-review JSONL files.
Two independent first-generation Nx learning targets are supported:

- `memory_relevance`: whether a specific retrieved hippocampal episode
  was relevant. Feedback must include the exact episode fingerprint and an
  explicit `relevant: true` or `false`.
- `uncertainty`: an explicitly human-reviewed score between 0 and 1.

Other kinds of review, including `word_sense`, are retained but **not**
treated as labels for these two tasks.

To review a recalled memory from IEx, replace placeholders with actual IDs:

```elixir
Core.TrainingArchive.record_feedback(%{
  session_id: "actual-session-id",
  turn_id: "actual-turn-id",
  reviewed_by: "developer",
  label: %{
    kind: "memory_relevance",
    episode_fingerprint: "actual-64-character-hex-fingerprint",
    relevant: true
  }
})
```

For an uncertainty review:

```elixir
Core.TrainingArchive.record_feedback(%{
  session_id: "actual-session-id",
  turn_id: "actual-turn-id",
  reviewed_by: "developer",
  label: %{kind: "uncertainty", value: 0.75}
})
```

These calls only record reviews; no weights are changed. Once there are
reviewed examples from at least **two independent sessions**, run:

```sh
mix neural.dataset --task memory_relevance --dir tmp/training
mix neural.dataset --task uncertainty --dir tmp/training
```

The Mix command compiles and builds Nx tensors, but does not start the LLM,
write training files or change the model. It displays tensor dimensions and
reviewed-example counts without revealing conversation content.

Programmatic usage (from IEx):

```elixir
{:ok, dataset} =
  Brain.Neural.TrainingBuilder.build("tmp/training", :memory_relevance)

# dataset.train.x / dataset.train.y -> Nx f32 tensors
# dataset.eval.x  / dataset.eval.y  -> held-out Nx f32 tensors
```

The builder validates reviewed-by/source, joined session and turn IDs,
memory-episode fingerprints and labels; rejects duplicate review targets and
malformed JSONL; and uses a deterministic **session-grouped** evaluation split.
It rejects splits that reuse the same recalled episode fingerprint across
train and evaluation sets. Each source file is capped at 50,000 lines in this
initial resource-conscious implementation.

The first feature schema is versioned and has twelve normalized signals:
confidence (plus an availability flag), mood DA/5HT/Glu/NE, LIFG-choice count,
input-token count, recall score/rank/count and presence.

The Nx batches **are not directly compatible** with the existing
`Brain.SelfCalibration.Training` model, which uses different features and
output targets. The next phase is to define and train an explicit task-matched
Axon circuit. This builder does not yet retrain, evaluate model predictions,
checkpoint weights, or instrument *every* hippocampal write and recall attempt.

## Hippocampal operation tracing (development milestone 2)

When `SYMBRELLA_TRAINING_ARCHIVE=on` is set, a chat worker activates
`Brain.Hippocampus.TurnTrace` while preparing a human-message reply.
The final LiveView identity-correction path is traced separately and its
events are combined with the worker's events.

The Hippocampus public API now captures:

- `episodic_encode`: an episode successfully written through
  `Brain.Hippocampus.encode/2`; includes the actual episode, encoding time,
  and a content fingerprint. This does not imply the episode was persisted to
  PostgreSQL successfully, because the existing writer is best-effort.
- `recall_attempt`: every observed call to `recall/2`,
  `recall_self_memories/2`, or `attach_episodes/2`, including zero-result
  attempts. Includes a per-query `recall_id`, projected cues, search
  operation, requested source and candidate count.
- `episodic_recall`: each ranked result from that call with its
  `recall_id`, score, rank, episode content and fingerprint.
- `fact_lookup`: attempted fact-key lookup and whether a result was found.
  The result can contain private information; keep the JSONL files private.

The previously implemented `fact_write`, `fact_recall`, and final
`SemanticInput`-based attached-episode projections remain available.
When a traced recalled episode matches one already attached to SemanticInput,
the final-state fallback does not duplicate it.

All events are joined with the same `session_id` and `turn_id` as
`turns.jsonl`, written in the existing private `memory_events.jsonl`.

From another Termux shell:

```sh
jq -r '.kind' tmp/training/memory_events.jsonl | sort | uniq -c

jq -r 'select(.kind=="recall_attempt") |
  [.turn_id,.operation,.result_count,.hit] | @tsv' \
  tmp/training/memory_events.jsonl | tail -20

jq -r 'select(.kind=="episodic_recall") |
  [.turn_id,.episode_fingerprint,.rank,.score] | @tsv' \
  tmp/training/memory_events.jsonl | tail -20
```

Review an actual `episodic_recall` fingerprint from the last command using
`Core.TrainingArchive.record_feedback/1` as documented above. After
reviewing examples from at least two separate sessions:

```sh
mix neural.dataset --task memory_relevance
```

### Current scope and non-goals

- Per worker, at most 512 events are retained; reaching the limit writes a
  `trace_truncated` marker. The training builder refuses to build a dataset
  containing any truncated hippocampal trace, rather than silently training
  on incomplete observations.
- Tracing is **opt-in** and process-local; memory operations on independent
  background tasks or non-chat API calls are not automatically associated
  with an active chat turn.
- A returned memory is a *candidate*, not proof it influenced the answer:
  `used_in_response: null` means unknown. Humans must review relevance.
- Search trace results come from Hippocampus before Core's additional local
  Jaccard filter, if one applies. They may therefore include candidates that
  the final SemanticInput does not retain.
- Trace records do not themselves modify model weights or the Hippocampus
  retrieval/admission policy.
- Existing archive data is not retroactively populated; only **new**
  interactions after restarting the runtime can produce these events.
- A permanent database-stable episode ID and a review interface are follow-up
  work. Current fingerprints are derived from episode content plus time, and
  are not a substitute for a durable episode primary key.

### Regression tests

```sh
mix test apps/brain/test/brain/hippocampus/turn_trace_test.exs
mix test apps/core/test/core/training_archive_test.exs
mix test apps/brain/test/brain/neural/training_builder_test.exs
```

## Episodic attachment cue fix

Live archive observations showed `attach: 0 results` while
`recall_self` retrieved memories. Code inspection identified a cue mismatch:
`Core.Recall.Execute` passes normalized words through
`hippo_opts[:cues]`, but `Brain.Hippocampus.handle_call({:attach, ...})`
previously passed the entire `SemanticInput` to a search implementation
that reads `winners` from map-shaped cues. Ordinary
`Core.SemanticInput` stores its lexical material in `tokens`.

Attachment now honors provided non-empty `cues`. With no cues (or an empty
list), it derives normalized terms from SI tokens when present; existing
`winners`-based callers remain compatible. The observed query cues and
the cues actually used for the lookup are now the same. Traced
`recall_attempt` events also include `cue_count` and `min_jaccard`
for diagnostic analysis.

This is a retrieval correctness fix, not a similarity-threshold reduction:
genuine zero-overlap searches still return no episode.

Run the dedicated regression suites:

```sh
mix test apps/brain/test/brain/hippocampus/hippocampus_recall_test.exs \
  apps/brain/test/brain/hippocampus/hippocampus_attach_episodes_test.exs \
  apps/core/test/core/recall/execute_hippo_integration_test.exs \
  apps/brain/test/brain/hippocampus/turn_trace_test.exs
```

Inspect new attempts after restarting the server:

```sh
jq -r 'select(.kind=="recall_attempt") |
  [.operation, (.cue_count // "unknown"), (.min_jaccard // "default"),
   .result_count, .hit] | @tsv' tmp/training/memory_events.jsonl | tail -20
```

Even after this change, a recall with no lexical overlap may be empty.
If `cue_count=0` persists with real conversation tokens, inspect the
upstream tokenization/recall invocation timing rather than changing the
Jaccard threshold blindly.


## First Axon experiment using existing archived memories

The initial archive may contain multiple `attach` recall candidates but no
`feedback.jsonl`. There is no valid supervised target until a person reviews
those candidates. You can use **existing** candidates immediately; no
LiveView review interface is required.

```sh
cd ~/Symbrella
mix neural.review --dir tmp/training
```

The command pairs each archived ordinary `attach` episode with its original
user question, shows the recalled content and retrieval score, then asks:

- `y` = human judge says the episode is relevant to the user question
- `n` = human judge says the episode is irrelevant
- `s` = unsure / skip without adding any label
- `q` = exit, retaining earlier reviews

All actions except `y` and `n` are nonlabeling. The command appends verified
`memory_relevance` feedback through `Core.TrainingArchive.record_feedback`.
Already reviewed turn+episode pairs will not be relabeled by the queue. The
records remain private under `tmp/training/`.

For a first supervised **pipeline smoke test**, even when all reviewed turns
share the same session:

```sh
mix neural.train --task memory_relevance --smoke
```

This mode passes all reviewed examples through the 12-feature builder,
trains a tiny `12 -> 4 ReLU -> 1 sigmoid` Axon model with MSE loss, and
saves `tmp/training/memory_relevance_smoke.etf`. It deliberately uses the
**same examples** for training and reported in-sample loss: there is no holdout
test or claim of generalization. A model trained on a handful of episodes is
not ready to advise retrieval ranking.

When the archive contains human-reviewed candidates from at least **two
independent sessions**, use the normal session-disjoint builder and evaluation:

```sh
mix neural.dataset --task memory_relevance
mix neural.train --task memory_relevance
```

Normal training compares held-out MSE against using the original hippocampal
retrieval score as a relevance prediction, and writes the experimental
checkpoint to `tmp/training/memory_relevance_pilot.etf`. Tiny training and
evaluation splits have noisy estimates; these are research diagnostics.

Both modes are strictly **offline** and **advisory**. Neither checkpoint is
automatically loaded by Symbrella, and the live hippocampal retrieval and
assistant responses are unchanged. No unreviewed samples are given guessed
or pseudo-labels.

Regression tests:

```sh
mix test apps/core/test/core/neural/review_queue_test.exs \
  apps/brain/test/brain/neural/memory_relevance_pilot_test.exs \
  apps/brain/test/brain/neural/training_builder_test.exs
```


## Correcting existing human relevance labels

Previously the review command hid labeled candidates, even when a person
mistakenly chose \`y\` for an irrelevant memory. Existing archives do **not**
need to be deleted or edited by hand. Reopen a specific turn:

\`\`\`sh
mix neural.review --recheck --turn turn-9f0a80367896c3ca99fd827abcd76558
\`\`\`

Or review every existing label again:

\`\`\`sh
mix neural.review --recheck
\`\`\`

The command displays the previous label for each matching memory. Choosing
the same decision leaves the archive untouched; choosing the opposite appends
a **new human feedback event with \`correction: true\`** to
\`feedback.jsonl\`. The original feedback record is retained as an audit
trail. \`Brain.Neural.TrainingBuilder\` uses the latest explicitly corrected
human label for that turn and exact episode fingerprint; unmarked duplicate
feedback still causes an error.

For the initial pilot, examples should be labeled **relevant only if the
recalled content would genuinely help answer the original user request**.
Shared generic words such as "its" or "a" do not imply relevance.

Count labels after corrections:

\`\`\`sh
jq -r 'select(.label.kind=="memory_relevance") |
  [.label.relevant,.correction] | @tsv' \
  tmp/training/feedback.jsonl
\`\`\`

Because the JSONL is an append-only audit trail, this command counts original
and corrected events; the trainer uses the **latest effective correction**
instead. The next \`mix neural.train ... --smoke\` output displays effective
positive and negative counts.

### Interpreting pilot scores

The trainer now prints the label distribution and a **class-prevalence
baseline** alongside hippocampal recall-score MSE. If all examples are
positive, a constant prediction of 1.0 has zero in-sample MSE; an Axon model
with in-sample MSE 0.23 has not beaten this trivial baseline. With only 13
reviews, even a holdout split over two sessions is far too small to provide
robust evidence of generalization.

The warning about initializing Axon with a parameter map was also addressed
by passing \`Axon.ModelState.empty()\` as the initial model state for the
experimental memory-relevance trainer. The Hippocampus \`handle_call/3\`
clauses have been regrouped to silence the separate Elixir compiler warning.


## Automatic catalog: the entire recorded cognitive archive

The manual \`memory_relevance\` review pilot is **only one** learning
objective. Symbrella can categorize much of her already-structured archive
without additional human prompts:

\`\`\`sh
mix neural.catalog --dir tmp/training
\`\`\`

This reads all recorded \`turns.jsonl\`, \`memory_events.jsonl\` and
\`feedback.jsonl\` (the latter two may be absent), and atomically regenerates
\`tmp/training/catalog.jsonl\`. The index contains a source-file and line
pointer, session/turn identity, a deterministic automatic category, available
cognitive signal groups, a limited set of **observed** metadata values, and
explicit provenance. It **does not** duplicate raw conversation texts or
episodes, change any original file, infer relevance labels, or train a model.

Current observed categories include:

- \`cognitive_turn\`: recorded intent, action, confidence and presence of
  language, syntax, semantics, affect, executive, self-model and trace signals
- \`memory_encoding\`: observed episodic writes
- \`memory_retrieval\`: recall attempts, returned episodes and miss events
- \`fact_memory\`: fact lookup/write/recall
- \`trace_integrity\`: recording overflow
- \`human_feedback\`: actual reviewed labels and corrections
- \`other_memory_event\`: unrecognized event kinds, preserved for future
  handling instead of silently dropped

**Observed intent is not a true intent label.** A runtime
\`selected_action\` is not necessarily a correct decision, and a hippocampal
recall hit is not necessarily relevant. The catalog keeps these distinctions
explicit using \`label_provenance\`.

## Learn without manually rating each episode: state transition

The first self-supervised objective pairs successive completed turns within a
session. It predicts the **next actually observed cognitive state** from the
current state (\`x(t) -> x(t+1)\`), never crossing a session boundary. It uses
a versioned, 16-feature Nx projection of logged confidence, mood signals,
language/semantic counts, self-monitoring and hippocampal event statistics:

\`\`\`sh
mix neural.dataset --task state_transition --smoke
mix neural.train --task state_transition --smoke
\`\`\`

The \`--smoke\` mode can run with just one session with two consecutive
completed turns, using all available state transitions in-sample. It provides
**no independent evaluation** and is useful for verifying the engineering
pipeline only.

Once at least two sessions each have consecutive completed turns, omit
\`--smoke\`:

\`\`\`sh
mix neural.dataset --task state_transition
mix neural.train --task state_transition
\`\`\`

The second, session-held-out split compares the network's next-state
prediction with a simple **persistence baseline** (predicting the next
state remains the same as the current one). With a small number of turns,
neither the result nor a lower MSE establishes meaningful cognitive ability.

The experiment writes a private
\`tmp/training/state_transition_smoke.etf\` or
\`tmp/training/state_transition_pilot.etf\` checkpoint. It is
**never installed into the live response, memory or action pathway**.

**Important scope limitation:** All available JSONL records are cataloged.
The state-transition learner currently uses **selected numeric features**
from the cognitive state and memory-event aggregates—not every raw token,
sense candidate, definition, response text, trace subtree or user statement.
Encoding those rich variable-length objects requires separate representation
models and carefully defined objectives. We should develop those incrementally
instead of pretending the small numeric projection is "the whole brain."

Regression tests:

\`\`\`sh
mix test apps/core/test/core/neural/catalog_test.exs \
  apps/brain/test/brain/neural/state_transition_pilot_test.exs
\`\`\`


## Residual cognitive-state experiment (v1, separate from direct predictor)

The first session-held-out direct Axon run on the initial archive had six
training and three held-out transitions. Its reported **held-out** state MSE
was **0.21468**, versus **0.10403** for the persistence baseline. With only
three held-out transitions these are diagnostic, not statistically meaningful.

We now have a second training objective that learns a residual instead of
reconstructing the next state:

```text
observed delta = next_observed_state - current_state
Axon(current_state) -> predicted delta
predicted next state = clip(current_state + predicted delta, 0, 1)
```

The final Axon layer uses zero-initialized weights and biases. Before
training, the residual is therefore exactly zero and the network reproduces
the persistence baseline. During training it minimizes MSE between predicted
and **observed** deltas. After training we report next-state MSE in the
original 16-dimensional bounded state space, making comparisons with
persistence and the direct pilot meaningful.

The residual objective does not require manual relevance ratings. It uses
the same chronological turns, same state feature schema, and same
session-separated dataset as `state_transition`; only the model/learning
objective differs.

```sh
cd ~/Symbrella
mix test apps/brain/test/brain/neural/state_transition_residual_test.exs \
  apps/brain/test/brain/neural/state_transition_pilot_test.exs

# Independent holdout: uses the same deterministic session split as v1
mix neural.train --task state_transition_residual

# Optional engineering smoke test: NO independent evaluation
mix neural.train --task state_transition_residual --smoke
```

Held-out output includes:

- Total Axon and persistence next-state mean squared error
- Each feature's Axon MSE, persistence MSE, and improvement
  (`persistence_mse - model_mse`; positive favors Axon)
- A clear sample-size warning and an explicit verdict on the observed sample

New private artifacts:

- `tmp/training/state_transition_residual_pilot.etf`
- `tmp/training/state_transition_residual_smoke.etf`

The **original** `state_transition_pilot.etf` and
`state_transition_smoke.etf` are not overwritten. Like all initial Axon
experiments, these weights are never connected to live cognitive state,
response selection, or hippocampal ranking.

Do not expect the residual network to beat persistence with only six
training transitions; the zero-output prior gives it a better starting point,
not a guarantee of learning. Larger archives, stable state features, and
multiple held-out sessions are still required before drawing conclusions.


## Corrected mood extraction: cognitive transition feature schema v2

**The original v1 16-feature training results are diagnostic only.** An
archive inspection found the 13 completed turns stored two distinct mood
representations:

- `cognition.mood` has the **derived** indices `exploration`, `inhibition`,
  `vigilance`, and `plasticity`.
- `cognition.self_model.mood.levels` has raw MoodCore neuromodulator values
  `da`, `5ht`, `glu`, and `ne` in 12 out of those 13 turns.

The v1 feature extractor incorrectly searched the derived mood for raw
neuromodulators, causing the raw features to become zero even though the
historical archive had recorded them. **No recollection is needed.**

The feature schema is now **v2, 24 features**: four raw neuromodulator readings
read only from the nested self-model snapshot, a presence bit for **each** raw
reading, four separate derived mood indices, the derived-mood presence bit,
and the rest of the former numeric cognitive/memory features. A missing raw
reading has numerical placeholder 0 **and** an explicit zero presence flag.
Do not interpret a missing raw measurement as a biological baseline of zero.

The dataset builder reports how many archived turns have complete raw
neuromodulator snapshots:

```sh
mix neural.dataset --task state_transition
```

Then retrain each experimental model **against the corrected v2 features**:

```sh
mix neural.train --task state_transition
mix neural.train --task state_transition_residual
```

New artifacts are kept separate from the flawed v1 experiments:

- `tmp/training/state_transition_v2_pilot.etf`
- `tmp/training/state_transition_v2_smoke.etf`
- `tmp/training/state_transition_residual_v2_pilot.etf`
- `tmp/training/state_transition_residual_v2_smoke.etf`

The v1 checkpoint files remain untouched. Because the feature schema and
number of MSE terms have changed, **do not directly compare v1 MSE to v2
MSE as a neural architecture improvement**. For each run, compare the model
with the **persistence baseline on that same v2 dataset**.

As in previous stages, 6 training and 3 held-out transitions are far too few
to conclude generalization. Missing-value flags make missingness explicit,
but this initial model still uses placeholder zero targets in its overall MSE.
A later masked-loss objective can avoid training continuous targets where the
raw level is missing, and more recorded sessions are needed to validate it.

Regression test commands:

```sh
mix test apps/brain/test/brain/neural/state_transition_pilot_test.exs \
  apps/brain/test/brain/neural/state_transition_residual_test.exs
```
