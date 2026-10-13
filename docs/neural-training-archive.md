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
