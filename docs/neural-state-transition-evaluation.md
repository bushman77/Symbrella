# Cognitive-state transition learning: audit and promotion boundary

This workflow is **offline and read-only** with respect to Symbrella's live
reasoning. A passing numeric gate is **not permission to deploy neural control**.
The original semantic, memory, and action-selection paths stay authoritative.

## 1. Collect and inspect archived observations

In development, completed conversations are appended automatically using
`config/config.exs` (`Core.TrainingArchive` defaults to `enabled?: true` in
development). Start Phoenix normally; no recording CLI variable is needed:

```sh
iex -S mix phx.server
```

Change `enabled?` or `dir` in `config/config.exs` to control local collection.
Test and production remain disabled by default.

The archive includes chat text and private memory. Do not commit or upload
`priv/`, `turns.jsonl`, or `memory_events.jsonl`.

From the project root, read an aggregate coverage report:

```sh
mix neural.dataset --task state_transition --dir priv
```

The command reports observed training/held-out cells per feature, as well as
missing raw neuromodulator snapshots and incomplete memory traces. It does not
change model weights. The 24-feature v4 projection is numeric and **does not**
encode all lexical, semantic, or episodic content.

Archive protections now reject duplicate turn IDs, backwards timestamps within
a session (when comparable timestamps are present), and explicitly truncated
hippocampal traces. Incomplete/missing observations are masked, not imputed.
Timestamps with insufficient information do not imply temporal certainty.

## 2. Train and evaluate only against observed targets

```sh
mix neural.train --task state_transition_residual --dir priv
```

Training and held-out evaluation use the same observed-feature mask. The
baseline predicts that the next state equals the current state. A residual
model must improve on that baseline on independent sessions—not merely reduce
training loss. With four or more usable sessions, two sessions are held out;
with fewer, one is held out and cannot satisfy the evidence gate.

The report includes per-feature paired MSE, valid observed-pair counts, an
aggregate relative improvement, and a **provisional conservative evidence
assessment**. Its `candidate_for_extended_shadow` status requires all of:

- Session-held-out evaluation, not smoke mode.
- At least two train and two evaluation sessions, and at least 20 transitions
  in each split.
- At least 100 actually observed evaluation feature cells.
- At least five observed evaluation pairs for each of confidence and the four
  raw neuromodulators (DA, 5HT, Glu, NE).
- At least 5% relative held-out MSE improvement over persistence.

These thresholds are preliminary engineering filters, not proof of statistical
significance or safe deployment. Tiny, sparse, one-session and zero-movement
datasets report `not_ready` even if the residual happens to beat persistence.

## 3. Evaluate in read-only shadow mode

After creating a *non-smoke* residual checkpoint and checking its report, run
the application with explicit shadow opt-in:

```sh
SYMBRELLA_NEURAL_SHADOW=on iex -S mix phx.server
```

Inside IEx:

```elixir
SymbrellaWeb.NeuralShadow.status()
```

The status now includes observed-cell-weighted aggregate residual/persistence
MSE, evaluated pairs, valid cells, and winning-pair count, in addition to the
latest evaluation. Replayed recent turn IDs within a session are ignored
instead of generating spurious transitions. Shadow predictions are appended to
private `priv/shadow_predictions.jsonl`; raw chat text is not included
in the shadow audit payload.

A checkpoint need not exist just to record observations; shadow mode then
remains unavailable. No shadow prediction updates MoodCore, Hippocampus,
LIFG, comprehension, or response decisions.

## 4. NEURAL-01: read-only nervous-system signal reception

The learned residual Axon network now emits *actual, masked model forecasts*
as telemetry after each archived completed turn, rather than leaving every
forecast only inside the Web shadow process. The permanently supervised
`Brain.Neural.SignalReceptor` and `Brain.Blackboard` receive two events:

- `[:brain, :neural, :state_prediction]`: available numeric current features,
  the predicted **next completed turn** values, and their deltas; all are
  marked `:model_prediction_not_verified`, `:shadow_only`, and
  `deployment_authorized?: false`.
- `[:brain, :neural, :state_evaluation]`: observed-cell-weighted retrospective
  residual/persistence MSE and verdict when the next same-session turn arrives.

Only actually observed feature inputs are transmitted: confidence, the four
raw neuromodulators and four derived mood dimensions. Missing features are
excluded, never turned into fictitious predictions. No chat text or recalled
episode content enters these events. The receptor stores at most 32 events in
memory; the Blackboard bridges them over the existing PubSub topic.

**To receive live model signals**, the residual checkpoint must already exist
at `priv/state_transition_residual_v4_pilot.etf`, and the training archive
must be enabled. Restart Phoenix with explicit shadow opt-in:

```sh
cd ~/Symbrella
SYMBRELLA_NEURAL_SHADOW=on iex -S mix phx.server
```

After one or more completed chat turns, inspect from IEx:

```elixir
SymbrellaWeb.NeuralShadow.status()
Brain.Neural.SignalReceptor.status()
Brain.Neural.SignalReceptor.latest()
Brain.Neural.SignalReceptor.history(5)
```

A first prediction requires one completed turn. Accuracy feedback requires a
*second completed turn in the same session*. If shadow startup reports a
missing checkpoint, train the offline model using
`mix neural.train --task state_transition_residual --dir priv` first and
inspect its assessment; a checkpoint with `:not_ready` still remains
research-only and may not be promoted to live control.

**No actuation occurs.** A predicted DA increase is not a MoodCore update,
nor is a predicted confidence level evidence that a statement is correct.
These are provisional sensory-like inputs to the structured Brain's
observation channel. Future use for decisions or neuromodulation requires a
separate validated actuator policy, explicit opt-in, and regression gates.

## 5. Controlled integration is deliberately blocked

Even when `assessment.status == :candidate_for_extended_shadow`, the artifact
says `deployment_authorized?: false` and retains the
`experimental_not_for_runtime` deployment status.

Before proposing an opt-in runtime blend, collect multiple independent holdout
runs; verify calibration and regression resistance across fast paths, missing
signals, sessions and time periods; document failure/rollback behavior; and
review model output safety. Deployment would require a separate explicit design,
implementation and test milestone. **There is no automatic promotion.**

Run focused tests, then the full umbrella suite:

```sh
mix test apps/brain/test/brain/neural/state_transition_audit_test.exs
mix test apps/brain/test/brain/neural/state_transition_pilot_test.exs
mix test apps/brain/test/brain/neural/state_transition_residual_test.exs
mix test apps/brain/test/brain/neural/signal_receptor_test.exs
mix test apps/symbrella_web/test/symbrella_web/neural_shadow_test.exs
mix test
```
