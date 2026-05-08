# Brain - Process Layer of Symbrella

This app hosts the process-level "brain" for Symbrella's Neuro-Symbolic
Synthetic Intelligence (NSSI).

Scientific accuracy note: Brain region names are engineering analogies, not
biological proof. See
[`docs/brain-core-scientific-contract.md`](../../docs/brain-core-scientific-contract.md)
for the project-wide contract that separates software guarantees from
brain-inspired terminology.

Where `core` is mostly about data structures and orchestration, `brain` is
about running processes: OTP regions for LIFG, Hippocampus, Thalamus, Curiosity,
Working Memory, mood, self-model, timing, and gating/valuation loops.

Design rule:

```text
db <- brain <- core <- web
```

Brain depends on `db`, never on `core` or `symbrella_web`. Core calls into Brain
as a client.

## Responsibilities

### Regions as OTP Processes

Major regions live under `Brain.*` and commonly `use Brain, region: :name`.
Important modules include:

- `Brain.LIFG` / `Brain.LIFG.Stage1` / `Brain.LIFG.Stage2` - sense selection,
  fallback/reanalysis, and post-selection integration.
- `Brain.Hippocampus` - episodic memory write, recall, scoring, evidence, and
  telemetry.
- `Brain.Thalamus` - relay and arbitration for curiosity, value, conflict, and
  mood signals.
- `Brain.Curiosity` - exploratory probe generation.
- `Brain.BasalGanglia` - pure WM gate returning `:allow`, `:boost`, or `:block`.
- `Brain.WorkingMemory` and `Brain.WM.*` - item normalization, decay, focus,
  recall, policy, and gating helpers.
- `Brain.DLPFC` - turns approved thalamus decisions into focus actions.
- `Brain.ACC`, `Brain.OFC`, `Brain.VmPFC`, `Brain.PFC`, `Brain.PMTG`,
  `Brain.ATL`, `Brain.Temporal`, `Brain.Cerebellum`, and `Brain.Amygdala` -
  region-level control, value, semantic, temporal, model, and affect signals.
- `Brain.MoodCore`, `Brain.MoodPolicy`, and `Brain.MoodWeights` - mood vector
  calculation and modulation.
- `Brain.Blackboard`, `Brain.SelfModel`, `Brain.SelfPortrait`,
  `Brain.SelfContinuity`, and `Brain.MetaMonitor` - cross-region state and
  self-model surfaces.
- `Brain.CycleClock` and `Brain.CycleMetrics` - runtime cycle timing.

Most regions keep a small public API, emit telemetry, and keep math/scoring
helpers pure where possible.

### Working Memory

`Brain.WorkingMemory` defines the canonical WM item shape:

```elixir
%{
  id: term(),
  source: atom() | String.t() | nil,
  activation: float(),
  score: float(),
  ts: non_neg_integer(),
  inserted_at: non_neg_integer(),
  last_bump: non_neg_integer(),
  payload: map()
}
```

WM updates should pass through the Brain gate path:

1. `Brain.BasalGanglia.decide/4` decides admission.
2. `Brain.WorkingMemory.normalize/3`, `upsert/3`, `decay/3`, and `trim/2`
   maintain the list.

Do not hand-craft WM items in unrelated modules.

### LIFG

Core prepares `si.sense_candidates`; Brain consumes them.

LIFG contracts:

- no char-grams in Stage-1 input,
- spans must align to word boundaries unless `mw: true`,
- MWEs are produced from word-level n-grams upstream in Core,
- decisions include winners, finalists, margins, audit data, and telemetry,
- weak or incompatible choices can trigger fallback/reanalysis paths.

Relevant tests live under `apps/brain/test/brain/lifg*` and
`apps/brain/test/brain/lifg/*`.

### Hippocampus

`Brain.Hippocampus` implements bounded episodic memory:

- writes at consolidation time,
- windowed in-memory recall,
- Db-backed episode storage through `Db.Episode` / `Db.Episodes`,
- token overlap, recency, outcome uplift, and optional vector evidence,
- evidence attachment back into semantic state for later scoring.

### Curiosity -> Thalamus -> DLPFC -> WM

This loop decides whether small exploratory probes deserve focus:

1. `Brain.Curiosity` emits a proposal.
2. `Brain.Thalamus` blends curiosity score, OFC value, ACC conflict, and mood.
3. `Brain.DLPFC` listens for approved decisions.
4. `Brain.focus/2` sends the candidate through Basal Ganglia and Working Memory.

Invariants:

- Curiosity and Thalamus do not mutate WM directly.
- DLPFC is the actor that turns approved curiosity decisions into focus.
- Curiosity-derived WM items preserve `payload[:reason] == :curiosity`.

## Telemetry and UI

Brain emits telemetry for LIFG decisions, guard violations, hippocampus recall,
curiosity proposals, thalamus decisions, WM gating, mood, and runtime cycle
signals. Tests use helpers in `apps/brain/test/support`.

`apps/symbrella_web` consumes selected snapshots and telemetry for the `/brain`
dashboard.

For the current mood/personality/LLM prompting contract, see
[`docs/modulator-to-prompt-contract.md`](../../docs/modulator-to-prompt-contract.md).

## Running Tests

From the umbrella root:

```bash
mix test apps/brain/test
mix test apps/brain/test/brain/lifg_stage1_invariants_test.exs
mix test apps/brain/test/brain/lifg_stage2_contract_test.exs
mix test apps/brain/test/brain/curiosity_flow_test.exs
mix test apps/brain/test/brain/thalamus_*test.exs
mix test apps/brain/test/brain/wm_*test.exs
```

## Changing Brain

When adding or changing a region:

1. Keep the dependency rule: `db <- brain <- core <- web`.
2. Put scoring/math in pure helpers where practical.
3. Wrap only the long-lived stateful behavior in processes.
4. Emit telemetry for major inputs, decisions, and invariant failures.
5. Add focused tests under `apps/brain/test/brain`.
6. Update guardrails when behavior changes the LIFG, WM, or Hippocampus
   contract.
