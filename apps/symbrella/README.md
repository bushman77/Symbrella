# Symbrella - Umbrella Runtime

`apps/symbrella` is the OTP runtime shell for the umbrella. It starts shared
infrastructure and the long-lived Brain services, but it deliberately does not
own the Phoenix endpoint.

The web endpoint lives in `apps/symbrella_web`; this app owns the brain-side
runtime and cross-app services.

## Supervision Responsibilities

`Symbrella.Application` starts, in order:

- `Symbrella.PubSub`
- the `Db` Ecto repo
- `Brain.Registry` and `Brain.CellSup`
- `Symbrella.TaskSup`
- `Lexicon.Finch`
- the local `Llm` runner
- Brain mood and policy services
- LIFG Stage-1 scoring
- Brain regions such as Amygdala, Cerebellum, LIFG, PMTG, ATL, Curiosity,
  Hippocampus, Meta, PFC, Thalamus, Temporal, OFC, DLPFC, ACC, CycleClock,
  Blackboard, SelfPortrait, and ML
- telemetry bridges after the tree is live

The child order matters because later regions depend on PubSub, Db, registries,
and mood/policy services already being available.

## What Belongs Here

Put code here when it is genuinely umbrella runtime infrastructure:

- top-level supervision,
- shared task supervisors,
- app-wide PubSub,
- mailer setup,
- lifecycle wiring between existing apps.

Do not put semantic pipeline logic, Brain region logic, schemas, or LiveView UI
here. Those belong in `core`, `brain`, `db`, and `symbrella_web`.

## Running

From the umbrella root:

```bash
mix deps.get
mix phx.server
```

`mix phx.server` starts both `Symbrella.Application` and
`SymbrellaWeb.Application` through the umbrella.

## Tests

Run from the umbrella root:

```bash
mix test apps/symbrella/test
```
