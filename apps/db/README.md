# Db — Persistence Layer for Symbrella

This app is the **database + Ecto layer** for the Symbrella umbrella.  
It owns all persistent storage for brain cells, episodes, and model metadata, and
exposes a single Ecto repo module: `Db`.

In the umbrella dependency chain, Db sits at the bottom:

```text
db ← brain ← core ← web
```

- **Nothing depends on Db “from the left”.**
- **Brain, Core, and Web may depend on Db as a normal Ecto repo.**

---

## What lives here

### 1. Repo

- `Db` — the Ecto repo module (note: the repo is called `Db`, *not* `Db.Repo`).
  - Used by other apps via `Db` / `Db.transaction/1` / `Db.query/3`, etc.
  - Configured in the umbrella’s `config/*.exs` files.

### 2. Schemas

These schemas are the persistent backing for Brain and Core:

- `Db.BrainCell`
  - Stores the long‑term “lexicon” of **brain cells**:
    - `id` (e.g. `"word|pos|sense"`) as primary key.
    - POS / type information.
    - neurotransmitter state and activation.
    - connectivity / position metadata.
  - Brain uses this as its *long‑term semantic memory*.

- `Db.Episode`
  - Episodic memory rows for **Hippocampus**:
    - tokens / lemmas and counts,
    - `meta` (scope, outcome flags, etc.),
    - timestamps (`inserted_at`, `updated_at`),
    - optional embedding column (pgvector) for similarity recall.
  - Queried by `Brain.Hippocampus` when doing episode recall.

- `Db.CerebellumModel` (and related helpers)
  - Stores metadata and weights for **cerebellum‑style models**:
    - model name + version,
    - task / scope,
    - serialized parameters or references to files.
  - Used by `Brain.Cerebellum.Store` to load/update trained models.

- Other helper modules (e.g. `Db.PostgrexTypes`, import helpers) live here as well.
  They are considered **infrastructure**, not business logic.

---

## Role in the umbrella

Db’s responsibilities:

- Define **schemas and migrations** for:
  - brain cells,
  - episodes,
  - model metadata,
  - future storage (lexicon rows, statistics, etc.).
- Provide a **single, well‑typed entry point** (`Db`) for all database access.
- Keep higher‑level apps (Brain/Core/Web) free of migration and schema clutter.

Db explicitly does **not**:

- Implement working‑memory logic (that’s `Brain.WorkingMemory`).
- Implement LIFG / curiosity / thalamus logic (all live in `apps/brain`).
- Orchestrate the semantic pipeline (that’s `apps/core`).

---

## Migrations & structure

Migrations live under:

```text
apps/db/priv/db/migrations     # core DB structures (brain_cells, episodes, models, …)
apps/db/priv/repo/migrations   # legacy / bootstrap migrations
```

Typical tables include:

- `brain_cells`
- `episodes`
- `cerebellum_models` (if present)

Run migrations from the umbrella root:

```bash
# create the database (once per environment)
mix ecto.create -r Db

# run migrations
mix ecto.migrate -r Db
```

> Reminder: the repo module is **`Db`**, not `Db.Repo`.

---

## Using Db from other apps

From any app in the umbrella (Brain, Core, etc.) you can call:

```elixir
# Basic query via Ecto
Db.all(Db.BrainCell)

# Transaction
Db.transaction(fn ->
  # read/write brain cells or episodes here
end)
```

For higher‑level logic:

- Brain modules (`Brain.Hippocampus`, `Brain.Cerebellum.Store`, etc.) should own
  most of the **“what to store / when to write”** decisions.
- Core and Web can read from Db directly when appropriate (offline tools, admin
  dashboards), but should avoid duplicating Brain’s semantics.

---

## Notes for future work

- Tighten pgvector integration for episode and model recall.
- Add small utility scripts/tests for DB‑only concerns (index health, vacuum hints).
- Expand schemas as new brain regions gain persistent state (e.g., more detailed
  episode metadata, statistics, or model registries).
