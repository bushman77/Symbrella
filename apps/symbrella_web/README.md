# SymbrellaWeb — Phoenix UI for Symbrella NSSI

`SymbrellaWeb` is the Phoenix web application for the **Symbrella — Neuro‑Symbolic Synthetic Intelligence (NSSI)** umbrella.
It provides the browser UI, including the **Brain dashboard** that visualizes brain regions, mood, intent, and working memory
over live telemetry from the `brain` and `core` apps.

This app does **not** own any business logic or persistence. It renders what the rest of the umbrella is thinking and doing.

---

## Role in the umbrella

At a high level:

- **`db`** — Postgres + pgvector, schemas like `Db.BrainCell` and `Db.Episode`.
- **`brain`** — OTP regions (LIFG, Hippocampus, Curiosity, Thalamus, WM, etc.), working memory and episodic recall.
- **`core`** — Orchestration, tokenization, intent classification, semantic input pipeline.
- **`symbrella_web` (this app)** — Phoenix/LiveView UI and telemetry‑driven brain visualizations.

`SymbrellaWeb` is responsible for:

- Mounting Phoenix LiveViews and controllers.
- Rendering the **Brain dashboard** UI (brain map SVG, region overlays, status panels).
- Showing HUD chips for **intent**, **mood**, and **cycle clock** driven by telemetry.
- Surfacing snapshots from `Brain` (e.g. working memory, region state) in a safe, read‑only way.

All stateful behavior stays in other apps; the web layer only calls public APIs and GenServers.

---

## Key UI pieces

The most important modules live under `apps/symbrella_web/lib/symbrella_web`:

- **LiveViews**
  - `live/brain_live.ex` — main Brain dashboard LiveView.
  - `live/brain_live/mood_hud.ex` — mood HUD telemetry wiring.
  - `live/home_live.ex` and `live/home_live/html.ex` — simple home page LiveView.

- **Brain components**
  - `components/brain/brain_panels.ex` — top‑level brain layout (header, HUD, map, info panel).
  - `components/brain/diagram.ex` — SVG brain diagram wrapper.
  - `components/brain/regions.ex` — region overlay registry + helpers.
  - `components/brain/info_panel.ex` — region info / status panel.
  - `components/brain/hippo_panel.ex` — Hippocampus / episodic memory panel.
  - `components/brain/intent_chip.ex`, `components/brain/lifg_decision.ex`, etc. — small HUD and decision widgets.

- **Region overlays**
  - `region/*.ex` — small modules describing SVG overlay geometry and metadata for each brain region
    (LIFG, Hippocampus, Thalamus, PMTG, Temporal, Cerebellum, etc.).

- **Telemetry helpers**
  - `hud/telemetry.ex` — helpers for wiring telemetry into HUD components.
  - `telemetry.ex` — standard Phoenix telemetry wiring for the web app itself.

- **Routing & layout**
  - `router.ex` — routes for the home page and Brain dashboard.
  - `components/core_components.ex`, `components/layouts.ex`, `components/layouts/root.html.heex` — shared layout and UI primitives.

---

## Running in development

From the **umbrella root** (`Symbrella/`), follow the main README quickstart.
The short version for starting the web UI is:

```bash
# once per machine (from umbrella root)
mix deps.get

# first‑time asset setup
cd apps/symbrella_web
mix tailwind.install --if-missing
mix esbuild.install --if-missing
cd ../../

# build assets (run again whenever you change CSS/JS)
mix tailwind default
mix esbuild default

# start the Phoenix endpoint
mix phx.server
```

Then open:

- **App root:** <http://localhost:4000>
- **Brain dashboard:** linked from the home page (and typically available as a LiveView route from the navbar).

The web app expects the `db`, `brain`, and `core` apps to be running in the same umbrella (which `mix phx.server` does for you via `Symbrella.Application`).

---

## Tests

Web‑layer tests live under:

- `apps/symbrella_web/test/`

Run them from the umbrella root:

```bash
mix test apps/symbrella_web
```

Integration tests for the Brain itself live under `apps/brain/test` and are run separately.

---

## Deployment notes

`SymbrellaWeb` follows standard Phoenix deployment practices:

- The web app is started by `Symbrella.Application` along with `db`, `brain`, and `core`.
- Static assets are built ahead of time using `mix tailwind default` and `mix esbuild default`.
- Configuration for ports, hosts, and secrets lives in `config/prod.exs` and `config/runtime.exs` at the umbrella level.

For general Phoenix deployment guidance, see the official docs:

- Deployment guides: <https://hexdocs.pm/phoenix/deployment.html>
- Phoenix docs: <https://hexdocs.pm/phoenix>
```
