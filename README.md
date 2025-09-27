# Symbrella Phoenix **umbrella** app.

**Erlang/OTP:** 28 • **Elixir:** 1.18.x • **Phoenix:** 1.8.x (Bandit)  
Watchers are **disabled by design** — rebuild assets manually when you want.

---

## Quickstart

```bash
# from the umbrella root
mix deps.get

# (first time on a machine) install asset tool binaries
cd apps/symbrella_web
mix tailwind.install --if-missing
mix esbuild.install --if-missing
cd ../../

# build assets (manual by design)
mix tailwind default
mix esbuild default

# run the server
mix phx.server

# App: http://localhost:4000
# Re-run the two commands whenever you change CSS/JS:
#   mix tailwind default
#   mix esbuild default
```
## Architecture Diagram

> Paste this in your README as-is to render on GitHub.

```mermaid
flowchart TD
  A["Phoenix UI (LiveView)"]
  B["Brain Supervisor"]
  C["Core Pipeline"]
  D[(Postgres)]

  A -->|HTTP / WebSocket| B

  subgraph E["Brain App"]
    B --> F["Brain.LIFG (Stage-1 WSD)"]
    B --> G["Brain.Cell (GenServer)"]
    G -->|Ecto| D
  end

  B -->|API calls| C

  subgraph H["Core App"]
    C --> I["Token / Tokenizer"]
    C --> J["SemanticInput"]
    C --> K["Lexicon / Senses"]
    C --> L["Recall / Plan"]
  end

```
```mermaid
sequenceDiagram
  participant UI as "Phoenix LiveView"
  participant Core as "Core Pipeline"
  participant Brain as "Brain GenServer"
  participant DB as Postgres
  participant Lex as "Core.Lexicon"

  UI->>Core: phrase
  Core->>Core: Token.tokenize()
  Core->>Brain: stm(tokens)
  Brain->>DB: hydrate/lookup active cells
  DB-->>Brain: rows / states
  Brain-->>Core: SI (with active cells)
  Core->>DB: ltm(SI)
  DB-->>Core: SI (db cells merged)
  Core->>Lex: all(SI.tokens)
  Lex-->>Core: senses
  Core-->>UI: SI (ready for recall/plan)

```
---

## Project Layout (summary)

```
symbrella_umbrella/
├─ apps/
│  ├─ brain/                 # Brain runtime + LIFG (disambiguation)
│  │  ├─ lib/brain/
│  │  │  ├─ cell.ex
│  │  │  ├─ lifg.ex
│  │  │  ├─ lifg_stage1.ex
│  │  │  └─ telemetry.ex
│  │  ├─ bench/              # microbenchmarks
│  │  └─ test/brain/         # unit, property, integration & benches
│  ├─ core/                  # SI pipeline, recall, lexicon glue
│  │  ├─ lib/core/
│  │  │  ├─ brain/           # adapter & index
│  │  │  ├─ lexicon/         # normalize/senses/stage
│  │  │  ├─ recall/          # plan/gate/execute
│  │  │  ├─ semantic_input.ex
│  │  │  └─ …
│  ├─ db/                    # Ecto schemas & repo types
│  │  └─ lib/db/
│  │     ├─ brain_cell.ex
│  │     ├─ lexicon.ex
│  │     └─ postgrex_types.ex
│  ├─ lexicon/               # pluggable lexicon behaviour
│  ├─ symbrella/             # OTP app shell
│  └─ symbrella_web/         # Phoenix + Tailwind/Esbuild
│     ├─ assets/             # css/js; watchers are manual by design
│     └─ lib/symbrella_web/  # controllers, live, components, etc.
├─ config/                   # umbrella-wide config
├─ assets/                   # shared styling
├─ mix.exs
└─ mix.lock
```

> ℹ️ If you see stray files at the repo root like `how origin` or
> `e --abbrev-ref --symbolic-full-name @{u}`, they likely came from an
> accidental shell redirection. Safe to delete.

---

## Minimal Assets

Ensure these exist in `apps/symbrella_web/assets`:

**js/app.js**
```js
// apps/symbrella_web/assets/js/app.js
import "phoenix_html"
```

**css/app.css**
```css
/* apps/symbrella_web/assets/css/app.css */
@tailwind base;
@tailwind components;
@tailwind utilities;
```

---

## Troubleshooting

- **Missing tailwind/esbuild**  
  Run (inside `apps/symbrella_web`):  
  `mix tailwind.install --if-missing && mix esbuild.install --if-missing`

- **Version warnings**  
  Align versions in `config/config.exs` (`:tailwind`, `:esbuild`) *or*
  reinstall with the version you prefer using the commands above.

- **No output files**  
  Rebuild:
  ```bash
  mix tailwind default && mix esbuild default
  ```
  Expected outputs:
  ```
  apps/symbrella_web/priv/static/assets/app.css
  apps/symbrella_web/priv/static/assets/app.js
  ```

---

## (Optional) One-Command Dev Script

Create `dev.sh` at the repo root:

```bash
#!/usr/bin/env bash
set -euo pipefail

mix deps.get
(
  cd apps/symbrella_web &&   mix tailwind.install --if-missing &&   mix esbuild.install --if-missing
)
mix tailwind default
mix esbuild default
mix phx.server
```

Then:
```bash
chmod +x dev.sh
./dev.sh
```

---

## License

TBD © 2025 Bradley (bushman77)
