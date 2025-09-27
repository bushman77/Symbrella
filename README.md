# Symbrella Phoenix **umbrella** app

## Repository Map (summary)

```
.
├─ apps/                                     # umbrella apps
│  ├─ brain/                                 # LIFG + Cell orchestration
│  │  ├─ lib/brain/
│  │  │  ├─ cell.ex                          # per-cell GenServer
│  │  │  ├─ lifg.ex                          # LIFG API + scoring mix
│  │  │  ├─ lifg_stage1.ex                   # Stage‑1 engine (selection/signals)
│  │  │  └─ telemetry.ex                     # metrics hooks
│  │  ├─ test/brain/                         # property, integration, benches
│  │  │  ├─ brain_lifg_test.exs
│  │  │  ├─ brain_lifg_property_test.exs
│  │  │  ├─ brain_lifg_integration_test.exs
│  │  │  └─ bench/bench_brain_lifg_bench.exs
│  │  └─ bench/                              # standalone run benches
│  ├─ core/                                  # pipeline & domain (SI, tokens, recall)
│  │  ├─ lib/core/
│  │  │  ├─ brain/                           # adapter entry points
│  │  │  │  └─ index.ex
│  │  │  ├─ recall/{plan,gate,execute}.ex    # recall planner + gate + executor
│  │  │  ├─ semantic_input.ex                # SI struct & helpers
│  │  │  ├─ token.ex                         # tokenizer
│  │  │  ├─ lexicon/*.ex                     # normalization/senses/stage
│  │  │  ├─ vectors.ex                       # vector helpers
│  │  │  └─ brain_adapter.ex                 # Core ↔ Brain adapter
│  │  └─ test/core/                          # unit tests
│  ├─ db/                                    # Ecto schema + repo + migrations
│  │  ├─ lib/db/brain_cell.ex                # Db.BrainCell schema
│  │  └─ priv/{db,repo}/migrations/          # consolidated + legacy migrations
│  ├─ lexicon/                               # Lexicon behaviour + facade
│  ├─ symbrella/                             # OTP app skeleton (mailer, app)
│  └─ symbrella_web/                         # Phoenix app (Bandit)
│     ├─ lib/symbrella_web/                  # router, endpoint, live, components
│     └─ assets/                             # Tailwind + esbuild sources
├─ config/                                   # umbrella-wide config
├─ assets/                                   # optional shared assets
├─ benchmark_result.txt                      # recent LIFG benchmarks
├─ README.md                                 # top-level guide
├─ AGENTS.md, PROJECT-RESUME-PLAYBOOK.md     # docs/playbooks
└─ mix.exs / mix.lock                        # umbrella definition
```

> **Note (cleanup):** I spotted likely stray files in the root:
> - `e --abbrev-ref --symbolic-full-name @{u}`
> - `how origin`  
> These look like accidental outputs from shell commands. Safe to remove via `git rm` if committed (or delete locally if untracked).


- **Erlang/OTP:** 28
- **Elixir:** 1.18.x
- **Phoenix:** 1.8.x (Bandit)
- Watchers are **disabled by design** — rebuild assets manually when you want.

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
```

- App: http://localhost:4000
- Re-run the two `mix tailwind default` / `mix esbuild default` commands whenever you change CSS/JS.

---

## Project Layout

```
symbrella_umbrella/
├─ apps/
│  ├─ symbrella/      # core/domain
│  └─ symbrella_web/  # Phoenix web UI (Bandit)
├─ config/            # umbrella-wide config
└─ mix.exs            # umbrella root
```

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
  Align versions in `config/config.exs` (`:tailwind`, `:esbuild`) *or* reinstall with the version you prefer using the commands above.

- **No output files**  
  Rebuild: `mix tailwind default && mix esbuild default`  
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
  cd apps/symbrella_web &&
  mix tailwind.install --if-missing &&
  mix esbuild.install --if-missing
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