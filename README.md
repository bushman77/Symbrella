# Symbrella

Phoenix **umbrella** app.

- **Erlang/OTP:** 28
- **Elixir:** 1.18.x
- **Phoenix:** 1.8.x (Bandit)
- Watchers are **disabled by design** â€” rebuild assets manually when you want.

---

## Quickstart

```bash
# from the umbrella root
mix deps.get

# (first time on a machine) install asset tool binaries
cd apps/symbrella_web
mix tailwind.install --if-missing
mix esbuild.install  --if-missing
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
â”œâ”€ apps/
â”‚  â”œâ”€ symbrella/         # core/domain
â”‚  â””â”€ symbrella_web/     # Phoenix web UI (Bandit)
â”œâ”€ config/               # umbrella-wide config
â””â”€ mix.exs               # umbrella root
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
  Rebuild:  
  `mix tailwind default && mix esbuild default`  
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
( cd apps/symbrella_web && mix tailwind.install --if-missing && mix esbuild.install --if-missing )
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

TBD Â© 2025 Bradley (bushman77)
