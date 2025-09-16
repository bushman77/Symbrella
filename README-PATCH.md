Symbrella POS-Activation Patch
==============================

This patch makes **every POS variant** of every token get ensured in the DB and show up as active
in `Brain.snapshot/0`. It also consolidates supervision under Symbrella, fixes timestamp types,
uses your `Db` repo module (not `Db.Repo`), and avoids runtime cell crashes by performing **soft activation**
(unless you later add a real `Brain.Cell`).

What’s included
---------------
- apps/core/lib/core/semantic_input.ex
- apps/core/lib/core/phrase_repo.ex
- apps/core/lib/core/phrase_repo/default.ex
- apps/core/lib/core/token.ex
- apps/core/lib/core.ex
- apps/db/lib/db/lexicon.ex
- apps/brain/lib/brain/activator.ex
- apps/brain/lib/brain/index.ex
- apps/brain/lib/brain.ex
- apps/symbrella/lib/symbrella/application.ex

How to apply
------------
1) From your Symbrella repo root:
   - Back up your current files (or rely on git).
   - Copy everything from this zip into your repo, preserving paths.
2) `mix deps.get && mix compile`
3) `iex -S mix`
4) Test:
   ```elixir
   iex> Brain.chat("hello there")
   iex> Brain.snapshot()
   # expect: active_cell_count > 0, many active_cell_ids, sensible timestamps
   ```

Notes
-----
- DB timestamps: we match `timestamps()` (naive) with `NaiveDateTime.utc_now()` in bulk inserts/updates.
- Supervision: only Symbrella supervises; we start `Db` before `Brain`, and we create `Brain.Registry` + `Brain.CellSup`.
- Runtime cells: `Brain.Activator` performs **soft activation** by default to prevent crashes if `Brain.Cell` doesn’t exist.
  You can wire a real `Brain.Cell` later and flip the switch (instructions inline in the file).
- Repo module: we consistently call `Db.insert_all/3`, `Db.all/2`, `Db.update_all/3`, `Db.exists?/1`.

Generated: 2025-09-16T03:59:36.061385Z
