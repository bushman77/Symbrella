Symbrella Patch v2 — New Pipeline Order
=======================================

Pipeline (new):
1) Tokenize *pure user input* only (no side-effects).
2) Detect **running braincells** from the Registry (by token × POS).
3) Parse the **DB** for any matching brain cells (by normalized token).
4) Check **NegCache** as the guard for **MWE** remote probing (no network call here).

What's included (drop-in):
- apps/brain/lib/brain/detector.ex         (NEW) running braincell detector via Registry
- apps/brain/lib/brain/mwe_guard.ex        (NEW) NegCache-based guard for MWE probes
- apps/brain/lib/brain/index.ex            (updated)
- apps/brain/lib/brain.ex                  (updated) orchestrates new order
- apps/core/lib/core/semantic_input.ex     (updated) fields for new stages
- apps/core/lib/core/token.ex              (tokenizer; pure input only)
- apps/core/lib/core.ex                    (SI constructor)
- apps/db/lib/db/lexicon.ex                (fetch_by_norms/1, fetch_by_ids/1; keeps ensure_* around)

How to apply
------------
cd /path/to/Symbrella
unzip -o Symbrella_patch_v2.zip -d .
mix deps.get && mix compile
iex -S mix

Then:
iex> Brain.chat("hello there")
iex> Brain.snapshot()

Generated: 2025-09-16T15:40:17.666356Z
