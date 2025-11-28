# Symbrella Project Guardrails
*(Directory Structure, Semantics & Approval Protocol — living doc)*  
**Last updated: 2025-11-27**

> **Purpose.** This is the single source of truth we both refer to before any refactor or file replacement, so we don’t mangle the project. It encodes directory layout, module boundaries, semantic contracts, and our pair‑programming approval flow.

---

## TL;DR

- **Approval tokens required.** Never ship changes without an explicit token in chat:  
  `Approve: P-###` or `Approve: P-### (FileScope: apps/.../file.ex)`.
- **Full‑File Patch Guardrail.** Before any code is written, paste the **entire current file**; responses are **full‑file replacements only**. Approvals should include FileScope when possible.
- **One umbrella‑root supervisor.** Start everything under `Symbrella.Application`. **No per‑app Application modules.**
- **Acyclic deps:** `db ← brain ← core ← web` (left depends on nothing to the right).
- **LIFG lives in `apps/brain`.** Core orchestrates the pipeline and can call Lexicon/Brain as a client.
- **LIFG path invariants:** words‑only (no char‑grams), boundary guard, MWE injection, sorted spans, config defaults set in `test/dev`.
- **Curiosity loop invariants:** `Brain.Curiosity → Brain.Thalamus → Brain.BasalGanglia/WM → Brain.DLPFC` is telemetry‑first, side‑effect bounded, and always gates via WM policy (no rogue focus inserts).
- **Guard usage rule:** keep guards simple. **No remote calls inside guards** (e.g., `String.trim/1`, `String.contains?/2`). Precompute values/booleans before `when`.
- **Deliverables:** Prefer **chat‑bubble code blocks only** (UTF‑8, LF). Avoid ZIPs/binaries; a `.zip` is okay **only when explicitly requested**. No unsolicited multi‑pane diffs.

See also: **README.md** (high level), **README_BRAIN_CHAIN.md** (pipeline notes).

---

## Guard Safety (Elixir)

Remote calls in guards are illegal in Elixir and will fail compilation.

**Bad (won’t compile):**
```elixir
def f(s) when is_binary(s) and String.trim(s) != "", do: ...
```

**Good (precompute):**
```elixir
def f(s) when is_binary(s) do
  s2 = String.trim(s)
  if s2 != "", do: ..., else: ...
end
```

If you need guard-like branching, prefer:
- `case` / `cond`
- precomputed booleans (e.g. `trimmed = String.trim(s); ok? = trimmed != ""`)
- small private helpers called from inside the function body (not in `when`)

---

## Canonical Umbrella Layout (minimal)

If a directory/file isn’t listed here, treat it as optional. If we add new dirs, we add them here first.

```
symbrella/
├── README.md
├── README_BRAIN_CHAIN.md
├── SYMBRELLA_PROJECT_GUARDRAILS.md
├── apps/
│   ├── brain/        # LIFG + brain regions live here
│   ├── core/         # orchestrator + semantic input + tokenization
│   ├── db/           # Ecto schemas + Repo + migrations only
│   ├── lexicon/      # lexicon adapters (if kept as separate app)
│   ├── symbrella/    # umbrella utility app; single root supervisor here
│   └── symbrella_web/# Phoenix UI (read-only snapshot; no brain mutation)
├── config/
│   ├── dev.exs
│   ├── test.exs
│   └── runtime.exs
└── mix.exs
```

**Module boundaries**
- **Brain owns LIFG**: parsing/disambiguation primitives live in `apps/brain`.
- **Core orchestrates**: tokenize → Brain.stm → Db.ltm → Lexicon, coordinates intent resolution.
- **Db**: schemas and Repo only; no business logic.
- **Web**: UI only; may call `GenServer.call(Brain, :snapshot)` for visibility (printing allowed, no mutation).

---

## Golden Pipeline

```elixir
phrase
|> Core.Token.tokenize()   # word tokens first; sentence-aware spans
|> Brain.stm()             # short-term focus / activation (processes)
|> Db.ltm()                # long-term memory fetch (rows)
|> Core.Lexicon.all()      # dictionary lookups as needed
```

**Bandwidth principle:** tokenize → check active cells → DB → lexicon (only if still unresolved).

---

## LIFG Path: Definition of Done (DoD)

Track progress in code reviews; mark ✅ when completed.

- [ ] No char-grams in LIFG path (enforced + unit test)
- [ ] Boundary guard (drop non‑word‑boundary substrings unless `mw: true`)
- [ ] MWE injection pass (word-level n‑grams before LIFG)
- [ ] Sense slate in SI (`si.sense_candidates` keyed by token index)
- [ ] Reanalysis fallback (flip to next‑best on integration failure)
- [ ] Telemetry tripwire (log/drop if a char‑gram reaches LIFG)
- [ ] Priming cache (optional; recency boost for recent winners)
- [ ] Invariant tests (spans sorted; no char‑grams; boundary-only unless `mw: true`)
- [ ] Config defaults (test/dev: `tokenizer_mode: :words`, `tokenizer_emit_chargrams: false`)
- [ ] Tokens sorted by start; spans hydrated from the sentence

**ID convention:** `"{word}|{pos}|{sense}"` for `Db.BrainCell.id` (string PK).

---

## Configuration Defaults

Set in `config/test.exs` and `config/dev.exs`:

```elixir
config :core,
  tokenizer_mode: :words,
  tokenizer_emit_chargrams: false
```

---

## Large Data / Dumps (important)

- Never commit multi‑GB dumps (Kaikki/Wiktextract, embeddings, etc.) to git.
- Keep them in `apps/*/priv/` **gitignored**, or store outside the repo and mount/symlink.
- Always document expected local paths + how to generate them (scripts + checksum).

---

## Approval Protocol (Pair‑Programming Guardrails)

**Nothing merges or “goes live” without an explicit approval token.**

1) **Proposal**: I present a patch proposal with ID `P-###` that includes:
   - Summary (what/why), risk level (Safe / Risky / Breaking), and scope
   - Key before/after excerpts
   - File list (replacements/additions)
   - Tests to add/update
   - Rollback plan

2) **Deliverables**: chat‑bubble code blocks only (UTF‑8, LF). Avoid ZIPs/binaries unless explicitly requested.

3) **Approval**: you respond with `Approve: P-###` (optionally with FileScope).  
   - If not approved, we do **no file replacements** (we can iterate as `P-###.1`, etc.)

4) **Special tags** (visibility):
   - `QuickFix-###`: small, surgical changes
   - `BREAKING-###`: migrations / API changes / module moves
   - `Hotfix-###`: urgent fix; still needs `Approve: …` before applying

5) **After approval**: final artifacts only, with no scope creep beyond the approved patch.
