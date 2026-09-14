# Symbrella Project Guardrails
*(Directory Structure, Semantics & Approval Protocol — living doc)*  
**Last updated: 2026-09-14**

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
- **Cognitive-control invariant:** candidate source -> cognitive/control evidence -> `Brain.BasalGanglia` canonical admission gate -> controlled PFC/WM update.
- **No new WM bypasses.** No new code path may directly insert arbitrary candidates into `Brain.WorkingMemory` while bypassing the canonical admission gate. Existing legacy/transitional paths may remain until explicitly refactored.
- **Responsibility boundaries:** LIFG selects/interprets. BasalGanglia gates. WorkingMemory maintains. SelfModel modulates. Core orchestrates.
- **Curiosity loop invariants:** `Brain.Curiosity -> Brain.Thalamus -> Brain.BasalGanglia -> Brain.DLPFC/PFC -> Brain.WorkingMemory` is telemetry-first, side-effect bounded, and should converge on the canonical admission gate.
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
- **Brain owns LIFG and cognitive-control regions**: parsing/disambiguation
  primitives, BasalGanglia gating, WorkingMemory mechanics, self-state,
  appraisal, mood, continuity, calibration, and reflective control live in
  `apps/brain`.
- **Core orchestrates**: tokenize → Brain.stm → Db.ltm → Lexicon, coordinates intent resolution.
- **Db**: schemas and Repo only; no business logic.
- **Web**: UI only; may call `GenServer.call(Brain, :snapshot)` for visibility (printing allowed, no mutation).

---

## Semantic Fetch Order

```elixir
phrase
|> Core.Token.tokenize()   # word tokens first; sentence-aware spans
|> Brain.stm()             # short-term focus / activation (processes)
|> Db.ltm()                # long-term memory fetch (rows)
|> Core.Lexicon.all()      # dictionary lookups as needed
```

This is a minimal semantic/evidence fetch order, not the full cognitive-control
loop. The bandwidth principle remains: tokenize -> check active cells -> DB ->
lexicon only if still unresolved.

---

## Cognitive Architecture Guardrails

Symbrella is a biologically-inspired computational architecture. Region names
are engineering abstractions and functional correlates, not claims of
biological equivalence. Do not document or implement them as a literal
neuron-for-neuron simulation, a clinical model, or evidence of consciousness,
sentience, feelings, or subjective experience.

Target cognitive-control path:

```text
Semantic / lexical representations
-> LIFG Stage1
-> LIFG Stage2 linguistic evidence
-> ACC / cognitive-control context
-> BasalGanglia canonical admission gate
-> Thalamic relay / gating control
-> DLPFC / PFC control
-> WorkingMemory mechanics
```

Feedback is allowed and expected when explicit and bounded:

```text
WorkingMemory / PFC
-> LIFG context
-> BasalGanglia context
-> memory retrieval
-> attention/control
-> SelfModel evidence
```

Responsibility boundaries:

- `Brain.LIFG.Stage1` selects the best-supported semantic interpretation.
- `Brain.LIFG.Stage2` finalizes linguistic evidence for downstream control.
  Current commit-shaped behavior is transitional and should not become the
  canonical final WM admission authority.
- `Brain.ACC` supplies conflict, uncertainty, ambiguity, and task-pressure
  context.
- `Brain.BasalGanglia` is the canonical target for general Working Memory
  admission decisions such as `:allow`, `:boost`, and `:block`.
- `Brain.Thalamus` relays and arbitrates control signals; it is not the semantic
  decision-maker.
- `Brain.DLPFC` / PFC applies executive/task control and must not become an
  uncontrolled direct writer to WorkingMemory.
- `Brain.WorkingMemory` normalizes, maintains, merges, activates, decays, trims,
  evicts, enforces capacity, and emits telemetry for admitted representations.
- `Brain.SelfModel` integrates runtime evidence and can provide bounded
  modulation; it is not itself the gate.
- `Core` sequences the pipeline and passes explicit evidence into Brain.

No new code path may directly insert arbitrary candidates into WorkingMemory
while bypassing the canonical admission gate. Existing legacy/transitional
paths in `Brain.LIFG.Stage2`, `Brain.WM.Policy`, and `Brain.BasalGanglia` are
Phase 9A consolidation work.

The target split is:

```text
BasalGanglia:
general cognitive admission decision

WM policy / WorkingMemory:
retention, decay, duplicate/lemma constraints,
capacity, normalization, merge, and eviction
```

---

## LIFG Path: Definition of Done (DoD)

Track progress in code reviews; mark ✅ when completed.

- [x] No char-grams in LIFG path (enforced + unit test)
- [x] Boundary guard (drop non‑word‑boundary substrings unless `mw: true`)
- [x] MWE injection pass (word-level n‑grams before LIFG)
- [x] Sense slate in SI (`si.sense_candidates` keyed by token index)
- [ ] Reanalysis fallback (flip to next‑best on integration failure)
- [x] Telemetry tripwire (log/drop if a char‑gram reaches LIFG)
- [ ] Priming cache (optional; recency boost for recent winners)
- [x] Invariant tests (spans sorted; no char‑grams; boundary-only unless `mw: true`)
- [x] Config defaults (test/dev: `tokenizer_mode: :words`, `tokenizer_emit_chargrams: false`)
- [x] Tokens sorted by start; spans hydrated from the sentence

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
