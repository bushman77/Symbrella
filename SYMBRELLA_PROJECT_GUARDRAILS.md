# Symbrella Project Guardrails
*(Directory Structure, Semantics & Approval Protocol — living doc)*  
**Last updated: 2025-11-27**

> **Purpose.** This is the single source of truth we both refer to before any refactor or file replacement, so we don’t mangle the project. It encodes directory layout, module boundaries, semantic contracts, and our pair-programming approval flow.

---

## TL;DR

- **Approval tokens required.** Never ship changes without an explicit token in chat:  
  `Approve: P-###` or `Approve: P-### (FileScope: apps/.../file.ex)`.
- **Full-File Patch Guardrail.** Before any code is written, paste the entire current file; responses are **full-file replacements only**. Approvals must include FileScope when possible.
- **One umbrella-root supervisor.** Start everything under `Symbrella.Application`. **No per-app Application modules.**
- **Acyclic deps:** `db ← brain ← core ← web` (left depends on nothing to the right).
- **LIFG lives in `apps/brain`.** Core orchestrates the pipeline and can call Lexicon / Brain as a client.
- **LIFG path invariants:** words-only (no char-grams), boundary guard, MWE injection, sorted spans, config defaults set in test/dev.
- **Curiosity loop invariants:** `Brain.Curiosity → Brain.Thalamus → Brain.BasalGanglia/WM → Brain.DLPFC` is telemetry-first, side-effect bounded, and always gates via WM policy (no rogue focus inserts).
- **Guard safety invariant (Elixir):** **Never call remote functions inside guards** (e.g., `String.trim/1`, `Regex.match?/2`, `Map.get/2`). Guards must stay guard-safe. If you need trimming/normalizing, do it in the function body.
- **Deliverables:** Prefer chat-bubble code blocks only (UTF-8, LF). Avoid ZIPs/binaries. Multi-file outputs are sent as consecutive chat bubbles with clear filename headers. **No unsolicited multi-pane diffs.**  
  *(Exception: a small `.zip` is OK only when explicitly requested.)*

See also: **README.md** (high-level), **README_BRAIN_CHAIN.md** (pipeline notes).

---

## Directory Structure & Boundaries

### Umbrella apps (high level)
- `apps/db` — persistence (Ecto Repo `Db`, migrations, schemas)
- `apps/brain` — brain regions + runtime cognition
- `apps/core` — orchestration + input/intent/planning (client of Brain)
- `apps/symbrella_web` — Phoenix/LiveView UI and web adapters

### Dependency direction (must remain acyclic)
**`db ← brain ← core ← web`**

- `db` depends on nothing to the right.
- `brain` may depend on `db` (Repo, schemas) but never `core` or `web`.
- `core` may depend on `brain` (as a client) but never `db` directly.
- `web` may depend on `core` (and via that, talk to `brain`), but must not pull `db`/`brain` the wrong way.

---

## Approval / Refactor Protocol (Pair-Programming)

1. **Before any refactor / replacement**
   - Paste the *entire current file*.
   - State the intent in one sentence.

2. **Assistant response format**
   - **Full-file replacement only** (not partial diffs), unless explicitly requested.
   - Keep changes minimal and scoped.

3. **Shipping**
   - Requires explicit token: `Approve: P-###`  
     Prefer: `Approve: P-### (FileScope: apps/.../file.ex)`.

---

## Elixir Guard Safety (Non-Negotiable)

### Rule
**Guards may only use guard-safe operations.**  
That means: **no remote calls** in `when` (no `String.*`, `Regex.*`, `Map.*`, etc).

### Why
- The compiler rejects it (e.g., `cannot invoke remote function String.trim/1 inside a guard`).
- Guards should be cheap, predictable, and side-effect free.

### Patterns to use instead

**Bad (illegal guard):**
```elixir
def foo(s) when is_binary(s) and String.trim(s) != "" do
  ...
end
```

**Good (guard-safe + body normalization):**
```elixir
def foo(s) when is_binary(s) do
  s = String.trim(s)
  if s == "", do: :empty, else: ...
end
```

**Good (helper predicate, no guards):**
```elixir
defp nonblank?(s) when is_binary(s) do
  String.trim(s) != ""
end
defp nonblank?(_), do: false
```

---

## LIFG Path Invariants (apps/brain)

- **Words-only path:** no char-grams may enter LIFG scoring.
- **Boundary guard:** drop non-word-boundary substrings unless `mw: true`.
- **MWE injection:** word-level n-grams before LIFG, never char-grams.
- **Sorted spans:** token spans must be hydrated from sentence, sorted by start.
- **Config defaults (test/dev):**
  - `tokenizer_mode: :words`
  - `tokenizer_emit_chargrams: false`
- **Telemetry tripwires:** log/drop if char-grams reach LIFG; log boundary drops.

---

## Curiosity Loop Invariants

- Flow: `Brain.Curiosity → Brain.Thalamus → Brain.BasalGanglia/WM → Brain.DLPFC`
- Telemetry-first: every step emits evidence / reasoning breadcrumbs.
- Side-effects bounded: no hidden writes outside the gating policy.
- WM gating is the single authority for focus inserts / promotions.

---

## Data / Storage Invariants (apps/db)

- Repo module is `Db` (use `Db.*`).
- DB concerns live in `apps/db` (migrations, schema constraints, Repo config).
- Avoid atom leaks from external data sources:
  - Do not create atoms from file/JSON content.
  - Prefer string keys, `Map.get/2`, and canonicalizers.

---
