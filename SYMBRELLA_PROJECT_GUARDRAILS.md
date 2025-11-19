# Symbrella Project Guardrails
*(Directory Structure, Semantics & Approval Protocol — living doc)*  
**Last updated: 2025-11-19**

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
- **Deliverables:** Prefer chat-bubble code blocks only (UTF-8, LF). Avoid ZIPs/binaries. Multi-file outputs are sent as consecutive chat bubbles with clear filename headers. **No unsolicited multi-pane diffs.**  
  *(Exception: a small `.zip` is OK only when explicitly requested.)*

See also: **README.md** (high-level), **README_BRAIN_CHAIN.md** (pipeline notes).

---
