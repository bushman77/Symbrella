defmodule Symbrella do
  @moduledoc ~S"""
  Symbrella — Neuro-Symbolic Synthetic Intelligence (**NSSI**)

  ## What this is

  Symbrella is a brain-inspired, **neuro-symbolic synthetic intelligence (NSSI)**:
  a hybrid cognitive architecture that fuses transparent symbolic structures with
  learned representations and episodic memory. It is built as an Elixir/Phoenix
  umbrella app with clearly separated concerns and acyclic dependencies.

  In short:
  - **Neuro-symbolic:** explicit symbols (BrainCells, spans, senses, intent patterns)
    work alongside vectors, embeddings, and lightweight Nx models.
  - **Brain-inspired:** modules map to functional “regions” (e.g., LIFG, PMTG,
    Hippocampus) to keep reasoning legible and traceable.
  - **Synthetic intelligence:** we’re engineering cognition, not just a model—
    the emphasis is on control, introspection, and steady evolution.

  ## Why it’s designed this way

  1. **Transparency & control.** We prefer auditable data paths over opaque end-to-end
     nets. Decisions are inspectable (tokens, spans, senses, evidence).
  2. **Composable cognition.** Regions encapsulate responsibilities and coordinate
     through minimal, explicit contracts (messages, structs, events).
  3. **Reliability through guardrails.** Invariants and tripwires prevent subtle
     regressions (e.g., char-grams leaking into LIFG, span ordering).
  4. **On-device friendliness.** Lightweight Nx and symbolic routines keep latency
     and resource use modest, enabling local/edge deployments.
  5. **Iterative science.** We can swap heuristics and models without derailing the
     architecture—each region evolves behind a stable interface.

  ## Current cognitive loop (high level)

  - **Input shaping (Core):** Build a `SemanticInput` snapshot (`si`), normalize tokens,
    inject MWEs, and record pattern roles.
  - **PMTG (controlled semantic retrieval):**
    - Enforce **MWE↔sense** compatibility (only MWEs compete for MWE senses, etc.).
    - Promote winners and near-winners into `si.sense_candidates` keyed by token index.
  - **Hippocampus (episodic bias):** Recall recent episodes with a recency half-life
    and surface cues into `evidence.episodes` to bias disambiguation.
  - **LIFG Stage 1 (symbolic disambiguation):**
    - Hard-reject **char-grams**; enforce **boundary guard** on spans.
    - Score candidates with a feature mix (lexical fit, relation priors, activation,
      intent bias, episode boost). Telemetry on any invariant trips.
  - **ATL finalize → Hippocampus write:** Persist an episode snapshot after sense
    integration; keep the write path cheap and always-on.

  ## Engineering guardrails (musts)

  - **No char-grams in LIFG path.** Char-grams are for fuzzy recall/search only.
  - **Boundary guard.** Drop substrings that don’t start/end on word boundaries unless `mw: true`.
  - **MWE injection pass.** Build word-level n-grams (2..4) before LIFG; inject only
    when the lexicon confirms existence.
  - **Telemetry tripwires.** Emit events like `:lifg_chargram_violation`,
    `:pmtg_no_mwe_senses`, `:lifg_boundary_drop`.
  - **Invariant tests.** Spans sorted, boundary-only unless `mw: true`, no char-grams in LIFG.
  - **Config defaults (dev/test).** `tokenizer_mode: :words`, `tokenizer_emit_chargrams: false`.
  - **Acyclic deps:** `db ← brain ← core ← web`. LIFG lives in `apps/brain` (not `core`).
  - **Approval protocol.** Any structural refactor requires an explicit token:
    `"Approve: P-###"`. Always review `SYMBRELLA_PROJECT_GUARDRAILS.md` first.

  ## Naming: NSSI

  We use **NSSI** (Neuro-Symbolic Synthetic Intelligence) to clearly distinguish the
  architecture from any single model and to avoid collisions with `si` (SemanticInput).
  NSSI refers to the *whole cognitive system*—regions, data flow, and learning components.

  ## Glossary (short)

  - **BrainCell:** a symbolic/typed unit with activation, connections, and optional vectors.
  - **PMTG:** controlled semantic retrieval; builds `si.sense_candidates` with compatibility checks.
  - **LIFG:** symbolic disambiguation; consumes candidates under strict guards and telemetry.
  - **Hippocampus:** episodic memory; writes/recalls episodes with recency dynamics.
  - **SemanticInput (`si`):** the working slate for a single turn (tokens, roles, candidates, evidence).

  ## Public surface (convention)

  The root `Symbrella` module carries this documentation and version identity for the
  umbrella. Runtime services (supervisors, regions, web UI) live in their respective
  apps; keep cross-app boundaries clean and audited.

  ---
  This module does not implement runtime logic; it declares intent.
  See guardrails for how we evolve NSSI safely and incrementally.
  """
end

