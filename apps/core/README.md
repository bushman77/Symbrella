# Core — Semantic Orchestration Layer

> Core is the **semantic pipeline** of the Symbrella umbrella.  
> It turns raw text into a rich `Core.SemanticInput` struct, resolves intent, plans recall, and coordinates with the synthetic brain (`apps/brain`) and storage (`apps/db`).

Scientific accuracy note: Core and Brain use brain-inspired vocabulary as an
engineering analogy. See
[`docs/brain-core-scientific-contract.md`](../../docs/brain-core-scientific-contract.md)
for the testable contract and prohibited overclaims.

Core **does not** run its own supervision tree or own database persistence.
Instead, it acts as the *translator and traffic controller* between:

- the outside world (strings, UI, API),
- the Brain processes (LIFG, Hippocampus, WM, Curiosity, etc.),
- and long-term storage (episodes, brain cells, lexicon entries).

---

## Role in the umbrella

At a high level, Symbrella’s processing pipeline looks like:

```elixir
phrase
|> Core.LIFG.Input.tokenize() # word tokens, sentence-aware spans
|> Core.Brain.STM.run()       # working memory / activation (apps/brain)
|> Core.MWE.Stage.run(:early) # word-level MWE candidates
|> Core.Pipeline.LTM.run()    # long-term memory fetch through Db
|> Core.LIFG.Attach.run_and_attach()
```

Within that flow, **Core owns**:

- the **semantic input state** (`Core.SemanticInput`),
- **tokenization and MWE injection** for the LIFG path,
- **intent resolution** (classifier + matrix fallback),
- **recall planning** (what to ask Hippocampus for, and when),
- **invariants** over tokens, spans, and sense slates.

Brain, Db, and Web apps treat Core as the **semantic plumbing** that keeps everything consistent and testable.

---

## Key responsibilities

### 1. `Core.SemanticInput` (SI)

`Core.SemanticInput` is the main carrier of semantic state through the pipeline.

It tracks (among other fields):

- `sentence` — original text (when available).
- `tokens` — cleaned, boundary-safe tokens for LIFG.
- `token_structs` — richer token structs with spans/flags.
- `pos_list` — POS tags per token (for intent & patterns).
- `intent`, `keyword`, `confidence` — resolved intent triple.
- `phrase_matches` — matched phrases / MWEs.
- `sense_candidates` — **per-token sense slates** that LIFG consumes.
- `activation_summary` — snapshot from Brain (active cells / WM).
- `pattern_roles` — structural tags used by the intent matrix and, later, ACC/OFC.
- `evidence` — attached recall, lexical, or episode evidence used by later
  stages without forcing a hard decision.

Everything Core does is centered around evolving this struct cleanly from “raw text” to “ready for LIFG + recall + response”.

---

### 2. LIFG-safe tokenization (`Core.LIFG.Input`)

The LIFG path has strict invariants:

- **Words only:** no char-grams are allowed into LIFG Stage-1.
- **Boundary guard:** substring spans that don’t align with word boundaries are dropped (unless they are explicit MWEs).
- **Word-level MWEs:** multi-word expressions are formed from word n-grams, not from character n-grams.

`Core.LIFG.Input` provides canonical entrypoints for this:

- `tokenize/1` — from a raw sentence to a list of LIFG-safe tokens.
- `tokenize/2` — same, with optional MWE injection.
- `tokenize/1,2` (SI variants) — accept a `Core.SemanticInput` and return an updated SI with cleaned tokens.

Under the hood it uses:

- `Core.Token` for tokenization and span invariants,
- `Brain.LIFG.Guard` and `Brain.LIFG.BoundaryGuard` for sanitization,
- `Core.MWE.Injector` for optional phrase injection (word-level n-grams).

---

### 3. MWE injection (`Core.MWE.Injector`)

Core contains a dedicated MWE injector that:

- takes a list of **single-word tokens** with spans,
- scans word-level n-grams (2..N),
- consults an `exists?/1` callback to decide which phrases are “real” MWEs,
- emits injected tokens like:

  ```elixir
  %{phrase: "new york city", mw: true, span: {0, 3}, n: 3, source: :mwe}
  ```

The injector:

- never talks to the database directly,
- stays generic (caller provides the `exists?/1` function),
- returns **deduped + ordered** tokens:

  - all single words first (sorted by `start`),
  - then MWEs (also sorted, shorter before longer at the same start).

This keeps the LIFG side simple and respects the `db ← brain ← core ← web` dependency chain.

---

### 4. Intent resolution (`Core.Intent.*`)

Core owns the entire **intent resolution pipeline**:

- primary classifier / heuristics,
- matrix-based fallback when confidence is low,
- telemetry and invariants around the intent triple.

Roughly:

- The classifier proposes `(intent, keyword, confidence)`.
- If `confidence` is below a configurable threshold, Core falls back to the pattern-based `Core.Intent.Matrix` **without throwing away context** (tokens, POS, keyword, etc.).
- Telemetry is emitted on fallback so behavior can be inspected and tuned.

Brain sees intent only as input (for biasing scores); it never owns the classifier itself.

---

### 5. Recall planning (`Core.Recall.*`)

Actual episodic recall happens in **Hippocampus** (apps/brain + apps/db), but Core is responsible for:

- deciding **when** to ask for recall,
- forming **cue tokens / scopes**,
- merging recall results into `si.evidence[:episodes]`.

Modules under `Core.Recall`:

- represent the **plan**, **gate**, and **executor** for recall stages,
- ensure that recall remains bounded (e.g. top-K, Jaccard thresholds),
- integrate easily with property tests and telemetry.

This keeps Hippocampus focused on the “memory science” while Core stays responsible for when/why recall is invoked.

---

### 6. Brain adapter (`Core.Brain*`)

Core never reaches down into Brain internals or processes. Instead, it uses thin adapters:

- `Core.Brain` / `Core.BrainAdapter` act as small facades for:
  - “ask Brain for a snapshot”,
  - “run a specific region pipeline”,
  - “fold Brain’s activation summary back into SI”.

The rule is:

> Core treats Brain like a **service**.  
> Brain never depends on Core.

That keeps the umbrella dependency chain clean and acyclic.

---

### 7. Response planning (`Core.Response.*`)

Core owns the response policy surface that turns semantic state into an output
plan. Important modules include:

- `Core.Response.Policy` — chooses response mode and safety posture.
- `Core.Response.Modes` — mode-specific phrasing strategy.
- `Core.Response.Personality` and `Core.Response.Affect` — style and affect
  shaping based on state.
- `Core.Response.Guardrails` — response-level constraints.
- `Core.Response.LlmPrompt` and `Core.Response.LlmSynthesis` — prompt building
  and optional calls into `apps/llm`.

Brain supplies signals; Llm supplies model text when needed. Core owns the
decision about how those pieces become a response.

The concrete mood/personality/LLM prompt mapping is documented in
[`docs/modulator-to-prompt-contract.md`](../../docs/modulator-to-prompt-contract.md).

---

## How to use Core (inside the umbrella)

Core is **not** meant to be published as a standalone Hex package right now.  
It is built to be used from other apps in the Symbrella umbrella (Brain, Web, CLI tools).

Typical usage from another app:

```elixir
# Run the production semantic pipeline
si1 = Core.resolve_input("alpha beta", mode: :prod)

# Inspect the enriched state
si1.lifg_choices
si1.response_plan
```

Exact entrypoints evolve over time, but the pattern remains:

- build or update a `Core.SemanticInput`,
- run it through Core’s pipeline,
- then pass the enriched SI to Brain and, eventually, back to Web/UI.

---

## Running tests

From the umbrella root:

```bash
# Core-only tests
mix test apps/core/test

# A specific test file
mix test apps/core/test/core/lifg_input_test.exs
```

Core tries to keep a **strong test harness** around:

- tokenizer invariants,
- MWE injection behavior,
- intent matrix and fallback behavior,
- recall planning and execution,
- response policy and LLM prompt shaping,
- SemanticInput structure & sense slate behavior.

---

## When to change Core (and when not to)

**Change Core** when you need to:

- add new fields or invariants to `Core.SemanticInput`,
- adjust tokenization / MWE rules for LIFG safety,
- tune intent resolution or fallback behavior,
- refine recall planning and execution,
- add new telemetry around semantic pipeline stages,
- adjust response planning, prompt construction, or model-synthesis policy.

**Do not change Core** to:

- talk directly to the database (`Db` owns persistence),
- own LIFG Stage-1 or WM behavior (that’s `apps/brain`),
- add web/UI logic (that belongs in `apps/symbrella_web`),
- start supervision trees (those live under `Symbrella.Application` in the umbrella root).

Keeping these boundaries clear is what allows Symbrella’s **db ← brain ← core ← web** layout to stay healthy and maintainable.
