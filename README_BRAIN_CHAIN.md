# Brain Chain (Semantic Disambiguation → Integration → Memory)

This doc sketches the current processing chain and the brain-inspired modules
that receive signals after the LIFG stage.

## Order of Operations

1. **Tokenize + N-grams (Core)**
   - Build tokens and contiguous word n-grams with spans.

2. **STM (Brain.stm/1)**
   - Short-term token queue; prepares candidates.

3. **LTM (Db.ltm/2 via Core.ltm_stage/2)**
   - Fetch known cells (definitions, seeds, etc.). May create seeds for missing norms.

4. **Lexicon Stage (Core.Lexicon.Stage)**
   - (Optional) Scrape/normalize dictionary senses and upsert into DB.
   - Re-fetch rows and merge so this turn can use new defs.

5. **LIFG Stage-1 (Brain.lifg_stage1/3)**
   - Left Inferior Frontal Gyrus “semantic control” / selection.
   - Outputs `lifg_choices` with per-token winning sense + scores/margins.

6. **Post-LIFG Fan-out (Brain.Chain.fanout/2)**
   - Broadcasts the enriched `SemanticInput` (with `lifg_choices`) to:
     - **Brain.ATL** (semantic hub, integration)
     - **Brain.PMTG** (controlled retrieval interface)
     - **Brain.AG** (multimodal combinatorial integration)
     - **Brain.MTL** (episodic binding / hippocampal complex)
     - **Brain.ACC** (conflict monitoring / control signal)
     - **Brain.BG** (gating / credit assignment)

7. **Activation Notify (Core → Brain)**
   - `{:activate_cells, rows, payload}` cast to Brain for global activation bookkeeping.

## Current Status

- ✅ LIFG selection operational; `lifg_choices` are attached to `SemanticInput`.
- ✅ Region stubs (GenServers) exist and are supervised; they store `last_si`.
- 🔜 Future work per region:
  - ATL: map winning senses → stable concept nodes, integrate across tokens.
  - pMTG: modulate access policy using ACC margin/conflict.
  - AG: compose larger relations/spans, candidate propositions.
  - MTL: bind current semantic state to episodic traces; later replay for priming.
  - ACC: compute conflict (e.g., low LIFG margin) → raise control gain.
  - BG: gate updates and route reinforcement to improve future priors.

## Introspection

- `Brain.Chain.snapshot/0` returns a map of `{region_name => state}`.
- Each region supports:
  - `Region.snapshot/0`
  - `Region.stimulate(si)` (manual poke for debugging)
