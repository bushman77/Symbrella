# N-027 — /social cognition audit (Claude, READ-ONLY)

**Label:** READ-ONLY. Code at `ca2acb6f` (`symbolic_generation`). I executed nothing and cannot run Elixir.
Evidence key: **[V]** verified by reading the cited file:line; **[H]** hypothesis; **[U]** unknown / not checked.
The post's factual claims (mint counts, listings, dates) were not checked and are not asserted here.

## 0. Answer to the central question

**Partly bypassed, but not where the brief suspects.**

1. The social path runs the *same pure computations* as chat: the LIFG Stage1 engine, `Brain.ATL.reduce`, structural competition, typed composition, `SentenceMeaning.build`, `ProvisionalInterpretation`, `Comprehension.Summary`. **[V]** `semantic_probe.ex:112-140`.
2. It runs them in a **context-free configuration** (semantic control, sense focus, reanalysis, MWE off; no MWE stage; `max_wordgram_n: 1`) and **never reads live Brain.Cell state**. Chat does: `Activation.prime` feeds the live STM activation snapshot into LIFG as a prior (`activation.ex:36-68`, `core.ex:169`). **[V]**
3. Neurons are written to only **after** comprehension, at a tiny amplitude, as a count-only decoration (§2, §5). The neuron layer does not participate in the comprehension. **[V]**
4. The deepest cause of "0 supported meanings" is not a missing brain region. The path that turns a finite S-V-O or progressive-spatial **declarative** into a supported meaning (`:participant_assertion`, built from `EpisodicProjection.validated_participant/1`, `sentence_meaning.ex:~726,814`) is **source-gated**: `EpisodicProjection.project/1` returns `[]` and `validated_participant/1` yields no candidates unless `source in [:user, :prod, :test]` (`episodic_projection.ex:48,91`; the provisional 4-token frame has the same gate, `sentence_meaning.ex:235`). Colony text is `:colony_external`, so for declarative statements **zero chat-style participant meanings is guaranteed by design**. **[V]** Question frames and structural-competition frames (`sentence_meaning.ex:168,676`) are *not* source-gated. The social route substitutes a second, narrow pilot interpreter, `Core.Social.DeclarativeProposition` ("not an open-ended English parser": 4-8 tokens, no `:`/`;`/newline, a small family of S-V-O frames; `declarative_proposition.ex:2-8,20-35`). **[V]**
   This is **not the only blocker**: structural competition (not gated) also returned `no_complete_candidate` for all 12 sentences in the pasted report, so the closed frame inventory (§6.2) blocks independently of the gate.
5. The question "untrusted vs processed" is conflated in one place: the `source` gate decides **whether the interpreter produces a meaning at all**, when it should only decide **what epistemic status the meaning carries**. See §7.

## 1. Execution path (LiveView click to Brain UI)

| # | Step | Location |
|---|------|----------|
| 1 | `analyze_cognition` event, `analyze_piece/2` | `social_live.ex:176-199` |
| 2 | `SocialCognitiveReport.analyze(input, brain_observation?: true, target:)` | `social_live.ex:184` |
| 3 | Split sentences (≤24/source, ≤48 total, ≤600 chars) and call `SemanticProbe.run(text, emit_telemetry?: false, brain_observation?: true)` per sentence | `social_cognitive_report.ex:~15-30,58-64` |
| 4 | Tokenize `Input.prepare(max_wordgram_n: 1)`; `active_cells: []`; checked LTM read + enrich | `semantic_probe.ex:44,56,~60-80` |
| 5 | `Signatures`, `RoleCompetition`, `SentenceCompetition`, `Relations.attach_edges(attach_related_cells?: false)` | `semantic_probe.ex:~85-92` |
| 6 | `Brain.LIFG.Stage1.run/2` with shadow opts (`semantic_control?: false`, `sense_focus?: false`, no reanalysis, no MWE fallback, private telemetry event names) | `semantic_probe.ex:112,331-353` |
| 7 | Pure `Brain.ATL.reduce`; `Brain.LIFG.StructuralCompetition.resolve`; `Composition.derive`; `SentenceMeaning.build`; `ProvisionalInterpretation.detect`; `ReportedProposition`, `DeclarativeEvidence`, `Comprehension.Summary` | `semantic_probe.ex:117-157` |
| 8 | `BrainObservation.observe(enabled?: true)`: Brain alive? then `Activation.feedback_plan(max_feedback: 4, semantic_propagation?: false)`; keep only attested, non-fallback, valid ids; clamp delta to 0.03; `Core.Brain.activate_cells` | `brain_observation.ex:27-33,52,71-73` |
| 9 | `apply_if_exported(Brain, :activate_cells, ..., :ok)` then `Brain.activate_cells` is a **gencast** | `core/brain.ex:19-21`; `brain.ex:153-155` |
| 10 | `handle_cast({:activate_cells,...})` then `CellRT.ensure_start_and_cast` (starts `Brain.Cell` if absent, casts `{:activate, payload}`) | `brain.ex:367-386`; `cell/runtime.ex:20-24,52-60` |
| 11 | `Brain.Cell` adds delta after decay and calls `Brain.report_activation`; the coordinator stores it and calls `safe_mood_register_activation` | `cell.ex:91-104`; `brain.ex:395-404` |
| 12 | `SocialBrainActivity.record(report)` (result **ignored**): all-source `runtime_active_neurons_at_completion`, `Bus.broadcast("brain:social")`, telemetry `[:core,:social,:cognitive_shadow]` and `[:core,:social,:neuron_observation]` | `social_live.ex:192`; `social_brain_activity.ex` (`record`) |
| 13 | Blackboard subscribes to both events (window 100); `/brain` social block and `SocialCognition.panel` render **counts only** | `blackboard.ex:52-53`; `config/brain/brain.exs:58`; `brain_live.ex:77,127-155`; `components/brain/social_cognition.ex` |

## 2. Where activity can be skipped, rejected, lost, or invisible

1. **Brain not running** gives `status: :brain_unavailable` (`brain_observation.ex:27-33`). Visible in the `/brain` block. **[V]**
2. **Empty plan** gives `:no_attested_targets`: the `grammatically_contradicted?` veto (`activation.ex:320-332`), non-attested ids, fallback ids, `Brain.Cell.valid_id?` failures. **[V]**
3. **Silent loss:** `apply_if_exported(..., :ok)` and `gencast` return `:ok` in every failure mode; `SocialBrainActivity.record` can return `:unavailable` and the LiveView discards it (`social_live.ex:192`). **[V]**
4. **Cell start failures** only `Logger.warning` (`cell/runtime.ex:59`); an invalid id raises/emits `[:brain,:cell,:id_rejected]`. **[V]**
5. **Amplitude:** the probe's choices are projected to `id/lemma/token_index/score/weak?/reliability` (`semantic_probe.ex` `project_choices`), dropping `semantic_control` and `candidate_probs`. `discriminated?/1` needs an applied `SemanticControl` adjustment (`activation.ex:334-348`), so **every social stimulus is the weakest tier, `@uncertain_delta` = 0.015** (`activation.ex:21`), at most 4 cells per sentence. This is independent of whether semantic control is enabled. **[V]**
6. **Lifetime:** half-life 5,000 ms, rest threshold 0.001 (`config/brain/brain.exs:55-56`; `cell/activation.ex:35-47`). 0.015 falls below 0.001 after log2(15) = 3.9 half-lives, about **19.5 s** (arithmetic, per cell). **[V]**
7. **UI:** neither `/brain` block nor panel shows per-cell or per-region activation from Colony; they show request counts and per-status sentence counts, plus an all-source neuron count at completion. LIFG/ATL region panels are fed by events the probe deliberately redirects to `[:core,:social,:shadow_stage1_stop|shadow_chargram|shadow_boundary|shadow_mwe]`, for which grep found **no handler**. So regions will not light up for social analysis, by design. **[V]**
8. **Side effect to decide on:** every `report_activation` calls `MoodHooks.register_activation` (`brain.ex:395-404`), so social stimulation touches the mood register. Effect size not read. **[U]**

## 3. Subsystem participation matrix

| Subsystem | Status in `/social` | Evidence |
|-----------|--------------------|----------|
| LIFG Stage1 scoring | **pure shadow**, context-free config; skips the `Brain.LIFG.run` wrapper (ATL finalize / sense candidates / MWE steps / effective opts) | `semantic_probe.ex:331-353`; `lifg.ex:293-330`; `stage1.ex:245` |
| ATL | **pure shadow** (`reduce`; `ingest` explicitly not performed) | `semantic_probe.ex:117,~150` |
| pMTG / MWE | **not invoked** (`max_wordgram_n: 1`, MWE fallback off; LTM lookup instead) | `semantic_probe.ex:44,336-343`; chat: `core.ex:162-175` |
| ACC | **hand-rolled shadow** (`conflict_shadow`: low-margin count); `Brain.ACC` itself **not invoked** | `semantic_probe.ex:~420-435`; `apps/brain/lib/brain/acc.ex` exists |
| Basal Ganglia / WM | **not invoked** (by design) | `working_memory_admission: :not_performed` |
| Thalamus | **not invoked** (feed review reports `:not_submitted`) | `observation_bridge.ex` `evaluate/4` |
| DLPFC / PFC | **not invoked** (response-side in chat) | n/a for comprehension |
| Hippocampus | **not invoked** by Analyze cognition; the feed review does a count-only scoped recall | `observation_bridge.ex` `scoped_recall` |
| Amygdala / neuromodulators | **not invoked** (chat: `core.ex:211`); indirect mood-register touch via `report_activation` | `brain.ex:395-404` **[U]** effect |
| Brain.Cell neurons | **active, write-only** (tiny post-hoc stimuli); live state **never read** | §2; `activation.ex:36-68` unused by probe |
| SentenceMeaning / Composition / Provisional / Summary | **active, pure**, same functions as chat; but **source-gated** (§0.4) and without `Diagnosis.blocked_roles` | `sentence_meaning.ex:235`; `composition.ex:18-30` |
| Chat-only stages omitted | ConstructionSchema, EventFrames, FrameUnifier, EpisodicProjection, TokenResolution, LexicalFocus, FuzzyEntities, RelationalFeedback, MWE stages | `core.ex:162-265`; grep finds no downstream consumer of `construction_schema`/`event_frames` outside `core.ex` and `construction_schema.ex`, so these are probably not the cause **[H]** |
| Discourse / reference | ordering-only relations in web layer | the pasted report |
| Curiosity / repair | **not invoked** | no call in the probe |

## 4. Ranked failure analysis

**Confirmed defects / gaps [V]**
- **F1.** The social path cannot produce chat-style supported *participant-assertion* meanings for declaratives (`source` gate, §0.4); a "0 supported interpretations" headline therefore cannot rise with better lexical evidence alone and is only a partial measure of comprehension quality (structural competition also found no frame, §6.2).
- **F2.** Live neuron state is never read in social (no `prime`, no STM prior), though `prime/2` already supports an isolated prior (`neuron_probe_snapshot:` / `semantic_probe_snapshot:`, `notify_brain?: false`, `activation.ex:44-66`).
- **F3.** `SemanticControl` is disabled although it looks pure (`semantic_control.ex`: no process, ETS or DB calls found by grep; not read in full). Isolation does not require disabling it.
- **F4.** The projection drops fields that would let stimuli reach higher tiers (§2.5).
- **F5.** Silent-loss points (§2.3) and the missing "delivered or rejected" receipt.
- **F6.** The probe does not call `Diagnosis.blocked_roles` / `admission_audit` (`composition.ex:18-30`), so failures lack role-level explanations.
- **F7.** No passive-voice or copular handling in `syntax/`, `semantic/` or `brain/lifg/` (grep for `passive` finds nothing relevant); "was built to be read", "are written for" cannot compose.
- **F8.** `Lexical ambiguity: flagged` and `weak_decision_rate_high` are non-discriminating (12/12), per my earlier note (`semantic_probe.ex` `conflict_shadow`; `summary.ex:15,295`).

**Intentional restrictions [V]:** no WM admission, no episodic persistence, no `ATL.ingest`, no synaptic or dictionary learning, `untrusted_external` labeling, no instruction execution.

**Hypotheses [H]**
- H1. Brad saw no "activity" because the stimuli are 0.015 / ≤4 cells / about 20 s and the UI never renders Colony neuron levels.
- H2. Universal `weak_decision_rate_high` is partly caused by context-free scoring (F3). Falsifiable by A/B (§5).
- H3. The `selected_pos_incompatible` withholds (sentence 2; second candidate in sentence 5) come from POS mismatches under context-free scoring; the report does not show which role.

**Unknown [U]:** actual `neuron_observation_statuses` for Brad's run; per-sentence weak rates; `MoodHooks.register_activation` behavior; exact differences between chat's LIFG entry (`Brain.lifg_run/2` if exported, else `Brain.LIFG.run/2`, `attach.ex:~312`) and `Stage1.run/2`.

## 5. Neural-activity diagnosis and minimal falsification

Most likely (H1): the pipeline delivers stimuli, but they are too small, short-lived and count-only to be noticed. Nothing in the code path I read proves a bug. Two reads settle it:

1. **No code needed:** after clicking Analyze cognition, read the existing `/brain` "Colony cognition" block (`brain_live.ex:127-155`): *Neuron stimuli issued*, and the per-status sentence counts (`stimuli_issued` / `disabled` / `brain_unavailable` / `no_attested_targets`). Paste those lines.
   - `brain_unavailable` or `disabled` means an environment or flag problem.
   - `no_attested_targets` means the plan veto.
   - `stimuli_issued` > 0 means delivery was requested and H1 stands.
2. **In the running node (`iex -S mix phx.server`),** one sentence, transient stimulation only (same effect as the click; LTM reads only):
   ```elixir
   {:ok, r} = Core.Social.SemanticProbe.run("Most NFT sites are written for human eyes.", brain_observation?: true, emit_telemetry?: false)
   IO.inspect(r.brain_observation)
   Brain.activation_snapshot() |> Enum.filter(fn {_id, a} -> a > 0 end) |> Enum.take(10) |> IO.inspect()
   ```
   Expected under H1: `status: :stimuli_issued`, and values near 0.015 that decay to nothing within about 20 s. If `stimuli_issued > 0` but the snapshot is immediately empty, look for the `Brain.Cell.Runtime: start_child` warning or `[:brain,:cell,:id_rejected]`, which would be a real delivery defect.

## 6. Comprehension diagnosis (why 12 sentences, 0 supported)

1. **By construction (F1):** declaratives from `:colony_external` never reach `SentenceMeaning`'s participant-assertion path; the substitute (`DeclarativeProposition`, 4-8 tokens, no `:`/`;`) covers a small S-V-O family. The metric "supported interpretations" therefore cannot rise with better lexical evidence alone.
2. **Closed frame inventory:** none of the 12 sentences fits a supported frame: passives (F7), cleft/relative (sentence 5), imperative + conditional (sentence 11), lists, em-dash asides, identifiers/URLs (sentences 8-10).
3. **Lexical sense/POS under context-free scoring (H2, H3):** unproven but cheap to test.
4. **Not the cause (probably):** dictionary POS coverage (high), ConstructionSchema / EventFrames omission **[H]**.
5. The one local clause ("the site hands an agent") was correctly withheld (embedded in a cleft); that is an abstention working as designed.

Defects vs deliberate limits: F1, F4, F5, F6, F8 are defects or integration gaps; F7 and the closed frame set are deliberate bounded-interpreter limits (and linear to extend, per my earlier cost count).

## 7. Recommendation

**Smallest meaningful correction: observability that distinguishes the states, then a measured context fix.** Not more patterns.

- **S0 (no behavior change):** add a per-sentence `stage_ledger` to the probe result: each stage `invoked_pure` / `skipped_by_design(reason)` / `disabled_by_config(key)`; the LIFG config actually used; weak rate and low-margin count; `Diagnosis.blocked_roles`; and, for `BrainObservation`, planned count, vetoes by reason, delivered ids' max activation read back from `Brain.activation_snapshot()` shortly after. Surface it in the existing `/brain` social block. This makes the eight states in the brief (never invoked / ran but nothing / gate-rejected / processed but no stimulus / delivered / rejected by Cell / decayed or not rendered / supported) distinguishable without a new framework.
- **S1 (measured, default unchanged):** A/B `semantic_control?: true` and `sense_focus?: true` in the probe via the existing `scorer:` injection (`semantic_probe.ex:332`), on a fixed fixture of these 12 constructions. Accept only if weak rate and `selected_pos_incompatible` withholds fall and no gate changes. Also decide whether the projection should carry `semantic_control` / `candidate_probs` (F4).

**Longer-term integration (design review before code):**
- **Attribution-wrapped interpretation.** Let the typed composition run for external text and put the source in the *output's epistemic status* (`external_unverified`, author-claimed) rather than in a gate that stops interpretation; chat consumers keep their `source` / `experience_origin` gates unchanged (`SynapticExperience.extract`, `EpisodicProjection`, `Ledger.record`). Needs an audit of every consumer of `sentence_meaning.status` first **[U]**. This retires the duplicated `DeclarativeProposition` pilot over time and is the same envelope idea as the claim projection proposed in N-023/N-024.
- **Isolated ephemeral observation context: it already half exists.** The probe is the context; `Activation.prime` already accepts an isolated prior and `notify_brain?: false`. Thread a per-analysis activation map across a post's sentences (discarded afterwards) so neurons *participate* without touching the Brain singleton; keep Brain.Cell stimulation as optional visualization. Preserves the BG/WM boundary (nothing is admitted).
- BG, WM, Hippocampus, Thalamus, DLPFC are **not needed** for read-only comprehension and should stay out. Candidates actually worth connecting: semantic control, the neural prior, role-level diagnosis, ACC as a real conflict signal.

## 8. Test strategy (extend what exists)

Existing: `brain_observation_test.exs` (gating, cap, unattested ids, unavailable Brain), `social_brain_activity_test.exs` (receipts, scopes). By titles, none asserts end-to-end delivery. Proposed:
1. **Delivery:** probe with a real `Brain` in test, then `Brain.activation_snapshot()` shows the planned ids with 0 < activation ≤ 0.03; and a negative case with a stopped `Brain` yields `:brain_unavailable`, not a silent `:ok`.
2. **Telemetry visibility:** `SocialBrainActivity.record` return value is surfaced (not ignored); a failed record shows in the UI.
3. **Prohibited writes:** run the full probe with observation enabled under the DB sandbox; assert no new rows in episodes / brain_cells / synapses, unchanged `Brain.snapshot_wm()`, no Ledger session entries.
4. **Semantic coverage:** a fixed, synthetic (non-quoted) fixture of the 12 constructions with expected per-sentence status; metrics: supported count, abstention reason, weak rate, POS-incompat count.
5. **Unsupported-claim rejection:** external text never yields a provenance other than `:external_unverified`; agent-directed imperatives yield no agency command and no WM/Ledger write.
6. **Independent holdout:** a corpus authored by the reviewer and withheld from the implementer.

## 9. Proposed tickets (NOT approved; Brad decides)

| Id | Scope | Files | Depends | Risk | Acceptance / rollback |
|----|-------|-------|---------|------|-----------------------|
| **S0** | stage ledger and delivery receipt, no behavior change | `semantic_probe.ex`, `brain_observation.ex`, `social_brain_activity.ex`, `brain_live.ex`, tests | none | low | UI distinguishes the 8 states for the pasted post; deterministic tests; revert one commit |
| **S1** | semantic-control A/B (opt-in, default unchanged) | `semantic_probe.ex` (+ projection fields), test fixture | S0 | medium (changes scores only under the opt) | numbers on the fixture; no gate or write change; revert |
| **S2** | design review: attribution-wrapped interpretation and consumer audit | docs only | S0, S1 data | none | written audit of `sentence_meaning.status` consumers |
| **S3** | isolated per-analysis prior | `semantic_probe.ex`, `social_cognitive_report.ex` | S2 | medium | no Brain singleton writes; prohibited-writes test; revert |

## 10. Where I disagree with the brief

1. "Bypasses too much of the brain": missing regions (BG, WM, Hippocampus, Thalamus) are not why comprehension fails; the `source` gate and a context-free LIFG configuration are, with evidence above.
2. "No visible activity means missing activation": not established. The path delivers requests; the amplitude/UI explanation (H1) is more likely, and the `/brain` block already holds the discriminating numbers.
3. N-023's ordering (claim projection first) should yield to S0/S1: smaller, measurable, and they address what Brad actually observed. The claim/attribution envelope reappears as S2.

## 11. Not checked

`MoodHooks.register_activation`; full `semantic_control.ex` and `sense_focus?` purity; `Brain.LIFG.run` vs `Stage1.run` differences in detail; `social_understanding.ex` / `social_discourse.ex` internals; any runtime behavior (nothing was run).

## Diagram

```
click "Analyze cognition" ──► SocialLive.analyze_piece ──► SocialCognitiveReport (split ≤24/src)
                                                                  │ per sentence
                                                                  ▼
 COMPUTATION (pure)     SemanticProbe.run ─ tokenize ─ LTM read ─ Signatures/Roles ─ LIFG Stage1 (ctx-free)
                                              ─ ATL.reduce ─ StructuralCompetition ─ Composition.derive
                                              ─ SentenceMeaning.build (participant path source-gated) ─ DeclarativeProposition pilot
                                                                  │
 STATE (transient)      BrainObservation ─ feedback_plan(≤4, 0.015) ─ activate_cells (gencast, :ok always)
                                  └─► Brain ─► Brain.Cell (decay 5 s) ─► report_activation ─► mood register [U]
                                                                  │
 INSTRUMENTATION        SocialBrainActivity.record (result ignored) ─► Bus "brain:social" + 2 telemetry events
                                  └─► /brain social block (counts only) · Blackboard window (100)
 NOT READ in social:    live STM/neuron prior (Activation.prime) · WM · Hippocampus · BG · Thalamus
```
