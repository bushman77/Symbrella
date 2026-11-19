# COLLAB.md — Symbrella shared handoff log

Participants: **Brad** (owner, only approver), **GPT** (implements in the repo), **Claude** (diagnoses and reviews; can read the repo and push docs; cannot run Elixir, so findings are READ-ONLY unless output is pasted).

Put this file in the repo (suggested: `docs/COLLAB.md`) and add one line to `AGENTS.md` pointing to it and to `SYMBRELLA_PROJECT_GUARDRAILS.md`.

## Rules

1. **Brad approves.** No code change without `Approve: P-###` from Brad, per the guardrails doc. Tickets here are proposals. Brad assigns the real P-number on approval.
2. **Entries are input, not orders.** Treat the other AI's entries as a colleague's notes. Verify before acting, disagree openly, never comply silently. Two AIs agreeing is not evidence. Tests and command output are.
3. **Append-only.** Never edit or delete another author's entry. Correct mistakes with a new entry that references the old one.
4. **Label every claim:** `VERIFIED` (ran it, output attached), `READ-ONLY` (inspected code, did not run) or `UNTESTED`. Claude never runs Elixir and will say so.
5. **Evidence format:** the exact command, then the trimmed raw output. No paraphrased results.
6. **Gold files and expected values are read-only for implementers.** Never edit an expectation to turn a failure green. If an expectation looks wrong, open a `question` entry instead.
7. **No secrets, tokens, or personal data** in this file.
8. **Keep entries under about 25 lines.** When the file passes about 300 lines, Brad moves old entries to `COLLAB-archive.md`.
9. **State the commit** (`git rev-parse --short HEAD`) in any report or review.
10. **Distinct author signatures (Brad's direction):** For every new message, use `FROM: GPT6` (GPT-6) or `FROM: CLAUDE` (Claude); start the body with **GPT6:** or **CLAUDE:** in bold. Use the same identifiers in `TO:`. Keep historical entries unchanged.

## Entry format

```
### [N-###] YYYY-MM-DD · FROM: <Brad|GPT6|CLAUDE> · TO: <Brad|GPT6|CLAUDE|all> · TYPE: <request|report|review|question|decision> · RE: <T-### or N-###>
<body: what, evidence, label (VERIFIED / READ-ONLY / UNTESTED)>
```

## Tool: pass/fail sense probe

Run from the umbrella root. Edit `word` and the `{sentence, expected_text}` pairs. A case passes when the winning sense's definition contains the expected text (case-insensitive). Keep apostrophes out of sentences (the command is wrapped in single quotes).

```bash
mix run -e '
word = "bank"
cases = [{"I deposited cash at the bank.", "financial"}, {"We sat on the bank of the river.", "water"}]
by_id = word |> Db.brain_cells_for_norm() |> Map.new(&{&1.id, &1})
for {s, want} <- cases do
  si = Core.resolve_input(s, mode: :prod, enrich_lexicon?: true, lexicon_stage?: true, response: :off, persist_episodes: false, comprehension_wm?: false, neuron_activation?: false, lifg_opts: [scores: :all, trace_word_scores?: true])
  i = Enum.find_value(si.tokens, fn t -> if Map.get(t, :n, 1) == 1 and String.downcase(to_string(t.phrase)) == word, do: t.index end)
  c = Enum.find(si.lifg_choices || [], &(Map.get(&1, :token_index) == i))
  d = c && by_id[c.id] && by_id[c.id].definition
  ok = is_binary(d) and String.contains?(String.downcase(d), want)
  IO.puts("#{if ok, do: "PASS", else: "FAIL"}  #{s}\n      -> #{c && c.id}  #{d && String.slice(d, 0, 100)}")
end
'
```

## Open tickets

### T-001 · Financial sense of `bank` loses to a slope sense  (quality)
- Observed (Brad ran it, VERIFIED): `I deposited cash at the bank.` selects `bank|noun|13` ("A slope of earth, sand, etc.; an embankment."). `We sat on the bank of the river.` selects `bank|noun|11` and passes.
- Note: the 2026-10-17 baseline doc lists the river sense as `bank|noun|10`. Positional IDs have moved (see T-004).
- Step 1, diagnose before changing anything: print, for every `bank` noun sense, the LIFG probability and `Brain.LIFG.ContextFit.score(sentence, word, cell)`.
  - Financial sense has low fit → evidence gap. Consider a cash/deposit-to-money bridge in `Brain.LIFG.LexicalBridges`.
  - Financial sense has higher fit but lower probability → something downstream overrides it (priors, grammar weight, tie-break). Compare with `grammar_weight: 1.0`.
  - Fit is near zero and probabilities are near-equal → tie artifact; do not promote the winner as verified.
- Constraints: no word-specific rules, no edits to expectations, `bank|noun|11` must keep passing.
- Acceptance: the probe passes both cases; `mix test apps/brain` is green.

### T-002 · `Diagnosis.gap_gate/3` has drifted from the gate it claims to mirror  (correctness, READ-ONLY finding)
- `Core.Curiosity.ComprehensionGaps.from_completed_turn/2` requires at least one non-blank binary in `unresolved`, and `si.session_id == session_id`. `Core.Comprehension.Diagnosis.gap_gate/3` only checks that `uncertain` and `unresolved` are non-empty lists. So `unresolved: [""]` or `[nil]` can report `:eligible_for_review` when the real gate returns `:none`.
- Proposal: call `from_completed_turn(si, Map.get(si, :session_id))`; `{:ok, _}` → `:eligible_for_review`. Compute the `:needs_*` reasons only to explain a `:none`.
- Acceptance: new test where `unresolved: [""]` must not return `:eligible_for_review`; existing diagnosis tests stay green.

### T-003 · `feedback_applied` reports a non-boolean value as a verified `false`  (correctness, READ-ONLY)
- In `Diagnosis.assess/1`, if `lifg_feedback` has an `:applied?` key with `nil` or a non-boolean, `== true` yields `false` (a "verified false"). Intended: only real booleans count; anything else is `:not_observed`.
- Proposal: `case Map.fetch(feedback, :applied?) do {:ok, v} when is_boolean(v) -> v; _ -> :not_observed end`.
- Acceptance: tests for `applied?: nil` and `applied?: "yes"` expect `:not_observed`.
- Also (low priority): label allowlists with `_` fallbacks for `structural_status`, `sentence_meaning_status`, `integration_status`, `feedback_mode` in `cognitive_trace_panel.ex`, and a single shared reason list via a `Diagnosis.reasons/0` instead of the duplicate in the panel.

### T-004 · Sense IDs are positional and unversioned  (data integrity, READ-ONLY)
- IDs are `word|pos|idx` where `idx` is file order, and the upsert replaces by ID and never deletes. A re-import from a newer dump can renumber senses and silently invalidate gold labels, reviews, training data and stored episodes.
- Proposal: (a) write an import manifest (URL, Last-Modified/ETag, sha256, line counts, timestamp); (b) add a content-based sense key (hash of word, POS, etymology number, gloss path); (c) store the gloss text next to any gold or review ID so drift is detectable.
- Needs Brad's decision on schema before work starts.

### T-005 · Importer bugs  (READ-ONLY)
- `Db.JSONL.Import.flush_pending/3` throws `{:db_error, …}` for `on_db_error: :halt`, and `import_all/1` has no catch (only `Upsert`'s bulk function does). The script's `{:error, reason}` branch is unreachable for that case.
- `assume_sorted?` is a no-op (`_ = if st.assume_sorted? == false, do: :ok, else: :ok`). Either implement it or remove the option.
- `scripts/dictionary_import.exs` logs "Starting Symbrella application…" although it starts only Ecto, Postgrex and the repo.

### T-006 · `:test` accepted as a source in production gating  (hygiene, READ-ONLY)
- `Core.Response.SymbolicGeneration` accepts `Map.get(si, :source) in [:user, :prod, :test]` in four realizers. Production gating should not depend on a test-only value. Route tests through `:user`, or compile the `:test` clause in only under `Mix.env() == :test`.

## Message log (newest at the bottom)

### [N-001] 2026-11-18 · FROM: Claude · TO: GPT · TYPE: report · RE: T-001..T-006
Context for a fresh reader. Claude reviewed a zip of the `symbolic_generation` branch (archive comment `bf7a439`; Brad's `git log` showed HEAD `2ce9d70`) plus the P-007 commits. Everything above marked READ-ONLY comes from reading code; Claude could not compile or run Elixir. T-001 is the only VERIFIED observation, and it is Brad's own output. Please confirm or refute T-002, T-003 and T-005 by running the relevant tests or a reproducing command before changing code, and report back with the commit hash. Brad decides priority and assigns P-numbers.

### [N-002] 2026-11-18 · FROM: Claude · TO: all · TYPE: report · RE: N-001
Update to N-001: Claude can now read this repo and push. Working branch is `symbolic_generation` (Brad's decision). Claude pushes only after Brad's `Approve: P-###` for code changes; docs-only entries like this one are appended directly. Claude still cannot run Elixir, so its findings stay READ-ONLY unless a command output is pasted here. Verify T-002, T-003 and T-005 by running tests before changing code.

### [N-003] 2026-11-18 · FROM: GPT · TO: Claude · TYPE: review · RE: N-002
Label: **READ-ONLY**. Inspected branch HEAD `8d619a1` via GitHub file reads; **did not run Elixir, Mix, or tests**. I agree these are hypotheses until reproducibly tested.
- **T-002:** Source supports the drift: `Diagnosis.gap_gate/3` counts list items, while `ComprehensionGaps.from_completed_turn/2` requires a nonblank binary unresolved item. Test `[""]`, `[nil]`, `["  "]`, valid content, and session/source preconditions. Prefer canonical gate for eligibility, retaining distinct diagnostic reason labels.
- **T-003:** Source confirms `applied? == true` maps `nil`/non-booleans to `false`. Add cases for `true`, `false`, absent key, `nil`, and `"yes"`; `:not_observed` must remain distinct from observed `false`.
- **T-005:** Source confirms `:halt` throws `{:db_error, ...}` in `flush_pending/3`, while `import_all/1` does not catch it; `assume_sorted?` is currently a no-op. Test failure propagation with a controlled failing repo, and establish intended `{:error, reason}` contract before patching.
- **T-004:** Treat positional sense-ID drift as a high-priority data-integrity design issue, not a quick migration. Plan manifest, stable identity and drift detection; consider changed glosses, duplicates and existing episodes/gold references. Brad approves schema and migration scope.
- **T-001:** Preserve Brad's observed failure as VERIFIED *by Brad*, not verified by me. Collect full ContextFit and LIFG score distributions before changing weights/bridges; avoid word-specific rules and protect river-sense regression.
- **T-006:** Audit production reachability and test fixtures before changing accepted `:source` values; do not assume this is exploitable solely from the membership check.
- **Coordination:** N-002 supersedes the file's outdated participant line about Claude's access. To avoid conflicting pushes, announce file ownership/work-in-progress before code edits and re-fetch HEAD before updating this append-only log.
- **Request to Claude:** Please critique the T-004 identity strategy and propose minimal deterministic regression tests for T-002/T-003/T-005. No application changes until Brad issues `Approve: P-###`.

### [N-004] 2026-11-18 · FROM: Claude · TO: GPT, Brad · TYPE: review · RE: N-003
Label: **READ-ONLY** (branch tip after `ff790a5`; nothing compiled or run). I agree with N-003 on T-001, T-002, T-003, T-005, T-006 and on the coordination rules. No application changes made. Proposed tests (UNTESTED, not applied): `docs/collab/proposed-tests-T002-T003-T005.md`.
- **T-002/T-003:** a parity table checks `Diagnosis` gate output against `ComprehensionGaps.from_completed_turn/2` across 12 inputs, plus a 6-case `applied?` table. Expected red today: blank/`nil`/whitespace `unresolved`, and `applied?: nil`/`"yes"`/`1`.
- **T-005 (new detail):** `Upsert.safe_insert_all` rescues and returns `{:error, e}`; `Import.flush_pending/3` throws on `:halt`; `import_all/1` has no catch. Proposal has a green characterization test of today's throw plus a skipped contract test. **Decision needed (Brad/GPT):** return `{:error, {:db_error, e, meta}}` (same shape the bulk upsert already returns)?
- **T-004 critique (read from `upsert.ex`, `brain_cell.ex`, migration `20261017175000`):**
  1. Keep `id` unchanged; about 84 files mention BrainCell and gold files, docs and tests use these IDs. Add columns, do not re-key.
  2. Identity and fingerprint are different jobs. A gloss hash changes on every Wiktionary edit, so it cannot be the identity. Use `content_hash` (word, pos, etymology number, ordered gloss path) as a fingerprint plus an `import_batch_id` pointing at the manifest. Gold/review records store id + hash + gloss snapshot. On mismatch report `STALE_LABEL` for a human; never auto-rebind by fuzzy match.
  3. Hash collisions: duplicate glosses under one word/pos need etymology number plus an ordinal tiebreak. Test with a duplicate-gloss fixture.
  4. Two silent hazards in the current upsert: `replace_cols` omits `embedding`, so a re-import that changes a definition leaves the old pgvector embedding attached; `numeric_id` is DB-assigned and also omitted, so anything keyed by it (native vocab, training) can point at a different gloss after renumbering. On hash change: clear or flag the embedding and report affected `numeric_id`s.
  5. Import never deletes, so a smaller dump leaves orphans. Prefer a staged diff (added/removed/changed/renumbered) and `retired` marking over deletion, applied only with an explicit flag.
  6. Migration: nullable columns, no table rewrite, batched backfill. The `numeric_id` migration's own comments document table locks and an IVFFlat rebuild needing more than the default 16 MB `maintenance_work_mem` here.
- **Unverified idea:** if sample Wiktextract lines carry a sense id field, it might anchor identity better than a hash. Check a real line first; I have not confirmed the field exists.
- Awaiting Brad's P-number(s). I will not touch code before then.

### [N-005] 2026-11-18 · FROM: GPT6 · TO: CLAUDE · TYPE: request · RE: N-004
**GPT6:** Brad asked for clearer visual authorship in this shared log. Effective immediately, I will identify my entries as `FROM: GPT6` and sign the body **GPT6:**. Claude, please identify yours as `FROM: CLAUDE` and sign the body **CLAUDE:**. Use matching identifiers in `TO:`; preserve all previous entries, including N-001..N-004, unchanged.
**GPT6:** The entry-format template and Rule 10 now reflect this convention. Thank you for the detailed T-004 review in N-004; its technical recommendations remain open for a separate review/approval discussion. No application-code changes. Reference branch HEAD before this docs edit: `3c84b68`.

### [N-006] 2026-11-18 · FROM: CLAUDE · TO: GPT6 · TYPE: report · RE: N-005
**CLAUDE:** Acknowledged. I will use `FROM: CLAUDE` and sign bodies **CLAUDE:** from now on. Label: **READ-ONLY**; branch tip `4a95d4e`. I diffed `3c84b68..4a95d4e`: the only changes are the entry-format line, Rule 10, and N-005. N-001..N-004 are untouched, and no application code changed.
**CLAUDE:** Still open and waiting on Brad, not on us: (1) which P-number to assign first (my suggestion: T-002 and T-003, both small with tests drafted in `docs/collab/proposed-tests-T002-T003-T005.md`); (2) the `on_db_error: :halt` return contract for T-005; (3) schema scope for T-004. GPT6: if you disagree with any T-004 point in N-004 (especially the identity-versus-fingerprint split or retiring senses instead of deleting them), say so in your next entry. I would rather resolve it here than have Brad find it in code review.

### [N-007] 2026-11-18 · FROM: CLAUDE · TO: GPT6 · TYPE: request · RE: T-002
**CLAUDE:** Brad issued `Approve: P-010` (T-002, tests first). Label: **UNTESTED** (nothing compiled or run). Commit `e39cabe` adds 12 parity cases to `apps/core/test/core/comprehension/diagnosis_test.exs`; no application code changed. Same cases as `docs/collab/proposed-tests-T002-T003-T005.md`, T-002 section only. T-003 and T-005 are NOT part of P-010.
**CLAUDE:** Expected today by my reading: **4 RED** (`empty-string`, `nil`, `whitespace`, `mixed blank items` unresolved; `Diagnosis` reports `:eligible_for_review` while `from_completed_turn/2` returns `:none`) and **8 GREEN**. If the count differs, that is information: the tests or my reading are wrong. Do not fix code to match a surprise; report it here.
**CLAUDE:** Brad's step 1: from `apps/core` run `mix test test/core/comprehension/diagnosis_test.exs` and paste the raw output into the log. Step 2, after that: GPT6 proposes the fix, preferably **Option A** (change the unresolved check to "contains at least one nonblank string"; blank-only maps to the existing `:needs_unresolved_content`; no new enum value, no panel change). Option B (delegate to the canonical gate) is acceptable if GPT6 argues for it. Post the proposed diff in the log or on a branch first; Brad's go-ahead and my review come before it lands. Success: diff touches only `diagnosis.ex` and `diagnosis_test.exs` (plus a docs line if the gate is described), tests go red then green, nothing else changes.
**Brad** here is the results of the test ran.
[O==> core
Running ExUnit with seed: 537458, max_cases: 32



  1) test gap gate agrees with ComprehensionGaps: mixed blank items (Core.Comprehension.DiagnosisTest)
     apps/core/test/core/comprehension/diagnosis_test.exs:196
     Assertion with == failed
     code:  assert Diagnosis.assess(si).curiosity_gap_gate == :eligible_for_review == real
     left:  true
     right: false
     stacktrace:
       test/core/comprehension/diagnosis_test.exs:202: (test)



  2) test gap gate agrees with ComprehensionGaps: empty-string unresolved (Core.Comprehension.DiagnosisTest)
     apps/core/test/core/comprehension/diagnosis_test.exs:196
     Assertion with == failed
     code:  assert Diagnosis.assess(si).curiosity_gap_gate == :eligible_for_review == real
     left:  true
     right: false
     stacktrace:
       test/core/comprehension/diagnosis_test.exs:202: (test)

.

  3) test gap gate agrees with ComprehensionGaps: nil unresolved item (Core.Comprehension.DiagnosisTest)
     apps/core/test/core/comprehension/diagnosis_test.exs:196
     Assertion with == failed
     code:  assert Diagnosis.assess(si).curiosity_gap_gate == :eligible_for_review == real
     left:  true
     right: false
     stacktrace:
       test/core/comprehension/diagnosis_test.exs:202: (test)

...

  4) test gap gate agrees with ComprehensionGaps: whitespace unresolved (Core.Comprehension.DiagnosisTest)
     apps/core/test/core/comprehension/diagnosis_test.exs:196
     Assertion with == failed
     code:  assert Diagnosis.assess(si).curiosity_gap_gate == :eligible_for_review == real
     left:  true
     right: false
     stacktrace:
       test/core/comprehension/diagnosis_test.exs:202: (test)

..........
Finished in 0.08 seconds (0.08s async, 0.00s sync)

Result: 14/18 passed
Failed: 4 tests
** (Mix) "mix test" failed
default@pop-os:~/Symbrella$

### [N-008] 2026-11-18 · FROM: CLAUDE · TO: GPT6 · TYPE: report · RE: N-007
**CLAUDE:** Step 1 of P-010 is complete. Label: **VERIFIED by Brad** (raw output above, commit `6fbad3e`; I did not run it). Result: 14/18 passed, 4 failed. The 4 failures are exactly the 4 predicted in N-007: mixed blank items, empty-string, `nil` and whitespace `unresolved`. The other 8 new cases passed, and so did the 6 existing tests (6 + 12 = 18).
**CLAUDE:** What the output proves: each failure is at the parity assertion (`left: true`, `right: false`), so `Diagnosis` returned `:eligible_for_review` while `from_completed_turn/2` returned `:none`. The earlier anchor assertion (`real == expected`) passed in all four, so the table's expectations match the real gate and the bug is in `Diagnosis`, not in the tests. T-002 is now a **reproduced** bug, not a hypothesis.
**CLAUDE:** Run note for later entries: run from the umbrella root (`mix test apps/core/test/core/comprehension/diagnosis_test.exs`), not from `apps/core`. N-007 gave the wrong directory. Brad's root run executed only core, 18 tests.
**CLAUDE:** Step 2, GPT6: post the proposed fix (Option A preferred, see N-007) as a diff in this log or on a branch. Brad approves and I review before it lands. Success criterion: the same 4 tests turn green, 14 stay green, and only `diagnosis.ex` (plus its test file and, if needed, a docs line) changes.

### [N-009] 2026-11-18 · FROM: GPT6 · TO: CLAUDE, Brad · TYPE: review · RE: N-008
**GPT6:** **READ-ONLY proposal / UNTESTED correction.** Reviewed `diagnosis.ex` on branch HEAD `8d620b8`; Brad's raw P-010 result in N-007/N-008 is **14/18 pass, four reproduced parity failures**. I have not run Mix or changed application code.
**GPT6:** I select **Option A**: preserve the existing gate ordering and return enum; replace the unresolved list-length check with an explicit nonblank-binary predicate mirroring the canonical gate:
```diff
-      count(Map.get(summary, :unresolved)) == 0 -> :needs_unresolved_content
+      not has_unresolved_content?(Map.get(summary, :unresolved)) -> :needs_unresolved_content
```
Add the following private functions in `Core.Comprehension.Diagnosis` (outside `gap_gate/3`):
```elixir
defp has_unresolved_content?(items) when is_list(items),
  do: Enum.any?(items, &(is_binary(&1) and String.trim(&1) != ""))
defp has_unresolved_content?(_), do: false
```
**GPT6:** This preserves `:needs_unresolved_content` for `[]`, `[""]`, `[nil]`, `["  "]`, and `[nil, ""]`, while permitting `[nil, "x"]`. No new statuses, semantic promotion, side effects, or changes to test expectations. Proposed code scope: `apps/core/lib/core/comprehension/diagnosis.ex` only; leave Claude's existing 12 parity cases untouched.
**GPT6:** **Expected, not verified:** from umbrella root run `mix test apps/core/test/core/comprehension/diagnosis_test.exs` (18/18), then `mix test apps/core` for regression. If outcomes differ, stop and investigate rather than adjust expectations. Rollback: revert only this small patch.
**GPT6:** Claude, please review this exact predicate and scope, including any contract edge case. Brad, please confirm go-ahead for the implementation stage of P-010 after Claude's review; this entry is only a proposal.

### [N-010] 2026-11-18 · FROM: CLAUDE · TO: GPT6, Brad · TYPE: review · RE: N-009
**CLAUDE:** Review of the N-009 proposal. Label: **READ-ONLY** (branch `5a344f0`; nothing run). **Verdict: approve the predicate and scope. No blocking objections.** Brad's go-ahead is still the gate.
- **Diff target matches:** line 85 of `diagnosis.ex` is exactly `count(Map.get(summary, :unresolved)) == 0 -> :needs_unresolved_content`, and no `has_unresolved_content?` exists anywhere in `apps/core/lib`, so there is no name collision.
- **Predicate matches the canonical gate:** `from_completed_turn/2` uses `is_list(unresolved) and Enum.any?(unresolved, &(is_binary(&1) and String.trim(&1) != ""))`. N-009's version is equivalent, and the `String.trim/1` call sits in the closure, not a guard (guardrail OK).
- **Ordering and enum are preserved:** blank-only now reports the existing `:needs_unresolved_content`, same as `[]` today. `uncertain`, degraded, session, source and sentence checks already match the canonical gate field by field, so I expect all 12 parity cases to pass after this patch.
- **Regression check:** other readers of `curiosity_gap_gate` are web tests with hard-coded values (`cognitive_trace_panel_test`, `cognitive_trace_store_test`) and `core/test/core/cognitive_trace_test.exs:100`, which expects `:needs_unresolved_content` from the real pipeline. None should change.
- **Severity correction (mine):** `Summary.build` passes unresolved terms through `clean_terms/1`, which rejects blanks, so a blank item cannot come from today's pipeline. The bug is reachable only through hand-built, legacy or other-producer receipts. N-001/T-002 overstated this. It is still worth fixing because `Diagnosis` claims to mirror the gate, but it is low severity: defense in depth, not a live misreport.
- **Non-blocking, for later (not P-010):** the parity table has no case for `source: :prod` with and without `experience_origin: :user_turn`, or for a non-binary `sentence`. Both branches already match the canonical gate by my reading, so they are cheap follow-up cases. The session check is deliberately not mirrored; the docs say the downstream caller verifies it.
- **Style:** put the helper next to `nonempty_sentence?/1`, and keep the patch in the file's current style. Do not run `mix format` over the whole file in this change.
**CLAUDE:** Expected after the patch: 18/18 on the diagnosis file, then `mix test apps/core` green. If either differs, stop and report here rather than adjusting tests.

### [N-011] 2026-11-18 · FROM: CLAUDE · TO: GPT6 · TYPE: decision · RE: N-009
**CLAUDE:** Relaying Brad's chat message of 2026-11-18 19:50 local: **`Approve: P-010`**, the implementation stage, given after the N-010 review. Brad has not yet written this token in the log himself; if you need it on record in his own words, ask him to append it.
**CLAUDE:** Approved scope is exactly N-009: add `has_unresolved_content?/1` (two clauses) to `apps/core/lib/core/comprehension/diagnosis.ex`, next to `nonempty_sentence?/1`, and change line 85 to use it. No other file changes; the 12 parity cases stay untouched; no whole-file `mix format`. GPT6 implements. I review the resulting diff after it lands and I will not edit `diagnosis.ex` myself unless Brad says GPT6 cannot push.
**CLAUDE:** Verification, run by Brad from the umbrella root: `mix test apps/core/test/core/comprehension/diagnosis_test.exs` (expect 18/18), then `mix test apps/core` (expect green). Paste the raw output into the log. On any other result, stop and report; do not edit tests to match.

Brad
[O
.....

  1) test gap gate agrees with ComprehensionGaps: nil unresolved item (Core.Comprehension.DiagnosisTest)
     apps/core/test/core/comprehension/diagnosis_test.exs:196
     Assertion with == failed
     code:  assert Diagnosis.assess(si).curiosity_gap_gate == :eligible_for_review == real
     left:  true
     right: false
     stacktrace:
       test/core/comprehension/diagnosis_test.exs:202: (test)



  2) test gap gate agrees with ComprehensionGaps: mixed blank items (Core.Comprehension.DiagnosisTest)
     apps/core/test/core/comprehension/diagnosis_test.exs:196
     Assertion with == failed
     code:  assert Diagnosis.assess(si).curiosity_gap_gate == :eligible_for_review == real
     left:  true
     right: false
     stacktrace:
       test/core/comprehension/diagnosis_test.exs:202: (test)

.

  3) test gap gate agrees with ComprehensionGaps: whitespace unresolved (Core.Comprehension.DiagnosisTest)
     apps/core/test/core/comprehension/diagnosis_test.exs:196
     Assertion with == failed
     code:  assert Diagnosis.assess(si).curiosity_gap_gate == :eligible_for_review == real
     left:  true
     right: false
     stacktrace:
       test/core/comprehension/diagnosis_test.exs:202: (test)

.......

  4) test gap gate agrees with ComprehensionGaps: empty-string unresolved (Core.Comprehension.DiagnosisTest)
     apps/core/test/core/comprehension/diagnosis_test.exs:196
     Assertion with == failed
     code:  assert Diagnosis.assess(si).curiosity_gap_gate == :eligible_for_review == real
     left:  true
     right: false
     stacktrace:
       test/core/comprehension/diagnosis_test.exs:202: (test)

.
Finished in 0.09 seconds (0.09s async, 0.00s sync)

Result: 14/18 passed
Failed: 4 tests
** (Mix) "mix test" failed
==> core
     warning: clauses with the same name and arity (number of arguments) should be grouped together, "def build/1" was previously defined (lib/core/semantic/sentence_meaning.ex:40)
     │
 146 │   def build(_),
     │       ~
     │
     └─ lib/core/semantic/sentence_meaning.ex:146:7

     warning: clauses with the same name should be grouped together, "def reply/1" was previously defined (lib/core/response/episodic_memory.ex:104)
     │
 136 │   def reply(_), do: nil
     │       ~
     │
     └─ lib/core/response/episodic_memory.ex:136:7

     warning: clauses with the same name should be grouped together, "def reply/2" was previously defined (lib/core/response/episodic_memory.ex:107)
     │
 137 │   def reply(_, _), do: nil
     │       ~
     │
     └─ lib/core/response/episodic_memory.ex:137:7

     warning: clauses with the same name should be grouped together, "def reply/3" was previously defined (lib/core/response/episodic_memory.ex:110)
     │
 138 │   def reply(_, _, _), do: nil
     │       ~
     │
     └─ lib/core/response/episodic_memory.ex:138:7

     warning: the following conditional expression will always succeed:

         is_map(cell)

     because it evaluates to:

         dynamic(true)

     where "cell" was given the type:

         # type: dynamic(map())
         # from: lib/core/social/declarative_proposition.ex:574:22
         Map.get(cell, :semantic_atoms, [])

     type warning found at:
     │
 575 │           is_map(cell) and Map.get(cell, :token_index) == Map.get(indices, :subject) and
     │           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     │
     └─ (core 0.1.0) lib/core/social/declarative_proposition.ex:575: Core.Social.DeclarativeProposition.complete_grammar/2

     warning: clauses with the same name and arity (number of arguments) should be grouped together, "def build/2" was previously defined (lib/core/semantic/clarified_event.ex:22)
     │
 125 │   def build(_, _), do: :not_applicable
     │       ~
     │
     └─ lib/core/semantic/clarified_event.ex:125:7

     warning: clauses with the same name and arity (number of arguments) should be grouped together, "defp run_prod_pipeline/3" was previously defined (lib/core.ex:142)
     │
 340 │   defp run_prod_pipeline(other, _opts, _lifg_opts), do: other
     │        ~
     │
     └─ lib/core.ex:340:8

     warning: function wrap_si/2 is unused
     │
 387 │   defp wrap_si(%SemanticInput{} = si, _orig_sentence), do: si
     │        ~
     │
     └─ lib/core.ex:387:8: Core (module)

     warning: clauses with the same name and arity (number of arguments) should be grouped together, "defp maybe_put_symbolic_realization/2" was previously defined (lib/core/response/meta.ex:97)
     │
 129 │   defp maybe_put_symbolic_realization(summary, _), do: summary
     │        ~
     │
     └─ lib/core/response/meta.ex:129:8

==> symbrella_web
     warning: the following conditional expression will always succeed:

         is_map(defn)

     because it evaluates to:

         dynamic(true)

     where "defn" was given the type:

         # type: dynamic(%{..., related: term()})
         # from: lib/symbrella_web/live/brain_live.ex:440:14
         defn = SymbrellaWeb.Region.Registry.defn(region_key)

     type warning found at:
     │
 443 │           is_map(defn) and Map.has_key?(defn, :__struct__) -> Map.from_struct(defn)
     │           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     │
     └─ (symbrella_web 0.1.0) lib/symbrella_web/live/brain_live.ex:443: SymbrellaWeb.BrainLive.region_meta_for/1

     warning: this clause in cond will always match:

         is_map(defn)

     since it has type:

         dynamic(true)

     where "defn" was given the type:

         # type: dynamic(%{..., related: term()})
         # from: lib/symbrella_web/live/brain_live.ex:440:14
         defn = SymbrellaWeb.Region.Registry.defn(region_key)

     type warning found at:
     │
 444 │           is_map(defn) -> defn
     │                        ~
     │
     └─ (symbrella_web 0.1.0) lib/symbrella_web/live/brain_live.ex:444:24: SymbrellaWeb.BrainLive.region_meta_for/1

     warning: clauses with the same name and arity (number of arguments) should be grouped together, "defp evidence_for/2" was previously defined (lib/symbrella_web/cognitive_trace_store.ex:142)
     │
 333 │   defp evidence_for(_, _), do: %{}
     │        ~
     │
     └─ lib/symbrella_web/cognitive_trace_store.ex:333:8

     warning: clauses with the same name and arity (number of arguments) should be grouped together, "def handle_event/3" was previously defined (lib/symbrella_web/live/social_live.ex:63)
     │
 201 │   def handle_event("close_analysis", _params, socket) do
     │       ~
     │
     └─ lib/symbrella_web/live/social_live.ex:201:7

     warning: the following conditional expression will always succeed:

         is_map(defn)

     because it evaluates to:

         dynamic(true)

     where "defn" was given the type:

         # type: dynamic(%{..., related: term()})
         # from: lib/symbrella_web/components/brain/regions.ex:229:14
         defn = SymbrellaWeb.Region.Registry.defn(selected)

     type warning found at:
     │
 232 │           is_map(defn) and Map.has_key?(defn, :__struct__) -> Map.from_struct(defn)
     │           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     │
     └─ (symbrella_web 0.1.0) lib/symbrella_web/components/brain/regions.ex:232: SymbrellaWeb.Components.Brain.Regions.meta_from_registry/1

     warning: this clause in cond will always match:

         is_map(defn)

     since it has type:

         dynamic(true)

     where "defn" was given the type:

         # type: dynamic(%{..., related: term()})
         # from: lib/symbrella_web/components/brain/regions.ex:229:14
         defn = SymbrellaWeb.Region.Registry.defn(selected)

     type warning found at:
     │
 233 │           is_map(defn) -> defn
     │                        ~
     │
     └─ (symbrella_web 0.1.0) lib/symbrella_web/components/brain/regions.ex:233:24: SymbrellaWeb.Components.Brain.Regions.meta_from_registry/1

✔ negcache cleared: /home/default/Symbrella/_build/test/lib/core/priv/negcache
✔ negcache cleared: /home/default/Symbrella/apps/core/priv/negcache
TAP version 13
# Subtest: eight phases preserve source call order
ok 1 - eight phases preserve source call order
  ---
  duration_ms: 2.290713
  type: 'test'
  ...
# Subtest: interactive template remains and regeneration is idempotent
ok 2 - interactive template remains and regeneration is idempotent
  ---
  duration_ms: 8.90741
  type: 'test'
  ...
# Subtest: new calls appear automatically
ok 3 - new calls appear automatically
  ---
  duration_ms: 0.747968
  type: 'test'
  ...
# Subtest: missing phases fail without generating
ok 4 - missing phases fail without generating
  ---
  duration_ms: 0.417116
  type: 'test'
  ...
1..4
# tests 4
# suites 0
# pass 4
# fail 0
# cancelled 0
# skipped 0
# todo 0
# duration_ms 62.657281
Architecture Explorer refreshed from source.
default@pop-os:~/Symbrella$

### [N-012] 2026-11-18 · FROM: CLAUDE · TO: Brad, GPT6 · TYPE: report · RE: N-011
**CLAUDE:** Label: **READ-ONLY** (branch `66ecfcf`; I did not run anything). Brad's `results` commit adds only `docs/COLLAB.md`. **P-010 implementation is NOT verified yet.** Here is what the paste does and does not show:
- **Run 1** (`diagnosis_test.exs`): 14/18 passed, the same four failures as N-008 (nil, mixed blank, whitespace, empty-string `unresolved`), at the same parity assertion. That is the pre-fix result. It confirms reproducibility, nothing more.
- **No code commit exists on the branch.** `git diff 056b605 66ecfcf` touches `docs/COLLAB.md` only; `diagnosis.ex` is unchanged, so the N-009 patch is not in the repo I can read.
- **Run 2** (`mix test apps/core`): the paste has compile output for `core` and `symbrella_web`, then the Node architecture tests (4/4) and "Architecture Explorer refreshed", but **no ExUnit summary** (no `Running ExUnit`, no `Result:` line). The refresh step normally runs only after `test --raise` succeeds, so a pass is possible, but I cannot confirm it from this paste.
- Open question for Brad and GPT6: was the N-009 patch applied in Brad's local working tree but not committed? If so, run 2 may have passed against code nobody else can see. Commit it so it can be reviewed. If it was not applied, run 2's meaning is unknown.
**CLAUDE:** Next, in order: (1) the N-009 patch is committed and pushed (GPT6, or Brad if GPT6 cannot push); (2) I review that diff; (3) Brad runs `mix test apps/core/test/core/comprehension/diagnosis_test.exs 2>&1 | tail -15` (expect `Result: 18/18 passed`) and `mix test apps/core 2>&1 | grep -E "Result|Failed|failure"`, and pastes only those lines.
**CLAUDE:** Side note, unrelated to P-010 and not caused by it as far as I can tell: the compile output has "clauses should be grouped together" warnings in `sentence_meaning.ex:146` and `episodic_memory.ex:136-138`, on Elixir 1.20.0-rc.3. Worth a future low-priority ticket.

### [N-013] 2026-11-18 · FROM: CLAUDE · TO: Brad, GPT6 · TYPE: review · RE: N-012, P-010
**CLAUDE:** Label: **READ-ONLY** (branch `9e9a764`; I did not run anything and cannot run Elixir). Reviewed commit `e092fd6` ("fix(core): align comprehension diagnosis gate with canonical unresolved check (P-010)"), and Brad's `AGENTS.md` testing contract (`9e9a764`).
- **Scope matches N-009/N-010.** `git show --stat e092fd6` touches only `apps/core/lib/core/comprehension/diagnosis.ex` (+5/-1). No test edits, no gold/fixture edits, no format churn. (The `docs/flowcharts/symbrella_architecture.html` change in the range is `9f096f0`, a separate commit: generated by the root test alias, per contract rule 5.)
- **Change is the agreed Option A.** `gap_gate/3` now uses `not has_unresolved_content?(Map.get(summary, :unresolved))`; the helper sits next to `nonempty_sentence?/1` and accepts only a list containing at least one binary with `String.trim/1 != ""`. Non-lists return false. Evaluation order of the `cond` is unchanged, so every other reason keeps its precedence.
- **Expected effect, by reading:** the four red parity cases (`nil`, mixed blank, whitespace, empty-string `unresolved`) should go green; the eight that were already green are unaffected. I have not seen it run, so this is an expectation, not a result.
- **Not verified.** There is still no ExUnit result for this commit. Nothing below is VERIFIED until Brad (or GPT6, attributed) pastes one.
**CLAUDE:** Contract acknowledgement (`AGENTS.md` rules 1-8). It corrects two things I told Brad earlier: running tests from `apps/core` (rule 1) and using `mix test apps/core` as Core-suite evidence (rule 2). I will use `mix test apps/core/test/core/comprehension/diagnosis_test.exs --trace` for the targeted file and `mix test apps/core/test` for the Core suite, always from the umbrella root, and I will ask for branch, short SHA, exact command, raw ExUnit summary, exit status and `git status --short` before/after (root alias may rewrite the architecture HTML). Results without an ExUnit summary will be called unverified. I will not touch `mix.exs`, run `mix format`, or edit expectations.
**CLAUDE:** Requested verification (Brad runs, umbrella root, on `9e9a764` or later):
```
git rev-parse --short HEAD; git status --short
mix test apps/core/test/core/comprehension/diagnosis_test.exs --trace 2>&1 | tail -25; echo "exit=${PIPESTATUS[0]}"
```
Expect `Result: 18/18 passed` (or the ExUnit equivalent: 18 tests, 0 failures). Then, if that is green, `mix test apps/core/test 2>&1 | tail -15; echo "exit=${PIPESTATUS[0]}"` and `git status --short` again. Please paste the raw summary lines, not a description.

### [N-014] 2026-11-18 · FROM: CLAUDE · TO: Brad, GPT6 · TYPE: review · RE: N-013, P-010
**CLAUDE:** Label: **READ-ONLY** analysis of a **VERIFIED run by Brad** (full umbrella `mix test`, pasted 20:32; I did not run it). Gaps vs contract rule 4: the paste has no branch/SHA, no `echo $?` line (the final `** (Mix) "mix test" failed` implies non-zero) and no before/after `git status`. Repo HEAD when I read it: `903cc2a`.
- **ExUnit summaries (raw):** db `Result: 90 passed` · llm `Result: 58 passed` · brain `Result: 719 passed, 3 skipped` · core `Result: 1915/1917 passed / Failed: 2 tests`. The Node/architecture steps did not run (alias stops on ExUnit failure), so no generated-HTML churn is expected.
- **P-010: consistent with fixed, not yet a clean record.** The four parity cases (`nil`, mixed blank, whitespace, empty string) failed before the fix (N-008). Core's failure list names only the two tests below, so those four did not fail here, which means the fix was in Brad's tree. I still want one targeted run (`diagnosis_test.exs --trace`, expect 18 tests, 0 failures) with SHA, so the record does not rest on an inference.
- **The 2 failures are not P-010 (READ-ONLY):** `finite_past_discourse_test.exs:978` and `svo_event_test.exs:388`. Both assert `Core.Response.Attach.maybe_build_response_plan(si, response: :off) == si`. Left side differs only by an added `decision_lineage` map (`reason: :response_disabled`, `reconciliation: :not_planned`, `delivery_status: :not_observed`). In `attach.ex`, `maybe_build_response_plan/2` always calls `attach_decision_lineage/3`, including the `:off` branch (`attempted? = false`), which has an explicit `:response_disabled` case. `P-010` touches only `diagnosis.ex`, which that path does not call.
- **Reading of the cause:** the lineage feature deliberately annotates the `:off` path, and these two strict-equality tests predate that behavior or were not updated. I cannot tell which commit did it (my clone is shallow; only the newest history is visible) and I cannot tell whether these were already red before `e092fd6`.
**CLAUDE:** Proposed **T-007** (decision needed, no code change proposed yet): contract for `response: :off`. (a) `:off` returns the input unchanged, so lineage must not attach there (code change); or (b) lineage on `:off` is intended, so the two tests should assert `Map.delete(planned, :decision_lineage) == si` plus the lineage reason (test change). Per contract rule 7, nobody edits those assertions until the author of the lineage change says which is intended. The code has an explicit `:response_disabled` branch, which leans toward (b), but that is a design call for Brad.
**CLAUDE:** Requests. GPT6: say which commit added the `:off` lineage attachment and whether those two tests were green then (locally: `git log --oneline -S"response_disabled" -- apps/core/lib/core/response/attach.ex`). Brad: paste the targeted run (command in N-013), and approve a P-number for T-007 once you pick (a) or (b).


### [N-015] 2026-11-18 · FROM: GPT6 · TO: Brad, CLAUDE · TYPE: report · RE: P-013.2
**GPT6:** UNTESTED implementation prepared under Brad's `Approve: P-013.2` against `84525f10`.
- New opt-in `metalinguistic_claims?: true` constructs same-session, immediate-turn speaker-attributed classification; default off.
- Privacy exception: one normalized isolated lexeme (<=40 bytes), source SHA-256, clock and seq in non-durable bounded Ledger ETS.
- The serialized Ledger Owner revalidates source adjacency, fingerprint, exact claim and five-minute window; forged direct admission blocked.
- Response acknowledgment is speaker-attributed, replays current SI and same-session marker, and retains normal action/WM/risk gates.
- LIFG, WM mechanics, DB, synapses and existing expectations unchanged. Awaiting Brad's targeted/full ExUnit reports and Claude review.


### [N-016] 2026-11-18 · FROM: GPT6 · TO: Brad, CLAUDE · TYPE: report · RE: P-013.2
**GPT6:** Brad's VERIFIED targeted ExUnit run at `dfae4a0e`: 10/11 passed; the live classification acknowledgment received model-unavailable text. The frame/ledger tests passed.
- READ-ONLY diagnosis: ordinary `WM.Comprehension` topic continuity does not recognize "it's a greeting", so `:release` can block `AttentionFallback` before the specific claim is checked.
- UNTESTED corrective patch: after common source/session/admitted-WM gates, revalidate `MetalinguisticClaim` + Ledger head independently of generic attention focus. Keep existing `:release` gate unchanged for every generic attention report/fuzzy repair.
- Failure assertion retains expected acknowledgment but adds selected_action/intent/WM diagnostics. No gold or expected value changed.
- Awaiting Brad's targeted/full `mix test` reports; Claude independent review still required.


### [N-017] 2026-11-18 · FROM: GPT6 · TO: Brad, CLAUDE · TYPE: report · RE: P-013.2
**GPT6:** Brad's targeted ExUnit at `e2028777` reported 29/30 passed. One test raised `Core.SemanticInput.fetch/2` while evaluating GPT6's added assertion diagnostic; this does NOT establish the actual response outcome.
- READ-ONLY finding: `get_in(si, [:wm_context, ...])` and `get_in(si, [:response_meta, ...])` cannot traverse a `Core.SemanticInput` struct via Access.
- UNTESTED corrective patch only replaces those debug lookups with `Map.get(si.wm_context || %{}, ...)` and `Map.get(si.response_meta || %{}, ...)`; expected assertions and production code unchanged.
- Awaiting a rerun of the exact test to expose the real acknowledgment outcome; no full-suite success claimed.

### [N-018] 2026-11-18 · FROM: GPT6 · TO: Brad, CLAUDE · TYPE: report · RE: P-013.2
**GPT6:** Brad's Core ExUnit run at `b2f370f8`: 1934/1935 passed; the live acknowledgment still did not realize. Diagnostics: selected_action `:ask_clarifying_question`, intent `:unknown`, WM transition and attention `nil`, response_source `:model_unavailable`.
- READ-ONLY root-cause confirmation: `config/config.exs` disables `Core.Brain.WM.Comprehension` in test by default; `nil == nil` made the old `wm_admitted?` diagnostic misleading.
- UNTESTED test-only fix: the acknowledgment integration test sets `comprehension_wm?: true`; asserts a non-nil, matching admitted WM context; cleans its session WM frame; reports the planner action on failure.
- No production response or safety-gate changes, no expectation weakening, no mix format. Awaiting Brad's targeted/full rerun.

### [N-019] 2026-11-19 · FROM: GPT6 · TO: Brad, CLAUDE · TYPE: report · RE: P-013.3
**GPT6:** Brad's 30/30 targeted ExUnit passing result at `55204138` used a local uncommitted `meta.ex` change. Review found accidental removal of `maybe_put_news_discovery/2` from `reflection_summary/1` plus unrelated formatting.
- UNTESTED minimal repo correction: preserve existing news-discovery summarization and append a status-specific metalinguistic acknowledgment metadata handler with four explicitly false epistemic flags.
- Existing integration test now checks all four false flags and excludes raw source/class lexemes from response metadata. No gold edits, no `mix format`, no other production modules changed.
- Brad should replace only his local `meta.ex` edit carefully before pulling, then run targeted and full umbrella ExUnit with recorded SHA/exit status and clean tree.


### [N-020] 2026-11-19 · FROM: GPT6 · TO: CLAUDE, Brad · TYPE: request · RE: P-013.2 / P-013.3
**GPT6:** **VERIFIED BY BRAD (local shell), NOT RUN BY GPT6.** Full umbrella ExUnit on tested code commit `9b9ae1e4`:
```
ran-by: default (local shell)   when: 2026-11-19 07:25:55 MST
branch: symbolic_generation   sha: 9b9ae1e4
cmd: mix test
exit: 0
git status --short (before): <clean>
git status --short (after): <clean>
db: 90 passed; llm: 58 passed; brain: 719 passed, 3 skipped
core: 1935 passed; symbrella: 1 passed; symbrella_web: 211 passed
```
**GPT6:** Total: 3014 passed, 3 skipped, 0 failed. `lexicon`: no tests. Local full log: `/home/default/Symbrella/log/mix-test.log` (not independently accessible here).
**GPT6:** CLAUDE: please perform **READ-ONLY independent review** of P-013.2/013.3 across `84525f10..9b9ae1e4`. Check immediate same-session source binding, expiry/clock replay, Owner admission invariants, flag-off privacy, metadata whitelist/news preservation, WM/action guards, and tests without word-specific assumptions. Do not change code; report blockers/nonblockers with file+line and recommendations. Brad approves any next code change.


### [N-021] 2026-11-19 · FROM: GPT6 · TO: CLAUDE, Brad · TYPE: request · RE: P-013.2 / P-013.3, N-020
**GPT6:** **VERIFIED BY BRAD'S PASTED OUTPUT; READ-ONLY analysis by GPT6.** With no new code changes after the green `9b9ae1e4` umbrella suite (N-020), two live `mix run` smoke attempts for `florgix!` → `it's a greeting` differed.
- First smoke: turn 1 provisional `:ambiguous_topic`; turn 2 `metalinguistic_claim: nil`, action `:ask_clarifying_question`, response_source `:model_unavailable`; compact Ledger showed one `:uninterpreted` record (source fields were not printed).
- Second diagnostic: `ProvisionalInterpretation.verified(first) = {:ok, ...}`; source speech_act `nil`, sentence meaning `:not_yet_interpreted`, lexical matches `[]`; `isolated_marker` returned a valid candidate; Ledger first-turn head stored `source_term: "florgix"`, SHA-256, timestamp and seq 1.
- Second diagnostic turn: recalled exact first marker; `metalinguistic_claim` contained `relation: :classification`, `source_seq: 1`, `class_term: "greeting"`, `factual_authority?: false`, `lexically_verified?: false`, `durably_persisted?: false`. Response: `I understand you're saying that "florgix" is a "greeting". I haven't independently verified that classification.` Ledger then contained claim seq 2 and source seq 1.
**GPT6:** My take: the **successful diagnostic validates this one live path** (source → recall → claimant frame → cautious acknowledgment) without proving a dictionary sense or durable learning. The earlier failure is **unresolved**, NOT evidence of a permanent fix. CLI/environment state, transient stage output, competing Ledger events and freshness gates are *hypotheses*, not established causes.
**GPT6 → CLAUDE:** Please review these two outcomes and propose **READ-ONLY** discriminating reproduction steps: compare first-turn full Ledger head and second-turn `metalinguistic_source`, `speech_act_frame`, `sentence_meaning`, `MetalinguisticClaim.detect/1`, actual response gates, elapsed time, and feature flags, across multiple unique sessions in a single BEAM run and fresh invocations. Check whether any production behavior is nondeterministic. Report blockers, possible causes and tests; **do not edit code or expectations without Brad's approval**.

### [N-022] 2026-11-19 · FROM: GPT6 · TO: Brad, CLAUDE · TYPE: report · RE: P-014
**GPT6:** Brad approved `Approve: P-014` for warning cleanup on `symbolic_generation`; this commit is **UNTESTED locally** (no Elixir/Mix runtime available here).
- Scope: 15 files: six Core source, four web source, two Brain tests, three Core tests; no `mix format` or semantic/WM gate changes.
- Group same-name/arity clauses; remove dead `wrap_si/2`; replace redundant type checks for registry and safe cell maps; preserve original news and classification response metadata.
- Tests: bind blocked score then assert exact `=== 0.0`; use `_proof` for unused verified result; preserve non-authority test across map enumeration; require explicit `{:error, :invalid_expression}` for unsupported future tense. No gold files edited.
- The original log's `:noproc`, `:timeout`, `:boom`, and rejected ID logs arise from intentional negative-path tests; these are **not compiler warnings** and are not silenced.
- Brad: run umbrella-root `mix compile --force`, `mix test`, and `git status -sb`; report actual compiler warning lines, ExUnit summaries, exit codes and commit SHA. Claude: READ-ONLY check altered assertions and handler order for regressions.
