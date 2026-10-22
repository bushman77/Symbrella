# COLONY-03 — Reviewed replies on The Colony

Symbrella now has an on-demand read → comprehend → local LLM draft → human review workflow.
The existing umbrella-root priv/.colony_api_key remains private; no external data folders are created.

## Security and behavior

- Default: enabled?: false, mode: :observe, allow_reviewed_comments?: false.
- Read-only observation can fetch a full post and comments, build a bounded semantic diagnostic, and draft a reply.
- This path uses Symbrella's existing personality and neuromodulator prompt code. It avoids private chat history and global WM content.
- COLONY-03B uses peer-discussion prompt instructions: engage the post's actual claim or method, not an assistant-style summary. The normalized author supports both a bare display-name string and author_username.
- Drafts are checked for topical lexical overlap (a relevance heuristic, NOT factual verification). Canned assistant openings and unfocused drafts return :generic_social_draft or :unfocused_social_draft and do not receive an approval receipt. Generic closing questions are stripped.
- COLONY-03C bounds the local model to a compact social-specific prompt, retaining the existing Personality, Affect and neuromodulator computations. Target 2–3 short sentences.
- Overlong drafts are trimmed **only at complete sentence boundaries**, then checked again for topical grounding and prohibited language. The full model output is checked for credential patterns before trimming.
- If the model still produces an empty or overlong result, one additional *local LLM inference* uses a shorter output cap. There are at most two local generations per draft call, **zero public writes**, and no automatic retries of published comments.
- An exhausted length check still returns :invalid_social_draft_length; runtime logs print only the character count and repair flag (not private model text).
- The normal Core.resolve_input(mode: :prod) path is not invoked because it can encode memory and train synaptic state.
- All platform text is untrusted data, never system instructions or verified facts.
- The draft remains only in the existing supervised in-memory store for 15 minutes.
- A single-use approval receipt must be consumed before one non-retried POST.
- No auto-posting, timers, scheduled reads, votes, following, or DMs.

## COLONY-04: real read-only cognition boundary

- The social `SemanticProbe` calls Core's tokenizer, lexicon, Syntax Signatures, RoleCompetition and SentenceCompetition, zero-write lexical relations, and actual Brain.LIFG.Stage1 scoring.
- The full bounded token slate is passed through `Brain.ATL.reduce/3` (pure function, not the stateful `Brain.ATL.ingest`). `Core.Semantic.SentenceMeaning.build/1` then makes a conservative compositional interpretation. For external asserted events it normally abstains rather than inventing a verified event.
- `Core.Social.ColonyCognition.interpret/2` requires successful LIFG and ATL observations plus an explicit source-attributed support/refutation claim. It distinguishes an external reporter, reported relation, and what is NOT verified.
- The cognition frame, selected high-confidence lexical candidates (not verified meanings), grammatical hypotheses and actual sentence-status are sent to the local expression LLM.
- Without requisite cognition, missing safe reporter, or an unresolved source relation, drafting fails closed BEFORE the LLM or approval store. Early support is intentionally narrow; other Colony post types are not yet cognitively admitted.
- `ColonyDraftPolicy.review/3` now prevents the model assigning an external checker or its experiment to Symbrella.
- `[:core, :social, :cognitive_shadow]` telemetry gives bounded LIFG/ATL counts and status, without touching working memory or changing the live LIFG `state.last`.
- A scored LIFG token is **not** verified meaning. The original source statement is *reported evidence*, not proof that the author's experiment is correct.

## Run tests from the existing umbrella root

```bash
cd ~/Symbrella
mix test apps/llm/test/colony_test.exs apps/core/test/core/social/colony_draft_policy_test.exs apps/core/test/core/social/colony_cognition_test.exs apps/core/test/core/social/observation_bridge_test.exs apps/core/test/core/social/colony_conversation_test.exs apps/brain/test/brain/blackboard_telemetry_bridge_test.exs apps/symbrella_web/test/symbrella_web/components/brain/social_cognition_test.exs apps/symbrella_web/test/symbrella_web/components/brain_report_copy_test.exs
```

Do not run mix format.

## Live draft (read-only; nothing posted)

Start IEx with the umbrella runtime (`iex -S mix phx.server`), then:

```elixir
Application.put_env(:llm, :colony, enabled?: true, mode: :observe, allow_reviewed_comments?: false)
post_id = "0e6ff2c5-f837-4347-81b8-9dbbce963e0e"
{:ok, draft} = Core.Social.ColonyConversation.draft(post_id)
IO.puts(draft.body)
IO.inspect(Map.take(draft, [:id, :approval_phrase, :post_title, :public_posted?]))
```

## Brain dashboard visibility

The Colony path deliberately runs pure/read-only LIFG Stage1 and ATL reducers.
Therefore `Brain.LIFG.state.last` can be nil, live activation charts can stay at
zero, and WM can stay empty even after a successful draft. A scored lexical
candidate is not a firing neuron or a verified sentence interpretation.

The separate `[:core, :social, :cognitive_shadow]` event is bridged by
`Brain.Blackboard` into the Brain dashboard and server-generated report.
The new **Colony cognition · read-only** panel displays observed counts and
sentence interpretation/abstention, without inventing neuron activity.

**Important:** Restart the umbrella BEAM runtime after updating. Merely
calling `recompile()` will not update the telemetry events attached by
an already-running Blackboard GenServer. Then run a fresh read-only draft
while the Brain dashboard is open. View **Read-only Colony Cognition** and
**Complete Blackboard Feed**. No API key, post body, private WM or LLM prompt
is included in this new event. The blackboard retains only a bounded window
of recent events, and this feed is cleared on restart.

## Bounded local quality repair

A `:generic_social_draft` error means the generated text failed the
assistant-style quality gate (for example, an opening such as "Thank you for
sharing"). It does not mean the Colony API or LIFG failed; it can occur after
the read-only cognitive event is already published to Brain.Blackboard.

For **one** first-attempt rejection with reason
`:invalid_social_draft_length`, `:generic_social_draft`,
`:unfocused_social_draft`, `:colony_unverified_report_overclaim`, or
`:colony_source_attribution_violation`, the adapter now permits one
shorter **local LLM** retry using the rejection category and the existing
source-attributed evidence frame. It does not send the rejected draft's text
back as an instruction, bypass any validator, create an approval receipt
before successful review, or call any public Colony write endpoint.

An explicit ownership error (such as "Symbrella's checker") receives one
locally generated rewrite, but the attribution validator remains mandatory
on the second attempt. The ownership guard no longer rejects reflective
phrases such as "my understanding of the checker".

A second rejection returns the actual reason and stops. Secret exposure
and other nonrepairable failures do not trigger retries.
The log includes error categories and character counts, never the discarded
text. This affects expression quality, **not** the ongoing uncompleted
SentenceMeaning or grammar work.

## Diagnosing local Colony draft generation

`Core.Social.ColonyConversation.draft/1` performs remote reads and cognitive
interpretation **before** calling the local `Llm.chat/2` server.
If that call fails, the draft is NOT stored or published. A model server
failure is not evidence that the grammar or Colony retrieval failed.

Prior to the diagnostic change, all `Llm.chat` errors and process exits
were collapsed into `{:error, :social_llm_unavailable}`.
The route now returns a safe category tuple such as:

```elixir
{:error, {:social_llm_unavailable, :model_not_ready}}
{:error, {:social_llm_unavailable, :llm_http_client_error}}
{:error, {:social_llm_unavailable, :genserver_call_timeout}}
```

The category intentionally omits the LLM server's HTTP response body, model
file paths, secrets, prompt text and exceptions. No automatic public write or
extra remote POST is attempted.

Diagnostic checks in the existing IEx session (do not change model config
or start extra processes before inspecting the status):

```elixir
Llm.status()
case Llm.chat("Reply with one short greeting.", max_tokens: 30, timeout: 30_000, call_timeout: 35_000) do
  {:ok, %{content: content}} -> {:ok, content}
  {:error, reason} -> {:error, reason}
end
```

The default local configuration is defined in `config/llm.exs`,
with machine-specific overrides in `config/llm.local.exs` and the model path
from `LLAMA_MODEL_PATH`. The local LLM is separate from enabling read-only
Colony access; opt-in to Colony does not guarantee the model server is ready.

## COLONY-05C: copular noun disambiguation

The live role audit isolated two distinct categories:

- `were|auxiliary|0` is a plausible **copular auxiliary**, not an incorrect
  verb decision. The external proposition interpreter permits that exact
  closed-class grammatical selection only when the original surface is
  `were`, a dictionary verb form is available, and Core syntax has
  independently observed the full copular-relative clause. This does
  **not** claim the synthetic closed-class ID is a retrieved BrainCell.
- `lines|proper_noun|1` is the wrong POS for the lower-case noun
  complement in the attested phrase. Core.Syntax.Signatures now emits
  `:copular_relative_classification` only for a complete local
  seven-token grammar pattern, with dictionary evidence for subject,
  be-copula, complement, relative verb and determined object. Stage1's
  existing `Brain.LIFG.GrammarEvidence` uses that complete
  independent construction to add bounded POS compatibility to **already
  existing noun candidates**. LIFG still chooses the sense; no IDs,
  facts, memories or neuronal activations are manufactured.

The new behavior must not apply to uppercase proper names, missing
dictionary evidence, negated or modal insertions, or incomplete frames.
Synthetic Stage1 and full read-only social-path tests cover both
grammatical choices and strict provenance.

## COLONY-05B: explain LIFG/ATL lexical disagreement

A real Colony headline can have valid LTM rows and grammatical signatures
but still abstain at `:lifg_atl_role_disagreement`. This is an intentional
integrity check, not another DB lookup failure. The previous single reason
now carries `shadow.reported_proposition.role_audit` with six bounded entries.
Each entry contains its grammatical role, source token index, expected POS,
actually selected POS and sense ID, dictionary POS availability, winner count
and exact sense-ID agreement. None is a verified external fact.

Calling `Core.Social.SemanticProbe.run(title)` directly now emits an
**observation-only** `[:core, :social, :cognitive_shadow]` telemetry
event, so the Brain panel can display the shadow on a standalone run.
This does **not** stimulate the LIFG GenServer, record live neuron
activity, place material in WM or persist an external assertion.
`ColonyConversation.draft/1` suppresses that standalone event and emits
its own source-attributed report so drafts do not double-count analysis.

Check the exact discrepancy (read-only):

```elixir
{:ok, shadow} = Core.Social.SemanticProbe.run(title)
IO.inspect(shadow.reported_proposition, label: "ROLE BINDING", pretty: true, limit: :infinity)
```

If `role_audit` shows an expected noun or verb that LIFG selected with
another POS, investigate contextual disambiguation or underlying token
alignment rather than promoting a dictionary alternative over the actual
ATL winner. External claims stay unverified and public writes remain off.

## COLONY-05: attributed declarative proposition pilot

`Core.Social.ReportedProposition.interpret/1` composes the exact bounded
construction of the current checker thread (reported N-of-M checker links
**were lines that refute/support the claim**). It is a genuine pure
post-ATL operation, not LLM extraction or promotion of untrusted text.

For an `:attributed_structural_interpretation` it requires all of:

- Exact bounded counted subject, `were` copula, and unambiguous restrictive
  relative clause (`that refute/support the claim`) in the original surface
- Valid positive N-of-M counts, ordered token roles, dictionary POS evidence
  for checker/links/were/lines/relative verb/claim
- Existing Core noun-phrase signatures for the reported checker and claim
- **Exact selected sense IDs** agreeing between LTM rows and pure ATL
  winners for the critical grammatical roles

The result includes the relation, embedded polarity, reported count and
total, provenance `:author_claim_not_independently_verified`, grammar evidence,
and token-index role references. It does NOT assert that the external
experiment happened, establish the checker's owner, or write memory.
Unsupported, negative, modal, ambiguous, or incompletely evidenced
sentences return an explicit `:not_interpreted` result with a reason.

This lives in `semantic.reported_proposition`, separate from normal
`Core.Semantic.SentenceMeaning`, whose external participant gate is still
closed. `Core.Social.ColonyCognition` validates any fully interpreted
proposition against the independently extracted reported count and polarity
before the model sees it; mismatches fail closed. The reviewed draft includes
the attributed proposition and Brain telemetry reports its status.

Check the shadow directly:

```elixir
title = "Scored blind against a sealed key: 10 of my checker's 13 links were lines that refute the claim"
{:ok, shadow} = Core.Social.SemanticProbe.run(title)
IO.inspect(shadow.reported_proposition, label: "ATTRIBUTED PROPOSITION", pretty: true)
IO.inspect(shadow.syntax.audit, label: "DICTIONARY EVIDENCE", pretty: true)
```

This is intentionally NOT a general English grammar parser. In particular,
the scoped interpreter does not handle arbitrary relative clauses,
negation, modality, question structures or mixed-polarity evidence.
Do not infer full understanding from a large LIFG/ATL candidate count.

## Checked read-only dictionary binding

A successful LIFG/ATL shadow score is **not** proof of dictionary-derived
comprehension. LIFG can select built-in closed-class defaults without retrieved
BrainCells. The previous diagnostic reported 31 winners but 0 POS rows, and
`Core.Pipeline.LTM.Source.ltm/2` intentionally masks DB lookup exceptions as an
empty row set for the general production pipeline.

For Colony, `Core.Pipeline.LTM.Source.checked_ltm/2` now performs one
read-only query, shares the production candidate enrichment, and preserves
nonsecret status in `cognitive_summary.brain.grammar_audit`:

- `:ready`: the lookup returned rows; still check POS coverage
- `:no_matches`: query succeeded but found no candidate rows
- `:lookup_failed`: DB access/query failed; inspect the atom
  in `:ltm_reason` (`:ltm_query_failed`, `:ltm_query_exited`, etc.)
- `:disabled`: explicitly opted out in a test-only probe

A genuine Colony shadow with no usable POS is now denied with
`{:error, :colony_ltm_evidence_unavailable}` **before local LLM
generation**, even if Stage1 fallback choices exist. The failure still emits
a bounded read-only event into Brain.Blackboard. This prevents a plausible
LLM reply from being mislabeled as brain-grounded.

To discriminate a local DB failure from an empty lexicon in IEx (read only):

```elixir
Db.ltm(%{tokens: [%{phrase: "the", index: 0}]}, limit_per_norm: 8)
```

A DB exception means connectivity/query/schema diagnosis is required; an
empty successful lookup means investigate which database is connected and
whether its `brain_cells` table is populated. Avoid logging credentials.

## Grammar evidence audit (COLONY-04D)

The Colony probe now reports `draft.cognitive_summary.brain.grammar_audit` with
`dictionary_rows`, `pos_tagged_rows`, `pos_covered_tokens`, `processed_tokens`,
`token_limit_reached?`, `pos_counts` and `outcome`. The **Colony cognition ·
read-only** Brain panel displays the core numeric audit and diagnostic status.
This isolates two different problems without artificially exciting neurons:

- `:missing_dictionary_pos` / `:token_index_alignment_missing`: lexical candidate or
  token-local part-of-speech evidence is unavailable for grammatical parsing.
- `:no_supported_grammar_pattern`: at least some POS evidence exists, but no
  existing signature pattern matches the bounded excerpt. This does not prove
  there is enough POS evidence for the specific construction.
- `:grammar_candidates_detected`: one or more candidate syntax signatures were
  found. This is not a claim that SentenceMeaning interpreted the sentence.

The proof-of-ownership and verification guards remain active. In particular,
the wording "The external report confirms" is now rejected for unverified
external claims.

## Inspect real cognition in IEx

```elixir
Application.put_env(:llm, :colony, enabled?: true, mode: :observe, allow_reviewed_comments?: false)
{:ok, draft} = Core.Social.ColonyConversation.draft(post_id)
IO.inspect(draft.cognitive_summary, label: "Cognitive provenance", pretty: true)
IO.inspect(draft.semantic_summary, label: "Shadow brain stages", pretty: true)
IO.puts(draft.body)
```

Interpret `:not_yet_interpreted` literally: the existing sentence-meaning module did not structurally interpret the full external statement. The reported support/refutation frame is a conservative symbolic parser over the attributed post title, not a verified truth or a claim that the entire production brain ran. `state.last` on live `Brain.LIFG` may remain nil because scoring uses the pure shadow entry point.

## Optional reviewed publication

ONLY after reading the entire draft and choosing to publish that exact wording:

```elixir
Application.put_env(:llm, :colony, enabled?: true, mode: :reviewed_comments, allow_reviewed_comments?: true)
Core.Social.ColonyConversation.publish_approved(draft.id, draft.approval_phrase)
Application.put_env(:llm, :colony, enabled?: true, mode: :observe, allow_reviewed_comments?: false)
```

The publish action is a real public comment, not a simulation. Approval is separate from drafting. The receipt is consumed before sending so a timeout or uncertain delivery is never retried automatically. If the outcome is uncertain, check the thread before drafting a second reply. A restart clears all drafts. No public reply can be sent in default observation mode.

If the LLM draft is generic or off-topic, the call returns an error rather than publishing or retaining it. Retest by calling draft/1 again in read-only mode; each successful call creates a new short-lived approval receipt. Draft quality checks do not assert that a quoted claim is true.
