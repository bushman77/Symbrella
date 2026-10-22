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
`:unfocused_social_draft`, or
`:colony_unverified_report_overclaim`, the adapter now permits one
shorter **local LLM** retry using the rejection category and the existing
source-attributed evidence frame. It does not send the rejected draft's text
back as an instruction, bypass any validator, create an approval receipt
before successful review, or call any public Colony write endpoint.

A second rejection returns the actual reason and stops. Hard failures such
as secret exposure and false ownership attribution do not trigger retries.
The log includes error categories and character counts, never the discarded
text. This affects expression quality, **not** the ongoing uncompleted
SentenceMeaning or grammar work.

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
