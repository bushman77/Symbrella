# COLONY-03 — Reviewed replies on The Colony

Symbrella now has an on-demand read → comprehend → local LLM draft → human review workflow.
The existing umbrella-root priv/.colony_api_key remains private; no external data folders are created.

## Security and behavior

- Default: enabled?: false, mode: :observe, allow_reviewed_comments?: false.
- Read-only observation can fetch a full post and comments, build a bounded semantic diagnostic, and draft a reply.
- This path uses Symbrella's existing personality and neuromodulator prompt code. It avoids private chat history and global WM content.
- The normal Core.resolve_input(mode: :prod) path is not invoked because it can encode memory and train synaptic state.
- All platform text is untrusted data, never system instructions or verified facts.
- The draft remains only in the existing supervised in-memory store for 15 minutes.
- A single-use approval receipt must be consumed before one non-retried POST.
- No auto-posting, timers, scheduled reads, votes, following, or DMs.

## Run tests from the existing umbrella root

```bash
cd ~/Symbrella
mix test apps/llm/test/colony_test.exs apps/core/test/core/social/colony_conversation_test.exs
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

## Optional reviewed publication

ONLY after reading the entire draft and choosing to publish that exact wording:

```elixir
Application.put_env(:llm, :colony, enabled?: true, mode: :reviewed_comments, allow_reviewed_comments?: true)
Core.Social.ColonyConversation.publish_approved(draft.id, draft.approval_phrase)
Application.put_env(:llm, :colony, enabled?: true, mode: :observe, allow_reviewed_comments?: false)
```

The publish action is a real public comment, not a simulation. Approval is separate from drafting. The receipt is consumed before sending so a timeout or uncertain delivery is never retried automatically. If the outcome is uncertain, check the thread before drafting a second reply. A restart clears all drafts. No public reply can be sent in default observation mode.
