# Colony social browser — read-only Phoenix route

The authenticated Symbrella web app now exposes:

- `/social` — list topics, 25 per page, newest first.
  `?page=2` advances using the existing Colony `Client.posts/1`
  offset and sorting API. Previous, Next and Refresh controls
  preserve the page in the browser URL.
- `/social/:post_id` — click any listed topic to retrieve its
  original post and comments using the existing `Client.thread_context/2`
  GET operation. The browser shows returned comments, their authors,
  timestamps and an indicator for comment replies. The display is
  bounded at 200 comments; the API may return fewer.

The pages live inside `SymbrellaWeb.Router`'s existing
protected `live_session :protected` and reuse the existing
Basic Auth/session protection. `SymbrellaWeb.SocialData` projects only
public Colony data for display and limits untrusted text lengths.
Phoenix HEEx escapes untrusted author/post/reply text.

There is no public write action on either route. Clicking a topic
does **not** generate a local draft, call an LLM, write a brain event,
update a database or publish a Colony comment. The only API write made
by the underlying read client is its token-authentication exchange,
not a social-content write.

## Configuration

The Colony read page is now **enabled automatically** on startup:
`config/llm.exs` and the adapter both default to
`enabled?: true, mode: :observe, allow_reviewed_comments?: false`
with `max_posts: 25`.

**Permanent credential:** Put your own Colony API key (`col_...`)
in the existing umbrella-root `priv/.colony_api_key` file with
owner-only permissions (e.g. `chmod 600 priv/.colony_api_key`),
or provide `COLONY_API_KEY` to the server process. Never commit
that key, a JWT, or a credential-bearing `.env` file.

**Automatic session tokens:** The read client exchanges the API key
for a **24-hour JWT** using `POST /api/v1/auth/token`. The supervised
`Llm.Colony.TokenCache` holds that JWT in **process memory only** and
reuses it for normal GET requests. Near its JWT `exp` deadline
(five-minute safety margin), or after an HTTP 401, it obtains a fresh
JWT automatically. After an application restart, the new process
reauthenticates from the same permanent API key. There is no
background polling or automatic posting. No JWT is persisted or
written to an environment variable.

To check the local, credential-free status from IEx:

```elixir
Llm.Colony.Adapter.status()
```

The response reports readiness without exposing your API key or JWT.
An existing `config/llm.local.exs` may explicitly override
`enabled?: true` or `max_posts: 25`. If the Social page still
reports "disabled", inspect that ignored local configuration and
adjust its Colony **read** options (never enable reviewed posting
implicitly). Restart the Phoenix server after making changes.

**Note:** an existing local, ignored `config/llm.local.exs` can
override `max_posts`; if it still sets fewer than 25, the
browser will report the configuration limit rather than silently
changing pagination semantics. Change the Colony setting only on
your local machine if you see this message. The Moltbook setting is
unrelated and unchanged.

## Verification

From the existing umbrella root on the development branch:

```bash
cd ~/Symbrella
git pull origin development
mix test \
  apps/llm/test/colony_token_cache_test.exs \
  apps/llm/test/colony_auth_refresh_test.exs \
  apps/symbrella_web/test/symbrella_web/live/social_live_test.exs
```

Start the existing server as usual, then visit:

```
http://localhost:4000/social
```

The tests inject a fake Colony client with **only** `posts/1` and
`thread_context/1` so they never contact the actual
provider, generate drafts or publish comments.

The UI intentionally shows the comments actually returned by the
provider's context endpoint, not a claimed complete archive of
every reply on the remote service. It uses API offset pagination,
which can shift when new posts arrive; it is not a frozen snapshot.
