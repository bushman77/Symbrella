# Runtime hardening roadmap

This roadmap reconciles the October 2026 static reviews. It separates
confirmed defects, deployment restrictions, and longer-term maintenance work.
No change here treats neural-shadow outputs as authoritative symbolic evidence.

## Increment 1: conversation isolation and atomic history writes

- Assign one random conversation key to each browser using Phoenix's signed
  session cookie. A remount retains its own key; unrelated browsers do not
  share a display history or implicit LLM context.
- Route chat and the latest-chat semantic report through that key. Never
  accept a caller-supplied session ID when handling semantic-report actions.
- Serialize LLM turn append operations through the ETS owner to avoid lost
  updates in simultaneous response completions.
- Replace the public, hardcoded development cookie-signing secret with an
  ephemeral runtime secret (or `SYMBRELLA_DEV_SECRET_KEY_BASE`).
- Retain the existing symbolic-memory trust checks, learning fixtures, tests,
  and no-disk private display-history policy.

**Verification before integration:** run `mix format --check-formatted` and
`mix test`, specifically the ChatHistory, ChatSessionContinuity, SemanticLive,
HomeLiveHistory, ConversationRecall and LlmChatHistoryOwner tests. Exercise two
real browsers simultaneously and refresh both. Check that per-browser history
and verified semantic reports do not bleed across sessions.

## Increment 2: access control before remote use

- Add explicit authentication to `/`, `/episodes`, `/semantic`, `/brain`,
  `/brain/:region`, `/brain/report.txt`, and development dashboards.
- Authorize LiveView socket connections *as well as* the initial HTTP request.
- Disable non-loopback operation until authorization and origin policy are
  configured, or document an authenticated reverse proxy/tunnel requirement.
- Require a production host, database URL/credentials and secret key. Set
  explicit, correct `check_origin` and TLS/reverse-proxy settings.
- Review `DevFileApiAuth`, `OpenAICompatController` and `RoleplayLive`:
  integrate and test with appropriate auth, or remove. Never put credentials
  into URL query parameters.

**Warning:** Increment 1 alone does not authorize network clients. The
existing web routes remain unauthenticated until Increment 2 is implemented.

## Increment 3: data safety and growth

- Audit `String.to_atom/1` by *reachability* and boundedness; replace only
  unbounded input conversions with exact mappings or string representations.
- Paginate `Db.Episode.list_all/0` consumers, including EpisodesLive.
- Make self-calibration recording configurable and add rotation, retention,
  and explicit backup/export policies. Do not sacrifice useful neural data.
- Give the LLM boot gate a configurable degraded mode so symbolic diagnostics
  remain accessible when the model cannot start.

## Increment 4: automated assurance

- Add CI for formatting, tests (with PostgreSQL/pgvector), and Credo.
- Add targeted LLM subprocess lifecycle tests, web authorization tests,
  and storage concurrency/retention tests.
- Introduce Dialyzer incrementally as typespec coverage grows; assess Sobelow
  as a supplementary Phoenix security check.
- Refactor large HomeLive and LIFG modules only with regression coverage.

## Boundaries to protect

- Verified semantic conclusions must remain source-revalidated and separate
  from uncertain lexical associations or neural predictions.
- Browser conversations may be isolated while shared cognitive processes
  remain global; that is an intentional distinction.
- Do not rewrite Git history or remove generated local training data as part
  of this runtime-hardening sequence.

## Phase 2 — Web access control

`development` retains anonymous access by default to avoid silently breaking
existing LAN/Termux use. Before exposing port 4000 outside a trusted network,
set both variables (use a unique password with at least 16 characters):

```sh
export SYMBRELLA_WEB_AUTH_USERNAME=symbrella
read -r -s -p 'Web password: ' SYMBRELLA_WEB_AUTH_PASSWORD; echo
export SYMBRELLA_WEB_AUTH_PASSWORD
```

Restart the application after configuring credentials. The browser prompts for
HTTP Basic credentials and then retains a signed, session-bound proof for
LiveView reconnects. To change/revoke access, rotate the password and restart.

**Basic authentication must not be used over unencrypted public HTTP.** Use a
trusted network, VPN, or properly configured HTTPS reverse proxy. The development
endpoint still binds to all interfaces. Production fails closed unless web
credentials, `DATABASE_URL`, `SYMBRELLA_HOST`, and `SECRET_KEY_BASE` exist; enable
TLS termination at the proxy and forward `X-Forwarded-Proto: https`.
