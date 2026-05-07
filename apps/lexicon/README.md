# Lexicon - External Word Sense Adapter

`Lexicon` is the umbrella app responsible for fetching dictionary-style lexical
evidence that Core can fold into a semantic turn.

It is intentionally small: it does not own tokenization, LIFG scoring, working
memory, or persistence. Those responsibilities stay in `core`, `brain`, and
`db`.

## Current Role

- Fetches English dictionary entries for a word through `Lexicon.enrich/2`.
- Normalizes the HTTP result into `{:ok, %{status:, url:, data:}}` or
  `{:error, reason}`.
- Decodes JSON with `Jason` when available.
- Is supervised indirectly by the umbrella runtime, which also starts
  `Lexicon.Finch` for future HTTP client work.

The current implementation calls `dictionaryapi.dev` and still uses `:hackney`.
New HTTP work in this Phoenix umbrella should use `Req`, matching the project
guidelines and the `llm` app.

## Public API

```elixir
Lexicon.enrich("umbrella")
Lexicon.enrich("working memory", headers: [{"accept", "application/json"}])
```

Blank input returns `{:error, :empty_word}`.

## Boundaries

Lexicon may:

- fetch lexical evidence from external sources,
- decode and lightly normalize remote payloads,
- expose simple adapter functions for Core.

Lexicon should not:

- write directly to the database,
- generate LIFG candidates on its own,
- mutate Brain working memory,
- own response generation.

Core decides when lexical enrichment is needed and how to merge the result into
`Core.SemanticInput`.

## Tests

There are no dedicated Lexicon tests yet. When tests are added, run them from
the umbrella root:

```bash
mix test apps/lexicon/test
```

If you add network-sensitive behavior, prefer test doubles or small adapter
functions so the normal test suite does not depend on a live third-party API.
