# Llm - Local Model Runner and Client

`Llm` owns Symbrella's local LLM boundary. It is currently built around a
llama.cpp-compatible `llama-server` process and exposes a small Elixir API for
chat, model listing, embeddings, status, and logs.

The app is a service layer, not the cognitive pipeline. Core decides when a
response needs model synthesis; Brain owns region state and memory; `Llm` only
starts or talks to the model backend.

## Current Role

- Starts and supervises a local `llama-server` OS process when configured.
- Provides `Llm.BootGate` so the umbrella can block startup until the runner is
  reachable.
- Polls readiness and restarts after crashes when policy allows it.
- Exposes OpenAI-compatible endpoints through `Req`:
  - `GET /v1/models`
  - `POST /v1/chat/completions`
  - `POST /v1/embeddings`
- Keeps a small in-memory log ring for debugging runner startup and crashes.
- Supports lazy start, explicit stop, and status inspection.

Configuration lives under `config :llm, Llm` in the umbrella config.

## Public API

```elixir
Llm.status()
Llm.start_llama()
Llm.stop_llama()
Llm.logs()
Llm.models()
Llm.chat("Summarize the current turn")
Llm.embeddings("working memory")
```

Most calls return `{:ok, value}` or `{:error, reason}`. `Llm.chat/2` accepts a
plain prompt or normalized chat messages.

## Runtime Requirements

- `llama-server` must be available on `PATH`, unless a custom executable path is
  configured.
- `model_path` must point to a local model file, or `LLAMA_MODEL_PATH` must be
  set.
- The configured port may be `0`; in that case the runner chooses an available
  local port and stores the resolved endpoint in `Llm.status/0`.

Useful config keys:

```elixir
config :llm, Llm,
  model_path: "/path/to/model.gguf",
  llama_server: "llama-server",
  auto_start_on_boot?: false,
  allow_lazy_start?: true,
  auto_restart_on_crash?: true,
  host: "127.0.0.1",
  port: 0,
  ctx: 2048,
  threads: 4

config :llm, Llm.BootGate,
  enabled?: true,
  timeout: 120_000
```

## Boundaries

Llm may:

- own the external model process,
- make HTTP calls with `Req`,
- expose chat and embedding calls to Core.

Llm should not:

- decide intent,
- write memory,
- mutate Brain regions,
- render web UI.

Core response modules, especially `Core.Response.LlmPrompt` and
`Core.Response.LlmSynthesis`, are the normal callers.

## Tests

Run from the umbrella root:

```bash
mix test apps/llm/test
```

Tests that exercise a real model runner should stay opt-in. The default suite
should favor mocked or unreachable-runner paths.
