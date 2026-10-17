# Hippocampal prompt budgeting (experimental)

Goal: determine whether relevant hippocampal recollections can replace older
chat-history turns while keeping answers correct. The default behavior remains
unchanged: **six turn pairs in the normal LLM prompt**.

## Measurements

Each free-form first-pass LLM request emits a content-free telemetry event:

`[:core, :response, :prompt_budget]`

The `measurements` include estimated input tokens for:

- `estimated_instruction_tokens`: the remaining system prompt, including
  symbolic and behavioral instructions;
- `estimated_history_tokens`: actual user/assistant chat-history window;
- `estimated_episodic_tokens`: quoted, same-session hippocampal recollections;
- `estimated_verified_evidence_tokens`: verified response-frame evidence;
- `estimated_retrieved_evidence_tokens`: retrieved source material (not
  necessarily independently verified);
- `estimated_user_tokens`: the current user message;
- `estimated_full_tokens`, `estimated_selective_tokens`,
  `estimated_savings_tokens`, and `estimated_savings_percent`.

The estimator is `ceil(chars / 4)`, applied to isolated components. It is
a **rough proxy**, not the Qwen/llama.cpp tokenizer, KV-cache usage, runtime
latency, provider-reported token counts, or billed tokens. Chat-template role
overhead and extra review/revision passes are excluded.

The event metadata includes the `history_mode`, eligibility, selection
reason, and history message counts. Raw prompts, memories, and chat text are
**not** included in this telemetry event.

## Compare without changing behavior

In `config/config.exs`:

```elixir
config :core, :llm_synthesis,
  # other settings...
  history_turn_pairs: 6,
  history_mode: :full,
  selective_recent_pairs: 2
```

`:full` remains the default. The program computes the selective candidate
counterfactually and records its possible estimated savings **without**
changing which messages go to the LLM.

The `selection_reason` explains why trimming was or was not eligible.
Selective mode is conservative: it needs a prompt-visible same-session
hippocampal recollection, sufficient history to trim, no ambiguous referential
follow-up, and no different earlier question currently being answered.

For local aggregate-only log lines, in the running IEx/Phoenix BEAM:

```elixir
Application.put_env(:core, :log_prompt_budget?, true)
```

The output starts with `[PromptBudget]`. Disable later using `false`.
The logger is off by default. Telemetry is emitted whether or not logging is
enabled.

## Opt in only after baseline checks

During an experimental run in the same BEAM:

```elixir
Application.put_env(
  :core,
  :llm_synthesis,
  Keyword.put(Application.get_env(:core, :llm_synthesis, []),
    :history_mode, :selective_recall)
)
```

This preserves the newest two turn pairs when a recollection is available
and the conservative safety gate allows trimming. To revert:

```elixir
Application.put_env(
  :core,
  :llm_synthesis,
  Keyword.put(Application.get_env(:core, :llm_synthesis, []),
    :history_mode, :full)
)
```

Do not interpret reduced prompt size as improved quality. With a real local
Qwen model, replay the same questions under each mode, then compare:
correctness, retained referents, factual citations, uncertainty, and actual
runtime input-token counts if exposed by llama.cpp.

## Automated regression

```bash
mix test apps/core/test/core/response/prompt_budget_comparison_test.exs --trace
```

The test compares two responses to the **same** recalled nickname with the
**same** six stored turn pairs using a deterministic fake model; asserts the
selective prompt is smaller and the answer unchanged; checks conservative
fallback for reference-dependent questions; and validates separate token
estimates for instructions, retrieved material, verified proof and episodes.

This is a correctness gate for the plumbing, **not** a substitute for
evaluating a live generative model.
