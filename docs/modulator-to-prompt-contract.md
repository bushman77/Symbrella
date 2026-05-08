# Modulator-to-Prompt Contract

This document defines how Symbrella's implemented neuromodulator state affects
LLM prompting.

It is a software contract, not a biological claim. The names are
brain-inspired labels for bounded control signals used by Brain and Core.

## Scope

Implemented now:

- `:da` - dopamine-like reward/exploration signal.
- `:"5ht"` - serotonin-like inhibition/stability signal.
- `:glu` - glutamate-like plasticity/activation signal.
- `:ne` - norepinephrine-like vigilance/arousal signal.

Not implemented as raw modulators:

- acetylcholine
- oxytocin
- Big Five personality traits
- named personality profiles such as Explorer or Guardian

Those may be future design ideas, but they must not be documented as current
runtime behavior until the code owns them.

## Runtime Flow

```text
Brain.MoodCore raw levels
  -> derived mood indices
  -> tone_hint telemetry/snapshot
  -> Core.Response.Personality bounded response state
  -> Core.Response.LlmPrompt system prompt
  -> LLM response behavior
```

The LLM should never receive claims that Symbrella has human feelings,
consciousness, or certainty beyond runtime evidence. The prompt may describe
the current state as simulated affect, mood, self-state, or response policy.

## Code Owners

These modules own the current implementation:

| Layer | Owner | Contract responsibility |
| --- | --- | --- |
| Raw modulator state | `Brain.MoodCore` | Store, clamp, decay, snapshot, and emit `:da`, `:"5ht"`, `:glu`, `:ne` |
| Mood side effects | `Brain.MoodHooks` | Optional safe calls into `Brain.MoodCore` from Brain paths |
| Intent nudges | `Brain.MoodPolicy` and `Brain.MoodCore.apply_intent/2` | Convert intent or intent telemetry into small bounded deltas |
| Appraisal nudges | `Brain.MoodCore.apply_appraisal/1` | Convert V/A/D plus tags into bounded deltas |
| Mood scoring helpers | `Brain.MoodWeights` | Convert mood snapshots into bounded score bias for Brain scoring paths |
| Response personality | `Core.Response.Personality` | Convert mood/runtime evidence into prompt-facing response posture |
| Prompt construction | `Core.Response.LlmPrompt` | Write mood, runtime, personality, affect, decision, and WM into the LLM system prompt |
| LLM call boundary | `Core.Response.LlmSynthesis` | Collect runtime evidence, build the prompt, call `Llm`, and retain prompt summary metadata |

No other module should invent prompt-facing modulator semantics. If another
module needs mood influence, it should consume the derived mood indices or call
a helper that is already part of this contract.

## Raw Levels

`Brain.MoodCore` owns the raw levels. Each level is clamped to `0.0..1.0` and
decays toward baseline using per-modulator half-lives.

| Raw key | Label | Current role |
| --- | --- | --- |
| `:da` | dopamine-like | Exploration, reward, novelty pressure |
| `:"5ht"` | serotonin-like | Inhibition, stability, calming pressure |
| `:glu` | glutamate-like | Plasticity, activation, learning/readiness pressure |
| `:ne` | norepinephrine-like | Vigilance, arousal, urgency/attention pressure |

`Brain.MoodCore` emits the current levels through `[:brain, :mood, :update]`
telemetry and exposes them through `Brain.MoodCore.snapshot/0`.

### Raw Level Inputs

Raw levels may be changed only through these public paths:

| Input path | Source | Effect |
| --- | --- | --- |
| `Brain.MoodCore.bump/1` | Explicit internal caller | Applies caller-provided deltas after clamping |
| `Brain.MoodCore.apply_intent/2` | Brain intent path | Applies small intent-specific deltas scaled by confidence |
| `Brain.MoodCore.apply_appraisal/1` | Affective appraisal path | Applies bounded deltas from valence, arousal, dominance, and tags |
| `Brain.MoodCore.register_activation/1` | Activation load path | Nudges attention/plasticity from active-cell count |
| `Brain.MoodCore.update_wm/1` | Working-memory path | Nudges attention/plasticity from WM density |
| `Brain.MoodCore.reset/0` | Tests or explicit reset | Restores baseline and clears saturation counters |
| `Brain.MoodCore.configure/1` | Runtime/test config | Updates baseline, half-life, clamp, saturation, shock, clock, or init |

All paths must preserve clamping to `0.0..1.0` and must not create atoms from
untrusted user input.

## Derived Mood Indices

The current code derives prompt-facing mood indices as:

```elixir
exploration = 0.6 * da + 0.4 * ne
inhibition = serotonin_5ht
vigilance = ne
plasticity = 0.5 * da + 0.5 * glu
```

Where `serotonin_5ht` is the `:"5ht"` raw level.

| Derived index | Inputs | Prompt-facing meaning |
| --- | --- | --- |
| `:exploration` | `0.6 * :da + 0.4 * :ne` | How much the response can lean curious, generative, and exploratory |
| `:inhibition` | `:"5ht"` | How much the response should restrain itself, stay stable, and avoid overreach |
| `:vigilance` | `:ne` | How much the response should self-check, de-escalate, or treat the context as high-pressure |
| `:plasticity` | `0.5 * :da + 0.5 * :glu` | How much the response can abstract, explain, adapt, or make cross-concept links |

### Snapshot Shape

`Brain.MoodCore.snapshot/0` must include at least:

```elixir
%{
  levels: %{da: float(), "5ht": float(), glu: float(), ne: float()},
  mood: %{
    exploration: float(),
    inhibition: float(),
    vigilance: float(),
    plasticity: float()
  },
  tone_hint: :warm | :cool | :deescalate | :neutral,
  dt_ms: non_neg_integer()
}
```

Additional fields are allowed, but the fields above are the stable contract for
Core response planning and UI inspection.

## Tone Hint

`Brain.MoodCore` maps derived indices to a `tone_hint`:

| Tone hint | Current gate |
| --- | --- |
| `:deescalate` | High vigilance and insufficient inhibition |
| `:warm` | Exploration is high, inhibition is not low, and vigilance is not spiking |
| `:cool` | Inhibition is high, exploration is low, and vigilance is not spiking |
| `:neutral` | No strong mood axis dominates |

Thresholds live in `config/mood.exs` under `config :brain, :mood_tone`.

Tone hint is advisory. It may steer prompt tone, but it must not override
guardrails, comprehension failures, or explicit user intent.

## Personality State

`Core.Response.Personality.decide/5` converts runtime evidence into a bounded
personality state. This is the main bridge from mood to prompting.

The output shape is:

```elixir
%{
  temperament: :steady | :curious | :careful | :direct | :supportive,
  response_profile:
    :safety_redirect
    | :self_check
    | :semantic_repair
    | :brain_explainer
    | :self_state_boundary
    | :technical_work
    | :social_chat
    | :direct_answer,
  assertiveness: float(),
  curiosity: float(),
  restraint: float(),
  warmth: float(),
  self_check: float(),
  abstraction: float(),
  explanation_depth: :brief | :normal | :deep,
  reasons: [atom()]
}
```

Current mood effects:

| Personality field | Mood driver |
| --- | --- |
| `temperament` | Becomes `:curious` when exploration is high, vigilance is not high, and runtime is not degraded |
| `curiosity` | Increases with `:exploration` |
| `restraint` | Increases with `:inhibition` |
| `self_check` | Rises sharply when `:vigilance` or norepinephrine is high |
| `abstraction` | Increases with `:plasticity` |
| `explanation_depth` | Can become `:deep` for brain explainer or technical work when stable and curious |
| `assertiveness` | Reduced by high vigilance or degraded runtime |
| `warmth` | Mostly selected by response profile and response decision tone |

High-priority safety and comprehension conditions override mood. Guardrails,
high-risk requests, degraded comprehension, and low confidence must force safer
profiles even when exploration is high.

### Personality Input Shape

`Core.Response.Personality.decide/5` expects:

```elixir
features :: map()
decision :: map()
mood :: map()
wm_items :: list()
context :: %{
  optional(:runtime_state) => map(),
  optional(:comprehension) => map()
}
```

Mood can appear directly in `mood` or nested inside `runtime_state.mood`.
`Core.Response.Personality` must default missing mood values to `0.5`, not
crash or infer extreme behavior from absent state.

### Override Order

Response profile selection must preserve this precedence:

1. Safety or guardrail redirect.
2. High vigilance or explicit de-escalation.
3. Self-state/care boundary requests.
4. Degraded comprehension, degraded runtime, or low confidence.
5. Brain-facing explanation requests.
6. Technical work requests.
7. Social chat.
8. Direct answer.

Mood tunes the selected profile; it does not bypass this order.

## Prompt Injection Points

`Core.Response.LlmPrompt.build_system_prompt/4` writes the state into the LLM
system prompt using these sections:

- `Mood state: ...`
- `Runtime state: ...`
- `Self-state: ...`
- `Runtime decision: ...`
- `Personality state: ...`
- `Simulated affect: ...`
- `Response profile: ...`
- profile-specific instruction from `Core.Response.Personality.directive/1`
- affect-specific instruction from `Core.Response.Affect.directive/1`
- `Working memory: ...`

The prompt must remain behavioral. It should say what to do, not claim that the
system literally feels a human emotion.

### Prompt Output Requirements

`Core.Response.LlmPrompt.build_system_prompt/4` must:

- include base identity and safety boundary instructions,
- preserve Symbrella's local-runtime premise when this Phoenix app is running
  on the user's machine,
- forbid generic hosted-chatbot claims such as "I run on remote servers" unless
  runtime evidence explicitly says that is true,
- avoid false "no memory" claims by distinguishing conversation context,
  working memory, episodic memory, database rows, logs, and temporary runtime
  traces,
- include mood context only as behavioral context,
- include runtime and self-state context when available,
- include personality state and response profile,
- include profile and affect directives,
- include working-memory concepts when available,
- avoid hidden chain-of-thought language,
- avoid claims of sentience, consciousness, or human feeling.

The system prompt may include numeric values rounded for compactness. It should
not include raw internal maps by inspection when a stable textual summary exists.

## Behavioral Rules

Use these rules when changing prompt or personality code:

- High exploration should increase curiosity and depth only when vigilance is
  not high and runtime comprehension is not degraded.
- High inhibition should increase restraint and reduce overreach.
- High vigilance should increase self-checking, de-escalation, and caution.
- High plasticity should increase abstraction and explanation ability, not
  hallucination tolerance.
- Safety, guardrails, and low comprehension always outrank mood.
- Mood may tune wording and response posture; it must not override factuality,
  user safety, or project boundaries.
- Prompt text must preserve the no-consciousness claim boundary.

## Required Tests

Changes to this contract require focused tests. Use these existing test areas as
the home for coverage:

| Behavior | Test area |
| --- | --- |
| Raw level clamping, decay, snapshot, and derived indices | `apps/brain/test/brain/mood_core_test.exs` |
| Tone hint behavior | `apps/brain/test/brain/mood_core_test.exs` |
| Personality profile precedence and mood effects | `apps/core/test/core/response/personality_test.exs` |
| Prompt sections and no-consciousness boundary | `apps/core/test/core/response/llm_prompt_test.exs` |
| End-to-end prompt summary through LLM synthesis | `apps/core/test/core/response/*` or a focused synthesis test |

Minimum assertions for the current four-modulator contract:

- `:da` and `:ne` affect `:exploration` according to
  `0.6 * da + 0.4 * ne`.
- `:"5ht"` is the only input to `:inhibition`.
- `:ne` is the only input to `:vigilance`.
- `:da` and `:glu` affect `:plasticity` according to
  `0.5 * da + 0.5 * glu`.
- high `:exploration` can increase personality curiosity,
  but high vigilance prevents the stable-curious deepening path.
- high `:inhibition` increases restraint.
- high `:vigilance` increases self-checking/de-escalation behavior.
- high `:plasticity` increases abstraction, not safety bypass.
- generated prompts include mood/personality context but do not claim human
  feelings or consciousness.

## Implementation Checklist

For ordinary changes to the current four-modulator prompt behavior:

1. Update `Brain.MoodCore` only if raw levels, deltas, decay, derived formulas,
   or tone selection change.
2. Update `config/mood.exs` only if thresholds, baselines, half-lives, or gains
   change.
3. Update `Core.Response.Personality` if derived mood should affect behavior
   fields differently.
4. Update `Core.Response.LlmPrompt` if prompt text, prompt sections, or boundary
   language changes.
5. Update `Core.Response.LlmSynthesis` only if runtime evidence collection or
   prompt metadata changes.
6. Add or adjust tests in the required test areas.
7. Update this document in the same change.

## Extension Rules

Before adding a new modulator:

1. Add the raw level to `Brain.MoodCore`.
2. Define decay, baseline, clamping, and telemetry fields.
3. Define derived indices or update existing formulas.
4. Update `Core.Response.Personality` mappings.
5. Update `Core.Response.LlmPrompt` prompt sections.
6. Add focused tests for the new behavior.
7. Update this contract and `config/mood.exs`.

Do not add prompt language for a modulator that is only a design idea.
