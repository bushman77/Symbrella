# Designing Elixir Systems with OTP: Architectural Guidelines

Source: `designing-elixir-systems-with-otp.pdf`, _Designing Elixir Systems with OTP_ by James Edward Gray II and Bruce A. Tate.

This document summarizes the book's system design principles and adapts them for Symbrella's Phoenix umbrella architecture.

## Core Idea

Build OTP systems in layers so each layer exposes only the complexity it owns. The book's memory aid is:

> Do fun things with big, loud worker-bees.

For our purposes, read that as:

1. Data
2. Functions
3. Tests
4. Boundaries
5. Lifecycles
6. Workers

The main lesson is not "use OTP everywhere." It is "use OTP where process machinery earns its cost, and keep the rest of the system as simple, pure, and testable as possible."

## Layer 1: Data

Start with data structures before reaching for processes, persistence, or Phoenix UI.

Guidelines:

- Shape data around access patterns. If a value will be updated often, choose a structure that makes the update clear and cheap.
- Prefer flat data over deeply nested structures when the domain allows it.
- Use structs when fields are known and meaningful.
- Treat immutable data as facts at a point in time. New facts should produce new values instead of mutating old ones.
- Avoid hiding ordinary data behind a process just because Elixir makes processes easy.
- Use processes for coordination, ownership, isolation, timing, or shared external state, not as a default data container.

For Symbrella:

- Cognitive events, semantic inputs, responses, attention decisions, mood states, and memory records should first be modeled as explicit data.
- A brain process should own time, coordination, or durable state transitions. It should not be the only place where business rules are understandable.

## Layer 2: Functional Core

The functional core is regular Elixir code: modules, structs, and functions. It should contain most business logic.

Guidelines:

- Keep the core mostly pure: same inputs, same outputs.
- Keep external services, GenServer calls, database writes, logging side effects, and Phoenix concerns outside the core.
- Build single-purpose functions.
- Name concepts with functions.
- Shape functions for composition.
- Keep functions at one level of abstraction.
- Prefer pattern matching and function heads for decisions when that makes the flow clearer.

For Symbrella:

- Cognitive policies should live in core modules where they can be tested without running the full supervision tree.
- The reasoning loop should be inspectable as data transformations, not only as messages moving between processes.
- A LiveView, controller, GenServer, or Oban-style worker should delegate domain decisions to a pure core whenever practical.

## Layer 3: Core Tests

Test the functional core while it is still free from process and infrastructure complexity.

Guidelines:

- Use fixture functions to build complex domain data.
- Let fixture functions accept overrides so tests can focus on the scenario.
- Compose test setup the same way production code composes data.
- Make random or time-dependent behavior deterministic where possible. When not possible, test stable properties instead of exact values.
- Keep tests focused on outcomes, not internal implementation details.

For Symbrella:

- Core tests should validate cognitive transitions, response shaping, policy decisions, and semantic normalization without requiring Phoenix or long-running processes.
- Use small scenario builders for brain inputs and memory records rather than repeating large maps in every test.

## Layer 4: Boundaries

The boundary layer wraps impurity: process machinery, persistence, external APIs, files, logs, and user interfaces.

Guidelines:

- Add a boundary only when there is real uncertainty or impurity to isolate.
- Keep boundary APIs thin and explicit.
- Hide raw GenServer message shapes behind named functions.
- Validate external input at the boundary before passing it into the core.
- Use `with` when composing steps that can fail.
- Prefer `GenServer.call/3` over `cast/2` when the caller needs ordering, confirmation, or back pressure.
- Use `cast/2` only for genuine fire-and-forget work where mailbox growth and lost ordering are acceptable.

For Symbrella:

- Public APIs such as `Brain.*`, `Core.*`, `Llm.*`, or `Db.*` should expose named operations, not internal process messages.
- HTTP and LLM calls belong at boundaries. Use `Req` for HTTP requests.
- Phoenix LiveViews should be treated as boundary modules: they render, validate UI input, and delegate domain work.

## Layer 5: Lifecycles

Lifecycle code defines how processes start, stop, restart, and get named.

Guidelines:

- Start long-lived processes from supervisors, not ad hoc call sites.
- Use `DynamicSupervisor` for per-user, per-session, per-task, or otherwise dynamic processes.
- Use `Registry` when processes need stable names or lookup by domain key.
- Put restart policy in supervision trees, not scattered through application code.
- Decide explicitly whether state should survive a crash, be rebuilt during `init/1`, or be intentionally lost.
- Keep child specs and `start_link/1` predictable.

For Symbrella:

- Long-lived cognitive subsystems should have explicit supervision ownership.
- Per-session cognition, cockpit connections, or future device bodies should be dynamically supervised when they represent independent lifecycles.
- If a process holds important state, document whether that state is reconstructible from events, persisted memory, or external context.

## Layer 6: Workers

Workers are process machinery for concurrency, isolation, scheduling, and scalability.

Guidelines:

- Add workers for a concrete reason: concurrency, isolation, timing, or throughput.
- Prefer existing OTP abstractions and proven dependencies over naked processes.
- Use `Task.async_stream/3` for bounded concurrent enumeration.
- Use GenServer timeouts or `Process.send_after/3` for scheduling inside a process when the schedule is part of that process's responsibility.
- Use pools when many callers must share a limited external resource.
- Treat worker code as boundary code. It coordinates work; it should not bury domain logic.

For Symbrella:

- LLM calls, recall fan-out, background reflection, self-calibration, and device/body integrations are good worker candidates.
- Keep concurrency limits explicit so curiosity, recall, or LLM support cannot overwhelm the system.
- Use workers to isolate failures. A failed noncritical integration should not crash the entire brain.

## Persistence and Phoenix Integration

The book recommends delaying persistence design until the functional core is understandable.

Guidelines:

- Do not let database schemas define the entire domain model by default.
- Treat persistence as a boundary service.
- Keep mapping between domain structs and persisted schemas explicit.
- In Phoenix, let LiveViews, controllers, channels, and forms act as framework boundaries.
- Avoid mixing UI state, persistence concerns, and core domain rules in one module.

For Symbrella:

- Memory persistence should record domain facts and events without forcing all brain structures to mirror database tables.
- Phoenix web modules should show and manipulate brain state through public APIs.
- If a LiveView owns transient UI state, keep that separate from cognitive state owned by the brain layer.

## Boundary Tests

Boundary tests exercise process, lifecycle, persistence, and integration behavior after the core is covered.

Guidelines:

- Test public APIs as users of the component would call them.
- Avoid retesting every pure core rule through GenServers.
- Use integration tests sparingly but make them meaningful.
- For timing code, prefer notifications, messages, or injectable clocks over sleeps.
- Mark tests `async: false` when they share named processes or global resources.
- Test persistence rollback and failure behavior at the persistence boundary.

For Symbrella:

- Write core tests for cognition rules and boundary tests for supervised process behavior.
- Use Phoenix LiveView tests for rendered workflows and DOM outcomes, not internal implementation.
- For brain process tests, assert messages, state transitions, or public API results rather than peeking into process internals unless the test is specifically about lifecycle/debuggability.

## Decision Checklist

Before adding a new module or process, ask:

1. What data facts does this feature introduce?
2. Can the main rule be implemented as a pure function first?
3. What external uncertainty exists: user input, process state, database, network, time, filesystem, LLM, or device?
4. Does this need a boundary API?
5. Does it need a supervised lifecycle?
6. Does it need concurrent workers, or can it run synchronously?
7. How will the core be tested separately from the boundary?
8. How will the boundary be tested without fragile sleeps or hidden global state?

## Symbrella Architectural Rules

- Model cognition as explicit data plus composable functions before adding process machinery.
- Put durable domain rules in core modules.
- Put GenServer callbacks, Phoenix callbacks, HTTP requests, database writes, timers, and logging at boundaries.
- Expose public APIs with named functions.
- Supervise long-lived and dynamically-created cognitive processes deliberately.
- Use `Registry` for process lookup by meaningful domain keys.
- Use `Task.async_stream/3` for bounded concurrent work.
- Prefer `call` when back pressure matters.
- Treat LLMs and devices as boundary services, not as the core architecture.
- Keep the cognitive loop visible, testable, and debuggable.
