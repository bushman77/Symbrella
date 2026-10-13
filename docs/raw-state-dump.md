# Raw Symbrella state dumps

Symbrella can write an append-only plain-text log of complete Core state
objects, with nested structs expanded and without inspection truncation.

From the umbrella root, restart the runtime with:

    SYMBRELLA_STATE_DUMP=all iex -S mix phx.server

Read the log from another Termux shell:

    tail -f tmp/state_dumps/symbrella_raw.log

## Modes

- SYMBRELLA_STATE_DUMP=off (default): disabled.
- SYMBRELLA_STATE_DUMP=final: complete pipeline map before struct
  conversion, returned Core.SemanticInput, and final web chat reply.
- SYMBRELLA_STATE_DUMP=all: all final records plus snapshots after each
  instrumented stage: syntax, dictionary, LIFG, ATL, response, hippocampus,
  activation, and other steps.

Override the path with SYMBRELLA_STATE_DUMP_PATH=/absolute/path/raw.log.

Each entry includes a UTC timestamp, stage, writer PID and the complete
Elixir term rendered using inspect/2 with unlimited limits and structs:false.
Intermediate records are state snapshots, not just trace summaries.
Memory fast paths are captured at the chat reply point.
These are readable diagnostics, not a lossless binary serialization or labels.

## Privacy and storage

The log file is under git-ignored tmp/ and gets mode 0600. It can contain
private messages, names, session identifiers, memory and other nested data.
Do not commit or publicly share it without review.

Mode all can grow very quickly because every stage contains a full copy
of the state accumulated so far. Use final for extended collection.
There is no automatic truncation, rotation, or retention policy.

## Test

    mix test apps/core/test/core/state_dump_test.exs
