defmodule Brain.LIFG.TripwireTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.Stage1

  setup do
    # capture char-gram + boundary drops
    {:ok, pid} = Agent.start_link(fn -> [] end)

    handler = fn _event, _measure, meta, _cfg ->
      Agent.update(pid, fn xs -> [meta | xs] end)
    end

    :telemetry.attach_many(
      "lifg-tripwire-test",
      [
        [:brain, :lifg, :chargram_violation],
        [:brain, :lifg, :boundary_drop]
      ],
      handler,
      nil
    )

    on_exit(fn ->
      :telemetry.detach("lifg-tripwire-test")
      Agent.stop(pid)
    end)

    {:ok, store: pid}
  end

  test "drops misaligned char-grams and emits telemetry", %{store: pid} do
    si = %{
      sentence: "Kick the ball",
      tokens: [
        # should drop
        %{index: 0, phrase: "ck t", span: {1, 5}, n: 1, mw: false},
        # should keep
        %{index: 1, phrase: "Kick", span: {0, 4}, n: 1, mw: false}
      ],
      sense_candidates: %{
        0 => ["junk"],
        1 => [%{id: "kick|verb", score: 0.8}]
      }
    }

    {:ok, %{choices: choices, audit: audit}} = Stage1.run(si)

    # only token 1 remains
    assert Enum.map(choices, & &1.token_index) == [1]
    assert audit.dropped_tokens == 1

    events = Agent.get(pid, & &1)
    assert Enum.any?(events, &match?(%{token_index: 0, phrase: "ck t"}, &1))
  end
end
