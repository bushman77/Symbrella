defmodule Brain.LIFG.TripwireTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.Tripwire

  setup do
    ref = "tripwire-test-#{System.unique_integer([:positive])}"

    :ok =
      :telemetry.attach(
        ref,
        [:lifg, :chargram, :leak],
        fn _event, meas, meta, _conf ->
          send(self(), {:leak, meas, meta})
        end,
        nil
      )

    on_exit(fn -> :telemetry.detach(ref) end)
    :ok
  end

  test "emits telemetry and drops char-gram tokens by default" do
    tokens = [
      # char-gram leak
      %{phrase: "ck t"},
      %{phrase: "hello"},
      # valid MWE
      %{phrase: "new york", mw: true}
    ]

    out = Tripwire.check_and_filter(tokens)

    # Telemetry fired
    assert_receive {:leak, %{count: 1, examples: examples}, %{on_leak: :drop}}, 100
    assert "ck t" in examples

    # Dropped only the bad one
    phrases = Enum.map(out, & &1.phrase)
    refute "ck t" in phrases
    assert "hello" in phrases
    assert "new york" in phrases
  end

  test "option :keep preserves tokens but still reports leak" do
    tokens = [%{phrase: "k th"}, %{phrase: "bank"}]
    out = Tripwire.check_and_filter(tokens, on_leak: :keep)

    assert_receive {:leak, %{count: 1}, %{on_leak: :keep}}, 100
    assert Enum.map(out, & &1.phrase) == ["k th", "bank"]
  end
end
