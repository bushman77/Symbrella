defmodule Core.Intent.AtomSafetyTest do
  use ExUnit.Case, async: false

  alias Core.Intent.Telemetry

  test "unknown intent labels remain strings and do not allocate new atoms" do
    label = "unknown_intent_" <> Integer.to_string(System.unique_integer([:positive]))
    assert_raise ArgumentError, fn -> String.to_existing_atom(label) end

    handler_id = "intent-atom-safety-" <> Integer.to_string(System.unique_integer([:positive]))
    parent = self()

    :ok =
      :telemetry.attach(
        handler_id,
        [:core, :intent, :selected],
        fn _event, _measurements, metadata, pid ->
          send(pid, {:selected_intent, metadata.intent})
        end,
        parent
      )

    try do
      assert :ok == Telemetry.emit_selected("hello", label, nil, 0.75)
      assert_receive {:selected_intent, ^label}
      assert_raise ArgumentError, fn -> String.to_existing_atom(label) end
    after
      :telemetry.detach(handler_id)
    end
  end

  test "known intent strings are still promoted to existing atoms" do
    handler_id = "intent-known-" <> Integer.to_string(System.unique_integer([:positive]))
    :ok = :telemetry.attach(handler_id, [:core, :intent, :selected],
      fn _, _, metadata, pid -> send(pid, {:selected_intent, metadata.intent}) end, self())

    try do
      Telemetry.emit_selected("hello", "ask", nil, 0.75)
      assert_receive {:selected_intent, :ask}
    after
      :telemetry.detach(handler_id)
    end
  end
end
