defmodule Core.Response.LlmHistoryTest do
  use ExUnit.Case, async: true

  alias Core.Response.LlmHistory

  test "append_turn clamps text and keeps bounded turn pairs" do
    messages =
      []
      |> LlmHistory.append_turn(" first ", "one", 2, 10)
      |> LlmHistory.append_turn("second", "two", 2, 10)
      |> LlmHistory.append_turn("third", String.duplicate("x", 12), 2, 10)

    assert [
             %{"role" => "user", "content" => "second"},
             %{"role" => "assistant", "content" => "two"},
             %{"role" => "user", "content" => "third"},
             %{"role" => "assistant", "content" => "xxxxxxxxxx…"}
           ] = messages
  end

  test "status summarizes messages without touching ETS" do
    messages = [
      %{"role" => "user", "content" => "hello"},
      %{"role" => "assistant", "content" => "hi"}
    ]

    status = LlmHistory.status(true, messages)

    assert status.table_present?
    assert status.history_present?
    assert status.message_count == 2
    assert status.turn_pairs == 1
    assert is_map(status.topics)
  end

  test "missing_status is stable for absent history table" do
    assert LlmHistory.missing_status() == %{
             table_present?: false,
             history_present?: false,
             message_count: 0,
             turn_pairs: 0
           }
  end
end
