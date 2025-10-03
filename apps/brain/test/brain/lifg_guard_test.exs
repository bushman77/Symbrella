defmodule Brain.LIFG.GuardTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.Guard

  test "adds :index and preserves fields" do
    toks = [%{phrase: "hi"}, %{phrase: "there"}]
    out = Guard.sanitize(toks)
    assert Enum.map(out, & &1.index) == [0, 1]
    assert Enum.map(out, & &1.phrase) == ["hi", "there"]
  end

  test "sorts by span if all spans present" do
    toks = [%{phrase: "b", span: {2,1}, index: 1}, %{phrase: "a", span: {0,1}, index: 0}]
    out = Guard.sanitize(toks)
    assert Enum.map(out, & &1.phrase) == ["a", "b"]
  end
end

