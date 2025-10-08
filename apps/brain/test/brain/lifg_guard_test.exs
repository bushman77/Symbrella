defmodule Brain.LIFG.GuardTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.Guard

  defmodule DemoTok do
    defstruct [:phrase, :span, :index]
  end

  test "sanitize: mapifies inputs, ensures index and phrase" do
    toks =
      [
        %{"phrase" => "alpha"},
        %DemoTok{phrase: "beta"},
        :gamma
      ]
      |> Guard.sanitize()

    assert Enum.map(toks, &is_map/1) == [true, true, true]
    assert Enum.all?(toks, &is_integer(&1.index))
    assert Enum.map(toks, & &1.phrase) == ["alpha", "beta", "gamma"]
  end

  test "sanitize: normalizes span to {start,end}, recovers when {start,len} or broken" do
    toks =
      [
        %{phrase: "Hello", span: {0, 5}},  # already {start,end}
        %{phrase: "Hi",    span: {6, 0}},  # treat as {start,len}-> recover via phrase length
        %{phrase: "!"}                      # no span
      ]
      |> Guard.sanitize()

    assert Enum.at(toks, 0).span == {0, 5}
    assert Enum.at(toks, 1).span == {6, 8} # 6 + byte_size("Hi") == 8
    refute Map.has_key?(Enum.at(toks, 2), :span)
  end

  test "sanitize: sorts by span start iff all spans are valid; otherwise preserves order" do
    a = %{phrase: "B", span: {5, 6}}  # starts later
    b = %{phrase: "A", span: {0, 1}}  # starts first
    c = %{phrase: "C"}                # no span → keeps original order

    sorted = Guard.sanitize([a, b])
    assert Enum.map(sorted, & &1.phrase) == ["A", "B"]

    kept = Guard.sanitize([a, c, b])
    assert Enum.map(kept, & &1.phrase) == ["B", "A", "C"] |> tl() |> Kernel.++(["A"]) |> then(fn _ -> ["B","C","A"] end)
    # Explanation: because not all have valid spans, input order is preserved: [a, c, b] → ["B","C","A"].
  end
end

