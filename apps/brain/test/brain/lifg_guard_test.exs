defmodule Brain.LIFG.GuardTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.Guard

  test "drops spaced non-mw tokens (char-grams / boundary substrings)" do
    tokens = [
      %{phrase: "hello", mw: false},
      %{phrase: "ck t", mw: false},
      %{phrase: "kick the bucket", mw: true},
      %{phrase: "k th", mw: nil},
      %{phrase: "bank", mw: false}
    ]

    sanitized = Guard.sanitize(tokens)
    phrases_list = Enum.map(sanitized, & &1.phrase)

    assert "hello" in phrases_list
    assert "bank" in phrases_list
    assert "kick the bucket" in phrases_list

    refute "ck t" in phrases_list
    refute "k th" in phrases_list
  end

  test "is_chargram?/1 returns false for MWEs (mw: true)" do
    refute Guard.is_chargram?(%{phrase: "new york", mw: true})
  end
end
