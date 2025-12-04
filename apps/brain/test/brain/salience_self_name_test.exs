# apps/brain/test/brain/salience_self_name_test.exs
defmodule Brain.SalienceSelfNameTest do
  use ExUnit.Case, async: true

  alias Brain.Salience.SelfName

  test "no terms configured returns :no_terms" do
    r = SelfName.detect("hey there", terms: [])
    assert r.hit? == false
    assert r.reason == :no_terms
    assert r.score == 0.0
    assert r.matches == []
  end

  test "detects self-name from sentence (case + punctuation tolerant)" do
    r = SelfName.detect("Hey, Bradley!", terms: ["bradley"], telemetry: false)
    assert r.hit? == true
    assert r.reason == :hit
    assert r.score >= 0.85
    assert [%{term: "bradley", at: _i, surface: _}] = r.matches
  end

  test "detects self-name from token list" do
    tokens = [
      %{index: 0, phrase: "good"},
      %{index: 1, phrase: "morning"},
      %{index: 2, phrase: "Symbrella"}
    ]

    r = SelfName.detect(tokens, terms: ["symbrella"], telemetry: false)
    assert r.hit? == true
    assert r.score >= 0.85
    assert Enum.any?(r.matches, fn m -> m.term == "symbrella" end)
  end

  test "detects multiword name via ngram match" do
    r =
      SelfName.detect("hi Bradley Smith how are you", terms: ["bradley smith"], telemetry: false)

    assert r.hit? == true
    assert r.score >= 0.95
    assert Enum.any?(r.matches, fn m -> m.term == "bradley smith" end)
  end

  test "does not match substrings (no 'brad' inside 'broad')" do
    r = SelfName.detect("a broad example", terms: ["brad"], telemetry: false)
    assert r.hit? == false
    assert r.reason == :miss
    assert r.matches == []
  end
end
