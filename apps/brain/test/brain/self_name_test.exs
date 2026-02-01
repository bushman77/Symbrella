defmodule Brain.SelfNameTest do
  use ExUnit.Case, async: true

  alias Brain.SelfName

  test "detects a single-token name (case-insensitive)" do
    tokens = [
      %{index: 0, phrase: "good"},
      %{index: 1, phrase: "morning"},
      %{index: 2, phrase: "Bradley"},
      %{index: 3, phrase: "!"}
    ]

    res = SelfName.detect(tokens, nil, names: ["bradley"])
    assert res.hit? == true
    assert res.score == 1.0
    assert res.token_indexes == [2]
    assert [%{name: "bradley", token_indexes: [2]}] = res.matches
  end

  test "detects a multi-token name phrase" do
    tokens = [
      %{index: 0, phrase: "hey"},
      %{index: 1, phrase: "bradley"},
      %{index: 2, phrase: "smith"},
      %{index: 3, phrase: "?"}
    ]

    res = SelfName.detect(tokens, nil, names: ["Bradley Smith"])
    assert res.hit? == true
    assert res.token_indexes == [1, 2]

    assert [%{name: "Bradley Smith", token_indexes: [1, 2], span: {start, stop}}] = res.matches
    assert stop - start == 2
  end

  test "does not match substrings" do
    tokens = [%{index: 0, phrase: "bradleyish"}]
    res = SelfName.detect(tokens, nil, names: ["bradley"])
    assert res.hit? == false
    assert res.token_indexes == []
    assert res.matches == []
  end

  test "ignores punctuation-only tokens safely" do
    tokens = [
      %{index: 0, phrase: ","},
      %{index: 1, phrase: "Bradley"},
      %{index: 2, phrase: "."}
    ]

    res = SelfName.detect(tokens, nil, names: ["bradley"])
    assert res.hit? == true
    assert res.token_indexes == [1]
  end
end
