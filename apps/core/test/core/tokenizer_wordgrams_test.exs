defmodule Core.TokenWordgramsTest do
  use ExUnit.Case, async: true
  alias Core.Token

  test "word-grams only; unigrams/bigrams/trigrams; no char fragments" do
    si = Token.tokenize("kick the ball hard", max_wordgram_n: 3)
    tokens = si.tokens
    phrases = tokens |> Enum.map(& &1.phrase) |> MapSet.new()

    expected =
      MapSet.new([
        # unigrams
        "kick",
        "the",
        "ball",
        "hard",
        # bigrams
        "kick the",
        "the ball",
        "ball hard",
        # trigrams
        "kick the ball",
        "the ball hard"
      ])

    assert phrases == expected

    # ensure no obvious char fragments like "Ki", "ck", etc.
    refute Enum.any?(tokens, fn t ->
             String.length(t.phrase) < 2 and String.match?(t.phrase, ~r/^\p{L}+$/u)
           end)
  end

  test "spans are word-indexed and end-exclusive; ordering is by start then longest→shortest" do
    si = Token.tokenize("hello there friend", max_wordgram_n: 2)
    tokens = si.tokens

    t_hello = Enum.find(tokens, &(&1.phrase == "hello"))
    t_big = Enum.find(tokens, &(&1.phrase == "hello there"))

    assert t_hello.span == {0, 1}
    assert t_hello.n == 1
    refute t_hello.mw

    assert t_big.span == {0, 2}
    assert t_big.n == 2
    assert t_big.mw

    # verify starts are non-decreasing
    starts = Enum.map(tokens, fn t -> elem(t.span, 0) end)
    assert starts == Enum.sort(starts)

    # verify per-start longest first
    grouped =
      tokens
      |> Enum.group_by(fn t -> elem(t.span, 0) end)
      |> Enum.into(%{}, fn {k, vs} -> {k, Enum.map(vs, & &1.n)} end)

    # for "hello there friend" with max_n=2:
    # start 0: [2, 1]; start 1: [2, 1]; start 2: [1]
    assert grouped[0] == [2, 1]
    assert grouped[1] == [2, 1]
    assert grouped[2] == [1]
  end
end
