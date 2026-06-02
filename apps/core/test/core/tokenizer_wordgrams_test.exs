defmodule Core.TokenWordgramsTest do
  use ExUnit.Case, async: true
  alias Core.Token

  test "tokens are unigrams and word-grams are unconfirmed phrase candidates" do
    si = Token.tokenize("kick the ball hard", max_wordgram_n: 3)
    tokens = si.tokens
    phrases = tokens |> Enum.map(& &1.phrase) |> MapSet.new()

    expected =
      MapSet.new([
        "kick",
        "the",
        "ball",
        "hard"
      ])

    assert phrases == expected
    assert Enum.all?(tokens, &(&1.n == 1 and &1.mw == false))

    candidate_phrases = si.phrase_candidates |> Enum.map(& &1.phrase) |> MapSet.new()

    assert candidate_phrases ==
             MapSet.new([
               "kick the",
               "the ball",
               "ball hard",
               "kick the ball",
               "the ball hard"
             ])

    assert Enum.all?(si.phrase_candidates, &(&1.mw == false and &1.confirmed? == false))

    # ensure no obvious char fragments like "Ki", "ck", etc.
    refute Enum.any?(tokens, fn t ->
             String.length(t.phrase) < 2 and String.match?(t.phrase, ~r/^\p{L}+$/u)
           end)
  end

  test "token spans are word-indexed and candidates keep phrase spans" do
    si = Token.tokenize("hello there friend", max_wordgram_n: 2)
    tokens = si.tokens

    t_hello = Enum.find(tokens, &(&1.phrase == "hello"))
    c_big = Enum.find(si.phrase_candidates, &(&1.phrase == "hello there"))

    assert t_hello.span == {0, 1}
    assert t_hello.n == 1
    refute t_hello.mw

    assert c_big.span == {0, 2}
    assert c_big.n == 2
    refute c_big.mw
    refute c_big.confirmed?

    # verify starts are non-decreasing
    starts = Enum.map(tokens, fn t -> elem(t.span, 0) end)
    assert starts == Enum.sort(starts)

    # verify phrase candidates are still grouped per start
    grouped =
      si.phrase_candidates
      |> Enum.group_by(fn t -> elem(t.span, 0) end)
      |> Enum.into(%{}, fn {k, vs} -> {k, Enum.map(vs, & &1.n)} end)

    # for "hello there friend" with max_n=2:
    # start 0: [2]; start 1: [2]
    assert grouped[0] == [2]
    assert grouped[1] == [2]
    refute Map.has_key?(grouped, 2)
  end
end
