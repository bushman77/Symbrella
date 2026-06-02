defmodule Core.TokenTest do
  use ExUnit.Case, async: true
  alias Core.Token

  test "word spans emit correctly (longest→shortest per start)" do
    si = Token.tokenize("hello there", max_wordgram_n: 2, span_mode: :words)
    # Expect base tokens in `tokens`; unconfirmed n-grams live in `phrase_candidates`.
    spans = Enum.map(si.tokens, & &1.span)
    assert spans == [{0, 1}, {1, 2}]
    assert Enum.map(si.phrase_candidates, & &1.span) == [{0, 2}]
  end

  test "char spans convert precisely and match slices" do
    si_words = Token.tokenize("hello there", max_wordgram_n: 2, span_mode: :words)
    si_chars = Token.to_char_spans(si_words)

    find = fn phrase, n ->
      Enum.find(si_chars.tokens, fn t -> t.phrase == phrase and t.n == n end)
    end

    assert find.("hello", 1).span == {0, 5}
    assert find.("there", 1).span == {6, 11}
    assert Enum.find(si_chars.phrase_candidates, &(&1.phrase == "hello there")).span == {0, 2}

    assert {:ok, _} = Token.check_span_invariants(si_chars)
  end
end
