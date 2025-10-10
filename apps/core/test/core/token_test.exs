defmodule Core.TokenTest do
  use ExUnit.Case, async: true
  alias Core.Token

  test "word spans emit correctly (longest→shortest per start)" do
    si = Token.tokenize("hello there", max_wordgram_n: 2, span_mode: :words)
    # Expect two starts (0, 1) with n-grams: {0,2} then {0,1}, then {1,2}
    spans = Enum.map(si.tokens, & &1.span)
    assert spans == [{0, 2}, {0, 1}, {1, 2}]
  end

  test "char spans convert precisely and match slices" do
    si_words = Token.tokenize("hello there", max_wordgram_n: 2, span_mode: :words)
    si_chars = Token.to_char_spans(si_words)

    find = fn phrase, n ->
      Enum.find(si_chars.tokens, fn t -> t.phrase == phrase and t.n == n end)
    end

    assert find.("hello there", 2).span == {0, 11}
    assert find.("hello", 1).span == {0, 5}
    assert find.("there", 1).span == {6, 11}

    assert {:ok, _} = Token.check_span_invariants(si_chars)
  end
end
