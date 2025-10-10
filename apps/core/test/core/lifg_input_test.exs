defmodule Core.LIFG.InputTest do
  use ExUnit.Case, async: true

  alias Core.LIFG.Input
  alias Core.Token
  alias Core.SemanticInput

  @doc """
  Verifies that Core.LIFG.Input.tokenize/1 (binary) produces character-index spans
  that exactly slice the normalized sentence, and that ordering is:
    per start index → longest → shortest.
  """
  test "binary path: char spans match slices; longest→shortest per start" do
    sentence = "hello there"

    # tokenize/1 (binary) returns a list of %Core.Token{}
    tokens = Input.tokenize(sentence)

    # Expect order: {0,11} "hello there", {0,5} "hello", {6,11} "there"
    assert Enum.map(tokens, &{&1.phrase, &1.span, &1.n}) == [
             {"hello there", {0, 11}, 2},
             {"hello", {0, 5}, 1},
             {"there", {6, 11}, 1}
           ]

    # Check invariant via Core.Token.check_span_invariants/1
    si = %SemanticInput{sentence: sentence, tokens: tokens}
    assert {:ok, _} = Token.check_span_invariants(si)
  end

  @doc """
  Verifies that Core.LIFG.Input.tokenize/1 (SI) returns an updated %SemanticInput{}
  whose tokens use boundary-aligned character spans and pass the invariant check.
  """
  test "SI path: returns SI with boundary-aligned char spans and invariant OK" do
    si_in = %SemanticInput{sentence: "hello there"}
    si_out = Input.tokenize(si_in)

    # Invariant must hold for the returned SI
    assert {:ok, _} = Token.check_span_invariants(si_out)

    # Same expected ordering and spans as the binary path
    assert Enum.map(si_out.tokens, &{&1.phrase, &1.span}) == [
             {"hello there", {0, 11}},
             {"hello", {0, 5}},
             {"there", {6, 11}}
           ]
  end
end
