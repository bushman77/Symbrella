defmodule Brain.LIFG.Stage1.PredicatePosBiasTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage1.PosBias

  defp bias(words, index, pos) do
    tokens =
      words
      |> Enum.with_index()
      |> Enum.map(fn {phrase, i} ->
        %{phrase: phrase, index: i}
      end)

    ctx = %{si: %{tokens: tokens}}
    tok = Enum.at(tokens, index)

    PosBias.window(ctx, tok, pos)
  end

  test "coordinated change-of-state verb favors verb" do
    words = ~w(i went to the doctor and got sick)

    assert bias(words, 6, "verb") > bias(words, 6, "noun")
    assert bias(words, 6, "verb") > bias(words, 6, "proper_noun")
  end

  test "predicate adjective follows change-of-state verb" do
    words = ~w(i went to the doctor and got sick)

    assert bias(words, 7, "adjective") >
             bias(words, 7, "noun")
  end

  test "works with another change-of-state construction" do
    words = ~w(he stumbled and became dizzy)

    assert bias(words, 3, "verb") > bias(words, 3, "noun")
    assert bias(words, 4, "adjective") > bias(words, 4, "noun")
  end

  test "does not blindly prefer verbs after and" do
    words = ~w(the doctor and nurse arrived)

    assert bias(words, 3, "verb") == 0.0
  end

  test "does not boost adjectives across a determiner" do
    words = ~w(i got a new car)

    assert bias(words, 3, "adjective") == 0.0
  end

  test "preserves the existing infinitive construction" do
    words = ~w(to get some cash)

    assert bias(words, 1, "verb") == 0.30
    assert bias(words, 1, "noun") == -0.15
  end
end
