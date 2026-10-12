defmodule Core.MWE.LexiconTest do
  use ExUnit.Case, async: true

  alias Core.{SemanticInput, TokenFilters, TokenResolution}
  alias Core.MWE.{Lexicon, Signatures}

  defp input(text) do
    %SemanticInput{sentence: text, tokens: [], trace: []}
    |> TokenFilters.rebuild_word_ngrams(3)
  end

  defp verb_sense(phrase, sense \ 2) do
    %{
      id: "#{phrase}|verb|#{sense}",
      norm: phrase,
      pos: "verb",
      definition: "To deactivate",
      gram_function: ["transitive"],
      activation: 0.3
    }
  end

  test "batched lookup preserves non-phrase POS, sense IDs, and source words" do
    original = input("I need to turn off the lights.")
    parent = self()

    resolved =
      original
      |> Signatures.run(stage: :early)
      |> Lexicon.attach(
        mwe_lookup: fn terms ->
          send(parent, {:lookup, terms})
          Enum.map(1..8, &verb_sense("turn off", &1))
        end
      )
      |> TokenResolution.resolve()

    assert_receive {:lookup, terms}
    assert "turn off" in terms
    assert Enum.map(resolved.tokens, & &1.phrase) == Enum.map(original.tokens, & &1.phrase)

    assert [unit] = Enum.filter(resolved.mwe_lexicon, &(&1.phrase == "turn off"))
    assert unit.span == {3, 4}
    assert unit.lexical_pos == ["verb"]
    assert length(unit.senses) == 8

    assert [cover] = Enum.filter(resolved.token_cover, &(&1.phrase == "turn off"))
    assert cover.token_span == {3, 4}
    assert cover.lexical_pos == ["verb"]
    assert "turn off|verb|2" in cover.sense_ids
    assert cover.pos_patterns != []
  end

  test "the same phrase at two positions keeps two independent spans" do
    result =
      input("turn off and turn off")
      |> Lexicon.attach(mwe_lookup: fn _ -> [verb_sense("turn off")] end)
      |> TokenResolution.resolve()

    assert Enum.map(result.mwe_lexicon, & &1.span) == [{0, 1}, {3, 4}]

    assert Enum.map(
             Enum.filter(result.token_cover, &(&1.kind == :phrase)),
             & &1.token_span
           ) == [{0, 1}, {3, 4}]
  end

  test "known four- and five-word MWEs resolve without increasing tokenizer n-grams" do
    for {phrase, n} <- [{"on the other hand", 4}, {"as far as i know", 5}] do
      result =
        input(phrase)
        |> Lexicon.attach(mwe_lookup: fn _ ->
          [%{id: "#{phrase}|phrase|1", norm: phrase, pos: "phrase"}]
        end)
        |> TokenResolution.resolve()

      assert [%{kind: :phrase, n: ^n, lexical_pos: ["phrase"]}] =
               Enum.filter(result.token_cover, &(&1.kind == :phrase))
      assert length(result.tokens) == n
    end
  end

  test "a DB miss preserves the lightweight structural MWE evidence" do
    result =
      input("turn off")
      |> Signatures.run(stage: :early)
      |> Lexicon.attach(mwe_lookup: fn _ -> [] end)
      |> TokenResolution.resolve()

    assert result.mwe_lexicon == []
    assert Enum.any?(result.mwe_matches, &(&1.phrase == "turn off"))
    assert Enum.any?(result.token_cover, &(&1.kind == :phrase))
    assert Enum.all?(result.token_cover, &(&1.lexical_pos in [nil, []]))
  end

  test "lookup can be disabled without touching input" do
    si = input("turn off")
    assert Lexicon.attach(si, mwe_lexicon: :off) == si
  end
end
