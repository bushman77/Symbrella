defmodule Core.MWE.LexiconTest do
  use ExUnit.Case, async: true

  alias Core.{SemanticInput, TokenFilters, TokenResolution}
  alias Core.MWE.{Lexicon, Signatures}

  defp input(text) do
    %SemanticInput{sentence: text, tokens: [], trace: []}
    |> TokenFilters.rebuild_word_ngrams(3)
  end

  defp verb_sense(phrase, sense \\ 2) do
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

  test "exact phrase lookup retains lexical evidence required for sense ranking" do
    rows = [
      %{
        id: "turn off|verb|2",
        norm: "turn off",
        pos: "verb",
        definition: "To deactivate a light.",
        example: "Turn off the machine.",
        synonyms: ["switch off", "deactivate"],
        semantic_atoms: ["tag:transitive"],
        status: "inactive",
        gram_function: ["transitive"],
        activation: 0.0
      }
    ]

    by_phrase = Lexicon.lookup_senses(["turn off"], mwe_lookup: fn terms ->
      assert terms == ["turn off"]
      rows
    end)

    assert %{"turn off" => [%{id: "turn off|verb|2"} = sense]} = by_phrase
    assert sense.example == "Turn off the machine."
    assert "deactivate" in sense.synonyms
    assert sense.semantic_atoms == ["tag:transitive"]
    assert sense.status == "inactive"
    assert sense.gram_function == ["transitive"]
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

      assert [%{kind: :phrase, n: ^n, lexical_pos: ["phrase"]} = cover] =
               Enum.filter(result.token_cover, &(&1.kind == :phrase))
      assert cover.span == {0, String.length(phrase)}
      assert cover.token_span == {0, n - 1}
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
    assert Enum.all?(result.token_cover, &(Map.get(&1, :lexical_pos) in [nil, []]))
  end

  test "lexical lookup never joins expressions across punctuation boundaries" do
    for sentence <- ["turn. off", "turn! off", "turn, off", "turn? off", "turn ; off"] do
      parent = self()

      si =
        input(sentence)
        |> Lexicon.attach(
          mwe_lookup: fn terms ->
            send(parent, {:terms, terms})
            [verb_sense("turn off")]
          end
        )
        |> TokenResolution.resolve()

      assert_receive {:terms, terms}
      refute "turn off" in terms
      assert si.mwe_lexicon == []
      assert Enum.all?(si.token_cover, &(&1.kind == :token))
    end
  end

  test "canonical source punctuation cannot manufacture dictionary MWE matches" do
    for sentence <- ["turn. off", "turn, off", "turn—off", "turn… off", "turn\noff"] do
      si = Core.Pipeline.Input.prepare(sentence, max_wordgram_n: 3)
      parent = self()

      result =
        si
        |> Signatures.run(stage: :early)
        |> Lexicon.attach(
          mwe_lookup: fn terms ->
            send(parent, {:canonical_mwe_terms, terms})
            [verb_sense("turn off")]
          end
        )

      assert_receive {:canonical_mwe_terms, terms}
      refute "turn off" in terms
      refute Enum.any?(List.wrap(result.mwe_matches), &(&1.phrase == "turn off"))
      assert result.mwe_lexicon == []
      assert result.tokens == si.tokens
    end
  end

  test "canonical MWE retains attested phrase and original UTF-8 token spans" do
    si = Core.Pipeline.Input.prepare("Café turn  off.", max_wordgram_n: 3)
    parent = self()

    result =
      si
      |> Signatures.run(stage: :early)
      |> Lexicon.attach(
        mwe_lookup: fn terms ->
          send(parent, {:canonical_valid_terms, terms})
          [verb_sense("turn off")]
        end
      )

    assert_receive {:canonical_valid_terms, terms}
    assert "turn off" in terms
    assert Enum.any?(result.mwe_matches, &(&1.phrase == "turn off"))
    assert [%{span: {1, 2}, phrase: "turn off"}] = result.mwe_lexicon
    assert result.tokens == si.tokens
    assert Enum.map(result.tokens, & &1.span) == Enum.map(si.tokens, & &1.span)
  end

  test "a valid expression may end in punctuation" do
    si =
      input("please turn off.")
      |> Lexicon.attach(mwe_lookup: fn _ -> [verb_sense("turn off")] end)
      |> TokenResolution.resolve()

    assert [%{phrase: "turn off", span: {1, 2}}] = si.mwe_lexicon
    assert Enum.any?(si.token_cover, fn cover ->
             cover.kind == :phrase and cover.token_span == {1, 2} and
               cover.dictionary_backed?
           end)
  end

  test "lookup can be disabled without touching input" do
    si = input("turn off")
    assert Lexicon.attach(si, mwe_lexicon: :off) == si
  end
end
