defmodule Core.TokenResolutionTest do
  use ExUnit.Case, async: true

  alias Core.{SemanticInput, TokenFilters, TokenResolution}
  alias Core.MWE.Signatures

  test "resolves POS signature phrase into a non-overlapping token cover" do
    si =
      %SemanticInput{sentence: "open the file now", tokens: [], trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)
      |> Map.put(:active_cells, [
        %{norm: "open", pos: "verb"},
        %{norm: "the", pos: "determiner"},
        %{norm: "file", pos: "noun"},
        %{norm: "now", pos: "adverb"}
      ])
      |> Signatures.run(stage: :late)
      |> TokenResolution.resolve()

    assert [%{phrase: "open the file", kind: :phrase}, %{phrase: "now", kind: :token}] =
             si.token_cover

    assert si.resolved_tokens == si.token_cover
    assert [%{stage: :token_resolution, decision: :resolved, meta: meta} | _] = si.trace
    assert meta.cover_count == 2
    assert meta.phrase_count == 1
  end

  test "falls back to base tokens when no phrase signature exists" do
    si =
      %SemanticInput{sentence: "blue quiet lamp", tokens: [], trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)
      |> TokenResolution.resolve()

    assert Enum.map(si.token_cover, & &1.phrase) == ["blue", "quiet", "lamp"]
    assert Enum.all?(si.token_cover, &(&1.kind == :token))
    assert [%{stage: :token_resolution, decision: :base_tokens} | _] = si.trace
  end

  test "verified two-word dictionary sense outranks overlapping five-word POS pattern" do
    si =
      %SemanticInput{sentence: "turn off the blue lights", tokens: [], trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)
      |> Core.MWE.Lexicon.attach(
        mwe_lookup: fn _ ->
          [%{id: "turn off|verb|2", norm: "turn off", pos: "verb"}]
        end
      )
      |> Map.update!(:mwe_matches, fn matches ->
        matches ++
          [%{
            span: {0, 4},
            size: 5,
            phrase: "turn off the blue lights",
            kind: :pos,
            pos: ["verb", "particle", "determiner", "adjective", "noun"]
          }]
      end)
      |> TokenResolution.resolve()

    assert [%{phrase: "turn off", dictionary_backed?: true, token_span: {0, 1}}] =
             Enum.filter(si.token_cover, &(&1.kind == :phrase))

    assert Enum.map(Enum.filter(si.token_cover, &(&1.kind == :token)), & &1.phrase) ==
             ["the", "blue", "lights"]
  end

  test "longer verified expression wins over an overlapping shorter verified expression" do
    si =
      %SemanticInput{sentence: "turn off the lights", tokens: [], trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)
      |> Core.MWE.Lexicon.attach(
        mwe_lookup: fn _ ->
          [
            %{id: "turn off|verb|2", norm: "turn off", pos: "verb"},
            %{id: "turn off the|verb|1", norm: "turn off the", pos: "verb"}
          ]
        end
      )
      |> TokenResolution.resolve()

    assert [%{phrase: "turn off the", dictionary_backed?: true, token_span: {0, 2}}] =
             Enum.filter(si.token_cover, &(&1.kind == :phrase))
  end

  test "sentence grammar blocks false verb MWE over a noun object and particle" do
    # Production smoke input: the dictionary has lights off|verb|1, but
    # here "lights" is the noun object of an earlier verb "turn".
    cells = [
      %{norm: "turn", pos: "verb"},
      %{norm: "the", pos: "determiner"},
      %{norm: "lights", pos: "noun"},
      %{norm: "lights", pos: "verb"},
      %{norm: "off", pos: "adverb"},
      %{norm: "off", pos: "preposition"}
    ]

    si =
      %SemanticInput{sentence: "Turn the lights off.", active_cells: cells, trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)
      |> Core.MWE.Lexicon.attach(
        mwe_lookup: fn _ ->
          [%{id: "lights off|verb|1", norm: "lights off", pos: "verb"}]
        end
      )
      |> Core.Syntax.Signatures.attach()
      |> TokenResolution.resolve()

    assert Enum.any?(si.sentence_signatures, fn
             %{id: :verb_particle_construction,
               roles: %{verb: 0, object: 2, particle: 3}} -> true
             _ -> false
           end)

    # The dictionary entry remains available; only its incompatible cover is
    # suppressed, so it cannot displace the independently supported object.
    assert Enum.any?(si.mwe_lexicon, &(&1.phrase == "lights off"))
    refute Enum.any?(si.token_cover, fn
             %{kind: :phrase, token_span: {2, 3}} -> true
             _ -> false
           end)

    {lifg_input, virtual} = Core.MWE.LIFGBridge.prepare(si)
    assert virtual == %{}
    assert lifg_input.tokens == si.tokens
  end

  test "a verb MWE is still available in its own verbal construction" do
    cells = [
      %{norm: "lights", pos: "verb"},
      %{norm: "off", pos: "adverb"},
      %{norm: "rocket", pos: "noun"},
      %{norm: "the", pos: "determiner"}
    ]

    si =
      %SemanticInput{sentence: "He lights off the rocket.", active_cells: cells, trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)
      |> Core.MWE.Lexicon.attach(
        mwe_lookup: fn _ ->
          [%{id: "lights off|verb|1", norm: "lights off", pos: "verb"}]
        end
      )
      |> Core.Syntax.Signatures.attach()
      |> TokenResolution.resolve()

    assert Enum.any?(si.token_cover, fn
             %{kind: :phrase, token_span: {1, 2}, dictionary_backed?: true} -> true
             _ -> false
           end)

    {_lifg_input, virtual} = Core.MWE.LIFGBridge.prepare(si)
    assert map_size(virtual) == 1
  end

  test "token cover does not treat an end-exclusive span as a length at punctuation" do
    si =
      %SemanticInput{sentence: "Lights off.", trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)
      |> Core.MWE.Lexicon.attach(
        mwe_lookup: fn _ ->
          [%{id: "lights off|verb|1", norm: "lights off", pos: "verb"}]
        end
      )
      |> TokenResolution.resolve()

    assert [%{kind: :phrase, token_span: {0, 1}, span: {0, 11}}] =
             Enum.filter(si.token_cover, &(&1.kind == :phrase))
  end

  test "curated lexical rules do not falsely claim dictionary priority" do
    si =
      %SemanticInput{sentence: "turn off the blue lights", tokens: [], trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)
      |> Signatures.run(stage: :early)
      |> Map.update!(:mwe_matches, fn matches ->
        matches ++ [%{span: {0, 4}, size: 5, phrase: "turn off the blue lights", kind: :pos}]
      end)
      |> TokenResolution.resolve()

    assert [%{phrase: "turn off the blue lights", dictionary_backed?: false}] =
             Enum.filter(si.token_cover, &(&1.kind == :phrase))
  end

  test "token resolution rejects externally supplied cross-sentence POS matches" do
    original =
      %SemanticInput{sentence: "turn. off", tokens: [], trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)

    result =
      %{original | mwe_matches: [
        %{span: {0, 1}, size: 2, phrase: "turn. off", kind: :pos}
      ]}
      |> TokenResolution.resolve()

    assert Enum.all?(result.token_cover, &(&1.kind == :token))
  end

  test "POS signatures do not join two sentences" do
    si =
      %SemanticInput{sentence: "turn. off", tokens: [], trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)
      |> Map.put(:active_cells, [
        %{norm: "turn", pos: "verb"},
        %{norm: "off", pos: "particle"}
      ])
      |> Signatures.run(stage: :late)
      |> TokenResolution.resolve()

    assert Enum.all?(si.token_cover, &(&1.kind == :token))
    assert Enum.empty?(Enum.filter(List.wrap(si.mwe_matches), &(&1.span == {0, 1})))
  end

  test "can be disabled with opts" do
    original =
      %SemanticInput{sentence: "open the file", tokens: [], trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)

    si = TokenResolution.resolve(original, token_resolution: :off)

    # Disabled means no transformation; SemanticInput defaults resolved_tokens to [].
    assert si == original
    assert si.token_cover == nil
    assert si.resolved_tokens == []
    refute Enum.any?(si.trace, &(&1.stage == :token_resolution))
  end
end
