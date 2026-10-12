defmodule Core.MWE.DiscoveryTest do
  use ExUnit.Case, async: true

  alias Core.{SemanticInput, TokenFilters}
  alias Core.MWE.{Discovery, Lexicon, Signatures}

  defp input(sentence, cells \\ []) do
    %SemanticInput{sentence: sentence, tokens: [], trace: [], active_cells: cells}
    |> TokenFilters.rebuild_word_ngrams(3)
  end

  test "a verified database-backed MWE is not relabelled as unverified" do
    si =
      input("please turn off the lights", [
        %{norm: "turn", pos: "verb"},
        %{norm: "off", pos: "particle"}
      ])
      |> Signatures.run(stage: :early)
      |> Lexicon.attach(mwe_lookup: fn _ ->
        [%{id: "turn off|verb|2", norm: "turn off", pos: "verb"}]
      end)
      |> Discovery.run()

    assert [%{phrase: "turn off", senses: [_]}] = si.mwe_lexicon
    refute Enum.any?(si.mwe_hypotheses, &(&1.phrase == "turn off"))
  end

  test "a curated expression missing from the dictionary is an unverified hypothesis" do
    si =
      input("please turn off the lights")
      |> Signatures.run(stage: :early)
      |> Lexicon.attach(mwe_lookup: fn _ -> [] end)
      |> Discovery.run()

    assert [%{phrase: "turn off", category: :curated_phrase, source: :mwe_discovery,
              status: :unverified, verified?: false, span: {1, 2}}] = si.mwe_hypotheses
    assert si.mwe_lexicon == []
    assert si.sense_candidates == %{}
  end

  test "known and unknown MWEs coexist without confusing their provenance" do
    si =
      input("turn off then phase out", [
        %{norm: "turn", pos: "verb"},
        %{norm: "off", pos: "particle"},
        %{norm: "phase", pos: "verb"},
        %{norm: "out", pos: "particle"}
      ])
      |> Signatures.run(stage: :early)
      |> Lexicon.attach(mwe_lookup: fn _ ->
        [%{id: "turn off|verb|2", norm: "turn off", pos: "verb"}]
      end)
      |> Discovery.run()

    assert Enum.any?(si.mwe_lexicon, &(&1.phrase == "turn off" and &1.span == {0, 1}))
    assert [%{phrase: "phase out", span: {3, 4}, verified?: false}] =
             si.mwe_hypotheses
    refute Enum.any?(si.mwe_hypotheses, &(&1.phrase == "turn off"))
    assert si.sense_candidates == %{}

    summary = Core.Response.Context.symbolic_frame(si)
    assert Enum.any?(summary.lexical.verified_mwes, &(&1.phrase == "turn off"))
    assert Enum.any?(summary.lexical.unverified_mwes, &(&1.phrase == "phase out"))
  end

  test "discovers a previously unseen phrasal verb from POS evidence" do
    si =
      input("we phase out", [
        %{norm: "phase", pos: "verb"},
        %{norm: "out", pos: "particle"}
      ])
      |> Lexicon.attach(mwe_lookup: fn _ -> [] end)
      |> Discovery.run()

    assert Enum.any?(si.mwe_hypotheses, fn h ->
      h.phrase == "phase out" and h.span == {1, 2} and
        h.category == :phrasal_verb and h.verified? == false
    end)
    assert si.mwe_lexicon == []
    assert si.sense_candidates == %{}
  end

  test "finds plausible noun compounds but rejects ordinary determiner-noun grammar" do
    si =
      input("the quantum garden", [
        %{norm: "the", pos: "determiner"},
        %{norm: "quantum", pos: "noun"},
        %{norm: "garden", pos: "noun"}
      ])
      |> Discovery.run()

    assert [%{phrase: "quantum garden", category: :compound_noun,
              span: {1, 2}}] = si.mwe_hypotheses
    refute Enum.any?(si.mwe_hypotheses, &(&1.phrase == "the quantum"))
  end

  test "a separated verb and particle emerge as grammatical MWE evidence, not a sense" do
    cells = [
      %{norm: "turn", pos: "verb"},
      %{norm: "the", pos: "determiner"},
      %{norm: "lights", pos: "noun"},
      %{norm: "off", pos: "particle"}
    ]

    si =
      input("turn the lights off", cells)
      |> Core.Syntax.Signatures.attach()
      |> Discovery.run()

    assert [%{
      phrase: "turn off",
      span: {0, 3},
      constituent_indices: [0, 3],
      particle_position: :separated,
      dictionary_check: :not_checked,
      category: :phrasal_verb,
      status: :unverified,
      verified?: false
    }] = si.mwe_hypotheses

    assert si.mwe_lexicon == []
    assert si.sense_candidates == %{}
    assert Enum.map(si.tokens, & &1.phrase) == ["turn", "the", "lights", "off"]
  end

  test "syntactic and adjacent POS evidence do not duplicate the same candidate" do
    cells = [%{norm: "phase", pos: "verb"}, %{norm: "out", pos: "particle"}]

    si =
      input("phase out", cells)
      |> Core.Syntax.Signatures.attach()
      |> Discovery.run()

    assert [%{
      phrase: "phase out",
      constituent_indices: [0, 1],
      particle_position: :adjacent,
      evidence: [:sentence_signature, :verb_particle_construction]
    }] = si.mwe_hypotheses
  end

  test "does not promote generic POS signatures into discovered lexical expressions" do
    si =
      input("open the file", [
        %{norm: "open", pos: "verb"},
        %{norm: "the", pos: "determiner"},
        %{norm: "file", pos: "noun"}
      ])
      |> Signatures.run(stage: :late)
      |> Discovery.run()

    assert si.mwe_hypotheses == []
    assert Enum.any?(si.mwe_matches, &(&1.phrase == "open the file"))
  end

  test "detects separate occurrences and respects sentence punctuation boundaries" do
    cells = [%{norm: "phase", pos: "verb"}, %{norm: "out", pos: "particle"}]

    repeated = input("phase out and phase out", cells) |> Discovery.run()
    assert Enum.map(repeated.mwe_hypotheses, & &1.span) == [{0, 1}, {3, 4}]

    crossed = input("phase. out", cells) |> Discovery.run()
    assert crossed.mwe_hypotheses == []
  end

  test "disabling discovery leaves all other semantic state untouched" do
    si = input("phase out", [%{norm: "phase", pos: "verb"}])
    assert Discovery.run(si, mwe_discovery: :off) == si

    result = Discovery.run(si)
    assert result.tokens == si.tokens
    assert result.mwe_lexicon == si.mwe_lexicon
    assert result.mwe_matches == si.mwe_matches
    assert result.sense_candidates == si.sense_candidates
  end

  test "hypotheses are exposed as explicitly unverified, bounded response evidence" do
    si =
      input("phase out", [%{norm: "phase", pos: "verb"}])
      |> Discovery.run()

    summary = Core.Response.Context.symbolic_frame(si)
    assert [%{phrase: "phase out", category: :phrasal_verb, verified?: false}] =
             summary.lexical.unverified_mwes
  end
end
