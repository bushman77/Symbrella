defmodule Core.Syntax.SentenceCompetitionTest do
  use ExUnit.Case, async: true

  alias Core.{SemanticInput, TokenFilters, TokenResolution}
  alias Core.MWE.{Decisions, Lexicon}
  alias Core.Syntax.{SentenceCompetition, Signatures}

  @sense_one %{
    id: "what do you know|phrase|1",
    norm: "what do you know", pos: "phrase",
    definition: "(colloquial, sarcastic) What a surprise; guess what.",
    example: "Who would have thunk it?", activation: 0.0
  }
  @sense_two %{
    id: "what do you know|phrase|2",
    norm: "what do you know", pos: "phrase",
    definition: "Used other than figuratively: see what, do, you, know; synonym of you don't know what you're talking about.",
    example: "Well, what do you know?", activation: 0.0
  }

  @lexical_cells [
    %{norm: "what", pos: "pronoun"},
    %{norm: "do", pos: "auxiliary"},
    %{norm: "did", pos: "auxiliary"},
    %{norm: "you", pos: "pronoun"},
    %{norm: "she", pos: "pronoun"},
    %{norm: "know", pos: "verb"},
    %{norm: "say", pos: "verb"},
    %{norm: "about", pos: "preposition"},
    %{norm: "japan's", pos: "proper_noun"},
    %{norm: "railgun", pos: "noun"},
    %{norm: "development", pos: "noun"},
    %{norm: "experiment", pos: "noun"},
    %{norm: "the", pos: "determiner"}
  ]

  defp input(sentence, senses \\ [@sense_one, @sense_two], cells \\ @lexical_cells) do
    lookup = fn phrases ->
      Enum.filter(senses, &(Map.get(&1, :norm) in phrases))
    end

    %SemanticInput{sentence: sentence, active_cells: cells, intent: :ask,
                   conversation_act: :question}
    |> TokenFilters.rebuild_word_ngrams(3)
    |> Lexicon.attach(mwe_lookup: lookup)
    |> Signatures.attach()
    |> SentenceCompetition.attach()
  end

  defp make_phrase(span, phrase, id) do
    %{
      token_index: 12, token_span: span, phrase: phrase, lemma: phrase,
      id: id, pos: "phrase", source: :mwe_lexicon, score: 0.529
    }
  end

  test "complete WH auxiliary clause outranks an overlapping ambiguous dictionary phrase" do
    si = input("What do you know about Japan's railgun development?")

    assert [%{id: :wh_aux_topic_question, roles: %{
              wh: 0, auxiliary: 1, subject: 2, predicate: 3, preposition: 4
            }}] = Enum.filter(si.sentence_signatures, &(&1.id == :wh_aux_topic_question))

    assert %{
             rejected_spans: [{0, 3}],
             comparisons: [%{
               phrase: "what do you know", winner: :compositional_clause,
               frame_id: :wh_aux_topic_question,
               discrimination_margin: margin
             }]
           } = si.sentence_competition
    assert margin >= 0.16

    assert :composition_wins == hd(si.trace).decision
    assert SentenceCompetition.reject_phrase?(si, {0, 3})

    covered = TokenResolution.resolve(si)

    refute Enum.any?(covered.token_cover, fn item ->
             item.kind == :phrase and item.token_span == {0, 3}
           end)

    # A stale/direct virtual MWE choice cannot bypass the sentence verdict
    # when Core.MWE.Decisions later constructs the canonical semantic view.
    word_choices =
      covered.tokens
      |> Enum.map(fn t ->
        %{token_index: t.index, lemma: t.phrase,
          id: "#{t.phrase}|noun|0", score: 0.2}
      end)

    with_phrase =
      %{covered |
        lifg_choices: word_choices,
        mwe_choices: [make_phrase({0, 3}, "what do you know",
                                  "what do you know|phrase|2")]
      }

    out = Decisions.attach(with_phrase)

    assert out.semantic_choices == word_choices
    assert out.mwe_choices == with_phrase.mwe_choices
    assert out.lifg_choices == word_choices
  end

  test "rhetorical MWE without complete topic frame remains available" do
    for sentence <- ["What do you know?", "Well, what do you know?"] do
      si = input(sentence)

      assert si.sentence_competition.rejected_spans == []
      refute Enum.any?(si.sentence_signatures, &(&1.id == :wh_aux_topic_question))

      covered = TokenResolution.resolve(si)

      assert Enum.any?(covered.token_cover, fn item ->
               item.kind == :phrase and
                 String.trim_trailing(String.downcase(item.phrase), "?") == "what do you know"
             end)
    end
  end

  test "same structural decision works for another verb and subject" do
    sense = %{
      id: "what did she say|phrase|1", norm: "what did she say", pos: "phrase",
      definition: "An exclamation of surprise", example: "What did she say?"
    }

    si = input("What did she say about the experiment?", [sense])

    assert Enum.any?(si.sentence_signatures, &(&1.id == :wh_aux_topic_question))
    assert [%{span: {0, 3}, winner: :compositional_clause}] =
             si.sentence_competition.comparisons
    assert SentenceCompetition.reject_phrase?(si, {0, 3})
  end

  test "structural hypothesis cannot override a dictionary MWE without a lexical verb" do
    no_verb = Enum.reject(@lexical_cells, &(&1.norm == "know"))
    si = input("What do you know about Japan's railgun development?",
               [@sense_one, @sense_two], no_verb)

    refute Enum.any?(si.sentence_signatures, &(&1.id == :wh_aux_topic_question))
    assert si.sentence_competition.rejected_spans == []
    assert Enum.any?(TokenResolution.resolve(si).token_cover,
             &(&1.kind == :phrase and &1.token_span == {0, 3}))
  end

  test "unrelated dictionary MWE elsewhere in a question is not suppressed" do
    other = %{
      id: "turn off|verb|1", norm: "turn off", pos: "verb",
      definition: "To deactivate a device", example: "Turn off the device"
    }

    si =
      input("What do you know about how to turn off the lights?",
        [@sense_one, @sense_two, other],
        @lexical_cells ++ [
          %{norm: "turn", pos: "verb"},
          %{norm: "off", pos: "particle"},
          %{norm: "lights", pos: "noun"}
        ])

    assert SentenceCompetition.reject_phrase?(si, {0, 3})

    other_span =
      si.mwe_lexicon
      |> Enum.find(&(&1.phrase == "turn off"))
      |> Map.fetch!(:span)

    refute SentenceCompetition.reject_phrase?(si, other_span)
  end

  test "the competition is bounded and preserves dictionary and candidate objects" do
    si = input("What do you know about Japan's railgun development?")
    prior_units = si.mwe_lexicon
    prior_cells = si.active_cells

    again = SentenceCompetition.attach(si)

    assert again.mwe_lexicon == prior_units
    assert again.active_cells == prior_cells
    assert again.sentence_competition == si.sentence_competition
    assert :ok = SemanticInput.validate_contract(again)
  end
end
