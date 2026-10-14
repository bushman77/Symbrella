defmodule Core.Semantic.SentenceMeaningTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.SentenceMeaning
  alias Core.Comprehension.Summary
  alias Core.SemanticInput

  @question "What do you know about Japan's railgun development?"

  defp input do
    words = ["What", "do", "you", "know", "about", "Japan's", "railgun", "development?"]

    roles = %{wh: 0, auxiliary: 1, subject: 2, predicate: 3, preposition: 4}

    %SemanticInput{
      sentence: @question,
      source: :test,
      trace: [],
      intent: :ask,
      tokens:
        words
        |> Enum.with_index()
        |> Enum.map(fn {word, index} ->
          %{index: index, phrase: word, n: 1, mw: false}
        end),
      sentence_signatures: [
        %{
          id: :wh_aux_topic_question,
          span: {0, 7},
          roles: roles,
          evidence: [:wh_fronted, :auxiliary_inversion, :explicit_prepositional_topic]
        },
        %{
          id: :possessive_noun_phrase,
          span: {5, 7},
          roles: %{possessor: 5, modifier: 6, head: 7},
          surface_possessor: "Japan"
        }
      ],
      atl_slate: %{
        constituents: [
          %{
            id: :wh_aux_topic_question,
            span: {0, 7},
            status: :resolved,
            roles: %{
              wh: %{sense_id: "what|pronoun|2"},
              auxiliary: %{sense_id: "do|auxiliary|0"},
              subject: %{sense_id: "you|pronoun|1"},
              predicate: %{sense_id: "know|verb|13"},
              preposition: %{sense_id: "about|preposition|1"}
            },
            unresolved_roles: []
          },
          %{
            id: :possessive_noun_phrase,
            span: {5, 7},
            status: :partially_resolved,
            unresolved_roles: [:possessor]
          }
        ]
      }
    }
  end

  test "produces a readable definition and structured interpretation for a verified question" do
    si = input()
    meaning = SentenceMeaning.build(si)

    assert meaning.sentence == @question
    assert meaning.definition ==
             "The speaker asks what you know about Japan's railgun development."
    assert meaning.kind == :wh_topic_question
    assert meaning.status == :structurally_interpreted
    assert meaning.speech_act == :request_information

    assert meaning.roles.subject.reference == :addressee
    assert meaning.roles.subject.sense_id == "you|pronoun|1"
    assert meaning.roles.predicate.sense_id == "know|verb|13"
    assert meaning.roles.topic.surface == "Japan's railgun development"
    assert meaning.roles.topic.possession.possessor == "Japan"
    assert meaning.roles.topic.possession.grounding_status == :unverified
    assert meaning.unresolved == [:topic_possessor_lexical_grounding]
    assert meaning.grounding.exact_lexical_meanings == :not_verified
    assert meaning.grounding.topic_facts == :not_checked
  end

  test "attaches sentence definition without changing lexical winners or claiming grounding" do
    si = input()
    out = SentenceMeaning.attach(si)
    summary = Summary.build(out)

    assert :ok = SemanticInput.validate_contract(out)
    assert out.sentence == si.sentence
    assert out.sentence_signatures == si.sentence_signatures
    assert out.atl_slate == si.atl_slate
    assert summary.sentence_definition == out.sentence_meaning.definition
    assert summary.sentence_meaning == out.sentence_meaning
    assert summary.sentence_meaning.grounding.topic_facts == :not_checked
    assert [%{stage: :sentence_meaning, decision: :structurally_interpreted} | _] = out.trace
  end

  test "does not hallucinate definitions from unverified or missing ATL roles" do
    si = input()
    unverified = put_in(si.atl_slate.constituents, [])

    assert %{definition: nil, status: :not_yet_interpreted,
             sentence: @question} = SentenceMeaning.build(unverified)

    no_frame = %{si | sentence_signatures: []}
    assert %{definition: nil, status: :not_yet_interpreted} =
             SentenceMeaning.build(no_frame)

    assert SentenceMeaning.build(%{sentence: "the bank"}) ==
             %{
               version: 1, sentence: "the bank", definition: nil,
               kind: :unknown, status: :not_yet_interpreted,
               source: :insufficient_verified_sentence_structure
             }
  end

  test "refuses to manufacture a fluent gloss for an uninflected third-person predicate" do
    si = input()

    tokens =
      si.tokens
      |> Enum.map(fn
        %{index: 2} = token -> %{token | phrase: "she"}
        token -> token
      end)

    meaning = SentenceMeaning.build(%{si | tokens: tokens})
    assert meaning.definition =~ "subject 'she'"
    assert meaning.definition =~ "predicate 'know'"
    refute meaning.definition =~ "she know about"
  end
end
