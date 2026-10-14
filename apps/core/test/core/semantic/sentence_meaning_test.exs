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
            id: :noun_phrase,
            span: {6, 7},
            status: :resolved,
            roles: %{
              modifier: %{norm: "railgun", token_index: 6,
                sense_id: "railgun|noun|1"},
              head: %{norm: "development", token_index: 7,
                sense_id: "development|noun|1"}
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

  test "composes a query proposition rather than claiming a fact" do
    si = input()
    meaning = SentenceMeaning.build(si)

    assert %{
      type: :information_request,
      queried: %{type: :wh_open, surface: "What"},
      epistemic_status: :question_not_assertion,
      content: %{
        predicate: %{surface: "know", sense_id: "know|verb|13"},
        subject: %{reference: :addressee, surface: "you"},
        complement: %{
          kind: :prepositional_topic,
          relation: %{surface: "about", sense_id: "about|preposition|1"},
          argument: %{
            head: %{surface: "development", sense_id: "development|noun|1"},
            modifiers: [%{surface: "railgun", sense_id: "railgun|noun|1"}],
            genitive: %{
              semantic_relation: :unspecified,
              status: :syntactically_observed,
              possessor: %{
                surface: "Japan", sense_id: nil,
                grounding_status: :unverified
              }
            },
            source: :atl_resolved_noun_phrase
          }
        }
      },
      grounding: %{external_facts: :not_checked,
        predicate_sense: :selected_not_contextually_verified}
    } = meaning.proposition

    assert meaning.proposition.provenance.topic_span == {5, 7}
    assert meaning.proposition.provenance.frame == :wh_aux_topic_question
  end

  test "keeps topic surface without inventing noun senses when ATL lacks a noun frame" do
    si = input()

    si =
      update_in(si.atl_slate.constituents, fn constituents ->
        Enum.reject(constituents, &(&1.id == :noun_phrase))
      end)

    argument =
      si
      |> SentenceMeaning.build()
      |> get_in([:proposition, :content, :complement, :argument])

    assert argument.surface == "Japan's railgun development"
    assert argument.head == nil
    assert argument.modifiers == []
    assert argument.source == :surface_only
    assert argument.grounding_status == :unverified
    assert argument.genitive.semantic_relation == :unspecified
  end

  test "generic predicate roles preserve vocabulary rather than hardcoding knowledge_about" do
    si = input()

    tokens =
      Enum.map(si.tokens, fn
        %{index: 3} = word -> %{word | phrase: "learn"}
        %{index: 4} = word -> %{word | phrase: "about"}
        word -> word
      end)

    constituents =
      Enum.map(si.atl_slate.constituents, fn
        %{id: :wh_aux_topic_question, roles: roles} = question ->
          %{question | roles: %{roles |
            predicate: %{sense_id: "learn|verb|1"}}}
        other -> other
      end)

    si = %{si | tokens: tokens, atl_slate: %{constituents: constituents}}
    proposition = SentenceMeaning.build(si).proposition

    assert proposition.content.predicate.surface == "learn"
    assert proposition.content.predicate.sense_id == "learn|verb|1"
    assert proposition.content.complement.relation.surface == "about"
    assert proposition.type == :information_request
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
    assert summary.sentence_proposition == out.sentence_meaning.proposition
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
