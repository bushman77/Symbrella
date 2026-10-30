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

  test "COLONY-14: modal conditional question reports ATL agreement without invented meaning" do
    roles = %{
      wh: 0, modal: 1, subject: 2, predicate: 3,
      condition_marker: 4, condition_subject: 5,
      condition_copula: 6, condition_complement: 7
    }

    input = %{
      sentence: "What would you do if you were free?",
      tokens: [
        %{index: 0, phrase: "What", n: 1},
        %{index: 1, phrase: "would", n: 1},
        %{index: 2, phrase: "you", n: 1},
        %{index: 3, phrase: "do", n: 1},
        %{index: 4, phrase: "if", n: 1},
        %{index: 5, phrase: "you", n: 1},
        %{index: 6, phrase: "were", n: 1},
        %{index: 7, phrase: "free?", n: 1}
      ],
      sentence_signatures: [%{
        id: :wh_modal_conditional_question,
        source: :core_syntax, span: {0, 7}, roles: roles
      }],
      atl_slate: %{constituents: [%{
        id: :wh_modal_conditional_question,
        span: {0, 7}, status: :resolved, unresolved_roles: [],
        roles: Map.new(roles, fn {name, idx} ->
          {name, %{token_index: idx, sense_id: "test|noun|#{idx}"}}
        end)
      }]}
    }

    assert %{
      status: :modal_conditional_atl_resolved,
      atl_role_agreement: true, unresolved_roles: [],
      semantic_composition: :not_yet_interpreted
    } = SentenceMeaning.question_diagnostic(input)

    # ATL structural resolution does not mean we have established the
    # hypothetical proposition or author intent.
    assert %{status: :not_yet_interpreted} = SentenceMeaning.build(input)

    partial = put_in(input.atl_slate.constituents, [%{
      id: :wh_modal_conditional_question,
      span: {0, 7}, status: :partially_resolved,
      unresolved_roles: [:condition_complement]
    }])

    assert %{status: :modal_conditional_roles_unresolved,
      unresolved_roles: [:condition_complement], atl_role_agreement: false} =
      SentenceMeaning.question_diagnostic(partial)
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
    assert meaning.proposition.content.complement.argument.type == :nominal_topic
    refute :topic_internal_structure in meaning.unresolved
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
    assert argument.type == :unparsed_topic
    assert :topic_internal_structure in SentenceMeaning.build(si).unresolved
    assert SentenceMeaning.build(si).proposition.epistemic_status == :question_not_assertion
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

  test "keeps negative information requests distinct from positive ones" do
    si = input()

    tokens =
      Enum.map(si.tokens, fn
        %{index: 1} = word -> %{word | phrase: "don't"}
        word -> word
      end)

    constituents =
      Enum.map(si.atl_slate.constituents, fn
        %{id: :wh_aux_topic_question, roles: roles} = question ->
          %{question | roles: %{roles |
            auxiliary: %{sense_id: "don't|auxiliary|0"}}}
        other -> other
      end)

    negative =
      %{si |
        sentence: "What don't you know about Japan's railgun development?",
        tokens: tokens,
        atl_slate: %{constituents: constituents}}
      |> SentenceMeaning.build()

    positive = SentenceMeaning.build(si)

    assert positive.polarity == :positive
    assert positive.proposition.polarity == :positive
    assert negative.polarity == :negative
    assert negative.proposition.polarity == :negative
    assert negative.proposition.operators.polarity.scope == :predicate
    assert negative.proposition.operators.auxiliary_negation?
    assert negative.proposition.epistemic_status == :question_not_assertion
    assert negative.definition =~ "you don't know about"
    refute positive.proposition.polarity == negative.proposition.polarity
  end

  test "does not elevate a nested noun phrase to a full topic reading" do
    si = input()
    tail = %{index: 8, phrase: "systems?", n: 1, mw: false}

    signatures =
      Enum.map(si.sentence_signatures, fn
        %{id: :wh_aux_topic_question} = signature ->
          %{signature | span: {0, 8}}
        other -> other
      end)

    constituents =
      Enum.map(si.atl_slate.constituents, fn
        %{id: :wh_aux_topic_question} = constituent ->
          %{constituent | span: {0, 8}}
        other -> other
      end)

    si =
      %{si |
        sentence: "What do you know about Japan's railgun development systems?",
        tokens: si.tokens ++ [tail],
        sentence_signatures: signatures,
        atl_slate: %{constituents: constituents}}

    argument =
      si
      |> SentenceMeaning.build()
      |> get_in([:proposition, :content, :complement, :argument])

    assert argument.surface == "Japan's railgun development systems"
    assert argument.head == nil
    assert argument.modifiers == []
    assert argument.source == :surface_only
    assert argument.grounding_status == :unverified
  end

  test "complex WH question interprets matrix roles but does not invent an embedded assertion" do
    sentence = "What would you look at to say the gathered had grown—and not only gotten longer?"
    words = [
      {0, "What"}, {1, "would"}, {2, "you"}, {3, "look"}, {4, "at"},
      {5, "to"}, {6, "say"}, {7, "the"}, {8, "gathered"}, {9, "had"},
      {10, "grown"}, {12, "and"}, {13, "not"}, {14, "only"},
      {15, "gotten"}, {16, "longer?"}
    ]
    roles = %{wh: 0, auxiliary: 1, subject: 2, predicate: 3, preposition: 4}
    atl_roles = Map.new(roles, fn {role, _index} ->
      {role, %{sense_id: "#{role}|test|1"}}
    end)

    si = %{
      sentence: sentence,
      tokens: Enum.map(words, fn {index, surface} ->
        %{index: index, phrase: surface, n: 1}
      end),
      sentence_signatures: [%{
        id: :wh_aux_topic_question, span: {0, 16}, roles: roles
      }],
      atl_slate: %{constituents: [%{
        id: :wh_aux_topic_question, status: :resolved,
        span: {0, 16}, roles: atl_roles
      }]}
    }

    meaning = SentenceMeaning.build(si)
    assert meaning.status == :structurally_interpreted
    assert meaning.speech_act == :request_information
    assert meaning.roles.subject.surface == "you"
    assert meaning.roles.predicate.surface == "look"
    assert meaning.proposition.epistemic_status == :question_not_assertion
    assert meaning.proposition.content.complement.argument.type == :unparsed_topic
    assert meaning.proposition.content.complement.argument.source == :surface_only
    assert :topic_internal_structure in meaning.unresolved
    refute Map.has_key?(meaning.proposition.content.complement.argument, :asserted_event)
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
