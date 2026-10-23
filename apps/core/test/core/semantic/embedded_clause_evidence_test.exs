defmodule Core.Semantic.EmbeddedClauseEvidenceTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.EmbeddedClauseEvidence
  alias Core.Syntax.Signatures
  alias Brain.ATL

  defp meaning do
    %{
      kind: :wh_topic_question,
      status: :structurally_interpreted,
      proposition: %{
        type: :information_request,
        epistemic_status: :question_not_assertion,
        content: %{complement: %{argument: %{
          type: :unparsed_topic, span: {5, 16}
        }}}
      }
    }
  end

  defp input do
    %{
      tokens: [
        %{index: 9, phrase: "had", n: 1},
        %{index: 10, phrase: "grown", n: 1}
      ],
      atl_slate: %{winners: [
        %{token_index: 9, id: "have|auxiliary|1"},
        %{token_index: 10, id: "grow|verb|1"}
      ]},
      sentence_signatures: [%{
        id: :perfect_predicate,
        span: {9, 10},
        roles: %{auxiliary: 9, verb: 10},
        evidence: [:have_auxiliary, :past_participle_form_available]
      }]
    }
  end

  defp qualification_input do
    base = input()
    nominal = %{
      id: :nominalized_adjective_subject_candidate,
      span: {7, 8},
      status: :resolved,
      roles: %{
        determiner: %{token_index: 7, sense_id: "the|determiner|1"},
        adjective: %{token_index: 8, sense_id: "gathered|adjective|1"}
      }
    }

    witness = %{
      id: :nominalized_adjective_subject_candidate,
      span: {7, 8},
      roles: %{determiner: 7, adjective: 8}
    }

    %{
      base |
      sentence: "What would you look at to say the gathered had grown—and not only gotten longer?",
      tokens: [
        %{index: 7, phrase: "the", n: 1},
        %{index: 8, phrase: "gathered", n: 1}
      ] ++ base.tokens ++ [
        %{index: 12, phrase: "and", n: 1},
        %{index: 13, phrase: "not", n: 1},
        %{index: 14, phrase: "only", n: 1},
        %{index: 15, phrase: "gotten", n: 1},
        %{index: 16, phrase: "longer?", n: 1}
      ],
      sentence_signatures: [witness | base.sentence_signatures],
      atl_slate: base.atl_slate
        |> Map.put(:constituents, [nominal])
        |> Map.update!(:winners, fn winners ->
          [
            %{token_index: 7, id: "the|determiner|1"},
            %{token_index: 8, id: "gathered|adjective|1"},
            %{token_index: 15, id: "get|verb|1"},
            %{token_index: 16, id: "long|adjective|1"}
            | winners
          ]
        end)
    }
  end

  test "a perfect-predicate signature inside unresolved topic stays non-assertive" do
    assert [candidate] = EmbeddedClauseEvidence.candidates(input(), meaning())
    assert candidate.construction == :perfect_predicate
    assert candidate.status == :syntactic_candidate_only
    assert candidate.auxiliary.surface == "had"
    assert candidate.predicate.surface == "grown"
    assert candidate.predicate.candidate_sense_id == "grow|verb|1"
    assert candidate.subject == :unresolved
    assert candidate.subject_candidate == nil
    assert candidate.relation_candidate == nil
    assert candidate.relation_diagnostic.status == :no_adjacent_subject_candidate
    assert candidate.subject_binding_status == :unresolved
    assert candidate.embedded_clause_status == :not_interpreted
    assert candidate.assertion_status == :not_asserted
    assert candidate.verification == :external_unverified
  end

  test "adjacent ATL resolved noun phrase is an unbound subject candidate" do
    input = input()
    nominal = %{id: :noun_phrase, status: :resolved, span: {7, 8},
      roles: %{
        determiner: %{token_index: 7, sense_id: "the|determiner|1"},
        head: %{token_index: 8, sense_id: "gathered|noun|1"}
      }}

    input = %{input |
      tokens: [
        %{index: 7, phrase: "the", n: 1},
        %{index: 8, phrase: "gathered", n: 1}
      ] ++ input.tokens,
      atl_slate: Map.put(input.atl_slate, :constituents, [nominal])
    }

    assert [candidate] = EmbeddedClauseEvidence.candidates(input, meaning())
    assert candidate.subject == :unresolved
    assert candidate.subject_binding_status == :unresolved
    assert candidate.subject_candidate.status == :adjacent_nominal_candidate_only
    assert candidate.subject_candidate.source == :atl_resolved_noun_phrase
    assert candidate.subject_candidate.surface == "the gathered"
    assert candidate.subject_candidate.span == {7, 8}
    assert candidate.subject_candidate.head.candidate_sense_id == "gathered|noun|1"
    assert candidate.subject_candidate.attachment_status == :unverified
    assert candidate.subject_candidate.coreference_status == :unresolved
    assert candidate.assertion_status == :not_asserted
  end

  test "rejects missing, detached, unresolved or MWE nominal candidates" do
    nominal = %{id: :noun_phrase, status: :resolved, span: {7, 8},
      roles: %{head: %{token_index: 8, sense_id: "group|noun|1"}}}
    base = input()
    base = %{base |
      tokens: [
        %{index: 7, phrase: "the", n: 1},
        %{index: 8, phrase: "group", n: 1}
      ] ++ base.tokens,
      atl_slate: Map.put(base.atl_slate, :constituents, [nominal])
    }

    assert [%{subject_candidate: %{surface: "the group"}}] =
      EmbeddedClauseEvidence.candidates(base, meaning())

    for altered <- [
      %{nominal | status: :partially_resolved},
      %{nominal | span: {6, 7}},
      put_in(nominal, [:roles, :head, :sense_id], nil),
      put_in(nominal, [:roles, :head, :sense_id], "group|noun|fallback"),
      put_in(nominal, [:roles, :head, :selection_scope], :lexical_mwe)
    ] do
      input = put_in(base, [:atl_slate, :constituents], [altered])
      assert [%{subject: :unresolved, subject_candidate: nil}] =
        EmbeddedClauseEvidence.candidates(input, meaning())
    end
  end

  test "diagnoses missing subject using original token senses and noun phrase evidence" do
    base = input()
    assert [%{subject_diagnostic: missing}] =
      EmbeddedClauseEvidence.candidates(base, meaning())

    assert missing.status == :no_adjacent_noun_phrase_signature
    assert missing.adjacent_noun_signatures == []
    assert missing.adjacent_atl_noun_phrases == []

    nominal = %{id: :noun_phrase, span: {7, 8},
      roles: %{determiner: 7, head: 8}}
    tokens = [
      %{index: 7, phrase: "the", n: 1},
      %{index: 8, phrase: "gathered", n: 1}
    ] ++ base.tokens

    winners = [
      %{token_index: 7, id: "the|determiner|1"},
      %{token_index: 8, id: "gather|verb|2"}
    ] ++ base.atl_slate.winners

    with_signature = %{base |
      tokens: tokens,
      sentence_signatures: [nominal | base.sentence_signatures],
      atl_slate: %{base.atl_slate | winners: winners}
    }

    assert [%{subject_candidate: nil, subject_diagnostic: no_atl}] =
      EmbeddedClauseEvidence.candidates(with_signature, meaning())

    assert no_atl.status == :noun_phrase_not_resolved_by_atl
    assert no_atl.adjacent_noun_signatures == [%{span: {7, 8}}]
    assert Enum.find(no_atl.token_window, &(&1.index == 8)).selected_pos == "verb"

    partial = %{id: :noun_phrase, span: {7, 8},
      status: :partially_resolved, unresolved_roles: [:head], roles: %{}}
    with_partial = put_in(with_signature, [:atl_slate, :constituents], [partial])

    assert [%{subject_candidate: nil, subject_diagnostic: unresolved}] =
      EmbeddedClauseEvidence.candidates(with_partial, meaning())

    assert unresolved.status == :atl_nominal_unresolved
    assert unresolved.adjacent_atl_noun_phrases ==
      [%{span: {7, 8}, status: :partially_resolved, unresolved_roles: [:head]}]
    assert unresolved.subject_binding_status == :unresolved
  end

  test "resolved ATL constituent with non-nominal head sense still abstains" do
    base = input()
    nominal = %{id: :noun_phrase, status: :resolved, span: {7, 8},
      roles: %{head: %{token_index: 8, sense_id: "gather|verb|2"}}}
    source = %{base |
      tokens: [%{index: 7, phrase: "the", n: 1},
        %{index: 8, phrase: "gathered", n: 1}] ++ base.tokens,
      sentence_signatures: [%{id: :noun_phrase, span: {7, 8},
        roles: %{determiner: 7, head: 8}} | base.sentence_signatures],
      atl_slate: Map.put(base.atl_slate, :constituents, [nominal])
    }

    assert [%{subject_candidate: nil, subject_diagnostic: diagnostic}] =
      EmbeddedClauseEvidence.candidates(source, meaning())

    assert diagnostic.status == :resolved_nominal_failed_candidate_checks
  end

  test "ATL-resolved adjectival nominalization is an unbound subject candidate" do
    base = input()
    nomination = %{
      id: :nominalized_adjective_subject_candidate,
      status: :resolved,
      span: {7, 8},
      roles: %{
        determiner: %{token_index: 7, sense_id: "the|determiner|1"},
        adjective: %{token_index: 8, sense_id: "gathered|adjective|1"}
      }
    }
    nominal_signature = %{
      id: :nominalized_adjective_subject_candidate,
      span: {7, 8},
      roles: %{determiner: 7, adjective: 8}
    }
    candidate_input = %{base |
      tokens: [%{index: 7, phrase: "the", n: 1},
        %{index: 8, phrase: "gathered", n: 1}] ++ base.tokens,
      sentence_signatures: [nominal_signature | base.sentence_signatures],
      atl_slate: base.atl_slate
        |> Map.put(:constituents, [nomination])
        |> Map.update!(:winners, fn winners ->
          [%{token_index: 7, id: "the|determiner|1"},
            %{token_index: 8, id: "gathered|adjective|1"} | winners]
        end)
    }

    assert [result] = EmbeddedClauseEvidence.candidates(candidate_input, meaning())
    assert result.subject == :unresolved
    assert result.subject_binding_status == :unresolved
    assert result.subject_candidate.status == :adjectival_nominalization_candidate_only
    assert result.subject_candidate.surface == "the gathered"
    assert result.subject_candidate.head.selected_pos == :adjective
    assert result.subject_candidate.head.candidate_sense_id == "gathered|adjective|1"
    assert result.subject_candidate.nominalization_status == :hypothesized_not_confirmed
    assert result.subject_candidate.attachment_status == :unverified
    assert result.subject_candidate.coreference_status == :unresolved
    assert result.assertion_status == :not_asserted
    assert result.relation_candidate.type == :embedded_perfect_clause_candidate
    assert result.relation_candidate.status == :question_scoped_structural_relation_candidate
    assert result.relation_diagnostic.status == :question_scoped_relation_candidate_available
    assert result.relation_candidate.surface == "the gathered had grown"
    assert result.relation_candidate.span == {7, 10}
    assert result.relation_candidate.subject.identity == :unresolved
    assert result.relation_candidate.subject.lexical_head_sense_id == "gathered|adjective|1"
    assert result.relation_candidate.predicate.selected_sense_id == "grow|verb|1"
    assert result.relation_candidate.scope == :unparsed_information_request_complement
    assert result.relation_candidate.attachment_status == :unverified
    assert result.relation_candidate.assertion_status == :not_asserted
    assert result.relation_candidate.embedded_clause_status == :not_fully_interpreted
    assert result.relation_candidate.remainder_status == :not_interpreted

    for changed <- [
      %{nomination | status: :partially_resolved},
      put_in(nomination, [:roles, :adjective, :sense_id], "gathered|verb|1"),
      put_in(nomination, [:roles, :adjective, :sense_id], "gathered|adjective|fallback"),
      put_in(nomination, [:roles, :adjective, :selection_scope], :lexical_mwe),
      %{nomination | span: {6, 7}}
    ] do
      invalid = put_in(candidate_input, [:atl_slate, :constituents], [changed])
      assert [%{subject_candidate: nil, relation_candidate: nil,
        subject_binding_status: :unresolved, assertion_status: :not_asserted}] =
        EmbeddedClauseEvidence.candidates(invalid, meaning())
    end

    missing_syntax = %{candidate_input |
      sentence_signatures: base.sentence_signatures}
    assert [%{subject_candidate: nil, subject_binding_status: :unresolved}] =
      EmbeddedClauseEvidence.candidates(missing_syntax, meaning())

    assert [%{subject_diagnostic: %{status: :adjacent_nominal_candidate_available}}] =
      EmbeddedClauseEvidence.candidates(candidate_input, meaning())

    absent_nominalization =
      %{candidate_input | atl_slate: Map.delete(candidate_input.atl_slate, :constituents)}
    assert [%{subject_candidate: nil}] =
      EmbeddedClauseEvidence.candidates(absent_nominalization, meaning())
  end

  test "real syntax-to-ATL path retains adjectival subject candidate without an assertion" do
    tokens =
      [
        {0, "What"}, {1, "would"}, {2, "you"}, {3, "look"},
        {4, "at"}, {5, "to"}, {6, "say"}, {7, "the"},
        {8, "gathered"}, {9, "had"}, {10, "grown"},
        {12, "and"}, {13, "not"}, {14, "only"},
        {15, "gotten"}, {16, "longer?"}
      ]
      |> Enum.map(fn {index, phrase} ->
        %{index: index, phrase: phrase, n: 1}
      end)

    si = %{
      sentence: "What would you look at to say the gathered had grown—and not only gotten longer?",
      tokens: tokens,
      active_cells: [
        %{norm: "the", pos: "determiner"},
        %{norm: "gathered", pos: "adjective"},
        %{norm: "had", pos: "auxiliary"},
        %{norm: "grown", pos: "verb",
          semantic_atoms: ["tag:form-of", "tag:past", "tag:participle"]}
      ]
    }

    signatures = Signatures.detect(si)
    assert Enum.any?(signatures, fn signature ->
      signature.id == :nominalized_adjective_subject_candidate and
        signature.span == {7, 8}
    end)
    assert Enum.any?(signatures, fn signature ->
      signature.id == :perfect_predicate and signature.span == {9, 10}
    end)

    choices = [
      %{token_index: 7, id: "the|determiner|1"},
      %{token_index: 8, id: "gathered|adjective|1"},
      %{token_index: 9, id: "had|auxiliary|1"},
      %{token_index: 10, id: "grow|verb|1"}
    ]

    slate = ATL.reduce(choices, tokens, signatures)
    assert Enum.any?(slate.constituents, fn c ->
      c.id == :nominalized_adjective_subject_candidate and
        c.span == {7, 8} and c.status == :resolved
    end)

    assert [evidence] = EmbeddedClauseEvidence.candidates(
      Map.merge(si, %{atl_slate: slate, sentence_signatures: signatures}), meaning()
    )
    assert evidence.subject == :unresolved
    assert evidence.subject_candidate.surface == "the gathered"
    assert evidence.subject_candidate.head.selected_pos == :adjective
    assert evidence.subject_candidate.attachment_status == :unverified
    assert evidence.relation_candidate.surface == "the gathered had grown"
    assert evidence.relation_candidate.assertion_status == :not_asserted
    assert evidence.relation_candidate.scope == :unparsed_information_request_complement
    assert evidence.embedded_clause_status == :not_interpreted
    assert evidence.assertion_status == :not_asserted
  end

  test "embedded relation abstains without aligned selected head or verb sense" do
    base = input()
    nomination = %{
      id: :nominalized_adjective_subject_candidate,
      status: :resolved, span: {7, 8},
      roles: %{
        determiner: %{token_index: 7, sense_id: "the|determiner|1"},
        adjective: %{token_index: 8, sense_id: "gathered|adjective|1"}
      }
    }
    signature = %{id: :nominalized_adjective_subject_candidate,
      span: {7, 8}, roles: %{determiner: 7, adjective: 8}}

    complete = %{base |
      tokens: [%{index: 7, phrase: "the", n: 1},
        %{index: 8, phrase: "gathered", n: 1} | base.tokens],
      sentence_signatures: [signature | base.sentence_signatures],
      atl_slate: base.atl_slate
        |> Map.put(:constituents, [nomination])
        |> Map.update!(:winners, fn winners ->
          [%{token_index: 7, id: "the|determiner|1"},
            %{token_index: 8, id: "gathered|adjective|1"} | winners]
        end)
    }

    for invalid_id <- [nil, "grown|adjective|1", "grown|verb|fallback"] do
      winners =
        complete.atl_slate.winners
        |> Enum.reject(&(&1.token_index == 10))
        |> then(fn remaining ->
          if invalid_id, do: [%{token_index: 10, id: invalid_id} | remaining],
            else: remaining
        end)

      input = put_in(complete, [:atl_slate, :winners], winners)
      assert [%{subject_candidate: %{surface: "the gathered"},
        relation_candidate: nil, relation_diagnostic: diagnostic,
        assertion_status: :not_asserted,
        subject_binding_status: :unresolved}] =
        EmbeddedClauseEvidence.candidates(input, meaning())
      assert diagnostic.assertion_status == :not_asserted
      assert diagnostic.status in [:predicate_lexical_selection_missing,
        :predicate_selected_sense_not_verb, :predicate_selected_sense_fallback]
    end

    head_mismatch =
      update_in(complete.atl_slate.winners, fn winners ->
        Enum.map(winners, fn
          %{token_index: 8} = w -> %{w | id: "gathered|verb|1"}
          w -> w
        end)
      end)
    input = put_in(complete, [:atl_slate, :winners], head_mismatch)
    assert [%{subject_candidate: nil, relation_candidate: nil,
      assertion_status: :not_asserted}] =
      EmbeddedClauseEvidence.candidates(input, meaning())
  end

  test "not-only qualification is a question-scoped lexical hypothesis, never a denial" do
    assert [candidate] =
      EmbeddedClauseEvidence.candidates(qualification_input(), meaning())

    assert candidate.relation_candidate.surface == "the gathered had grown"
    assert candidate.qualification_diagnostic.status == :qualification_candidate_available

    q = candidate.qualification_candidate
    assert q.type == :not_only_qualification_candidate
    assert q.surface == "and not only gotten longer"
    assert q.span == {12, 16}
    assert q.punctuation_bridge == :original_punctuation_witnessed
    assert q.focus_marker == :not_only
    assert q.focus_scope == :not_only_not_clausal_negation
    assert q.qualifier_predicate.selected_sense_id == "get|verb|1"
    assert q.degree_term.selected_sense_id == "long|adjective|1"
    assert q.auxiliary_reuse == :possible_not_established
    assert q.coordination_status == :structural_candidate_only
    assert q.semantic_comparison_status == :not_interpreted
    assert q.asserted_event? == false
    assert q.assertion_status == :not_asserted
    assert q.verification == :external_unverified
    assert candidate.subject_binding_status == :unresolved
    assert candidate.embedded_clause_status == :not_interpreted
  end

  test "qualification abstains if punctuation gap is not witnessed in source" do
    input = %{qualification_input() |
      sentence: "What would you look at to say the gathered had grown and not only gotten longer?"}

    assert [%{
      relation_candidate: %{surface: "the gathered had grown"},
      qualification_candidate: nil,
      qualification_diagnostic: %{status: :coordinator_not_witnessed},
      assertion_status: :not_asserted
    }] = EmbeddedClauseEvidence.candidates(input, meaning())
  end

  test "qualification does not silently turn a different focus marker into not-only" do
    input = qualification_input()
    altered_tokens = Enum.map(input.tokens, fn
      %{index: 13} = token -> %{token | phrase: "never"}
      token -> token
    end)

    assert [%{
      qualification_candidate: nil,
      qualification_diagnostic: %{status: :focus_marker_missing}
    }] = EmbeddedClauseEvidence.candidates(%{input | tokens: altered_tokens}, meaning())
  end

  test "qualification withholds evidence without supported verb and degree senses" do
    base = qualification_input()

    for {index, invalid_id, reason} <- [
      {15, nil, :qualifier_verb_selection_missing},
      {15, "get|noun|1", :qualifier_verb_not_supported},
      {15, "get|verb|fallback", :qualifier_verb_not_supported},
      {16, nil, :degree_selection_missing},
      {16, "long|noun|1", :degree_selection_not_supported},
      {16, "long|adjective|fallback", :degree_selection_not_supported}
    ] do
      winners =
        base.atl_slate.winners
        |> Enum.reject(&(&1.token_index == index))
        |> then(fn other ->
          if invalid_id, do: [%{token_index: index, id: invalid_id} | other],
            else: other
        end)

      altered = put_in(base, [:atl_slate, :winners], winners)
      assert [%{
        qualification_candidate: nil,
        qualification_diagnostic: %{status: ^reason},
        assertion_status: :not_asserted
      }] = EmbeddedClauseEvidence.candidates(altered, meaning())
    end
  end

  test "qualification cannot exist without the established question-scoped base relation" do
    base = qualification_input()
    winners =
      Enum.reject(base.atl_slate.winners, &(&1.token_index == 10))
    altered = put_in(base, [:atl_slate, :winners], winners)

    assert [%{
      relation_candidate: nil,
      qualification_candidate: nil,
      qualification_diagnostic: %{status: :base_relation_missing},
      assertion_status: :not_asserted
    }] = EmbeddedClauseEvidence.candidates(altered, meaning())

    asserted = put_in(meaning(), [:proposition, :epistemic_status], :asserted)
    assert EmbeddedClauseEvidence.candidates(base, asserted) == []
  end

  test "no candidate is inferred outside the attested complement" do
    other = put_in(meaning(), [:proposition, :content, :complement, :argument, :span],
      {11, 16})
    assert EmbeddedClauseEvidence.candidates(input(), other) == []
  end

  test "signature role mismatch or missing surfaces cannot create a candidate" do
    bad_roles = put_in(input(), [:sentence_signatures, Access.at(0), :roles, :verb], 14)
    assert EmbeddedClauseEvidence.candidates(bad_roles, meaning()) == []

    no_predicate = %{input() | tokens: [%{index: 9, phrase: "had", n: 1}]}
    assert EmbeddedClauseEvidence.candidates(no_predicate, meaning()) == []
  end

  test "no candidate for nominal topics or untrusted assertions" do
    nominal = put_in(meaning(), [:proposition, :content, :complement, :argument, :type],
      :nominal_topic)
    assert EmbeddedClauseEvidence.candidates(input(), nominal) == []

    asserted = put_in(meaning(), [:proposition, :epistemic_status], :asserted)
    assert EmbeddedClauseEvidence.candidates(input(), asserted) == []
  end
end
