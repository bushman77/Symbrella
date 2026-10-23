defmodule Core.Semantic.EmbeddedClauseEvidenceTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.EmbeddedClauseEvidence

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

  test "a perfect-predicate signature inside unresolved topic stays non-assertive" do
    assert [candidate] = EmbeddedClauseEvidence.candidates(input(), meaning())
    assert candidate.construction == :perfect_predicate
    assert candidate.status == :syntactic_candidate_only
    assert candidate.auxiliary.surface == "had"
    assert candidate.predicate.surface == "grown"
    assert candidate.predicate.candidate_sense_id == "grow|verb|1"
    assert candidate.subject == :unresolved
    assert candidate.subject_candidate == nil
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

    for changed <- [
      %{nomination | status: :partially_resolved},
      put_in(nomination, [:roles, :adjective, :sense_id], "gathered|verb|1"),
      put_in(nomination, [:roles, :adjective, :sense_id], "gathered|adjective|fallback"),
      put_in(nomination, [:roles, :adjective, :selection_scope], :lexical_mwe),
      %{nomination | span: {6, 7}}
    ] do
      invalid = put_in(candidate_input, [:atl_slate, :constituents], [changed])
      assert [%{subject_candidate: nil, subject_binding_status: :unresolved,
        assertion_status: :not_asserted}] =
        EmbeddedClauseEvidence.candidates(invalid, meaning())
    end

    absent_nominalization =
      %{candidate_input | atl_slate: Map.delete(candidate_input.atl_slate, :constituents)}
    assert [%{subject_candidate: nil}] =
      EmbeddedClauseEvidence.candidates(absent_nominalization, meaning())
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
