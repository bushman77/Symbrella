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
    assert [candidate] = EmbeddedClauseEvidence.inspect(input(), meaning())
    assert candidate.construction == :perfect_predicate
    assert candidate.status == :syntactic_candidate_only
    assert candidate.auxiliary.surface == "had"
    assert candidate.predicate.surface == "grown"
    assert candidate.predicate.candidate_sense_id == "grow|verb|1"
    assert candidate.subject == :unresolved
    assert candidate.embedded_clause_status == :not_interpreted
    assert candidate.assertion_status == :not_asserted
    assert candidate.verification == :external_unverified
  end

  test "no candidate is inferred outside the attested complement" do
    other = put_in(meaning(), [:proposition, :content, :complement, :argument, :span],
      {11, 16})
    assert EmbeddedClauseEvidence.inspect(input(), other) == []
  end

  test "signature role mismatch or missing surfaces cannot create a candidate" do
    bad_roles = put_in(input(), [:sentence_signatures, Access.at(0), :roles, :verb], 14)
    assert EmbeddedClauseEvidence.inspect(bad_roles, meaning()) == []

    no_predicate = %{input() | tokens: [%{index: 9, phrase: "had", n: 1}]}
    assert EmbeddedClauseEvidence.inspect(no_predicate, meaning()) == []
  end

  test "no candidate for nominal topics or untrusted assertions" do
    nominal = put_in(meaning(), [:proposition, :content, :complement, :argument, :type],
      :nominal_topic)
    assert EmbeddedClauseEvidence.inspect(input(), nominal) == []

    asserted = put_in(meaning(), [:proposition, :epistemic_status], :asserted)
    assert EmbeddedClauseEvidence.inspect(input(), asserted) == []
  end
end
