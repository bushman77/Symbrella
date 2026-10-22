defmodule Core.Social.ReportedPropositionTest do
  use ExUnit.Case, async: true

  alias Core.Social.ReportedProposition

  @title "Scored blind against a sealed key: 10 of my checker's 13 links were lines that refute the claim"
  @words ~w(Scored blind against a sealed key 10 of my checker s 13 links were lines that refute the claim)

  defp input do
    tokens =
      @words
      |> Enum.with_index()
      |> Enum.map(fn {word, index} ->
        %{index: index, phrase: word, n: 1, kind: :word}
      end)

    role_pos = %{
      8 => "determiner",
      9 => "noun",
      12 => "noun",
      13 => "verb",
      14 => "noun",
      16 => "verb",
      17 => "determiner",
      18 => "noun"
    }

    cells =
      Enum.map(role_pos, fn {index, pos} ->
        %{id: "#{String.downcase(Enum.at(@words, index))}|#{pos}|1",
          token_index: index, pos: pos}
      end)

    winners =
      Enum.map(cells, fn cell ->
        %{id: cell.id, token_index: cell.token_index}
      end)

    noun_phrases = [
      %{id: :noun_phrase, span: {8, 9}, roles: %{determiner: 8, head: 9}},
      %{id: :noun_phrase, span: {17, 18}, roles: %{determiner: 17, head: 18}}
    ]

    %{
      source: :colony_external,
      sentence: @title,
      tokens: tokens,
      active_cells: cells,
      sentence_signatures: noun_phrases,
      atl_slate: %{winners: winners}
    }
  end

  test "joins a counted subject, copular predicate and relative clause without verifying the report" do
    proposition = ReportedProposition.interpret(input())

    assert proposition.status == :attributed_structural_interpretation
    assert proposition.kind == :reported_declarative_proposition
    assert proposition.relation == :copular_classification
    assert proposition.embedded_relation == :reported_refutation
    assert proposition.reported_count == 10
    assert proposition.reported_total == 13
    assert proposition.subject == :reported_checker_links
    assert proposition.complement == :lines
    assert proposition.object == :the_claim
    assert proposition.possessor == :speaker_reported_not_verified
    assert proposition.verification == :author_claim_not_independently_verified
    assert proposition.source == :colony_external
    assert proposition.role_indices == %{
      checker: 9, links: 12, were: 13, lines: 14, relative_verb: 16, claim: 18
    }
  end

  test "an attested copular auxiliary is valid even when its selected ID is closed-class" do
    si = input()

    signature = %{
      id: :copular_relative_classification,
      source: :core_syntax,
      roles: %{copula: 13, complement_head: 14}
    }

    winners =
      Enum.map(si.atl_slate.winners, fn winner ->
        if winner.token_index == 13,
          do: %{winner | id: "were|auxiliary|0"},
          else: winner
      end)

    cells =
      Enum.map(si.active_cells, fn cell ->
        if cell.token_index == 13, do: Map.merge(cell, %{norm: "were", word: "were"}), else: cell
      end)

    result =
      ReportedProposition.interpret(%{
        si | active_cells: cells,
             sentence_signatures: [signature | si.sentence_signatures],
             atl_slate: %{winners: winners}
      })

    assert result.status == :attributed_structural_interpretation
    assert result.embedded_relation == :reported_refutation

    # Neither the exact token nor the complete Core frame may be fabricated.
    without_frame =
      %{si | active_cells: cells, atl_slate: %{winners: winners}}
    rejected = ReportedProposition.interpret(without_frame)
    assert rejected.status == :not_interpreted
    assert rejected.reason == :lifg_atl_role_disagreement

    without_dictionary =
      %{si | active_cells: Enum.reject(cells, &(&1.token_index == 13)),
             sentence_signatures: [signature | si.sentence_signatures],
             atl_slate: %{winners: winners}}
    assert ReportedProposition.interpret(without_dictionary).reason ==
             :missing_attested_dictionary_roles
  end

  test "supports the same syntax with the opposite reported evidence polarity" do
    si = input()
    sentence = String.replace(@title, "that refute", "that support")

    updated_cells =
      Enum.map(si.active_cells, fn cell ->
        if cell.token_index == 16,
          do: %{cell | id: "support|verb|1"},
          else: cell
      end)

    updated_winners =
      Enum.map(si.atl_slate.winners, fn winner ->
        if winner.token_index == 16,
          do: %{winner | id: "support|verb|1"},
          else: winner
      end)

    proposition =
      ReportedProposition.interpret(%{
        si | sentence: sentence, active_cells: updated_cells,
             atl_slate: %{winners: updated_winners},
             tokens: List.replace_at(si.tokens, 16, %{Enum.at(si.tokens, 16) | phrase: "support"})
      })

    assert proposition.status == :attributed_structural_interpretation
    assert proposition.embedded_relation == :reported_support
    assert proposition.reported_count == 10
    assert proposition.reported_total == 13
  end

  test "can parse the same bounded headline followed by a separate body sentence" do
    si = input() |> Map.put(:sentence, @title <> ". Follow-up body is unverified.")
    assert ReportedProposition.interpret(si).status == :attributed_structural_interpretation
  end

  test "rejects missing dictionary role evidence instead of trusting the sentence surface" do
    si = input()
    cells = Enum.reject(si.active_cells, &(&1.token_index == 16))
    assert %{status: :not_interpreted, reason: :missing_attested_dictionary_roles} =
             ReportedProposition.interpret(%{si | active_cells: cells})
  end

  test "rejects missing grammatical noun-phrase candidates" do
    assert %{status: :not_interpreted, reason: :missing_nominal_signature_agreement} =
             ReportedProposition.interpret(%{input() | sentence_signatures: []})
  end

  test "rejects LIFG and ATL disagreement even if dictionary POS offers a possible role" do
    si = input()
    winners =
      Enum.map(si.atl_slate.winners, fn winner ->
        if winner.token_index == 16,
          do: %{winner | id: "refute|noun|1"},
          else: winner
      end)

    result = ReportedProposition.interpret(%{si | atl_slate: %{winners: winners}})
    assert %{status: :not_interpreted, reason: :lifg_atl_role_disagreement} = result
    assert length(result.role_audit) == 6

    disputed = Enum.filter(result.role_audit, &(not &1.agreed?))
    assert [%{role: :relative_verb, token_index: 16,
              expected_pos: "verb", selected_pos: ["noun"],
              dictionary_pos_available?: true}] = disputed
    assert Enum.count(result.role_audit, & &1.agreed?) == 5
  end

  test "abstains for unsupported clauses, counterfactuals, and untrusted source" do
    for sentence <- [
          "10 of my checker's 13 links were not lines that refute the claim",
          "10 of my checker's 13 links might be lines that refute the claim",
          "10 of my checker's 13 links were lines that support and refute the claim",
          "10 of 13 links support the claim",
          "These links could refute the claim"
        ] do
      assert %{status: :not_interpreted} =
               ReportedProposition.interpret(%{input() | sentence: sentence})
    end

    assert %{status: :not_interpreted, reason: :untrusted_or_invalid_input} =
             ReportedProposition.interpret(%{input() | source: :user})
  end

  test "cannot invent an invalid reported count" do
    title = String.replace(@title, "10 of", "14 of")
    assert %{status: :not_interpreted, reason: :invalid_reported_counts} =
             ReportedProposition.interpret(%{input() | sentence: title})
  end
end
