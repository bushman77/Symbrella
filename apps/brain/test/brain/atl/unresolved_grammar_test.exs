defmodule Brain.ATL.UnresolvedGrammarTest do
  use ExUnit.Case, async: true

  alias Brain.ATL
  alias Brain.ATL.UnresolvedGrammar
  alias Brain.LIFG.RoleCompetition

  defp input do
    tokens =
      ~w(The fox is flarping on the fence)
      |> Enum.with_index()
      |> Enum.map(fn {phrase, index} -> %{phrase: phrase, index: index, n: 1} end)

    cells = [
      %{id: "the|determiner|1", norm: "the", pos: "determiner"},
      %{id: "fox|noun|1", norm: "fox", pos: "noun"},
      %{id: "is|verb|1", norm: "is", pos: "verb"},
      %{id: "on|preposition|1", norm: "on", pos: "preposition"},
      %{id: "fence|noun|1", norm: "fence", pos: "noun"}
    ]

    signature = %{
      id: :progressive_lexical_gap_candidate,
      source_span_sha256:
        (:crypto.hash(:sha256, "the fox is flarping on the fence")
          |> Base.encode16(case: :lower)),
      source: :core_syntax,
      span: {0, 6},
      roles: %{
        subject_determiner: 0, subject_head: 1, auxiliary: 2,
        predicate: 3, preposition: 4,
        location_determiner: 5, location_head: 6
      },
      evidence: [
        :contiguous_surface, :attested_nominal_subject,
        :unresolved_progressive_predicate, :attested_preposition,
        :attested_nominal_location
      ],
      required_features: %{predicate: [:unresolved_ing_surface]}
    }

    si = %{tokens: tokens, active_cells: cells, sentence_signatures: [signature]}
    Map.put(si, :role_competition, RoleCompetition.resolve(si))
  end

  defp baseline(si) do
    ATL.reduce([
      %{id: "fox|noun|1", token_index: 1, lemma: "fox"},
      %{id: "on|preposition|1", token_index: 4, lemma: "on"}
    ], si.tokens, si.sentence_signatures)
  end

  test "ATL retains a separate, nonfactual unresolved role while leaving winners untouched" do
    si = input()
    slate = baseline(si)
    observed = UnresolvedGrammar.attach(si, slate)

    assert [%{
      token_index: 3, role: :predicate, status: :unresolved,
      source: :lifg_role_competition,
      construction: :progressive_lexical_gap_candidate,
      lexical_pos_verified?: false,
      exact_dictionary_sense_verified?: false,
      referent_identity_verified?: false,
      event_occurrence_verified?: false,
      factual_authority?: false
    }] = observed.unresolved_grammar

    assert observed.unresolved_grammar_count == 1
    assert observed.winners == slate.winners
    assert observed.winner_count == slate.winner_count
    assert observed.by_id == slate.by_id
    assert observed.by_norm == slate.by_norm
    assert Map.get(observed, :constituents, []) == Map.get(slate, :constituents, [])
    refute Map.has_key?(observed.by_id, "flarping|verb|1")
  end

  test "forged or stale LIFG observation is rejected before reaching ATL" do
    si = input()
    slate = baseline(si)

    for changed <- [
      %{si | role_competition: %{si.role_competition |
        unresolved_roles: %{3 => %{status: :supported, pos: "verb"}}}},
      %{si | sentence_signatures: []},
      %{si | active_cells: Enum.reject(si.active_cells, &(&1.norm == "fox"))},
      %{si | tokens: List.replace_at(si.tokens, 3,
        %{phrase: "dancing", index: 3, n: 1})},
      %{si | sentence_signatures:
        [%{hd(si.sentence_signatures) |
          source_span_sha256: String.duplicate("0", 64)}]}
    ] do
      assert UnresolvedGrammar.attach(changed, slate).unresolved_grammar == []
      assert UnresolvedGrammar.attach(changed, slate).unresolved_grammar_count == 0
    end
  end

  test "a selected dictionary sense at the missing slot suppresses unresolved label" do
    si = input()
    slate = baseline(si)

    selected = %{slate | winners: slate.winners ++ [
      %{token_index: 3, id: "flarping|verb|1", norm: "flarping"}
    ]}

    assert UnresolvedGrammar.attach(si, selected).unresolved_grammar == []

    fallback = %{slate | winners: slate.winners ++ [
      %{token_index: 3, id: "flarping|phrase|fallback", norm: "flarping"}
    ]}
    assert UnresolvedGrammar.attach(si, fallback).unresolved_grammar_count == 1
  end

  test "without a LIFG observation ATL does not invent grammatical evidence" do
    si = input()
    slate = baseline(si)
    assert UnresolvedGrammar.attach(Map.delete(si, :role_competition), slate)
      .unresolved_grammar == []
    assert UnresolvedGrammar.attach(%{}, slate).unresolved_grammar == []
  end
end
