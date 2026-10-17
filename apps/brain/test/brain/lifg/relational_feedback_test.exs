defmodule Brain.LIFG.RelationalFeedbackTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.RelationalFeedback

  defp fixture do
    %{
      tokens: [
        %{index: 0, phrase: "river", n: 1},
        %{index: 1, phrase: "bank?", n: 1}
      ],
      sentence_signatures: [
        %{id: :noun_phrase, span: {0, 1},
          roles: %{modifier: 0, head: 1}}
      ],
      atl_slate: %{
        constituents: [
          %{id: :noun_phrase, span: {0, 1}, status: :resolved,
            roles: %{
              modifier: %{token_index: 0, sense_id: "river|noun|1"},
              head: %{token_index: 1, sense_id: "bank|noun|finance"}
            }}
        ]
      },
      lifg_choices: [
        %{token_index: 0, id: "river|noun|1"},
        %{token_index: 1, id: "bank|noun|finance",
          candidate_probs: %{
            "bank|noun|finance" => 0.5,
            "bank|noun|river" => 0.5
          }}
      ],
      sense_candidates: %{
        1 => [
          %{id: "bank|noun|finance", lemma: "bank", pos: "noun",
            definition: "A financial institution."},
          %{id: "bank|noun|river", lemma: "bank", pos: "noun",
            definition: "The side of a river."}
        ]
      }
    }
  end

  test "ATL noun relationship supplies independent surface context to LIFG" do
    result = RelationalFeedback.evaluate(fixture())
    assert result.status == :evaluated
    assert result.edges_considered == 2
    assert result.recommended_changes == 1

    assert %{token_index: 1, selected_id: "bank|noun|finance",
             proposed_id: "bank|noun|river", revision?: true,
             status: :revision_proposed, context_cues: ["river"],
             adjustments: [%{id: "bank|noun|river",
                             source: :atl_structural_relation_dictionary_context}]} =
             Enum.find(result.decisions, &(&1.token_index == 1))

    new_probs = Enum.find(result.decisions, &(&1.token_index == 1)).posterior
    assert new_probs["bank|noun|river"] > new_probs["bank|noun|finance"]
    assert_in_delta Enum.sum(Map.values(new_probs)), 1.0, 0.000001
  end

  test "changing the other token's selected sense does not create self-confirmation" do
    si = fixture()
    original_constituent = hd(si.atl_slate.constituents)
    other_sense = %{token_index: 0, sense_id: "river|noun|999"}
    changed_constituent = %{
      original_constituent |
      roles: Map.put(original_constituent.roles, :modifier, other_sense)
    }
    modified = put_in(si.atl_slate.constituents, [changed_constituent])

    original = RelationalFeedback.evaluate(si)
    changed = RelationalFeedback.evaluate(modified)

    assert Enum.find(original.decisions, &(&1.token_index == 1)).adjustments ==
             Enum.find(changed.decisions, &(&1.token_index == 1)).adjustments
  end

  test "unresolved or unmatched ATL relations do not contribute evidence" do
    si = fixture()

    for bad <- [
      put_in(si.atl_slate.constituents,
        [%{hd(si.atl_slate.constituents) | status: :partially_resolved}]),
      %{si | sentence_signatures: []},
      put_in(si.atl_slate.constituents, [])
    ] do
      result = RelationalFeedback.evaluate(bad)
      assert result.edges_considered == 0
      assert result.recommended_changes == 0
      assert result.status == :no_verified_relations
    end
  end

  test "question topic guides predicate without generic predicate feeding back into noun" do
    words = ["What", "do", "you", "know", "about", "railgun", "development?"]
    si = %{
      tokens: words |> Enum.with_index()
        |> Enum.map(fn {phrase, index} -> %{index: index, phrase: phrase, n: 1} end),
      sentence_signatures: [
        %{id: :wh_aux_topic_question, span: {0, 6},
          roles: %{wh: 0, auxiliary: 1, subject: 2, predicate: 3, preposition: 4}},
        %{id: :noun_phrase, span: {5, 6},
          roles: %{modifier: 5, head: 6}}
      ],
      atl_slate: %{
        constituents: [
          %{id: :wh_aux_topic_question, span: {0, 6}, status: :resolved,
            roles: %{
              predicate: %{token_index: 3, sense_id: "know|verb|13"},
              preposition: %{token_index: 4, sense_id: "about|preposition|1"}
            }},
          %{id: :noun_phrase, span: {5, 6}, status: :resolved,
            roles: %{
              modifier: %{token_index: 5, sense_id: "railgun|noun|1"},
              head: %{token_index: 6, sense_id: "development|noun|1"}
            }}
        ]
      }
    }

    edges = RelationalFeedback.structural_edges(si)
    assert Enum.any?(edges, fn edge ->
      edge.target_index == 3 and edge.partner_index == 6 and
        edge.relation == :predicate_topic and edge.partner_surface == "development"
    end)

    refute Enum.any?(edges, fn edge ->
      edge.target_index == 6 and edge.partner_index == 3
    end)
  end

  test "no complete original LIFG posterior means no winner revision" do
    si = fixture()

    incomplete =
      update_in(si.lifg_choices, fn choices ->
        Enum.map(choices, fn
          %{token_index: 1} = choice ->
            %{choice | candidate_probs: %{"bank|noun|finance" => 0.5}}
          choice -> choice
        end)
      end)

    result = RelationalFeedback.evaluate(incomplete)
    assert result.recommended_changes == 0
    assert %{status: :missing_complete_probabilities} =
             Enum.find(result.decisions, &(&1.token_index == 1))
  end

  test "an unrelated dictionary definition cannot be boosted by syntax alone" do
    si = fixture()

    no_match =
      put_in(si.sense_candidates[1],
        [
          %{id: "bank|noun|finance", definition: "An institution for money."},
          %{id: "bank|noun|river", definition: "A raised surface."}
        ])

    result = RelationalFeedback.evaluate(no_match)
    assert result.recommended_changes == 0
    assert %{status: :insufficient_independent_evidence} =
             Enum.find(result.decisions, &(&1.token_index == 1))
  end
end
