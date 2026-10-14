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
                             source: :original_sentence_dictionary_context}]} =
             Enum.find(result.decisions, &(&1.token_index == 1))

    new_probs = Enum.find(result.decisions, &(&1.token_index == 1)).posterior
    assert new_probs["bank|noun|river"] > new_probs["bank|noun|finance"]
    assert_in_delta Enum.sum(Map.values(new_probs)), 1.0, 0.000001
  end

  test "changing the other token's selected sense does not create self-confirmation" do
    si = fixture()
    modified =
      put_in(si.atl_slate.constituents,
        [%{hd(si.atl_slate.constituents) |
            roles: %{hd(si.atl_slate.constituents).roles |
              modifier: %{token_index: 0, sense_id: "river|noun|999"}}}])

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
