defmodule Brain.LIFG.StructuralCompetitionTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.StructuralCompetition

  defp tokens do
    ~w(The cat is sleeping under the table)
    |> Enum.with_index()
    |> Enum.map(fn {word, index} -> %{index: index, phrase: word, n: 1} end)
  end

  defp progressive do
    %{
      id: :progressive_locative_candidate,
      source: :core_syntax,
      span: {0, 6},
      roles: %{
        subject_determiner: 0, subject_head: 1, auxiliary: 2,
        predicate: 3, preposition: 4, location_determiner: 5,
        location_head: 6
      },
      required_features: %{
        subject_head: [:pos_noun],
        predicate: [:present_participle],
        preposition: [:pos_preposition],
        location_head: [:pos_noun]
      },
      evidence: [:contiguous_surface, :attested_present_participle]
    }
  end

  test "a complete sentence is preferred despite conflicting local lexical winners" do
    si = %{
      tokens: tokens(),
      sentence_signatures: [progressive()],
      lifg_choices: [
        %{token_index: 1, id: "cat|adjective|1"},
        %{token_index: 3, id: "sleeping|adjective|1"},
        %{token_index: 4, id: "under|preposition|1"},
        %{token_index: 6, id: "table|noun|1"}
      ]
    }

    result = StructuralCompetition.resolve(si)

    assert result.status == :preferred
    assert result.preferred.construction == :progressive_locative_candidate
    assert result.preferred.matched_roles == 2
    assert result.preferred.conflicting_roles == 2
    assert result.factual_authority? == false
    assert si.lifg_choices == [
      %{token_index: 1, id: "cat|adjective|1"},
      %{token_index: 3, id: "sleeping|adjective|1"},
      %{token_index: 4, id: "under|preposition|1"},
      %{token_index: 6, id: "table|noun|1"}
    ]
  end

  test "equally supported competing complete readings remain unresolved" do
    alt = %{progressive() |
      id: :another_complete_clause,
      roles: %{subject_head: 1, predicate: 3, object_head: 6},
      required_features: %{subject_head: [:pos_noun], predicate: [:pos_verb]}
    }

    si = %{
      tokens: tokens(),
      sentence_signatures: [progressive(), alt],
      lifg_choices: [%{token_index: 1, id: "cat|noun|1"},
                     %{token_index: 3, id: "sleeping|verb|1"},
                     %{token_index: 4, id: "under|preposition|1"},
                     %{token_index: 6, id: "table|noun|1"}]
    }

    result = StructuralCompetition.resolve(si)

    assert result.status == :unresolved
    assert is_nil(result.preferred)
    assert length(result.candidates) == 2
    assert result.discrimination_margin < 0.12
  end

  test "constituent signatures alone do not claim whole-sentence interpretation" do
    sig = %{id: :noun_phrase, source: :core_syntax, span: {0, 1},
            roles: %{determiner: 0, head: 1}, evidence: [:pos_pattern]}

    result = StructuralCompetition.resolve(%{
      tokens: tokens(), sentence_signatures: [sig],
      lifg_choices: [%{token_index: 1, id: "cat|noun|1"}]
    })

    assert result.status == :no_complete_candidate
    assert result.candidates == []
  end

  test "one supporting role cannot overrule several contradictory roles" do
    si = %{
      tokens: tokens(),
      sentence_signatures: [progressive()],
      lifg_choices: [
        %{token_index: 1, id: "cat|adjective|1"},
        %{token_index: 3, id: "sleeping|adjective|1"},
        %{token_index: 4, id: "under|preposition|1"},
        %{token_index: 6, id: "table|verb|1"}
      ]
    }

    result = StructuralCompetition.resolve(si)
    assert result.status == :unresolved
    assert result.preferred == nil
    assert hd(result.candidates).matched_roles == 1
    assert hd(result.candidates).conflicting_roles == 3
  end

  test "local lexical disagreement without positive evidence abstains" do
    si = %{
      tokens: tokens(),
      sentence_signatures: [progressive()],
      lifg_choices: [%{token_index: 1, id: "cat|adjective|1"},
                     %{token_index: 3, id: "sleeping|adjective|1"},
                     %{token_index: 4, id: "under|noun|1"},
                     %{token_index: 6, id: "table|verb|1"}]
    }

    result = StructuralCompetition.resolve(si)
    assert result.status == :unresolved
    assert result.preferred == nil
    assert hd(result.candidates).conflicting_roles == 4
  end
end
