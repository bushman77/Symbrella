defmodule Brain.LIFG.VerbArgumentShadowScoringTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.VerbArgumentShadowScoring, as: Shadow

  @transitive %{
    id: "draw|verb|129", norm: "draw", pos: "verb", status: "inactive",
    definition: "To produce a picture with a pencil.",
    gram_function: ["transitive"],
    example: nil, synonyms: [], semantic_atoms: []
  }
  @intransitive %{
    id: "draw|verb|138", norm: "draw", pos: "verb", status: "inactive",
    definition: "To make drawings.",
    gram_function: ["intransitive"],
    example: nil, synonyms: [], semantic_atoms: []
  }

  @strong %{
    status: :candidate, frame_id: :verb_direct_object_hypothesis,
    strength: :strong, supports: :transitive, rejects_intransitive?: false,
    roles: %{verb: 1, subject: 0, object_head: 3, object_determiner: 2},
    evidence: [:attested_verb_pos, :bounded_subject_frame, :surface_determiner,
               :attested_nominal_head, :immediately_following_nominal]
  }

  test "strong direct-object hypothesis only positively supports transitive senses" do
    original = [@transitive, @intransitive]
    report = Shadow.score("They draw a picture.", "draw", 1, original, @strong)
    rows = Map.new(report.candidates, &{&1.id, &1})

    assert report.observation_strength == :strong
    assert report.adjusted_senses == 1
    assert report.activated_senses == 0
    assert report.production_winners_changed? == false
    assert_in_delta rows["draw|verb|129"].grammar_bonus, 0.12, 1.0e-8
    assert rows["draw|verb|138"].grammar_bonus == 0.0
    assert rows["draw|verb|129"].combined > rows["draw|verb|129"].lexical
    assert rows["draw|verb|138"].combined == rows["draw|verb|138"].lexical
    assert original == [@transitive, @intransitive]
    assert Enum.all?(report.candidates, &(&1.status == "inactive"))
  end

  test "weak direct-object evidence is bounded below the strong bonus" do
    weak = %{
      @strong |
      strength: :weak,
      evidence: [:attested_verb_pos, :bounded_subject_frame,
                 :attested_nominal_head, :bare_nominal_immediately_following_verb]
    }
    report = Shadow.score("They draw water.", "draw", 1, [@transitive, @intransitive], weak)
    row = Enum.find(report.candidates, &(&1.id == "draw|verb|129"))

    assert report.observation_strength == :weak
    assert_in_delta row.grammar_bonus, 0.06, 1.0e-8
  end

  test "abstention and wrong verb indices produce no grammatical adjustment" do
    abstain = %{status: :abstain, supports: nil, reason: :following_prepositional_phrase_not_direct_object}
    cells = [@transitive, @intransitive]

    for {observation, index} <- [{abstain, 1}, {@strong, 2}] do
      report = Shadow.score("They draw with a pencil.", "draw", index, cells, observation)
      assert report.adjusted_senses == 0
      assert report.observation_strength == nil
      assert Enum.all?(report.candidates, &(&1.grammar_bonus == 0.0))
    end
  end

  test "incomplete or untrusted observations cannot inject a bonus" do
    incomplete = %{ @strong | evidence: [:attested_verb_pos] }
    report = Shadow.score("They draw a picture.", "draw", 1,
      [@transitive, @intransitive], incomplete)
    assert report.adjusted_senses == 0
  end

  test "does not consider status an eligibility gate" do
    cells = [@transitive, @intransitive]
    active = Enum.map(cells, &Map.put(&1, :status, "active"))
    a = Shadow.score("They draw a picture.", "draw", 1, cells, @strong)
    b = Shadow.score("They draw a picture.", "draw", 1, active, @strong)

    assert Enum.map(a.candidates, &{&1.id, &1.combined}) ==
             Enum.map(b.candidates, &{&1.id, &1.combined})
  end

  test "no candidates or malformed request leaves no adjustments" do
    assert Shadow.score("They draw a picture.", "draw", 1, [], @strong).candidates == []
    assert Shadow.score(nil, "draw", 1, [@transitive], @strong).adjusted_senses == 0
  end
end
