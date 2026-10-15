defmodule Brain.LIFG.VerbRoleShadowFitTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.VerbRoleShadowFit

  @cells [
    %{
      id: "draw|verb|81", pos: "verb", status: "inactive",
      definition: "To take up (water) from a well or other source.",
      gram_function: ["transitive"], example: nil, synonyms: [], semantic_atoms: []
    },
    %{
      id: "draw|verb|113", pos: "verb", status: "inactive",
      definition: "To take up water from a well by lifting it in a container.",
      gram_function: ["intransitive"], example: nil, synonyms: [], semantic_atoms: []
    },
    %{
      id: "draw|verb|17", pos: "verb", status: "inactive",
      definition: "Of a vessel: to require a certain depth of water to float in.",
      gram_function: ["transitive"], example: nil, synonyms: [], semantic_atoms: []
    }
  ]

  @argument %{
    status: :candidate,
    frame_id: :verb_direct_object_hypothesis,
    strength: :weak,
    supports: :transitive,
    rejects_intransitive?: false,
    roles: %{verb: 1, object_head: 2},
    evidence: [:attested_verb_pos, :bounded_subject_frame,
               :attested_nominal_head, :bare_nominal_immediately_following_verb]
  }

  @source %{
    status: :candidate,
    frame_id: :verb_object_source_hypothesis,
    roles: %{
      verb: 1, object_head: 2, source_preposition: 3,
      source_head: 5, source_determiner: 4
    },
    words: %{object: "water", preposition: "from", source: "well"},
    evidence: [
      :attested_direct_object, :source_phrase_adjacent_to_object,
      :explicit_from_preposition, :attested_nominal_source
    ]
  }

  defp result(sentence \\ "They draw water from a well.", argument \\ @argument, role \\ @source, cells \\ @cells) do
    VerbRoleShadowFit.score(sentence, "draw", 1, cells, argument, role)
  end

  test "matches water from a well as a structured gloss relation, not a nautical water mention" do
    before = @cells
    report = result()
    rows = Map.new(report.candidates, &{&1.id, &1})

    assert report.role_observation_accepted?
    assert report.role_adjusted_senses == 1
    assert report.activated_senses == 0
    assert report.production_winners_changed? == false
    assert rows["draw|verb|81"].role_bonus == 0.10
    assert rows["draw|verb|17"].role_bonus == 0.0
    assert rows["draw|verb|113"].role_bonus == 0.0
    assert rows["draw|verb|81"].role_evidence == %{
      object: "water", preposition: "from", source: "well"
    }
    assert hd(report.candidates).id == "draw|verb|81"
    assert before == @cells
    assert Enum.all?(report.candidates, &(&1.status == "inactive"))
  end

  test "separate gloss word overlap does not imply a source relation" do
    decoy = %{
      Enum.at(@cells, 0) |
      definition: "A well contains water and attracts visitors from a town."
    }
    cells = [decoy | tl(@cells)]
    report = result("They draw water from a well.", @argument, @source, cells)
    assert report.role_adjusted_senses == 0
  end

  test "invalid source index or supplied source word cannot invent a role" do
    bad_index = put_in(@source, [:roles, :source_head], 99)
    wrong_source = put_in(@source, [:words, :source], "lake")
    missing_evidence = Map.put(@source, :evidence, [])

    for role <- [bad_index, wrong_source, missing_evidence] do
      report = result("They draw water from a well.", @argument, role)
      refute report.role_observation_accepted?
      assert report.role_adjusted_senses == 0
    end
  end

  test "punctuation and separated source phrases never match a forged observation" do
    for sentence <- [
      "They draw water. From a well.",
      "They draw water; from a well.",
      "They draw water, from a well.",
      "They draw water from a, well."
    ] do
      report = result(sentence)
      refute report.role_observation_accepted?, sentence
      assert report.role_adjusted_senses == 0
    end
  end

  test "abstaining for missing object leaves meaning unresolved, not intransitive" do
    no_argument = %{status: :abstain, reason: :following_prepositional_phrase_not_direct_object}
    no_source = %{status: :abstain, reason: :direct_object_unresolved}
    report = result("They draw water from a well.", no_argument, no_source)
    refute report.role_observation_accepted?
    assert report.role_adjusted_senses == 0
    assert Enum.all?(report.candidates, &(&1.role_bonus == 0.0))
  end

  test "changing lifecycle status cannot alter semantic role eligibility" do
    active = Enum.map(@cells, &Map.put(&1, :status, "active"))
    a = result()
    b = result("They draw water from a well.", @argument, @source, active)
    assert Enum.map(a.candidates, &{&1.id, &1.combined}) ==
             Enum.map(b.candidates, &{&1.id, &1.combined})
  end
end
