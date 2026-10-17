defmodule Brain.LIFG.RoleGlossEnumerationTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.RoleGlossCompatibility, as: Gloss
  alias Brain.LIFG.VerbRoleShadowFit, as: Shadow

  @sword_gloss "(transitive) Senses relating to extracting or selecting. — To pull (something) out; to extract, to remove. — To pull out (a firearm, sword, or other weapon) from a holster, sheath, etc.; to unsheathe."

  @argument %{
    status: :candidate, frame_id: :verb_direct_object_hypothesis,
    strength: :strong, supports: :transitive, rejects_intransitive?: false,
    roles: %{verb: 1, subject: 0, object_head: 3, object_determiner: 2},
    evidence: [:attested_verb_pos, :bounded_subject_frame, :surface_determiner,
               :attested_nominal_head, :immediately_following_nominal]
  }

  @role %{
    status: :candidate, frame_id: :verb_object_source_hypothesis,
    roles: %{verb: 1, object_head: 3, source_preposition: 4,
             source_head: 6, source_determiner: 5},
    words: %{object: "sword", preposition: "from", source: "sheath"},
    evidence: [:attested_direct_object, :source_phrase_adjacent_to_object,
               :explicit_from_preposition, :attested_nominal_source]
  }

  @senses [
    %{
      id: "draw|verb|79", norm: "draw", pos: "verb",
      status: "inactive", gram_function: ["transitive"],
      definition: @sword_gloss, example: nil, synonyms: [], semantic_atoms: []
    },
    %{
      id: "draw|verb|112", norm: "draw", pos: "verb",
      status: "inactive", gram_function: ["intransitive"],
      definition: "To pull out a firearm, sword, or other weapon from a holster, sheath, etc.",
      example: nil, synonyms: [], semantic_atoms: []
    },
    %{
      id: "draw|verb|17", norm: "draw", pos: "verb",
      status: "inactive", gram_function: ["transitive"],
      definition: "Of a vessel, to require a depth of water.",
      example: nil, synonyms: [], semantic_atoms: []
    }
  ]

  test "real sword-sense wording binds alternative object to alternative source" do
    assert {:ok, evidence} =
             Gloss.match(@sword_gloss,
               %{object: "sword", preposition: "from", source: "sheath"})

    assert evidence.pattern == :enumerated_object_source
    assert evidence.gloss_marker == "from"
    assert evidence.object_alternatives == 3
    assert evidence.source_alternatives == 3
    assert evidence.object_head == "sword"
    assert evidence.source_head == "sheath"
  end

  test "enumerated gloss relationship is independent of source marker paraphrase" do
    assert {:ok, evidence} =
             Gloss.match(@sword_gloss,
               %{object: "sword", preposition: "out of", source: "sheath"})

    assert evidence.pattern == :enumerated_object_source
  end

  test "dictionary sense gets one bonus without changing its lifecycle or competing senses" do
    before_cells = @senses

    report = Shadow.score(
      "They draw a sword from its sheath.", "draw", 1, before_cells,
      @argument, @role
    )

    rows = Map.new(report.candidates, &{&1.id, &1})

    assert report.role_observation_accepted?
    assert report.role_adjusted_senses == 1
    assert rows["draw|verb|79"].role_bonus == 0.10
    assert rows["draw|verb|79"].role_match.pattern == :enumerated_object_source
    assert rows["draw|verb|112"].role_bonus == 0.0
    assert rows["draw|verb|17"].role_bonus == 0.0
    assert before_cells == @senses
    assert report.activated_senses == 0
    assert Enum.all?(report.candidates, &(&1.status == "inactive"))
  end

  test "source must belong to the same bounded clause, not an unrelated later definition" do
    for gloss <- [
          "To pull out (a firearm, sword, or other weapon) from a holster; a sheath may be used.",
          "To pull out (a firearm, sword, or other weapon) from a holster. A sheath is used.",
          "To pull out (a firearm, sword, or other weapon) from a holster — a sheath is supplied.",
          "To pull out (a firearm, sword, or other weapon) from a holster - sheath.",
          "To pull out (a firearm, sword, or other weapon) from a holster\nfrom a sheath.",
          "To pull out (a firearm, sword, or other weapon) into a holster, sheath.",
          "To pull out (a firearm, sword, or other weapon). From a sheath.",
          "To pull out (a firearm, sword, or other weapon) from a holster, and then to secure a sheath."
        ] do
      assert :no_match ==
               Gloss.match(gloss, %{object: "sword", source: "sheath", preposition: "from"}),
             inspect(gloss)
    end
  end

  test "reversed roles and unrelated list members cannot fabricate a match" do
    for {gloss, relation} <- [
          {"To pull out (a sheath, holster, or holder) from a sword.",
           %{object: "sword", source: "sheath", preposition: "from"}},
          {@sword_gloss, %{object: "sheath", source: "sword", preposition: "from"}},
          {@sword_gloss, %{object: "water", source: "sheath", preposition: "from"}},
          {@sword_gloss, %{object: "sword", source: "well", preposition: "from"}}
        ] do
      assert :no_match == Gloss.match(gloss, relation)
    end
  end

  test "ordinary exact head matching remains independent of enumeration matching" do
    assert {:ok, %{pattern: "from"}} =
             Gloss.match("To take (water) from a well.",
               %{object: "water", source: "well", preposition: "from"})

    assert {:ok, %{pattern: "out of"}} =
             Gloss.match("To withdraw money out of the bank.",
               %{object: "money", source: "bank", preposition: "from"})
  end
end
