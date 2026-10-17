defmodule Brain.LIFG.VerbRoleShadowFitGeneralizationTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.VerbRoleShadowFit

  @cases [
    %{
      sentence: "They draw water from a well.",
      verb: "draw", object: "water", source: "well", marker: "from",
      verb_index: 1, object_index: 2, marker_index: 3, marker_end: 3,
      determiner_index: 4, source_index: 5, strength: :weak,
      gloss: "To lift (water) out of a well."
    },
    %{
      sentence: "They pull a sword out of its sheath.",
      verb: "pull", object: "sword", source: "sheath", marker: "out of",
      verb_index: 1, object_index: 3, marker_index: 4, marker_end: 5,
      determiner_index: 6, source_index: 7, strength: :strong,
      gloss: "To remove a sword from its sheath."
    },
    %{
      sentence: "They withdraw money from a bank.",
      verb: "withdraw", object: "money", source: "bank", marker: "from",
      verb_index: 1, object_index: 2, marker_index: 3, marker_end: 3,
      determiner_index: 4, source_index: 5, strength: :weak,
      gloss: "To take money out of a bank."
    }
  ]

  defp observations(c) do
    grammar =
      if c.strength == :strong do
        [:attested_verb_pos, :bounded_subject_frame, :surface_determiner,
         :attested_nominal_head, :immediately_following_nominal]
      else
        [:attested_verb_pos, :bounded_subject_frame,
         :attested_nominal_head, :bare_nominal_immediately_following_verb]
      end

    argument = %{
      status: :candidate,
      frame_id: :verb_direct_object_hypothesis,
      roles: %{verb: c.verb_index, object_head: c.object_index},
      strength: c.strength, supports: :transitive,
      rejects_intransitive?: false, evidence: grammar
    }

    role_positions = %{
      verb: c.verb_index, object_head: c.object_index,
      source_preposition: c.marker_index, source_head: c.source_index,
      source_determiner: c.determiner_index
    }

    role_positions =
      if c.marker == "out of",
        do: Map.put(role_positions, :source_marker_end, c.marker_end),
        else: role_positions

    marker_evidence =
      if c.marker == "out of",
        do: :explicit_out_of_source_marker,
        else: :explicit_from_preposition

    role = %{
      status: :candidate, frame_id: :verb_object_source_hypothesis,
      roles: role_positions,
      words: %{object: c.object, source: c.source, preposition: c.marker},
      evidence: [:attested_direct_object, :source_phrase_adjacent_to_object,
                 marker_evidence, :attested_nominal_source]
    }
    {argument, role}
  end

  defp cells(c) do
    [
      %{
        id: "#{c.verb}|verb|1", norm: c.verb, pos: "verb",
        status: "inactive", gram_function: ["transitive"],
        definition: c.gloss, example: nil, synonyms: [], semantic_atoms: []
      },
      %{
        id: "#{c.verb}|verb|2", norm: c.verb, pos: "verb",
        status: "inactive", gram_function: ["intransitive"],
        definition: "A different sense referring to #{c.object}.",
        example: nil, synonyms: [], semantic_atoms: []
      },
      %{
        id: "#{c.verb}|verb|3", norm: c.verb, pos: "verb",
        status: "inactive", gram_function: ["transitive"],
        definition: "A different sense concerning #{c.source}.",
        example: nil, synonyms: [], semantic_atoms: []
      }
    ]
  end

  test "grammar and per-sense gloss matching generalize across draw, pull and withdraw" do
    for c <- @cases do
      {argument, role} = observations(c)
      original = cells(c)
      report = VerbRoleShadowFit.score(c.sentence, c.verb, c.verb_index,
        original, argument, role)

      by_id = Map.new(report.candidates, &{&1.id, &1})
      assert report.role_observation_accepted?, c.sentence
      assert report.role_adjusted_senses == 1, c.sentence
      assert by_id["#{c.verb}|verb|1"].role_bonus == 0.10
      assert by_id["#{c.verb}|verb|2"].role_bonus == 0.0
      assert by_id["#{c.verb}|verb|3"].role_bonus == 0.0
      assert report.activated_senses == 0
      assert Enum.all?(original, &(&1.status == "inactive"))
    end
  end

  test "forged verb surface and untrusted role indices cannot add semantic evidence" do
    c = Enum.at(@cases, 1)
    {argument, role} = observations(c)

    for {sentence, altered_role} <- [
          {"They draw a sword out of its sheath.", role},
          {c.sentence, put_in(role, [:roles, :source_head], 999)},
          {c.sentence, put_in(role, [:roles, :source_marker_end], 99)},
          {c.sentence, put_in(role, [:words, :source], "bank")}
        ] do
      report = VerbRoleShadowFit.score(sentence, c.verb, c.verb_index,
        cells(c), argument, altered_role)
      refute report.role_observation_accepted?
      assert report.role_adjusted_senses == 0
    end
  end

  test "lifecycle changes do not alter candidate-specific compatibility" do
    c = Enum.at(@cases, 2)
    {argument, role} = observations(c)
    inactive = cells(c)
    active = Enum.map(inactive, &Map.put(&1, :status, "active"))

    a = VerbRoleShadowFit.score(c.sentence, c.verb, c.verb_index,
      inactive, argument, role)
    b = VerbRoleShadowFit.score(c.sentence, c.verb, c.verb_index,
      active, argument, role)

    assert Enum.map(a.candidates, &{&1.id, &1.combined}) ==
             Enum.map(b.candidates, &{&1.id, &1.combined})
  end
end
