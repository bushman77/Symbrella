defmodule Brain.LIFG.PrepositionSemanticsTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.{PrepositionSemantics, Stage1}

  @sentence "The fisherman sat on the bank of the river."

  @tokens [
    %{index: 0, phrase: "The", n: 1},
    %{index: 1, phrase: "fisherman", n: 1},
    %{index: 2, phrase: "sat", n: 1},
    %{index: 3, phrase: "on", n: 1},
    %{index: 4, phrase: "the", n: 1},
    %{index: 5, phrase: "bank", n: 1},
    %{index: 6, phrase: "of", n: 1},
    %{index: 7, phrase: "the", n: 1},
    %{index: 8, phrase: "river", n: 1}
  ]

  @pp %{
    id: :prepositional_phrase, source: :core_syntax, span: {3, 5},
    roles: %{preposition: 3, determiner: 4, head: 5, object: 5},
    evidence: [:preposition, :noun_phrase_complement]
  }

  @ordinary %{
    id: "on|preposition|1", norm: "on", pos: "preposition", token_index: 3,
    definition: "Indicating position or location. — Positioned at the upper surface of, touching from above."
  }

  @topic %{
    id: "on|preposition|19", norm: "on", pos: "preposition", token_index: 3,
    definition: "Dealing with the subject of; about; concerning."
  }

  @obsolete %{
    id: "on|preposition|46", norm: "on", pos: "preposition", token_index: 3,
    definition: "(dialectal, obsolete, regional) Of."
  }

  @predicate %{
    id: "sat|verb|1", norm: "sat", pos: "verb", token_index: 2,
    definition: "The simple past tense of sit."
  }

  @place %{
    id: "bank|noun|11", norm: "bank", pos: "noun", token_index: 5,
    definition: "An edge of river, lake, or other watercourse."
  }

  defp input do
    %{
      sentence: @sentence, tokens: @tokens, sentence_signatures: [@pp],
      active_cells: [@predicate, @place, @ordinary, @topic, @obsolete]
    }
  end

  defp scores, do: %{@ordinary.id => 0.6, @topic.id => 0.6, @obsolete.id => 0.6}

  test "attested physical PP supports only dictionary senses with spatial glosses" do
    original = scores()

    {updated, report} =
      PrepositionSemantics.adjust(original, [@ordinary, @topic, @obsolete], input(), 3,
        enabled?: true)

    assert report.applied?
    assert report.reason == :supported_spatial_prepositional_compatibility
    assert report.predicate_index == 2
    assert report.object_index == 5
    assert report.predicate_witness == @predicate.id
    assert report.object_witness == @place.id
    assert [%{id: id, delta: 0.12}] = report.adjustments
    assert id == @ordinary.id
    assert_in_delta updated[@ordinary.id], 0.72, 0.0000001
    assert updated[@topic.id] == original[@topic.id]
    assert updated[@obsolete.id] == original[@obsolete.id]
    assert Enum.sort(Map.keys(updated)) == Enum.sort(Map.keys(original))
  end

  test "topic, missing object evidence, and unauthenticated PP all abstain" do
    original = scores()

    topic = input()
    |> Map.put(:sentence, "The fisherman gave a lecture on the bank of the river.")
    |> Map.put(:active_cells, [
      %{@predicate | definition: "To explain a subject."}, @place, @ordinary, @topic
    ])

    assert {^original, %{applied?: false, reason: :missing_posture_predicate_witness}} =
             PrepositionSemantics.adjust(original, [@ordinary, @topic, @obsolete],
               topic, 3, enabled?: true)

    wrong_object = input() |> Map.put(:active_cells, [
      @predicate, %{@place | definition: "A financial institution."},
      @ordinary, @topic
    ])
    assert {^original, %{applied?: false, reason: :missing_contextual_location_object_witness}} =
             PrepositionSemantics.adjust(original, [@ordinary, @topic, @obsolete],
               wrong_object, 3, enabled?: true)

    unsupported = input() |> Map.put(:sentence_signatures, [
      %{@pp | source: :unverified_text}
    ])
    assert {^original, %{applied?: false, reason: :missing_attested_prepositional_phrase}} =
             PrepositionSemantics.adjust(original, [@ordinary, @topic, @obsolete],
               unsupported, 3, enabled?: true)
  end

  test "disabled option remains score preserving" do
    original = scores()
    assert {^original, %{applied?: false, reason: :disabled}} =
             PrepositionSemantics.adjust(original, [@ordinary, @topic], input(), 3)
  end

  test "Stage1 opt-in scores spatial sense while retaining topic alternative" do
    si = input()
    |> Map.put(:sense_candidates, %{3 => [
      Map.put(@ordinary, :activation, 0.5),
      Map.put(@topic, :activation, 0.5)
    ]})

    assert {:ok, %{choices: choices}} =
             Stage1.run(si, preserve_sense_candidates: true,
               scores: :all, semantic_control?: false,
               usage_register?: false, spatial_pp_senses?: true,
               cerebellum_learn?: false)

    choice = Enum.find(choices, &(&1.token_index == 3))
    assert is_map(choice)
    assert choice.spatial_pp.applied?
    assert choice.chosen_id == @ordinary.id
    assert Map.has_key?(choice.scores, @topic.id)
    assert Map.has_key?(choice.scores, @ordinary.id)
  end
end
