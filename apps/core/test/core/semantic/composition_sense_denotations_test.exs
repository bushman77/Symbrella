defmodule Core.Semantic.CompositionSenseDenotationsTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition.SenseDenotations

  defp inspect_gloss(id, definition) do
    SenseDenotations.propose(%{
      id: id, norm: "on", pos: "preposition",
      definition: definition
    })
  end

  defp subtypes(report), do: Enum.map(report.candidates, & &1.subtype)

  test "distinguishes surface position, proximity and transport from generic spatiality" do
    surface = inspect_gloss("on|preposition|1",
      "Indicating position or location. — Positioned at the upper surface of, touching from above.")
    nearby = inspect_gloss("on|preposition|6",
      "Indicating position or location. — Near; adjacent to; alongside; just off.")
    transport = inspect_gloss("on|preposition|7",
      "Indicating position or location. — Aboard (a mode of transport, especially public transport, or transport that one sits astride or uses while standing).")

    assert subtypes(surface) == [:upper_surface]
    assert subtypes(nearby) == [:adjacency]
    assert subtypes(transport) == [:aboard_transport]
    assert hd(transport.candidates).required_context == [:transport_landmark]
    refute surface.interpreted_word_sense?
    refute transport.changes_lifg?
  end

  test "retains overlapping contact, force and instrumental uses" do
    combined = inspect_gloss("on|preposition|12",
      "Denoting physical contact or interaction with an object, such as impact or application of force. — Denoting performance or action by contact with the surface, upper part, or outside of anything By means of; with.")

    assert combined.multiple_readings?
    assert :force_target in subtypes(combined)
    assert :contact_means in subtypes(combined)
    assert :means in subtypes(combined)
    assert Enum.all?(combined.candidates, &(&1.verification == :not_established))
  end

  test "retains two hypotheses in a mixed imprecation and resting definition" do
    mixed = inspect_gloss("on|preposition|38",
      "To the account or detriment of; denoting imprecation or invocation, or coming to, falling, or resting upon.")

    assert mixed.multiple_readings?
    assert :resting_upon in subtypes(mixed)
    assert :imprecation in subtypes(mixed)
  end

  test "topic and adversary families do not become physical-location meanings" do
    topic = inspect_gloss("on|preposition|19",
      "Dealing with the subject of; about; concerning.")
    opponent = inspect_gloss("on|preposition|39",
      "(especially when numbers of combatants or competitors are specified) Against; in opposition to.")

    assert Enum.map(topic.candidates, & &1.relation_family) == [:discourse_topic]
    assert Enum.map(opponent.candidates, & &1.relation_family) == [:opposition]
  end

  test "unknown and incomplete glosses abstain without guessing from the sense ID" do
    unknown = inspect_gloss("on|preposition|28",
      "(ireland, especially) Indicating the person experiencing an emotion, cold, thirst, hunger, etc.")
    # The explicit emotion cue in this definition is recognized, but its
    # presence does not assert that a particular sentence uses this sense.
    assert :experiencer in subtypes(unknown)

    empty = inspect_gloss("on|preposition|1", nil)
    refute empty.multiple_readings?
    assert empty.unresolved?
    assert empty.candidates == []
  end
end
