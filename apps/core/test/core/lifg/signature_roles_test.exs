defmodule Core.LIFG.SignatureRolesTest do
  use ExUnit.Case, async: true

  alias Core.LIFG.SignatureRoles

  defp si(signatures), do: %{sentence_signatures: signatures}

  defp separated(verb, object, particle) do
    %{
      id: :verb_particle_construction,
      span: {verb, particle},
      roles: %{verb: verb, object: object, particle: particle},
      required_features: %{
        verb: [:pos_verb],
        object: [:pos_noun_or_pronoun],
        particle: [:pos_adverb, :pos_preposition]
      }
    }
  end

  test "a complete separated construction requires a genuine existing verb alternative" do
    input = si([separated(0, 2, 3)])

    assert SignatureRoles.required_pos(input, 0) == "verb"
    assert SignatureRoles.required_pos(input, 1) == nil
    assert SignatureRoles.required_pos(input, 2) == nil
    assert SignatureRoles.required_pos(input, 3) == nil
  end

  test "the same construction works within a declarative after its explicit subject" do
    input = si([separated(1, 3, 4)])
    assert SignatureRoles.required_pos(input, 1) == "verb"
    assert SignatureRoles.required_pos(input, 0) == nil
  end

  test "a competing subject NP and attested finite-past verb block a false verb-particle POS requirement" do
    # A bat flew through the cave. The generic dictionary alternatives
    # license a speculative "bat flew through" particle hypothesis, but
    # the independently attested subject and past-tense predicate conflict.
    subject = %{
      id: :noun_phrase,
      span: {0, 1},
      roles: %{determiner: 0, head: 1}
    }

    past = %{
      id: :finite_past_candidate,
      span: {2, 2},
      roles: %{verb: 2},
      evidence: [:simple_past_form_available]
    }

    input = si([separated(1, 2, 3), subject, past])
    assert SignatureRoles.required_pos(input, 1) == nil

    # A noun phrase hypothesis alone must not erase the previously
    # supported genuine separated-verb construction.
    assert SignatureRoles.required_pos(si([separated(1, 2, 3), subject]), 1) == "verb"

    # A past-tense form on some unrelated token cannot contradict the
    # specific intervening object/predicate position.
    unrelated_past = %{past | roles: %{verb: 5}}
    assert SignatureRoles.required_pos(
      si([separated(1, 2, 3), subject, unrelated_past]), 1
    ) == "verb"
  end

  test "adjacent, incomplete and invalid span candidates cannot force verb POS" do
    adjacent = %{
      id: :verb_particle_construction,
      span: {0, 1},
      roles: %{verb: 0, particle: 1}
    }

    invalid_span = %{
      id: :verb_particle_construction,
      span: {0, 2},
      roles: %{verb: 0, object: 1, particle: 3}
    }

    wrong_order = %{
      id: :verb_particle_construction,
      span: {0, 3},
      roles: %{verb: 0, object: 3, particle: 2}
    }

    assert SignatureRoles.required_pos(si([adjacent]), 0) == nil
    assert SignatureRoles.required_pos(si([invalid_span]), 0) == nil
    assert SignatureRoles.required_pos(si([wrong_order]), 0) == nil
    assert SignatureRoles.required_pos(si([]), 0) == nil
  end

  test "existing have-to-infinitive role behavior is preserved" do
    sig = %{
      id: :have_verb_to_infinitive,
      roles: %{auxiliary: 0, verb: 1, infinitive_marker: 2, complement: 3}
    }

    assert SignatureRoles.required_pos(si([sig]), 3) == "verb"
    assert SignatureRoles.required_pos(si([sig]), 2) == "particle"
    assert SignatureRoles.required_pos(si([sig]), 1) == nil
  end
end
