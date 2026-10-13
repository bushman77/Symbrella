defmodule Core.MWE.SenseResolutionTest do
  use ExUnit.Case, async: true

  alias Core.MWE.SenseResolution
  alias Core.Comprehension.Summary

  @turn_off_senses [
    %{
      id: "turn off|verb|1", norm: "turn off", pos: "verb",
      definition: "(obsolete, transitive) To dismiss; to fire.",
      example: "They turned him off.", gram_function: ["transitive"]
    },
    %{
      id: "turn off|verb|2", norm: "turn off", pos: "verb",
      definition: "(transitive) To power down, to switch off, to deactivate an appliance, light or mechanism.",
      example: "Turn off the machine and unplug it.",
      synonyms: ["deactivate", "shut off"], gram_function: ["transitive"]
    },
    %{
      id: "turn off|verb|4", norm: "turn off", pos: "verb",
      definition: "(transitive) To rotate a tap or valve to stop water.",
      gram_function: ["transitive"]
    },
    %{
      id: "turn off|verb|5", norm: "turn off", pos: "verb",
      definition: "(transitive) To repulse or disgust somebody.",
      gram_function: ["transitive"]
    }
  ]

  defp choice(index, id, definition \\ nil) do
    %{
      token_index: index,
      id: id,
      lemma: id |> String.split("|") |> hd(),
      definition: definition,
      score: 0.04
    }
  end

  defp tokens(words) do
    words
    |> Enum.with_index()
    |> Enum.map(fn {w, i} -> %{index: i, phrase: w, n: 1} end)
  end

  defp discontinuous(words, object_index, object_id, object_gloss \\ nil) do
    last = length(words) - 1
    phrase = String.downcase(hd(words)) <> " " <> "off"

    sig = %{
      id: :verb_particle_construction,
      span: {0, last},
      roles: %{verb: 0, object: object_index, particle: last},
      required_features: %{
        verb: [:pos_verb],
        object: [:pos_noun],
        particle: [:pos_adverb, :pos_preposition]
      }
    }

    %Core.SemanticInput{
      sentence: Enum.join(words, " "),
      tokens: tokens(words),
      intent: :command,
      confidence: 0.82,
      lifg_choices: [
        choice(0, String.downcase(hd(words)) <> "|verb|1"),
        choice(object_index, object_id, object_gloss),
        choice(last, "off|adverb|1")
      ],
      sentence_signatures: [sig],
      mwe_hypotheses: [
        %{
          phrase: phrase, category: :phrasal_verb,
          constituent_indices: [0, last], particle_position: :separated
        }
      ],
      atl_slate: %{
        constituents: [
          %{id: :verb_particle_construction, span: {0, last}, status: :resolved}
        ]
      }
    }
  end

  defp lookup(senses) do
    fn terms ->
      Enum.filter(senses, fn sense ->
        sense.norm in terms
      end)
    end
  end

  test "contiguous verb MWE selects context-supported exact dictionary sense over obsolete first sense" do
    si = %Core.SemanticInput{
      sentence: "Turn off the lights.",
      tokens: tokens(~w(Turn off the lights.)),
      lifg_choices: [
        choice(1, "off|adjective|1"),
        choice(2, "the|determiner|1"),
        choice(3, "lights|noun|1", "(plural) plural of light")
      ],
      mwe_choices: [
        %{
          id: "turn off|verb|1", token_index: 4, token_span: {0, 1},
          phrase: "Turn off", lemma: "turn off", pos: "verb",
          source: :mwe_lexicon, score: 0.125
        }
      ],
      mwe_lexicon: [
        %{phrase: "turn off", span: {0, 1}, senses: @turn_off_senses}
      ]
    }

    updated = SenseResolution.rerank_contiguous(si)

    assert updated.mwe_choices |> hd() |> Map.fetch!(:id) == "turn off|verb|2"
    assert Enum.any?(updated.semantic_choices, &(&1.id == "turn off|verb|2"))
    refute Enum.any?(updated.semantic_choices, &(&1.id == "turn off|verb|1"))
    assert updated.lifg_choices == si.lifg_choices
    assert updated.tokens == si.tokens
    assert Enum.any?(updated.trace, fn
             %{stage: :mwe_sense_refinement, decision: :reranked} -> true
             _ -> false
           end)

    with_evidence = SenseResolution.attach_verified(updated)
    assert [%{id: "turn off|verb|2", source: :mwe_lexicon}] =
             with_evidence.lexical_constructions

    summary = Summary.build(with_evidence)
    assert "turn off" in summary.understood
    assert [%{id: "turn off|verb|2"}] = summary.lexical_meanings
  end

  test "separated grammar verifies real dictionary phrasal sense without fabricating child words" do
    si =
      discontinuous(~w(Turn the lights off.), 2, "lights|noun|1",
        "(plural) plural of light"
      )

    after_ = SenseResolution.attach_verified(si, mwe_lookup: lookup(@turn_off_senses))

    assert [%{
      id: "turn off|verb|2", source: :verified_discontinuous_mwe,
      dictionary_verified?: true, constituent_indices: [0, 3],
      object_index: 2
    }] = after_.lexical_constructions

    assert after_.lifg_choices == si.lifg_choices
    assert after_.tokens == si.tokens

    summary = Summary.build(after_)
    assert "turn off" in summary.understood
    assert [%{id: "turn off|verb|2", phrase: "turn off"}] = summary.lexical_meanings
    refute "turn" in summary.uncertain
    refute "off" in summary.uncertain
  end

  test "tap object supports a different sense of the same lexical verb" do
    si = discontinuous(~w(Turn the tap off.), 2, "tap|noun|1", "A water tap.")
    after_ = SenseResolution.attach_verified(si, mwe_lookup: lookup(@turn_off_senses))

    assert [%{id: "turn off|verb|4"}] = after_.lexical_constructions
  end

  test "selected noun object's device gloss provides lexical type evidence for other verbs" do
    switch = %{
      id: "switch off|verb|1", norm: "switch off", pos: "verb",
      definition: "To stop or disable a device by turning a switch.",
      gram_function: ["transitive"]
    }

    si =
      discontinuous(~w(Switch the fan off.), 2, "fan|noun|2",
        "A mechanical device for moving air."
      )

    after_ = SenseResolution.attach_verified(si, mwe_lookup: lookup([switch]))
    assert [%{id: "switch off|verb|1"}] = after_.lexical_constructions
  end

  test "a zero-evidence tie never manufactures a confident meaning" do
    si = discontinuous(~w(Turn the employees off.), 2, "employees|noun|1")

    after_ = SenseResolution.attach_verified(si, mwe_lookup: lookup(@turn_off_senses))

    assert after_.lexical_constructions == []
    assert Summary.build(after_).lexical_meanings == []
    assert after_.lifg_choices == si.lifg_choices
  end

  test "a partial grammar frame cannot be promoted into a verified meaning" do
    si = discontinuous(~w(Turn the lights off.), 2, "lights|noun|1")
    si = %{si | atl_slate: %{constituents: []}}

    after_ =
      SenseResolution.attach_verified(si,
        mwe_lookup: fn _ -> raise "dictionary must not be queried without an ATL frame" end
      )

    assert after_.lexical_constructions == []
  end
end
