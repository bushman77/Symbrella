defmodule Core.MWE.ObjectSenseTest do
  use ExUnit.Case, async: true

  alias Core.MWE.{ObjectSense, SenseResolution}
  alias Core.Comprehension.Summary
  alias Core.SemanticInput

  defp choice(index, id, gloss) do
    %{
      token_index: index,
      id: id,
      lemma: id |> String.split("|") |> hd(),
      definition: gloss,
      score: 0.03,
      source: :active_cells
    }
  end

  defp input(noun \\ "computer") do
    old =
      if noun == "computer",
        do: "(archaic, historical) A person employed to perform computations.",
        else: "A person employed to perform a task."

    noun_id = noun <> "|noun|1"

    %SemanticInput{
      sentence: "Power the #{noun} down.",
      tokens: [
        %{index: 0, n: 1, phrase: "Power"},
        %{index: 1, n: 1, phrase: "the"},
        %{index: 2, n: 1, phrase: noun},
        %{index: 3, n: 1, phrase: "down."}
      ],
      intent: :command,
      lifg_choices: [
        choice(0, "power|verb|1",
          "(transitive) To provide power for a mechanical or electronic device."),
        choice(1, "the|determiner|1", "Used before a noun"),
        choice(2, noun_id, old),
        choice(3, "down|adverb|16", "Into a state of non-operation.")
      ],
      sentence_signatures: [
        %{
          id: :verb_particle_construction,
          span: {0, 3},
          roles: %{verb: 0, determiner: 1, object: 2, particle: 3},
          required_features: %{verb: [:pos_verb], object: [:pos_noun],
                               particle: [:pos_adverb]},
          evidence: [:separated_particle, :determiner_noun_object]
        }
      ],
      mwe_hypotheses: [
        %{
          phrase: "power down",
          category: :phrasal_verb,
          constituent_indices: [0, 3],
          particle_position: :separated,
          status: :unverified
        }
      ],
      atl_slate: %{
        constituents: [
          %{id: :verb_particle_construction, span: {0, 3}, status: :resolved}
        ]
      },
      active_cells: [
        %{id: noun_id, norm: noun, pos: "noun", definition: old}
      ]
    }
  end

  test "contextual second pass selects a real electronic computer noun sense" do
    si = input()
    alternative = %{
      id: "computer|noun|2",
      norm: "computer",
      pos: "noun",
      definition: "An electronic device for processing, storing, and retrieving information."
    }

    si = %{si | active_cells: si.active_cells ++ [alternative]}
    out = ObjectSense.refine(si)

    assert Enum.at(out.lifg_choices, 2).id == "computer|noun|2"
    assert Enum.at(out.lifg_choices, 2).definition == alternative.definition
    assert Enum.at(out.lifg_choices, 2).score == Enum.at(si.lifg_choices, 2).score
    assert Enum.at(out.lifg_choices, 2).context_score >= 0.12

    assert Enum.at(out.semantic_choices, 2).id == "computer|noun|2"
    assert Enum.at(out.lifg_choices, 0).id == "power|verb|1"
    assert out.tokens == si.tokens
    assert out.sentence_signatures == si.sentence_signatures
    assert Enum.any?(out.trace, fn
             %{stage: :object_sense_refinement, decision: :reranked} -> true
             _ -> false
           end)
  end

  test "the electronic noun allows verification of a real power down dictionary sense" do
    si = input()

    si =
      %{si | sense_candidates: %{2 => [
        %{id: "computer|noun|1", norm: "computer", pos: "noun",
          definition: "An archaic person who calculates."},
        %{id: "computer|noun|2", norm: "computer", pos: "noun",
          definition: "An electronic device that computes information."}
      ]}}
      |> ObjectSense.refine()

    phrase = %{
      id: "power down|verb|1", norm: "power down", pos: "verb",
      definition: "(ambitransitive) To switch off (something electrical)."
    }

    out =
      SenseResolution.attach_verified(si, mwe_lookup: fn terms ->
        assert "power down" in terms
        [phrase]
      end)

    assert [%{
      id: "power down|verb|1", source: :verified_discontinuous_mwe,
      object_index: 2
    }] = out.lexical_constructions
    assert [%{id: "power down|verb|1"}] = Summary.build(out).lexical_meanings
  end

  test "no alternative or no discriminating evidence leaves object unchanged" do
    si = input()
    assert ObjectSense.refine(si).lifg_choices == si.lifg_choices

    extra = %{
      id: "computer|noun|3", norm: "computer", pos: "noun",
      definition: "A person appointed to do calculations."
    }

    without_evidence = %{si | active_cells: si.active_cells ++ [extra]}
    assert ObjectSense.refine(without_evidence).lifg_choices == si.lifg_choices
  end

  test "a pronoun cannot be replaced with a noun sense" do
    si = input("it")
    choices =
      Enum.map(si.lifg_choices, fn
        %{token_index: 2} = c -> %{c | id: "it|pronoun|1"}
        other -> other
      end)

    si = %{si | lifg_choices: choices}
    assert ObjectSense.refine(si).lifg_choices == choices
  end

  test "incomplete or missing syntactic hypothesis never triggers noun reanalysis" do
    si = input()
    alternative = %{
      id: "computer|noun|2", norm: "computer", pos: "noun",
      definition: "An electronic device for processing information."
    }

    si = %{si | active_cells: si.active_cells ++ [alternative]}

    assert ObjectSense.refine(%{si | mwe_hypotheses: []}).lifg_choices == si.lifg_choices
    assert ObjectSense.refine(%{si | sentence_signatures: []}).lifg_choices ==
             si.lifg_choices
  end
end
