defmodule Core.Brain.SemanticPropagationTest do
  use ExUnit.Case, async: true

  alias Core.Brain.{Activation, SemanticPropagation}
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @svo_evidence [:contiguous_surface, :attested_nominal_subject,
                 :attested_nominal_object, :attested_finite_present_verb]
  @verb_tags ["tag:form-of", "tag:indicative", "tag:present",
              "tag:singular", "tag:third-person"]

  defp assertion do
    choices = [
      %{token_index: 1, id: "artist|noun|1", lemma: "artist"},
      %{token_index: 2, id: "draws|verb|1", lemma: "draws"},
      %{token_index: 4, id: "picture|noun|1", lemma: "picture"}
    ]

    %{
      sentence: "The artist draws a picture.", source: :user,
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: "artist", n: 1},
        %{index: 2, phrase: "draws", n: 1},
        %{index: 3, phrase: "a", n: 1},
        %{index: 4, phrase: "picture.", n: 1}
      ],
      sentence_signatures: [
        %{id: :finite_present_svo_candidate, source: :core_syntax,
          span: {0, 4}, evidence: @svo_evidence,
          roles: %{subject_determiner: 0, subject_head: 1, predicate: 2,
                   object_determiner: 3, object_head: 4}}
      ],
      active_cells: [
        %{id: "artist|noun|1", norm: "artist", pos: "noun"},
        %{id: "picture|noun|1", norm: "picture", pos: "noun"},
        %{id: "draws|verb|1", norm: "draws", pos: "verb",
          semantic_atoms: @verb_tags, definition: "An inflected form of draw."}
      ],
      lifg_choices: choices,
      semantic_choices: choices,
      atl_slate: %{winners: choices}, trace: []
    }
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  test "attested inflection creates only a bounded, non-sense lemma pulse" do
    si = assertion()

    assert si.sentence_meaning.status == :structurally_interpreted
    assert %{lemma: "draw", source_id: "draws|verb|1", delta: 0.03,
             scope: :verb_lemma_not_sense} = SemanticPropagation.lemma_pulse(si)

    assert [
      %{id: "artist|noun|1", delta: 0.025,
        evidence: :structural_role_not_lexically_verified},
      %{id: "draws|verb|1", delta: 0.025,
        evidence: :structural_role_not_lexically_verified},
      %{id: "picture|noun|1", delta: 0.025,
        evidence: :structural_role_not_lexically_verified}
    ] = SemanticPropagation.role_plan(si)

    plan = Activation.feedback_plan(si)
    assert Enum.count(plan, &(&1.evidence == :structural_role_not_lexically_verified)) == 3
    assert Activation.feedback_plan(si, semantic_propagation?: false) != plan
  end

  test "dictionary morphology is required; an unsupported gloss cannot bridge" do
    si = assertion()
    no_tags = %{si | active_cells:
      Enum.map(si.active_cells, fn
        %{id: "draws|verb|1"} = cell -> %{cell | semantic_atoms: []}
        cell -> cell
      end)}

    assert SemanticPropagation.lemma_pulse(no_tags) == nil
  end

  test "a selected ordinary verb with no inflection never manufactures a bridge" do
    si = assertion()
    altered =
      si
      |> Map.put(:active_cells, [
        %{id: "draw|verb|1", norm: "draw", pos: "verb", definition: "To depict."}
      ])
      |> Map.put(:lifg_choices, [
        %{token_index: 1, id: "artist|noun|1"},
        %{token_index: 2, id: "draw|verb|1"},
        %{token_index: 4, id: "picture|noun|1"}
      ])
      |> Map.put(:atl_slate, %{winners: [
        %{token_index: 1, id: "artist|noun|1"},
        %{token_index: 2, id: "draw|verb|1"},
        %{token_index: 4, id: "picture|noun|1"}
      ]})

    assert SemanticPropagation.lemma_pulse(altered) == nil
  end

  test "unverified, contradicted, or injected assertions do not propagate" do
    si = assertion()
    invalid = %{si | episodic_candidates: []}
    assert SemanticPropagation.role_plan(invalid) == []
    assert SemanticPropagation.lemma_pulse(invalid) == nil

    forged = %{si | sentence_meaning: %{status: :not_yet_interpreted}}
    assert SemanticPropagation.role_plan(forged) == []
    assert SemanticPropagation.lemma_pulse(forged) == nil

    injected = %{si | episodic_candidates: [
      %{source_frame: :finite_present_svo, relation: "verb_draw",
        entity: "the artist", value: "a castle"}
    ]}
    assert SemanticPropagation.role_plan(injected) == []
    assert SemanticPropagation.lemma_pulse(injected) == nil
  end

  test "a lemma prior is shared across existing base-verb senses, not invented IDs" do
    prior = %{"draw" => 0.03}
    cells = for n <- 1..3, do: %{
      id: "draw|verb|#{n}", token_index: 0, score: 0.5, source: :ltm
    }

    noun = %{id: "draw|noun|1", token_index: 0, score: 0.5}
    other = %{id: "paint|verb|1", token_index: 0, score: 0.5}
    si = %{active_cells: cells ++ [noun, other], semantic_pulses: prior,
           neuron_activation: %{}, trace: []}

    primed = Activation.prime(si, notify_brain?: false)
    assert Enum.all?(Enum.take(primed.active_cells, 3), fn c ->
      c.runtime_primed? and c.prior_semantic_activation == 0.03 and
        c.activation > c.score
    end)
    refute Map.has_key?(Enum.at(primed.active_cells, 3), :activation)
    refute Map.has_key?(Enum.at(primed.active_cells, 4), :activation)
    assert Enum.map(primed.active_cells, & &1.id) == Enum.map(si.active_cells, & &1.id)
  end

  test "experimental disable switch removes the lemma prior without changing lexicon" do
    si = %{active_cells: [%{id: "draw|verb|7", token_index: 0, score: 0.5}],
      semantic_pulses: %{"draw" => 0.03}, neuron_activation: %{}, trace: []}

    off = Activation.prime(si, neuron_activation?: false)
    assert off.semantic_pulses == %{}
    assert off.active_cells == si.active_cells
  end
end
