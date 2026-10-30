defmodule Core.Semantic.CompositionLemmaGroundedPredicateTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition.LemmaGroundedPredicate, as: Predicate

  defp si(word, inflection, lemma, selected_id) do
    form_id = "#{word}|verb|1"

    %Core.SemanticInput{
      sentence: "The bird is #{word} on the fence.",
      tokens: [%{phrase: word, n: 1, index: 3}],
      lifg_choices: [
        %{token_index: 3, id: selected_id,
          candidate_probs: %{form_id => 0.11, selected_id => 0.21}}
      ],
      active_cells: [
        %{
          id: form_id, norm: word, pos: "verb",
          token_index: 3, definition: "#{inflection} of #{lemma}",
          semantic_atoms: ["tag:form-of", "pos_raw:verb", "tag:participle"]
        }
      ]
    }
  end

  defp base_senses do
    [
      %{
        id: "sit|verb|1", norm: "sit", pos: "verb",
        definition: "To be seated; to maintain an upright body position."
      },
      %{
        id: "sit|verb|2", norm: "sit", pos: "verb",
        definition: "To serve as a member of a legislative assembly."
      },
      %{
        id: "sit|noun|1", norm: "sit", pos: "noun",
        definition: "A period of time spent sitting."
      },
      %{
        id: "stand|verb|1", norm: "stand", pos: "verb",
        definition: "To be in an upright position."
      }
    ]
  end

  test "sitting resolves its lemma through scored form-of evidence, not selected adjective" do
    input = si("sitting", "present participle and gerund", "sit", "sitting|adjective|3")
    before_choice = hd(input.lifg_choices)

    discovery = Predicate.discover(input, 3)
    assert discovery.status == :forms_attested
    assert discovery.selected_lifg_id == "sitting|adjective|3"
    assert discovery.requested_lemmas == ["sit"]
    assert [%{inflected_sense_id: "sitting|verb|1",
              lemma: "sit",
              morphological_only?: true}] = discovery.forms

    grounded = Predicate.ground(discovery, base_senses())
    assert grounded.status == :predicate_feature_candidates
    assert length(grounded.lemma_senses) == 2
    assert [%{feature: :seated_posture, family: :physical_posture,
              source_lemma_sense_id: "sit|verb|1",
              supporting_text: "be seated"}] = grounded.feature_hypotheses

    assert hd(input.lifg_choices) == before_choice
    assert grounded.selected_lifg_id == "sitting|adjective|3"
    refute grounded.semantic_compatibility_proven?
    assert grounded.selected_lifg_unchanged?
  end

  test "sat follows a different attested morphological inflection to the same lemma" do
    input = si("sat", "simple past and past participle", "sit", "sat|noun|1")
    grounded = input |> Predicate.discover(3) |> Predicate.ground(base_senses())

    assert grounded.status == :predicate_feature_candidates
    assert Enum.map(grounded.forms, & &1.lemma) == ["sit"]
    assert Enum.map(grounded.feature_hypotheses, & &1.source_lemma_sense_id) ==
             ["sit|verb|1"]
    assert grounded.selected_lifg_id == "sat|noun|1"
    refute grounded.semantic_compatibility_proven?
  end

  test "the same discovery rules also support unrelated inflections, without sit-specific code" do
    input =
      si("walking", "present participle and gerund", "walk", "walking|adjective|1")

    discovered = Predicate.discover(input, 3)
    assert discovered.requested_lemmas == ["walk"]

    grounded = Predicate.ground(discovered, [
      %{id: "walk|verb|7", norm: "walk", pos: "verb",
        definition: "To move from one place to another on foot."}
    ])

    assert [%{feature: :physical_motion, source_lemma_sense_id: "walk|verb|7"}] =
             grounded.feature_hypotheses
  end

  test "a morphological tag is required, even if a string contains 'of sit'" do
    input = si("sat", "simple past and past participle", "sit", "sat|noun|1")
    altered =
      Map.update!(input, :active_cells, fn cells ->
        Enum.map(cells, &Map.put(&1, :semantic_atoms, ["pos_raw:verb"]))
      end)

    assert Predicate.discover(altered, 3).status == :no_attested_form_of_verb
  end

  test "unscored forms and mismatched token indices cannot authorize lemma discovery" do
    input = si("sat", "simple past and past participle", "sit", "sat|noun|1")
    without_score =
      Map.put(input, :lifg_choices, [
        %{token_index: 3, id: "sat|noun|1",
          candidate_probs: %{"sat|noun|1" => 0.9}}
      ])

    assert Predicate.discover(without_score, 3).status == :no_attested_form_of_verb

    wrong_index =
      Map.update!(input, :active_cells, fn cells ->
        Enum.map(cells, &Map.put(&1, :token_index, 7))
      end)

    assert Predicate.discover(wrong_index, 3).status == :no_attested_form_of_verb
  end

  test "unrelated lemma and nonverb rows do not introduce features" do
    input = si("sat", "simple past and past participle", "sit", "sat|noun|1")
    discovery = Predicate.discover(input, 3)

    no_lemma =
      Predicate.ground(discovery, [
        %{id: "stand|verb|1", norm: "stand", pos: "verb",
          definition: "To be seated."},
        %{id: "sit|noun|1", norm: "sit", pos: "noun",
          definition: "To be seated."}
      ])

    assert no_lemma.status == :lemma_senses_unavailable
    assert no_lemma.feature_hypotheses == []
  end

  test "uncategorized lemma meanings are retained without fabricated properties" do
    discovery =
      si("sat", "simple past and past participle", "sit", "sat|noun|1")
      |> Predicate.discover(3)

    result = Predicate.ground(discovery, [
      %{id: "sit|verb|88", norm: "sit", pos: "verb",
        definition: "To take part in a formal deliberative body."}
    ])

    assert result.status == :lemma_senses_unclassified
    assert result.lemma_senses != []
    assert result.feature_hypotheses == []
    refute result.semantic_compatibility_proven?
  end
end
