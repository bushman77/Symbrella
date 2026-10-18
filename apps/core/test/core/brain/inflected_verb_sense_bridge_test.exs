defmodule Core.Brain.InflectedVerbSenseBridgeTest do
  use ExUnit.Case, async: false

  alias Core.Brain.InflectedVerbSenseBridge
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @svo_evidence [:contiguous_surface, :attested_nominal_subject,
                 :attested_nominal_object, :attested_finite_present_verb]
  @verb_tags ["tag:form-of", "tag:indicative", "tag:present",
              "tag:singular", "tag:third-person"]

  defp statement(patient \\ "picture") do
    choices = [
      %{token_index: 1, id: "artist|noun|1", lemma: "artist"},
      %{token_index: 2, id: "draws|verb|1", lemma: "draws"},
      %{token_index: 4, id: "#{patient}|noun|1", lemma: patient}
    ]

    %{
      sentence: "The artist draws a #{patient}.",
      source: :user,
      session_id: "bridge-fixture",
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: "artist", n: 1},
        %{index: 2, phrase: "draws", n: 1},
        %{index: 3, phrase: "a", n: 1},
        %{index: 4, phrase: patient <> ".", n: 1}
      ],
      sentence_signatures: [
        %{id: :finite_present_svo_candidate, source: :core_syntax,
          span: {0, 4}, evidence: @svo_evidence,
          roles: %{subject_determiner: 0, subject_head: 1,
                   predicate: 2, object_determiner: 3, object_head: 4}}
      ],
      active_cells: [
        %{id: "artist|noun|1", norm: "artist", pos: "noun"},
        %{id: "draws|verb|1", norm: "draws", pos: "verb",
          semantic_atoms: @verb_tags, definition: "An inflected form of draw."},
        %{id: "#{patient}|noun|1", norm: patient, pos: "noun"}
      ],
      lifg_choices: choices,
      semantic_choices: choices,
      atl_slate: %{winners: choices},
      trace: []
    }
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  defp senses do
    [
      %{id: "draw|verb|1", norm: "draw", pos: "verb",
        definition: "To pull a heavy object along the ground.",
        gram_function: ["transitive"], synonyms: []},
      %{id: "draw|verb|2", norm: "draw", pos: "verb",
        definition: "To create a picture by tracing lines.",
        gram_function: ["transitive"], synonyms: []},
      %{id: "draw|verb|3", norm: "draw", pos: "verb",
        definition: "To take water from a well.",
        gram_function: ["transitive"], synonyms: []},
      %{id: "draw|noun|1", norm: "draw", pos: "noun",
        definition: "A tied result."}
    ]
  end

  test "attested form surfaces genuine base dictionary sense slate without choosing meaning" do
    si = statement()
    result = InflectedVerbSenseBridge.project(si, fetch_base_senses: fn base ->
      assert base == "draw"
      senses()
    end)

    assert result.status == :read_only_sense_inventory
    assert result.original_surface_id == "draws|verb|1"
    assert result.attested_base_lexeme == "draw"
    assert result.base_sense_count == 3
    assert result.all_sense_ids == ["draw|verb|1", "draw|verb|2", "draw|verb|3"]
    assert result.candidates_with_context_evidence > 0
  end

  test "lexical scoring proposes a bounded hypothesis but never verifies a specific sense" do
    si = statement()

    result = InflectedVerbSenseBridge.project(si,
      fetch_base_senses: fn _ -> senses() end)

    assert result.status == :read_only_sense_inventory
    assert result.highest_context_fit > 0
    assert [%{id: "draw|verb|2", context_fit: best} | _] = result.ranked
    assert best > 0
    assert result.lexical_hypothesis ==
             %{candidate_id: "draw|verb|2", source: :dictionary_context_overlap}
    assert result.surface_to_base_form_attested?
    refute result.surface_to_specific_base_sense_verified?
    refute result.gold_sense_known?
    refute result.selected_new_lifg_sense?
    refute result.synaptic_weight_transferred?
    refute result.applied_to_lifg?
    refute result.brain_cells_stimulated?
    assert si.lifg_choices == statement().lifg_choices
  end

  test "missing morphology witness blocks lookup" do
    si = statement()
    changed =
      si
      |> Map.update!(:active_cells, fn cells ->
        Enum.map(cells, fn
          %{id: "draws|verb|1"} = c -> %{c | semantic_atoms: []}
          c -> c
        end)
      end)

    result = InflectedVerbSenseBridge.project(changed,
      fetch_base_senses: fn _ -> raise "should not call DB" end)

    assert %{status: :skipped, reason: :ineligible_or_unattested_form} = result
  end

  test "no dictionary data, duplicate senses and too many senses fail closed" do
    si = statement()

    assert %{status: :skipped, reason: :base_dictionary_unavailable} =
             InflectedVerbSenseBridge.project(si,
               fetch_base_senses: fn _ -> :error end)

    assert %{status: :skipped, reason: :insufficient_base_verb_senses} =
             InflectedVerbSenseBridge.project(si,
               fetch_base_senses: fn _ -> Enum.take(senses(), 1) end)

    assert %{status: :skipped, reason: :duplicate_dictionary_senses} =
             InflectedVerbSenseBridge.project(si,
               fetch_base_senses: fn _ -> Enum.take(senses(), 2) ++ Enum.take(senses(), 1) end)

    too_many = Enum.map(1..257, fn index ->
      %{id: "draw|verb|#{index}", norm: "draw", pos: "verb",
        definition: "To draw an object."}
    end)

    assert %{status: :skipped, reason: :oversized_base_verb_slate} =
             InflectedVerbSenseBridge.project(si,
               fetch_base_senses: fn _ -> too_many end)
  end

  test "assistant text and unverified role construction cannot fetch base senses" do
    si = statement()

    for changed <- [
          %{si | source: :assistant},
          %{si | sentence: "The artist might draw a picture."},
          %{si | atl_slate: %{winners: []}}
        ] do
      assert %{status: :skipped} =
               InflectedVerbSenseBridge.project(changed,
                 fetch_base_senses: fn _ -> raise "should not query dictionary" end)
    end
  end

  test "read-only projector does not change original lexical or semantically verified state" do
    si = statement("water")
    original = si
    result = InflectedVerbSenseBridge.project(si,
      fetch_base_senses: fn _ -> senses() end)

    assert result.base_sense_count == 3
    assert result.applied_to_lifg? == false
    assert si == original
  end
end
