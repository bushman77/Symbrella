defmodule Core.Brain.SynapticSlateAuditTest do
  use ExUnit.Case, async: true

  alias Core.Brain.SynapticSlateAudit

  @verb_tags ["tag:form-of", "tag:indicative", "tag:present",
              "tag:singular", "tag:third-person"]

  defp input(scores, bucket_ids) do
    %{
      lifg_choices: [
        %{token_index: 1, id: "draws|verb|1", candidate_probs: scores}
      ],
      sense_candidates: %{
        1 => Enum.map(bucket_ids, fn id -> %{id: id} end)
      },
      active_cells: [
        %{id: "draws|verb|1",
          semantic_atoms: @verb_tags,
          definition: "An inflected form of draw."}
      ]
    }
  end

  test "two to four candidates may contain only one true verb sense" do
    ids = ["draws|verb|1", "draws|noun|1", "draws|noun|2"]
    si = input(%{
      "draws|verb|1" => 0.6,
      "draws|noun|1" => 0.3,
      "draws|noun|2" => 0.1
    }, ids)

    assert %{status: :observed, verbs: [result]} = SynapticSlateAudit.inspect(si)
    assert result.scored_count == 3
    assert result.scored_pos_counts == %{"verb" => 1, "noun" => 2}
    assert result.same_surface_verb_sense_count == 1
    assert result.same_surface_verb_ids == ["draws|verb|1"]
    assert result.score_slate_complete?
    assert result.attested_base_form == "draw"
    assert result.base_form_relation_attested?
    refute result.base_sense_identity_verified?
    refute result.synaptic_mapping_to_base_senses?
  end

  test "same-surface real verb competitors are counted without creating base-sense mappings" do
    ids = ["draws|verb|1", "draws|verb|2", "draws|noun|1"]
    si = input(%{
      "draws|verb|1" => 0.3,
      "draws|verb|2" => 0.5,
      "draws|noun|1" => 0.2
    }, ids)

    assert %{verbs: [result]} = SynapticSlateAudit.inspect(si)
    assert result.same_surface_verb_sense_count == 2
    assert result.scored_pos_counts == %{"verb" => 2, "noun" => 1}
    assert result.attested_base_form == "draw"
    refute result.synaptic_mapping_to_base_senses?
  end

  test "absent morphology proof does not assert an inflection bridge" do
    ids = ["draws|verb|1", "draws|noun|1"]
    si =
      input(%{"draws|verb|1" => 0.8, "draws|noun|1" => 0.2}, ids)
      |> Map.put(:active_cells, [
        %{id: "draws|verb|1", semantic_atoms: [], definition: "To draw."}
      ])

    assert %{verbs: [result]} = SynapticSlateAudit.inspect(si)
    assert result.attested_base_form == nil
    refute result.base_form_relation_attested?
  end

  test "incomplete scored slates are explicitly reported" do
    si = input(%{"draws|verb|1" => 1.0}, ["draws|verb|1", "draws|noun|1"])
    assert %{verbs: [result]} = SynapticSlateAudit.inspect(si)
    refute result.score_slate_complete?
  end
end
