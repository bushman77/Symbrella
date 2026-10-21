defmodule Brain.LIFG.ArgumentCompatibilityTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.ArgumentCompatibility

  @frame %{
    status: :candidate, source: :original_syntax_and_dictionary_pos,
    meaning_verified?: false, event_verified?: false,
    construction: :recipient_object, predicate: 2,
    predicate_surface: "write", recipient: 3,
    object_head: 5, object_status: :dictionary_noun_witness,
    evidence: [:supported_predicate_role, :bounded_argument_order,
               :determiner_witness, :nominal_head_surface,
               :pronoun_recipient_witness]
  }

  @cells [
    %{id: "write|verb|1", norm: "write", pos: "verb",
      semantic_atoms: ["tag:ambitransitive"]},
    %{id: "write|verb|4", norm: "write", pos: "verb",
      semantic_atoms: ["tag:ditransitive"]},
    %{id: "write|verb|5", norm: "write", pos: "verb",
      semantic_atoms: ["tag:ditransitive"]},
    %{id: "write|verb|8", norm: "write", pos: "verb",
      semantic_atoms: ["tag:intransitive"]},
    %{id: "write|noun|1", norm: "write", pos: "noun",
      semantic_atoms: []}
  ]

  test "recipient-object syntax positively supports ditransitive grammar but leaves exact senses tied" do
    assert [%{status: :shadow_only, candidate_count: 4,
              semantic_sense_verified?: false, winner_changed?: false,
              applied_to_lifg?: false, brain_cells_stimulated?: false} = report] =
             ArgumentCompatibility.compare([@frame], @cells)

    supports = Map.new(report.candidates, &{&1.id, &1.positive_support})
    assert supports["write|verb|4"] == 0.10
    assert supports["write|verb|5"] == 0.10
    assert supports["write|verb|1"] == 0.0
    assert supports["write|verb|8"] == 0.0
    refute Map.has_key?(supports, "write|noun|1")
  end

  test "direct object is compatible with transitive, ambitransitive and ditransitive" do
    frame = %{@frame | construction: :direct_object, recipient: nil,
      evidence: Enum.reject(@frame.evidence, &(&1 == :pronoun_recipient_witness))}
    assert [%{candidates: candidates}] =
             ArgumentCompatibility.compare([frame], @cells)
    supports = Map.new(candidates, &{&1.id, &1.positive_support})
    assert supports["write|verb|1"] == 0.06
    assert supports["write|verb|4"] == 0.04
    assert supports["write|verb|8"] == 0.0
  end

  test "missing provenance and evidence refuse all scoring" do
    for frame <- [
      Map.put(@frame, :status, :verified),
      Map.put(@frame, :meaning_verified?, true),
      Map.put(@frame, :source, :arbitrary),
      Map.put(@frame, :evidence, [:pronoun_recipient_witness])
    ] do
      assert ArgumentCompatibility.compare([frame], @cells) == []
    end

    assert ArgumentCompatibility.compare([], @cells) == []
    assert ArgumentCompatibility.compare(nil, @cells) == []
  end
end
