defmodule Db.JSONL.DefinitionBatchAuditTest do
  use ExUnit.Case, async: true

  alias Db.JSONL.DefinitionBatchAudit

  test "recreates importer positional IDs from complete source word groups" do
    entries = [
      %{
        "word" => "free", "pos" => "adj",
        "etymology_number" => 1,
        "senses" => [
          %{"glosses" => ["Unconstrained.", "Not imprisoned."]},
          %{"glosses" => ["Unconstrained.", "Without obligations."]}
        ]
      },
      %{
        "word" => "Free", "pos" => "adj",
        "senses" => [%{"glosses" => ["Not bound.", "Available."]}]
      }
    ]

    {cells, counters} = DefinitionBatchAudit.source_cells(entries, %{})

    assert Enum.map(cells, & &1.id) ==
             ["free|adjective|1", "free|adjective|2", "free|adjective|3"]

    assert Enum.at(cells, 0).definition ==
             "Unconstrained. — Not imprisoned."

    assert "ety:1" in Enum.at(cells, 0).semantic_atoms
    refute "ety:1" in Enum.at(cells, 2).semantic_atoms

    {later, _counters} =
      DefinitionBatchAudit.source_cells([
        %{"word" => "free", "pos" => "adj", "senses" => [%{"glosses" => ["Later."]}]}
      ], counters)

    assert Enum.map(later, & &1.id) == ["free|adjective|4"]
  end

  test "identifies genuinely reviewable definitions without treating them as approved" do
    {source, _} =
      DefinitionBatchAudit.source_cells([
        %{
          "word" => "free", "pos" => "adj",
          "senses" => [
            %{"glosses" => ["Unconstrained.", "Not imprisoned."]},
            %{"glosses" => ["Unconstrained.", "Without obligations."]}
          ]
        }
      ], %{})

    stored =
      Enum.map(source, fn cell ->
        %{cell |
          definition: "Unconstrained.",
          semantic_atoms: cell.semantic_atoms,
          status: "active",
          activation: 0.91}
      end)

    summary =
      DefinitionBatchAudit.new(3)
      |> DefinitionBatchAudit.inspect_group("free", "adjective", source, stored)

    assert summary.reviewed_groups == 1
    assert summary.candidate_groups == 1
    assert summary.definitions_to_extend == 2
    assert summary.blocked_groups == 0
    assert [%{word: "free", definitions_to_extend: 2}] = summary.candidate_examples

    # No DB calls or writes: source and stored objects remain unchanged.
    assert Enum.all?(stored, &(&1.activation == 0.91 and &1.status == "active"))

    current =
      DefinitionBatchAudit.new()
      |> DefinitionBatchAudit.inspect_group("free", "adjective", source, source)

    assert current.already_current == 1
    assert current.definitions_to_extend == 0
  end

  test "blocks incomplete word/POS groups and mismatched examples" do
    {source, _} =
      DefinitionBatchAudit.source_cells([
        %{"word" => "cat", "pos" => "noun",
          "senses" => [%{"glosses" => ["Animals.", "Domestic feline."]}]}
      ], %{})

    summary =
      DefinitionBatchAudit.new(3)
      |> DefinitionBatchAudit.inspect_group("cat", "noun", source, [])

    assert summary.blocked_groups == 1
    assert summary.blocked_by_reason[:id_census_mismatch] == 1

    [cell] = source
    stored = [%{cell | definition: "Animals.", example: "Not the source example."}]

    summary =
      DefinitionBatchAudit.new(3)
      |> DefinitionBatchAudit.inspect_group("cat", "noun", source, stored)

    assert summary.blocked_by_reason[:example_conflict] == 1
  end

  test "skips POS without reconstructed multi-gloss evidence" do
    {source, _} =
      DefinitionBatchAudit.source_cells([
        %{"word" => "word", "pos" => "noun",
          "senses" => [%{"glosses" => ["One short definition."]}]}
      ], %{})

    summary =
      DefinitionBatchAudit.new()
      |> DefinitionBatchAudit.inspect_group("word", "noun", source, [])

    assert summary.no_extended_gloss == 1
    assert summary.reviewed_groups == 0
  end
end
