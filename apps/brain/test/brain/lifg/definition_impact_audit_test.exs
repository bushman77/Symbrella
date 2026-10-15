defmodule Brain.LIFG.DefinitionImpactAuditTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.DefinitionImpactAudit

  defp originals do
    Enum.map([129, 138], fn number ->
      %Db.BrainCell{
        id: "draw|verb|#{number}",
        norm: "draw",
        word: "draw",
        pos: "verb",
        definition: "Senses relating to depicting or representing.",
        example: nil,
        synonyms: [],
        antonyms: [],
        semantic_atoms: [],
        gram_function: ["transitive"],
        status: "inactive",
        activation: 0.85,
        connections: []
      }
    end)
  end

  test "compares definition-only projection without mutating lexical or cognitive state" do
    old = originals()

    source =
      Enum.map(old, fn cell ->
        case cell.id do
          "draw|verb|129" ->
            %{cell | definition:
              "Senses relating to depicting or representing. — To produce a picture with a pencil."}

          _ ->
            %{cell | definition:
              "Senses relating to depicting or representing. — To carve wood."}
        end
      end)

    assert {:ok, result} =
             DefinitionImpactAudit.compare(
               "They draw a picture.", "draw", "draw|verb|129", old, source
             )

    assert result.before.expected_fit == 0.0
    assert result.projected.expected_fit > 0.0
    assert result.expected_fit_delta > 0.0
    assert result.projected.top_id == "draw|verb|129"
    assert result.projected.hypothesized_id_rank == 1
    assert result.projected.expected_definition_matches == ["picture"]

    assert Enum.all?(old, fn row ->
      row.definition == "Senses relating to depicting or representing." and
        row.status == "inactive" and row.activation == 0.85 and
        row.connections == []
    end)
  end

  test "rejects missing IDs, mismatched or duplicated source census" do
    old = originals()

    assert {:error, :no_candidates} =
             DefinitionImpactAudit.compare("They draw a picture.", "draw", "draw|verb|129", [], [])

    assert {:error, :id_census_mismatch} =
             DefinitionImpactAudit.compare("They draw a picture.", "draw", "draw|verb|129", old, [])

    assert {:error, :expected_id_not_found} =
             DefinitionImpactAudit.compare("They draw a picture.", "draw", "draw|verb|999", old, old)

    assert {:error, :duplicate_ids} =
             DefinitionImpactAudit.compare("They draw a picture.", "draw", "draw|verb|129", old ++ old, old)

    assert {:error, :invalid_arguments} =
             DefinitionImpactAudit.compare(nil, "draw", "draw|verb|129", old, old)
  end
end
