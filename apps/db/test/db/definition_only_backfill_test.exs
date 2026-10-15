defmodule Db.JSONL.DefinitionOnlyBackfillTest do
  use ExUnit.Case, async: true

  alias Db.JSONL.DefinitionOnlyBackfill

  defp fixtures do
    source = [
      %Db.BrainCell{
        id: "free|adjective|1", norm: "free", word: "free", pos: "adjective",
        definition: "(social) Unconstrained. — Not imprisoned or enslaved.",
        example: "A free person.", gram_function: [],
        synonyms: [], antonyms: [],
        semantic_atoms: ["pos_raw:adj", "qual:social", "usage:region:british english"]
      },
      %Db.BrainCell{
        id: "free|adjective|2", norm: "free", word: "free", pos: "adjective",
        definition: "(social) Unconstrained. — Without obligations.",
        example: "Free time.", gram_function: [],
        synonyms: [], antonyms: [],
        semantic_atoms: ["pos_raw:adj", "qual:social", "cat:en:software"]
      },
      %Db.BrainCell{
        id: "free|adjective|3", norm: "free", word: "free", pos: "adjective",
        definition: "Obtainable without payment.",
        example: nil, gram_function: [],
        synonyms: [], antonyms: [],
        semantic_atoms: ["pos_raw:adj"]
      }
    ]

    stored =
      Enum.map(source, fn cell ->
        %Db.BrainCell{
          cell |
          definition: String.split(cell.definition, " — ", parts: 2) |> hd(),
          semantic_atoms:
            Enum.reject(cell.semantic_atoms, fn atom ->
              String.starts_with?(atom, "cat:") or String.starts_with?(atom, "usage:")
            end),
          status: "active",
          activation: 0.93
        }
      end)

    {source, stored}
  end

  test "builds definition-only plan with full positional ID census" do
    {source, stored} = fixtures()

    assert {:ok, plan} =
             DefinitionOnlyBackfill.plan("free", "adjective", source, stored)

    assert plan.row_count == 3
    assert plan.patch_count == 2
    assert length(plan.fingerprint) == 64
    assert Enum.map(plan.patches, & &1.id) ==
             ["free|adjective|1", "free|adjective|2"]

    assert Enum.at(plan.patches, 0).new_definition ==
             "(social) Unconstrained. — Not imprisoned or enslaved."

    assert Enum.all?(stored, &(&1.status == "active" and &1.activation == 0.93))
    assert Enum.all?(plan.patches, &(&1.semantic_atoms == ["pos_raw:adj", "qual:social"]))

    assert {:ok, reordered} =
             DefinitionOnlyBackfill.plan("free", "adjective",
               Enum.reverse(source), Enum.reverse(stored))

    assert reordered.fingerprint == plan.fingerprint
  end

  test "wrong fingerprint rejects without touching database" do
    {source, stored} = fixtures()
    {:ok, plan} = DefinitionOnlyBackfill.plan("free", "adjective", source, stored)
    assert {:error, :fingerprint_mismatch} =
             DefinitionOnlyBackfill.apply(plan, String.duplicate("0", 64))
  end

  test "rejects missing, duplicate and inconsistent positional source IDs" do
    {source, stored} = fixtures()

    assert {:error, {:id_census_mismatch, _, _}} =
             DefinitionOnlyBackfill.plan("free", "adjective", source, tl(stored))

    assert {:error, :duplicate_ids} =
             DefinitionOnlyBackfill.plan("free", "adjective", source ++ [hd(source)], stored)

    altered = List.update_at(source, 0, &%{&1 | norm: "different"})
    assert {:error, :word_pos_mismatch} =
             DefinitionOnlyBackfill.plan("free", "adjective", altered, stored)
  end

  test "rejects incompatible glosses, examples, relations, grammar and tags" do
    {source, stored} = fixtures()
    [first | rest] = stored

    for {bad, reason} <- [
      {%{first | definition: "Different gloss."}, :definition_conflict},
      {%{first | example: "Changed example."}, :example_conflict},
      {%{first | synonyms: ["liberty"]}, :relation_conflict},
      {%{first | gram_function: ["attributive-only"]}, :grammar_conflict},
      {%{first | semantic_atoms: ["pos_raw:unrelated"]}, :lexical_metadata_conflict}
    ] do
      assert {:error, {"free|adjective|1", ^reason}} =
               DefinitionOnlyBackfill.plan("free", "adjective", source, [bad | rest])
    end
  end

  test "never rewrites a definition with no original gloss to anchor it" do
    {source, stored} = fixtures()
    [first | rest] = stored

    assert {:error, {"free|adjective|1", :definition_conflict}} =
             DefinitionOnlyBackfill.plan("free", "adjective",
               source, [%{first | definition: nil} | rest])
  end
end
