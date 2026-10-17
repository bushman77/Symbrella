defmodule Db.BrainCellGrammarChangesetTest do
  use ExUnit.Case, async: true

  alias Db.{BrainCell, JSONL.Populate, JSONL.Upsert}

  defp attrs(functions) do
    %{
      id: "money|noun|1",
      word: "money",
      norm: "money",
      pos: "noun",
      type: "wiktextract",
      definition: "A medium of exchange.",
      gram_function: functions
    }
  end

  test "preserves compound noun grammar usually plural" do
    cs = BrainCell.changeset(%BrainCell{}, attrs(["uncountable", "usually plural"]))
    assert cs.valid?
    assert Ecto.Changeset.get_field(cs, :gram_function) ==
             ["uncountable", "usually plural"]
  end

  test "synthesizes compound noun grammar but rejects standalone usually/plural" do
    cs = BrainCell.changeset(%BrainCell{}, attrs(["countable", "usually", "plural"]))
    assert cs.valid?
    assert Ecto.Changeset.get_field(cs, :gram_function) ==
             ["countable", "usually plural"]
  end

  test "round trip from Wiktextract sense to validated insert attributes retains label" do
    [cell] =
      Populate.populate_structs_from_senses([
        %{
          "_meta" => %{"word" => "money", "pos" => "noun"},
          "glosses" => ["A medium of exchange."],
          "tags" => ["uncountable", "usually", "plural"]
        }
      ])

    assert "usually plural" in cell.gram_function

    attrs = Upsert.cell_to_attrs(cell, ~N[2026-10-15 00:00:00])
    cs = BrainCell.changeset(%BrainCell{}, attrs)

    assert cs.valid?
    assert "usually plural" in Ecto.Changeset.get_field(cs, :gram_function)
  end
end
