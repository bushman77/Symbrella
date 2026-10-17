defmodule Db.JSONL.DefinitionWordReviewTest do
  use ExUnit.Case, async: true

  alias Db.JSONL.{DefinitionBatchAudit, DefinitionWordReview}

  test "collects all normalized spellings in original order, including noncontiguous runs" do
    lines =
      [
        %{"word" => "Point", "pos" => "noun",
          "senses" => [%{"glosses" => ["A small mark.", "A dot."]}]},
        %{"word" => "other", "pos" => "noun",
          "senses" => [%{"glosses" => ["Irrelevant."]}]},
        %{"word" => "point", "pos" => "noun",
          "senses" => [%{"glosses" => ["A distinct element.", "A detail."]}]},
        %{"word" => "POINT", "pos" => "verb",
          "senses" => [%{"glosses" => ["To aim.", "Direct."]}]}
      ]
      |> Enum.map(&(Jason.encode!(&1) <> "\n"))

    assert {:ok, entries, stats} = DefinitionWordReview.collect(lines, "POINT")

    assert Enum.map(entries, & &1["word"]) == ["Point", "point", "POINT"]
    assert stats.total_lines == 4
    assert stats.source_entries == 3
    assert stats.source_senses == 3
    assert stats.normalized_spellings ==
             %{"Point" => 1, "point" => 1, "POINT" => 1}

    {cells, _} = DefinitionBatchAudit.source_cells(entries, %{})

    assert Enum.map(cells, & &1.id) ==
             ["point|noun|1", "point|noun|2", "point|verb|1"]
  end

  test "refuses partial source reconstruction after malformed JSON" do
    valid = Jason.encode!(%{"word" => "point", "pos" => "noun",
      "senses" => [%{"glosses" => ["First."]}]})

    assert {:error, {:malformed_source_json, 2, _}} =
             DefinitionWordReview.collect([valid, "{bad json", valid], "point")
  end

  test "blank target words are rejected rather than scanning indiscriminately" do
    assert {:error, :blank_word} = DefinitionWordReview.collect([], "   ")
    assert {:error, :invalid_word} = DefinitionWordReview.collect([], nil)
  end
end
