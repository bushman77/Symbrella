defmodule Db.JSONL.DefinitionShortlistReviewTest do
  use ExUnit.Case, async: true

  alias Db.JSONL.{DefinitionBatchAudit, DefinitionOnlyBackfill, DefinitionShortlistReview}

  test "collects noncontiguous, case-varied source entries in one full scan" do
    entries = [
      %{"word" => "CAT", "pos" => "noun",
        "senses" => [%{"glosses" => ["Animals.", "A felid."]}]},
      %{"word" => "point", "pos" => "noun",
        "senses" => [%{"glosses" => ["A mark.", "A small dot."]}]},
      %{"word" => "different", "pos" => "noun",
        "senses" => [%{"glosses" => ["Something else."]}]},
      %{"word" => "Cat", "pos" => "noun",
        "senses" => [%{"glosses" => ["Animals.", "A domestic cat."]}]},
      %{"word" => "POINT", "pos" => "noun",
        "senses" => [%{"glosses" => ["A mark.", "A punctuation mark."]}]}
    ]

    lines = Enum.map(entries, &(Jason.encode!(&1) <> "\n"))

    assert {:ok, grouped, stats} =
             DefinitionShortlistReview.collect(lines, ["cat", "point"])

    assert stats.lines_scanned == 5
    assert stats.matching_source_entries == 4
    assert stats.matching_source_senses == 4

    assert Enum.map(grouped["cat"], & &1["word"]) == ["CAT", "Cat"]
    assert Enum.map(grouped["point"], & &1["word"]) == ["point", "POINT"]

    {source, _} = DefinitionBatchAudit.source_cells(grouped["cat"], %{})
    assert Enum.map(source, & &1.id) == ["cat|noun|1", "cat|noun|2"]

    stored =
      Enum.map(source, fn cell ->
        %{cell | definition: "Animals."}
      end)

    assert {:ok, plan} =
             DefinitionOnlyBackfill.plan("cat", "noun", source, stored)

    assert plan.row_count == 2
    assert plan.patch_count == 2
  end

  test "never silently completes a shortlist when source JSON is malformed" do
    line = Jason.encode!(%{"word" => "cat", "pos" => "noun", "senses" => []})
    assert {:error, {:malformed_source_json, 2, _}} =
             DefinitionShortlistReview.collect([line, "{oops", line], ["cat"])
  end

  test "rejects invalid or excessive shortlist requests" do
    assert {:error, :no_targets} = DefinitionShortlistReview.collect([], [])
    assert {:error, :invalid_target} = DefinitionShortlistReview.collect([], [""])
    assert {:error, :duplicate_normalized_words} =
             DefinitionShortlistReview.collect([], ["Cat", "cat"])

    too_many = Enum.map(1..13, &"word#{&1}")
    assert {:error, {:too_many_words, 12}} =
             DefinitionShortlistReview.collect([], too_many)

    assert {:error, :invalid_targets} =
             DefinitionShortlistReview.collect([], "cat")
  end
end
