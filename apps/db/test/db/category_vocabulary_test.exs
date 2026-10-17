defmodule Db.JSONL.CategoryVocabularyTest do
  use ExUnit.Case, async: true

  alias Db.JSONL.CategoryVocabulary

  test "separates raw category frequencies from distinct senses with evidence" do
    source = %{
      "word" => "example",
      "senses" => [
        %{"categories" => [
          %{"name" => "Places in Georgia, USA", "kind" => "place", "source" => "w"},
          %{"name" => "Computing", "kind" => "other", "source" => "w"},
          %{"name" => "Pages with entries", "kind" => "other", "source" => "w+disamb"}
        ]},
        %{"categories" => [
          %{"name" => "Pages with entries", "kind" => "other", "source" => "w"},
          %{"name" => "Pages with entries", "kind" => "other", "source" => "w+disamb"}
        ]},
        %{"glosses" => ["No categories here."]}
      ]
    }

    summary = CategoryVocabulary.new() |> CategoryVocabulary.ingest(source)

    assert summary.entries == 1
    assert summary.source_senses == 3
    assert summary.raw_category_values == 5
    assert summary.senses_with_no_categories == 1
    assert summary.senses_with_only_disamb_or_maintenance == 1

    assert summary.category_values_by_class == %{
      place: 1,
      other_review: 1,
      disambiguation_source: 2,
      likely_maintenance: 1
    }

    assert summary.senses_by_class == %{
      place: 1,
      other_review: 1,
      disambiguation_source: 2,
      likely_maintenance: 1
    }

    assert CategoryVocabulary.top_names(summary, :other_review, 10) ==
             [{"Computing", 1}]

    assert CategoryVocabulary.top_names(summary, :disambiguation_source, 10) ==
             [{"Pages with entries", 2}]
  end

  test "counts repeated category values twice but only one exposed sense" do
    summary =
      CategoryVocabulary.new()
      |> CategoryVocabulary.ingest(%{
        "senses" => [
          %{"categories" => [
            %{"name" => "Software", "source" => "w", "kind" => "other"},
            %{"name" => "Software", "source" => "w", "kind" => "other"}
          ]}
        ]
      })

    assert summary.source_senses == 1
    assert summary.raw_category_values == 2
    assert summary.senses_by_class == %{other_review: 1}
    assert CategoryVocabulary.top_names(summary, :other_review, 1) ==
             [{"Software", 2}]
  end

  test "ignores malformed non-list senses without database interaction" do
    summary =
      CategoryVocabulary.new()
      |> CategoryVocabulary.ingest(%{"senses" => nil})
      |> CategoryVocabulary.ingest(%{"senses" => [nil, "invalid", %{}]})

    assert summary.entries == 1
    assert summary.source_senses == 1
    assert summary.senses_with_no_categories == 1
    assert summary.raw_category_values == 0
  end
end
