defmodule Db.JSONL.PopulateSenseEvidenceTest do
  use ExUnit.Case, async: true

  alias Db.JSONL.Populate

  test "keeps hierarchical sense glosses separate under stable word|POS|index IDs" do
    senses = [
      %{
        "_meta" => %{"word" => "Cash", "pos" => "name"},
        "glosses" => ["A surname originating as an occupation."]
      },
      %{
        "_meta" => %{"word" => "Cash", "pos" => "name"},
        "glosses" => ["A male given name transferred from the surname."]
      },
      %{
        "_meta" => %{"word" => "Cash", "pos" => "name"},
        "glosses" => [
          "A number of places in the United States:",
          "A minor city in Craighead County, Arkansas; a corruption of Cache River."
        ],
        "categories" => [
          %{"kind" => "place", "name" => "Cities in Arkansas, USA",
            "langcode" => "en", "source" => "w"},
          %{"kind" => "other", "name" => "Pages with 2 entries",
            "source" => "w+disamb"}
        ]
      },
      %{
        "_meta" => %{"word" => "Cash", "pos" => "name"},
        "glosses" => [
          "A number of places in the United States:",
          "An unincorporated community in Gordon County, Georgia."
        ],
        "categories" => [
          %{"kind" => "place", "name" => "Places in Georgia, USA",
            "langcode" => "en", "source" => "w"}
        ]
      }
    ]

    cells = Populate.populate_structs_from_senses(senses)

    assert Enum.map(cells, & &1.id) == [
      "cash|proper_noun|1", "cash|proper_noun|2",
      "cash|proper_noun|3", "cash|proper_noun|4"
    ]

    assert Enum.at(cells, 0).definition ==
             "A surname originating as an occupation."

    arkansas = Enum.at(cells, 2)
    georgia = Enum.at(cells, 3)

    assert arkansas.definition ==
             "A number of places in the United States: — " <>
               "A minor city in Craighead County, Arkansas; a corruption of Cache River."

    assert georgia.definition ==
             "A number of places in the United States: — " <>
               "An unincorporated community in Gordon County, Georgia."

    assert "cat:en:cities in arkansas, usa" in arkansas.semantic_atoms
    assert "cat:en:places in georgia, usa" in georgia.semantic_atoms
    refute Enum.any?(arkansas.semantic_atoms, &String.contains?(&1, "pages with 2 entries"))
    refute Enum.any?(georgia.semantic_atoms, &String.contains?(&1, "arkansas"))

    assert Enum.all?(cells, &(&1.synonyms == []))
    assert Enum.all?(cells, &(&1.status == "inactive"))
  end

  test "uses raw glosses only when cleaned glosses are absent" do
    senses = [
      %{"_meta" => %{"word" => "alpha", "pos" => "noun"},
        "glosses" => ["", "  "],
        "raw_glosses" => ["A useful raw heading.", "A defining raw detail."]},
      %{"_meta" => %{"word" => "alpha", "pos" => "noun"},
        "glosses" => ["A final usable gloss.", "A final usable gloss."],
        "raw_glosses" => ["Should not replace available clean glosses."]}
    ]

    [raw, cleaned] = Populate.populate_structs_from_senses(senses)
    assert raw.id == "alpha|noun|1"
    assert raw.definition == "A useful raw heading. — A defining raw detail."
    assert cleaned.id == "alpha|noun|2"
    assert cleaned.definition == "A final usable gloss."
  end

  test "keeps each sense's category and synonym data isolated" do
    senses = [
      %{"_meta" => %{"word" => "test", "pos" => "noun",
                     "entry_synonyms" => ["shared"]},
        "glosses" => ["The first sense"],
        "synonyms" => [%{"word" => "specific"}],
        "categories" => [
          %{"source" => "w", "name" => "Computing", "kind" => "other"}
        ]},
      %{"_meta" => %{"word" => "test", "pos" => "noun",
                     "entry_synonyms" => ["shared"]},
        "glosses" => ["The second sense"]}
    ]

    [first, second] = Populate.populate_structs_from_senses(senses)

    assert first.synonyms == ["specific"]
    assert second.synonyms == []
    assert "cat:en:computing" in first.semantic_atoms
    refute "cat:en:computing" in second.semantic_atoms
  end
end
