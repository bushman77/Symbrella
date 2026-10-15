defmodule Db.JSONL.CategoryEvidencePolicyTest do
  use ExUnit.Case, async: true

  alias Db.JSONL.{CategoryEvidencePolicy, Populate}

  defp category(name, kind \\ "other", source \\ "w") do
    %{"name" => name, "kind" => kind, "source" => source}
  end

  test "promotes curated domains and place categories as sense-specific evidence" do
    assert CategoryEvidencePolicy.version() == 1
    assert CategoryEvidencePolicy.decide(category("Computing")) ==
             {:emit, :semantic_domain, "cat:en:computing"}

    assert CategoryEvidencePolicy.decide(category("Software")) ==
             {:emit, :semantic_domain, "cat:en:software"}

    assert CategoryEvidencePolicy.decide(category("Algebra")) ==
             {:emit, :semantic_domain, "cat:en:algebra"}

    assert CategoryEvidencePolicy.decide(category("Cities in Arkansas, USA", "place")) ==
             {:emit, :place, "cat:en:cities in arkansas, usa"}

    assert CategoryEvidencePolicy.decide(category("History", "topic")) ==
             {:emit, :topic, "cat:en:history"}

    assert CategoryEvidencePolicy.decide(category("English surnames")) ==
             {:emit, :name_class, "cat:en:english surnames"}
  end

  test "regional and grammar labels use a different namespace from semantic domains" do
    assert CategoryEvidencePolicy.decide(category("Canadian English")) ==
             {:emit, :regional_usage, "usage:region:canadian english"}

    assert CategoryEvidencePolicy.decide(category("British English")) ==
             {:emit, :regional_usage, "usage:region:british english"}

    assert CategoryEvidencePolicy.decide(category("English pluralia tantum")) ==
             {:emit, :grammatical_usage, "usage:grammar:english pluralia tantum"}
  end

  test "never promotes Wiktionary maintenance and unknown other labels" do
    for name <- [
      "Pages with entries", "English entries with incorrect language header",
      "Mandarin terms with redundant transliterations",
      "Japanese terms with redundant script codes",
      "Terms with French translations", "English terms with collocations"
    ] do
      assert {:skip, :maintenance} = CategoryEvidencePolicy.decide(category(name))
    end

    assert {:skip, :disambiguation_source} =
             CategoryEvidencePolicy.decide(category("Cities in Arkansas, USA", "place", "w+disamb"))

    assert {:skip, :unreviewed_other} =
             CategoryEvidencePolicy.decide(category("An unreviewed topic"))

    assert {:skip, :unknown_kind} =
             CategoryEvidencePolicy.decide(category("Lifeforms", "lifeform"))

    assert {:skip, :legacy_string} =
             CategoryEvidencePolicy.decide("Computing")

    assert {:skip, :wrong_source} =
             CategoryEvidencePolicy.decide(category("Computing", "other", "x"))
  end

  test "newly populated senses retain full glosses but no misleading semantic atoms" do
    [cell] = Populate.populate_structs_from_senses([
      %{
        "_meta" => %{"word" => "free", "pos" => "adj"},
        "glosses" => ["Unconstrained.", "Not imprisoned or enslaved."],
        "categories" => [
          category("Software"),
          category("British English"),
          category("English terms with collocations"),
          category("Pages with entries", "other", "w+disamb"),
          category("An unreviewed topic"),
          category("Software")
        ]
      }
    ])

    assert cell.id == "free|adjective|1"
    assert cell.definition == "Unconstrained. — Not imprisoned or enslaved."
    assert cell.semantic_atoms == [
      "cat:en:software",
      "usage:region:british english",
      "pos_raw:adj"
    ]
  end
end
