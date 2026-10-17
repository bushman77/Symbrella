defmodule Db.JSONL.SenseEvidenceAuditTest do
  use ExUnit.Case, async: true

  alias Db.JSONL.SenseEvidenceAudit, as: Audit

  test "Cash place entries are distinct despite their identical first glosses" do
    arkansas = %{
      "word" => "Cash",
      "lang_code" => "en",
      "pos" => "name",
      "senses" => [
        %{
          "glosses" => [
            "A number of places in the United States:",
            "A minor city in Craighead County, Arkansas."
          ],
          "categories" => [
            %{"source" => "w", "kind" => "place", "name" => "Cities in Arkansas, USA"},
            %{"source" => "w+disamb", "kind" => "other", "name" => "Pages with entries"}
          ]
        }
      ]
    }

    georgia = %{
      "word" => "Cash",
      "lang_code" => "en",
      "pos" => "name",
      "senses" => [
        %{
          "glosses" => [
            "A number of places in the United States:",
            "An unincorporated community in Gordon County, Georgia."
          ],
          "categories" => [
            %{"source" => "w", "kind" => "place", "name" => "Places in Georgia, USA"}
          ]
        }
      ]
    }

    summary =
      Audit.new()
      |> Audit.ingest(arkansas)
      |> Audit.ingest(georgia)
      |> Audit.finish()

    assert summary.entries == 2
    assert summary.source_senses == 2
    assert summary.senses_with_multiple_glosses == 2
    assert summary.senses_with_source_categories == 2
    assert summary.senses_with_both == 2
    assert summary.source_category_values == 2
    assert summary.word_runs == 1
    assert summary.shared_first_gloss_groups == 1
    assert summary.shared_first_gloss_senses == 2
    assert summary.groups_distinguished_by_full_gloss == 1
    assert summary.senses_in_distinguishable_groups == 2
    assert summary.groups_not_distinguished_by_full_gloss == 0

    assert [%{word: "cash", source_pos: "name", distinct_full_glosses: 2}] =
             summary.examples
  end

  test "source counts are separate from stored DB counts, and word runs flush" do
    a = %{
      "word" => "bank", "lang" => "English", "pos" => "noun",
      "senses" => [%{"glosses" => ["Financial institution."]},
                   %{"glosses" => ["Financial institution."]}]
    }

    b = %{
      "word" => "development", "lang_code" => "en", "pos" => "noun",
      "senses" => [%{"glosses" => ["Process.", "Growth."]}]
    }

    summary =
      Audit.new(sample_limit: 0)
      |> Audit.ingest(a)
      |> Audit.ingest(b)
      |> Audit.finish()

    assert summary.source_senses == 3
    assert summary.word_runs == 2
    assert summary.shared_first_gloss_groups == 1
    assert summary.groups_not_distinguished_by_full_gloss == 1
    assert summary.groups_distinguished_by_full_gloss == 0
    assert summary.senses_with_multiple_glosses == 1
    assert summary.examples == []
  end

  test "raw glosses are used only if cleaned glosses are absent" do
    entry = %{
      "word" => "alpha", "lang_code" => "en", "pos" => "noun",
      "senses" => [
        %{"glosses" => ["  ", ""], "raw_glosses" => ["Raw heading", "Raw detail"]},
        %{"glosses" => ["Clean gloss", "Clean gloss"],
          "raw_glosses" => ["Raw heading", "Raw detail"]},
        %{"glosses" => ["Clean heading", "Clean detail"],
          "categories" => [%{"source" => "w+disamb", "name" => "Maintenance"}]}
      ]
    }

    summary = Audit.new() |> Audit.ingest(entry) |> Audit.finish()
    assert summary.senses_with_multiple_glosses == 2
    assert summary.senses_with_source_categories == 0
    assert summary.source_category_values == 0
  end

  test "non-English or malformed entries are excluded without crashing" do
    summary =
      Audit.new()
      |> Audit.ingest(%{"word" => "hola", "lang_code" => "es", "pos" => "noun"})
      |> Audit.ingest(%{"word" => "wrong", "lang_code" => "en", "pos" => "noun",
                        "senses" => nil})
      |> Audit.ingest("garbled")
      |> Audit.finish()

    assert summary.invalid_or_non_english_entries == 2
    assert summary.source_senses == 0
    assert summary.entries == 1
  end
end
