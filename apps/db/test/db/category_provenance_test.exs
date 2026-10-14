defmodule Db.JSONL.CategoryProvenanceTest do
  use ExUnit.Case, async: true

  alias Db.JSONL.CategoryProvenance

  test "separates useful-looking lexical categories from source maintenance metadata" do
    assert CategoryProvenance.classify(%{
      "name" => "Cities in Arkansas, USA", "kind" => "place", "source" => "w"
    }) == :place

    assert CategoryProvenance.classify(%{
      "name" => "Hydrology", "kind" => "topic", "source" => "w"
    }) == :topic

    # These are useful, but 'other' must undergo separate vetting.
    assert CategoryProvenance.classify(%{
      "name" => "English surnames", "kind" => "other", "source" => "w"
    }) == :other_review

    assert CategoryProvenance.classify(%{
      "name" => "English pluralia tantum", "kind" => "other", "source" => "w"
    }) == :other_review

    assert CategoryProvenance.classify(%{
      "name" => "Pages with 2 entries", "kind" => "other", "source" => "w+disamb"
    }) == :disambiguation_source

    assert CategoryProvenance.classify(%{
      "name" => "English links with redundant wikilinks",
      "kind" => "other", "source" => "w"
    }) == :likely_maintenance

    assert CategoryProvenance.classify(%{
      "name" => "English entries with incorrect language header",
      "kind" => "other", "source" => "w"
    }) == :likely_maintenance

    assert CategoryProvenance.classify(%{
      "name" => "Terms with redundant transliterations",
      "kind" => "other", "source" => "w"
    }) == :likely_maintenance

    assert CategoryProvenance.classify(%{
      "name" => "Other source label", "kind" => "other", "source" => "w+other"
    }) == :non_w_source
  end

  test "does not promote every category just because Wiktionary reported it" do
    assert CategoryProvenance.classify(%{
      "name" => "Novel unreviewed field", "source" => "w", "kind" => "other"
    }) == :other_review

    assert CategoryProvenance.classify(%{
      "name" => "Unreviewed kind", "source" => "w", "kind" => "unknown"
    }) == :untyped

    assert CategoryProvenance.classify(%{
      "name" => "", "source" => "w", "kind" => "topic"
    }) == :missing_name

    assert CategoryProvenance.classify("Historical category") == :legacy_string
  end
end
