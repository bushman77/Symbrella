defmodule Db.JSONL.FullFidelityAuditTest do
  use ExUnit.Case, async: true
  @moduletag :tmp_dir

  alias Db.JSONL.{FullFidelityAudit, Populate}

  defp entries do
    [
      %{
        "word" => "-cele", "lang_code" => "en", "pos" => "suffix",
        "etymology_number" => 1,
        "synonyms" => [%{"word" => "entry-level-uncertain"}],
        "senses" => [
          %{
            "glosses" => ["A tumour."], "tags" => ["morpheme"],
            "synonyms" => [%{"word" => "-oma", "sense" => "tumour"}],
            "topics" => ["medicine", "sciences"],
            "examples" => [%{"text" => "first"}, %{"text" => "second"}]
          },
          %{
            "glosses" => ["A hernia."], "tags" => ["morpheme"],
            "topics" => ["medicine", "sciences"]
          }
        ]
      },
      %{
        "word" => "-cele", "lang_code" => "en", "pos" => "suffix",
        "etymology_number" => 2,
        "senses" => [
          %{
            "glosses" => ["Alternative form of -coel."],
            "tags" => ["alt-of", "morpheme"],
            "alt_of" => [%{"word" => "-coel"}]
          }
        ]
      }
    ]
  end

  defp source_file(tmp_dir, entries) do
    path = Path.join(tmp_dir, "english.jsonl")
    File.write!(path, Enum.map_join(entries, "\n", &Jason.encode!/1) <> "\n")
    path
  end

  defp expected_rows do
    {first, counters} =
      Populate.populate_structs_from_senses_with_counters(
        Enum.map(Enum.at(entries(), 0)["senses"], fn sense ->
          Map.put(sense, "_meta",
            %{"word" => "-cele", "pos" => "suffix", "etymology_number" => 1})
        end), %{})

    {second, _} =
      Populate.populate_structs_from_senses_with_counters(
        Enum.map(Enum.at(entries(), 1)["senses"], fn sense ->
          Map.put(sense, "_meta",
            %{"word" => "-cele", "pos" => "suffix", "etymology_number" => 2})
        end), counters)

    Map.new(first ++ second, &{&1.id, Map.from_struct(&1)})
  end

  test "audits every sense and inventories evidence not modeled in BrainCells", %{tmp_dir: dir} do
    source = source_file(dir, entries())
    rows = expected_rows()

    lookup = fn ids ->
      send(self(), {:lookup, ids})
      Map.take(rows, ids)
    end

    report = FullFidelityAudit.audit(source,
      lookup: lookup, db_count: 3, batch: 2, progress: 0)

    assert_receive {:lookup, ["-cele|phrase|1", "-cele|phrase|2"]}
    assert_receive {:lookup, ["-cele|phrase|3"]}
    assert report.source_senses == 3
    assert report.compared == 3
    assert report.matching_rows == 3
    assert report.absent_ids == 0
    assert report.total_field_mismatches == 0
    assert report.fidelity_pass
    assert String.length(report.source_sha256) == 64
    assert report.sha256_covers_complete_file
    assert report.source_evidence.senses_with_topics == 2
    assert report.source_evidence.topic_values == 4
    assert report.source_evidence.additional_examples == 1
    assert report.source_evidence.senses_with_relationship_qualifiers == 1
    assert report.source_evidence.senses_with_other_typed_relationships == 1
    assert report.source_evidence.entry_relationship_entries == 1
    assert report.unprojected_sense_keys["topics"] == 2
    assert report.unprojected_sense_keys["alt_of"] == 1
    assert rows["-cele|phrase|2"].synonyms == []
  end

  test "reports missing rows and separate lexical field mismatches", %{tmp_dir: dir} do
    source = source_file(dir, entries())
    rows = expected_rows()

    modified =
      rows
      |> Map.update!("-cele|phrase|1", &Map.put(&1, :synonyms, []))
      |> Map.update!("-cele|phrase|2", &Map.put(&1, :definition, "Wrong gloss"))
      |> Map.delete("-cele|phrase|3")

    report = FullFidelityAudit.audit(source,
      lookup: &Map.take(modified, &1), db_count: 2,
      batch: 2, progress: 0, sample_limit: 2)

    assert report.absent_ids == 1
    assert report.compared == 2
    assert report.mismatch_fields.synonyms == 1
    assert report.mismatch_fields.definition == 1
    assert report.total_field_mismatches == 2
    refute report.fidelity_pass
    assert [%{id: "-cele|phrase|3"}] = report.samples.absent_id
  end

  test "trial scans cannot claim whole-database fidelity", %{tmp_dir: dir} do
    source = source_file(dir, entries())
    report = FullFidelityAudit.audit(source,
      lookup: &Map.take(expected_rows(), &1),
      limit_lines: 1, db_count: 3, progress: 0)

    assert report.source_senses == 2
    assert report.compared == 2
    refute report.complete_source_scan
    refute report.sha256_covers_complete_file
    refute report.fidelity_pass
  end

  test "array comparisons ignore order but not missing elements" do
    assert FullFidelityAudit.equivalent?(:synonyms, ["a", "b"], ["b", "a"])
    refute FullFidelityAudit.equivalent?(:synonyms, ["a", "b"], ["a"])
    refute FullFidelityAudit.equivalent?(:definition, "A", "B")
  end
end
