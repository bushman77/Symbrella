defmodule Db.JSONL.SenseEvidenceBackfillTest do
  use ExUnit.Case, async: true

  alias Db.JSONL.{Populate, SenseEvidenceBackfill}

  defp fixtures do
    source =
      Populate.populate_structs_from_senses([
        %{
          "_meta" => %{"word" => "Cash", "pos" => "name"},
          "glosses" => ["A surname originating as an occupation."],
          "categories" => [
            %{"source" => "w", "kind" => "other", "name" => "English surnames"}
          ]
        },
        %{
          "_meta" => %{"word" => "Cash", "pos" => "name"},
          "glosses" => [
            "A number of places in the United States:",
            "A minor city in Craighead County, Arkansas."
          ],
          "categories" => [
            %{"source" => "w", "kind" => "place", "name" => "Cities in Arkansas, USA"},
            %{"source" => "w+disamb", "kind" => "other", "name" => "Pages with entries"}
          ]
        },
        %{
          "_meta" => %{"word" => "Cash", "pos" => "name"},
          "glosses" => [
            "A number of places in the United States:",
            "A community in Gordon County, Georgia."
          ],
          "categories" => [
            %{"source" => "w", "kind" => "place", "name" => "Places in Georgia, USA"}
          ]
        }
      ])

    stored =
      Enum.map(source, fn %Db.BrainCell{} = cell ->
        first_gloss = String.split(cell.definition, " — ", parts: 2) |> hd()

        %Db.BrainCell{
          cell |
          definition: first_gloss,
          semantic_atoms: Enum.reject(cell.semantic_atoms,
            &String.starts_with?(&1, "cat:")),
          status: "active",
          activation: 0.93,
          connections: [%{"rel" => "already-learned"}]
        }
      end)

    {source, stored}
  end

  test "produces a stable, explicitly reviewed plan without mutating input rows" do
    {source, stored} = fixtures()
    originals = :erlang.term_to_binary({source, stored})

    assert {:ok, plan} =
             SenseEvidenceBackfill.plan("Cash", "proper_noun", source, stored)

    assert plan.row_count == 3
    assert plan.patch_count == 3
    assert byte_size(plan.fingerprint) == 64
    assert Enum.map(plan.rows, & &1.id) ==
             ~w(cash|proper_noun|1 cash|proper_noun|2 cash|proper_noun|3)

    arkansas = Enum.find(plan.patches, &(&1.id == "cash|proper_noun|2"))

    assert arkansas.stored_definition == "A number of places in the United States:"
    assert String.contains?(arkansas.reconstructed_definition, "Arkansas")
    assert arkansas.added_categories == ["cat:en:cities in arkansas, usa"]
    refute Enum.any?(arkansas.added_categories, &String.contains?(&1, "pages with"))
    assert stored |> Enum.all?(&(&1.status == "active" and &1.activation == 0.93))
    assert :erlang.term_to_binary({source, stored}) == originals

    assert {:ok, repeated} =
             SenseEvidenceBackfill.plan("Cash", "proper_noun",
               Enum.reverse(source), Enum.reverse(stored))

    assert repeated.fingerprint == plan.fingerprint
  end

  test "rejects stale fingerprints before attempting any database write" do
    {source, stored} = fixtures()
    {:ok, plan} = SenseEvidenceBackfill.plan("Cash", "proper_noun", source, stored)

    assert {:error, :fingerprint_mismatch} =
             SenseEvidenceBackfill.apply(plan, String.duplicate("0", 64))
  end

  test "rejects unexpected IDs and duplicated sense identities" do
    {source, stored} = fixtures()

    assert {:error, {:source_db_id_mismatch, _, _}} =
             SenseEvidenceBackfill.plan("Cash", "proper_noun",
               source, tl(stored))

    assert {:error, :duplicate_sense_ids} =
             SenseEvidenceBackfill.plan("Cash", "proper_noun",
               source ++ [hd(source)], stored)
  end

  test "rejects source reorder that does not match the original stored gloss" do
    {source, stored} = fixtures()
    [first, second, third] = source

    # A previous import could have assigned a source sense to a different ID.
    # When the original gloss differs, this must prevent an unsafe update.
    shifted = [
      %{first | id: third.id},
      second,
      %{third | id: first.id}
    ]

    assert {:error, {_, :existing_gloss_not_source_prefix}} =
             SenseEvidenceBackfill.plan("Cash", "proper_noun", shifted, stored)
  end

  test "rejects mismatched example or grammatical metadata" do
    {source, stored} = fixtures()
    [first | rest] = stored

    assert {:error, {_, :existing_example_mismatch}} =
             SenseEvidenceBackfill.plan("Cash", "proper_noun",
               source, [%{first | example: "Changed."} | rest])

    assert {:error, {_, :existing_grammar_mismatch}} =
             SenseEvidenceBackfill.plan("Cash", "proper_noun",
               source, [%{first | gram_function: ["countable"]} | rest])
  end

  test "refuses to modify non-category metadata or a changed existing gloss" do
    {source, stored} = fixtures()
    [first | rest] = stored

    assert {:error, {_, :existing_noncategory_metadata_mismatch}} =
             SenseEvidenceBackfill.plan("Cash", "proper_noun",
               source,
               [%{first | semantic_atoms: ["pos_raw:changed"]} | rest])

    assert {:error, {_, :existing_gloss_not_source_prefix}} =
             SenseEvidenceBackfill.plan("Cash", "proper_noun",
               source, [%{first | definition: "Unrelated meaning."} | rest])
  end
end
