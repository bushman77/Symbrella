defmodule Db.JSONL.ImportBatchTest do
  use ExUnit.Case, async: true

  @moduletag :tmp_dir

  alias Db.JSONL.Import

  # In-memory collection of batches; tests the real importer without modifying DB.
  defmodule CollectingRepo do
    def insert_all(Db.BrainCell, rows, options) do
      send(self(), {:inserted_batch, rows, options})
      {length(rows), nil}
    end
  end

  test "imports the final partial batch and preserves sense-level evidence", %{tmp_dir: tmp_dir} do
    entries = [
      %{
        "word" => "dictionary",
        "pos" => "noun",
        "senses" => [
          %{
            "glosses" => ["A reference collection.", "A collection of defined terms."],
            "synonyms" => [%{"word" => "lexicon"}],
            "examples" => [%{"text" => "Consult the dictionary."}],
            "categories" => [%{
              "kind" => "other",
              "name" => "Computing",
              "source" => "w"
            }]
          }
        ]
      },
      %{
        "word" => "water",
        "pos" => "noun",
        "senses" => [%{"glosses" => ["A liquid."]}]
      },
      %{
        "word" => "well",
        "pos" => "noun",
        "senses" => [%{"glosses" => ["A source of water."]}]
      }
    ]

    path = Path.join(tmp_dir, "english.jsonl")
    File.write!(path, Enum.map_join(entries, "\n", &Jason.encode!/1) <> "\n")

    assert {:ok, %{upserted: 3, batches: 2, invalid_skipped: 0,
                   db_errors: 0, bad_json: 0}} =
             Import.import_all(
               path: path,
               repo: CollectingRepo,
               batch_size: 2,
               validate?: true,
               every: 0,
               on_decode_error: :raise,
               on_db_error: :raise
             )

    assert_receive {:inserted_batch, [first, second], opts}
    assert_receive {:inserted_batch, [third], _opts}
    refute_receive {:inserted_batch, _, _}

    assert first.id == "dictionary|noun|1"
    assert first.definition ==
             "A reference collection. — A collection of defined terms."
    assert first.example == "Consult the dictionary."
    assert first.synonyms == ["lexicon"]
    assert "cat:en:computing" in first.semantic_atoms
    assert second.id == "water|noun|1"
    assert third.id == "well|noun|1"

    # Upserts change only source lexical data, not existing BrainCell lifecycle.
    assert {:replace, columns} = Keyword.fetch!(opts, :on_conflict)
    refute :status in columns
    refute :activation in columns
    refute :connections in columns
  end

  test "even a single sense is inserted when batch size is larger", %{tmp_dir: tmp_dir} do
    path = Path.join(tmp_dir, "single.jsonl")
    File.write!(path, Jason.encode!(%{
      "word" => "alpha",
      "pos" => "noun",
      "senses" => [%{"glosses" => ["The first letter."]}]
    }) <> "\n")

    assert {:ok, %{upserted: 1, batches: 1}} =
             Import.import_all(
               path: path,
               repo: CollectingRepo,
               batch_size: 2_000,
               validate?: true,
               every: 0,
               on_decode_error: :raise,
               on_db_error: :raise
             )

    assert_receive {:inserted_batch, [%{id: "alpha|noun|1"}], _opts}
  end
end
