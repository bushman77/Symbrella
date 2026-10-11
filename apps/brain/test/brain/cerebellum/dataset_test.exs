defmodule Brain.Cerebellum.DatasetTest do
  use ExUnit.Case, async: true

  alias Brain.Cerebellum
  alias Brain.Cerebellum.Dataset

  test "loads and separates reviewed training and evaluation records" do
    records = [
      sample("bank-1", "bank", "train"),
      sample("crane-1", "crane", "eval"),
      sample("seal-1", "seal", "eval")
    ]

    assert {:ok, dataset} = load_records(records)

    assert dataset.v == 1
    assert dataset.feature_schema == 2
    assert dataset.total == 3
    assert dataset.train_count == 1
    assert dataset.eval_count == 2

    assert [training] = dataset.train
    assert training.id == "bank-1"
    assert training.split == :train
    assert training.source == :human_review
    assert byte_size(training.context_key) == 32

    assert Enum.map(dataset.eval, & &1.id) ==
             ["crane-1", "seal-1"]

    assert is_number(hd(training.candidates).context_fit)
    assert Map.has_key?(training.base_scores, training.label_id)
  end

  test "rejects group leakage across training and evaluation" do
    records = [
      sample("bank-train", "bank", "train"),
      sample("bank-eval", "bank", "eval")
    ]

    assert {:error, {:split_leakage, "bank"}} =
             load_records(records)
  end

  test "rejects duplicate IDs and repeated sentence occurrences" do
    first = sample("bank-1", "bank", "train")

    duplicate_id =
      sample("bank-1", "crane", "eval")

    assert {:error, {:duplicate_id, "bank-1"}} =
             load_records([first, duplicate_id])

    duplicate_occurrence =
      sample("crane-2", "crane", "eval")
      |> Map.put("sentence", first["sentence"])

    assert {:error, {:duplicate_occurrence, "crane-2"}} =
             load_records([first, duplicate_occurrence])
  end

  test "rejects unverified sources and missing contextual features" do
    valid = sample("bank-1", "bank", "train")

    unverified = Map.put(valid, "source", "stage1")

    assert {:error, {:line, 1, :unverified_source}} =
             load_records([unverified])

    missing_context =
      update_in(valid, ["candidates"], fn [first, second] ->
        [Map.delete(first, "context_fit"), second]
      end)

    assert {:error, {:line, 1, :invalid_candidate}} =
             load_records([missing_context])
  end

  test "rejects invalid labels and malformed context keys" do
    valid = sample("bank-1", "bank", "eval")

    invalid_label =
      Map.put(valid, "label_id", "missing-sense")

    assert {:error, {:line, 1, :label_not_in_candidates}} =
             load_records([invalid_label])

    invalid_context =
      Map.put(valid, "context_key_hex", "invalid")

    assert {:error, {:line, 1, :invalid_context_key}} =
             load_records([invalid_context])
  end

  test "enforces dataset version and feature schema" do
    valid = sample("bank-1", "bank", "eval")

    wrong_version = Map.put(valid, "v", 99)

    assert {:error, {:line, 1, :unsupported_dataset_version}} =
             load_records([wrong_version])

    wrong_schema = Map.put(valid, "feature_schema", 1)

    assert {:error, {:line, 1, :unsupported_feature_schema}} =
             load_records([wrong_schema])

    assert {:error, :empty_dataset} = load_records([])
  end

  defp sample(id, group, split) do
    correct = "#{group}|sense|1"
    competing = "#{group}|sense|2"

    context_key =
      Cerebellum.context_key({:lifg_stage1, intent: :ask, mwe: false})

    %{
      "v" => 1,
      "feature_schema" => 2,
      "id" => id,
      "group_id" => group,
      "sentence" => "The #{group} appeared in #{id}.",
      "token" => group,
      "token_index" => 1,
      "split" => split,
      "source" => "human_review",
      "reviewed_by" => "test-reviewer",
      "label_id" => correct,
      "context_key_hex" => Base.encode16(context_key, case: :lower),
      "candidates" => [
        %{
          "id" => correct,
          "context_fit" => 0.9,
          "lex_fit" => 0.8,
          "rel_prior" => 0.5,
          "activation" => 0.5,
          "intent_bias" => 0.2
        },
        %{
          "id" => competing,
          "context_fit" => 0.1,
          "lex_fit" => 0.8,
          "rel_prior" => 0.5,
          "activation" => 0.5,
          "intent_bias" => 0.2
        }
      ],
      "base_scores" => %{
        correct => 0.45,
        competing => 0.55
      }
    }
  end

  defp load_records(records) do
    filename =
      "cerebellum-dataset-" <>
        Integer.to_string(System.unique_integer([:positive])) <> ".jsonl"

    path = Path.join(System.tmp_dir!(), filename)

    contents =
      Enum.map_join(records, "\n", &Jason.encode!/1) <> "\n"

    File.write!(path, contents)

    try do
      Dataset.load_jsonl(path)
    after
      File.rm(path)
    end
  end
end
