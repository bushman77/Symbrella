defmodule Brain.Cerebellum.SnapshotTest do
  use ExUnit.Case, async: true

  alias Brain.Cerebellum
  alias Brain.Cerebellum.Dataset
  alias Brain.Cerebellum.Snapshot

  test "keeps actual candidate IDs, five features, and baseline scores" do
    assert {:ok, snapshot} = Snapshot.build(sample())

    assert snapshot.v == 1
    assert snapshot.feature_schema == 2
    assert snapshot.source == "stage1_unreviewed"
    assert snapshot.split == "unassigned"
    assert snapshot.label_id == nil
    assert snapshot.reviewed_by == nil
    assert snapshot.group_id == "bank"
    assert snapshot.token_index == 5
    assert snapshot.base_winner_id == "bank|financial|22"
    assert String.starts_with?(snapshot.id, "stage1-")
    assert byte_size(snapshot.context_key_hex) == 64

    assert snapshot.base_scores == %{
             "bank|river|11" => 0.46,
             "bank|financial|22" => 0.54
           }

    assert Enum.map(snapshot.candidates, & &1.id) ==
             ["bank|river|11", "bank|financial|22"]

    assert hd(snapshot.candidates).context_fit == 0.9
    refute Map.has_key?(hd(snapshot.candidates), :perception_bias)
  end

  test "appends valid JSONL, without marking predictions as reviewed" do
    file =
      Path.join(
        System.tmp_dir!(),
        "cerebellum-stage1-" <>
          Integer.to_string(System.unique_integer([:positive])) <> ".jsonl"
      )

    on_exit(fn -> File.rm(file) end)

    assert :ok = Snapshot.record(file, sample())
    assert :ok = Snapshot.record(file, %{sample() | token: "crane"})

    lines = file |> File.read!() |> String.trim() |> String.split("\n")
    assert length(lines) == 2

    [first, second] = Enum.map(lines, &Jason.decode!/1)
    assert first["sentence"] == "I sat by the bank of the river."
    assert first["base_scores"]["bank|river|11"] == 0.46
    assert second["group_id"] == "crane"
    refute first["id"] == second["id"]

    # It must be human-reviewed before Dataset can accept it.
    assert {:error, {:line, 1, _}} = Dataset.load_jsonl(file)
  end

  test "rejects incomplete features, mismatched IDs and invalid contexts" do
    assert {:error, :invalid_context_key} =
             sample()
             |> Map.put(:context_key, <<1, 2, 3>>)
             |> Snapshot.build()

    assert {:error, :score_candidates_mismatch} =
             sample()
             |> Map.put(:base_scores, %{"wrong" => 0.5})
             |> Snapshot.build()

    [first, second] = sample().candidates

    assert {:error, :invalid_candidates} =
             sample()
             |> Map.put(:candidates, [Map.delete(first, :context_fit), second])
             |> Snapshot.build()

    assert {:error, :invalid_path} = Snapshot.record("", sample())
  end

  defp sample do
    context_key =
      Cerebellum.context_key({:lifg_stage1, intent: :ask, mwe: false})

    %{
      sentence: "I sat by the bank of the river.",
      token: "bank",
      token_index: 5,
      context_key: context_key,
      candidates: [
        %{
          id: "bank|river|11",
          context_fit: 0.9,
          lex_fit: 0.8,
          rel_prior: 0.7,
          activation: 0.5,
          intent_bias: 0.2,
          perception_bias: 0.3
        },
        %{
          id: "bank|financial|22",
          context_fit: 0.2,
          lex_fit: 0.8,
          rel_prior: 0.7,
          activation: 0.5,
          intent_bias: 0.2
        }
      ],
      base_scores: %{
        "bank|river|11" => 0.46,
        "bank|financial|22" => 0.54
      }
    }
  end
end
