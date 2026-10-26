# Symbrella / development — experimental in-memory, held-out sense retrieval.
# Place in the existing ~/Symbrella/scripts directory, then run from umbrella root:
#   mix run --no-start scripts/there_neural_heldout_pilot.exs
# Prerequisites: populated Db.NativeVocab and priv/there_sense_pilot.jsonl
# No DB writes, directories created, model files saved, or source files modified.
# This wraps the existing native contrastive toy; do not interpret its results as
# validated word-sense-disambiguation accuracy.

{:ok, _} = Application.ensure_all_started(:db)
case Process.whereis(Db) do
  nil -> {:ok, _} = Db.start_link()
  _pid -> :ok
end

source_path = "scripts/train_native_contrastive_toy.exs"
source = File.read!(source_path)

old_init = """
embedding_table =
  {vocab_size, embedding_dim}
  |> Nx.iota(type: {:f, 32})
  |> Nx.multiply(0.01)
  |> Nx.sin()
  |> Nx.multiply(0.02)
"""

new_init = """
:rand.seed(:exsss, {11, 22, 33})
embedding_values =
  for _ <- 1..(vocab_size * embedding_dim) do
    (:rand.uniform() - 0.5) * 0.04
  end
embedding_table =
  embedding_values
  |> Nx.tensor(type: {:f, 32})
  |> Nx.reshape({vocab_size, embedding_dim})
"""

unless String.contains?(source, old_init) do
  raise "Expected deterministic sine initializer not found. No files changed."
end

inference = ~S"""
section.("Seven held-out sentences: in-memory sense retrieval")

pilot_file = "priv/there_sense_pilot.jsonl"
unless File.regular?(pilot_file), do: raise("Missing evaluation file: #{pilot_file}")

pilot_cases =
  pilot_file
  |> File.stream!()
  |> Stream.reject(&(String.trim(&1) == ""))
  |> Enum.map(&Jason.decode!/1)

# Use exactly the weighting statistics from the training documents.
# In particular, the evaluation sentences do not change training IDF values.
training_weights_by_id =
  NativeTokenWeights.build_token_weights_by_id(left_rows ++ right_rows,
    mode: weight_mode,
    pad_id: pad_id,
    unk_id: unk_id
  )

queries =
  Enum.map(pilot_cases, fn row ->
    # No gold part of speech or expected sense ID is supplied to the model.
    # The target word is the same for every query, so is not a sense label.
    query_text = "there\n" <> row["sentence"]
    ids = NativeVocab.encode_text(query_text, max_tokens: max_tokens)
    padded = pad_ids.(ids, max_tokens, pad_id)
    mask = mask_for.(ids, max_tokens)
    weights =
      Enum.map(padded, fn id ->
        cond do
          id == pad_id -> 0.0
          id == unk_id and weight_mode == "batch_idf" -> 0.25
          true -> Map.get(training_weights_by_id, id, 1.0)
        end
      end)

    %{row: row, ids: padded, mask: mask, weights: weights}
  end)

if queries == [], do: raise("Evaluation file is empty")

query_input = Nx.tensor(Enum.map(queries, & &1.ids), type: {:s, 64})
query_masks = Nx.tensor(Enum.map(queries, & &1.mask), type: {:s, 64})
query_weights = Nx.tensor(Enum.map(queries, & &1.weights), type: {:f, 32})

sense_vectors =
  Symbrella.NativeContrastiveToy.encode(
    embedding_table, left_input_ids, left_attention_mask, left_token_weights
  )

query_vectors =
  Symbrella.NativeContrastiveToy.encode(
    embedding_table, query_input, query_masks, query_weights
  )

similarity_flat =
  Nx.dot(query_vectors, Nx.transpose(sense_vectors))
  |> Nx.to_flat_list()

case_results =
  pilot_cases
  |> Enum.with_index()
  |> Enum.map(fn {row, i} ->
    scores = Enum.slice(similarity_flat, i * batch_size, batch_size)
    ranked =
      scores
      |> Enum.with_index()
      |> Enum.sort_by(fn {score, _j} -> -score end)
      |> Enum.map(fn {score, j} -> %{id: Enum.at(encoded_pairs, j).id, score: Float.round(score, 4)} end)

    expected = row["expected_id"]
    expected_rank = Enum.find_index(ranked, &(&1.id == expected))
    unless expected_rank, do: raise("Missing sense ID in candidate list: #{inspect(expected)}")

    result = %{
      sentence: row["sentence"],
      expected: expected,
      predicted: hd(ranked).id,
      expected_rank: expected_rank + 1,
      top_three: Enum.take(ranked, 3)
    }
    IO.inspect(result, pretty: true, limit: :infinity)
    result
  end)

correct = Enum.count(case_results, &(&1.expected == &1.predicted))
IO.inspect(%{
  cases: length(case_results),
  top1_correct: correct,
  top1_percent: Float.round(100.0 * correct / length(case_results), 2),
  top5_correct: Enum.count(case_results, &(&1.expected_rank <= 5)),
  comparison_lifg_baseline_correct: 3,
  caveat: "Provisional human-reviewed labels needed; toy trained on dictionary examples only; training-batch accuracy is not generalization."
}, label: "HELD-OUT PILOT RESULT", pretty: true)
"""

marker = "section.(\"Result\")"
unless String.contains?(source, marker) do
  raise "Expected result marker not found. No files changed."
end

patched =
  source
  |> String.replace(old_init, new_init, global: false)
  |> String.replace(marker, inference <> "\n" <> marker, global: false)

System.argv([
  "--norm", "there",
  "--limit", "24",
  "--max-tokens", "128",
  "--embedding-dim", "32",
  "--steps", "30",
  "--learning-rate", "0.1",
  "--view-mode", "semantic_compact",
  "--weight-mode", "batch_idf"
])

Code.eval_string(patched, [], file: Path.expand(source_path))
