# Symbrella Native Context V2 — strictly provisional, read-only corpus builder.
#
# Run from ~/Symbrella on branch development:
#   mix run --no-start scripts/build_native_context_v2_pilot.exs
#
# Reads ONLY priv/native_context_v1_candidates.jsonl.
# Writes versioned files ONLY under EXISTING umbrella-root priv/.
# Does not connect to or alter the database; does not learn vocabulary, train,
# create directories, or touch model weights, synapses, or BrainCells.
#
# The V1 source labels remain provisional, not human-reviewed ground truth.

{opts, rest, bad} =
  OptionParser.parse(System.argv(),
    strict: [input: :string, collision_samples: :integer, progress: :integer]
  )

if rest != [] or bad != [] do
  raise ArgumentError,
    "Usage: mix run --no-start scripts/build_native_context_v2_pilot.exs " <>
      "[--input priv/native_context_v1_candidates.jsonl] " <>
      "[--collision-samples 20] [--progress 20000]"
end

root = File.cwd!()
priv = Path.join(root, "priv")

unless File.dir?(priv) and File.dir?(Path.join(root, "scripts")) do
  raise "Run from the existing ~/Symbrella umbrella root; will not create directories"
end

input = Path.expand(Keyword.get(opts, :input, "priv/native_context_v1_candidates.jsonl"))
collision_samples = Keyword.get(opts, :collision_samples, 20)
progress = Keyword.get(opts, :progress, 20_000)

unless File.regular?(input), do: raise("Missing input manifest: #{input}")
unless collision_samples in 0..200 and progress >= 0,
  do: raise("--collision-samples must be 0..200 and --progress >= 0")

# Explicit distinct filenames. Never overwrite the V1 manifest or its audits.
paths = %{
  train: Path.join(priv, "native_context_v2_train.jsonl"),
  seen_validation: Path.join(priv, "native_context_v2_seen_validation.jsonl"),
  unseen_validation: Path.join(priv, "native_context_v2_unseen_validation.jsonl"),
  unseen_test: Path.join(priv, "native_context_v2_unseen_test.jsonl"),
  collisions: Path.join(priv, "native_context_v2_collision_samples.jsonl"),
  report: Path.join(priv, "native_context_v2_report.json")
}

defmodule Symbrella.NativeContextV2Pilot do
  @moduledoc false

  def normalize(sentence) do
    sentence
    |> String.downcase()
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
  end

  def key(row), do: normalize(row["sentence"])

  def sha(text),
    do: Base.encode16(:crypto.hash(:sha256, text), case: :lower)

  def plus(map, key, amount \\ 1),
    do: Map.update(map, key, amount, &(&1 + amount))

  def valid?(row) do
    span = row["target_span"]
    tokens = row["tokens"]
    with true <- is_binary(row["sentence"]) and String.trim(row["sentence"]) != "",
         true <- is_binary(row["sense_id"]) and row["sense_id"] != "",
         true <- is_binary(row["norm"]) and row["norm"] != "",
         true <- is_binary(row["pos"]) and row["pos"] != "",
         true <- row["split"] in ["train", "validation", "test"],
         true <- row["reconciliation"] == "matched_current",
         true <- row["label_status"] == "source_aligned_provisional_not_semantically_verified",
         true <- is_binary(row["distinguishing_gloss"]) and String.trim(row["distinguishing_gloss"]) != "",
         true <- is_list(tokens),
         [a, b] <- span,
         true <- is_integer(a) and is_integer(b) and a >= 0 and b > a and b <= length(tokens),
         true <- row["encoded_target_span"] == [a + 1, b + 1],
         true <- key(row) != "" do
      true
    else
      _ -> false
    end
  end

  def scan(input, progress) do
    input
    |> File.stream!([:read], :line)
    |> Stream.with_index(1)
    |> Enum.reduce(%{rows: [], invalid: 0, total: 0,
                     sha_ctx: :crypto.hash_init(:sha256)},
      fn {line, i}, state ->
        if progress > 0 and rem(i, progress) == 0,
          do: IO.puts("Read #{i} candidate rows")
        state = %{state | total: state.total + 1,
          sha_ctx: :crypto.hash_update(state.sha_ctx, line)}
        case Jason.decode(line) do
          {:ok, %{} = row} ->
            if valid?(row),
              do: %{state | rows: [row | state.rows]},
              else: %{state | invalid: state.invalid + 1}
          _ -> %{state | invalid: state.invalid + 1}
        end
      end)
    |> then(fn state ->
      state
      |> Map.put(:rows, Enum.reverse(state.rows))
      |> Map.put(:input_sha256,
        Base.encode16(:crypto.hash_final(state.sha_ctx), case: :lower))
      |> Map.delete(:sha_ctx)
    end)
  end

  def classify_sentences(rows, sample_count) do
    by_sentence = Enum.group_by(rows, &key/1)
    acc = %{
      retained: [], samples: [], groups: map_size(by_sentence),
      conflicting_sentence_groups: 0, conflicting_sentence_rows: 0,
      same_target_conflict_groups: 0,
      different_target_only_groups: 0,
      cross_word_split_groups: 0,
      repeated_single_label_groups: 0,
      duplicate_single_label_rows: 0
    }

    by_sentence
    |> Enum.sort_by(fn {s, _rows} -> sha(s) end)
    |> Enum.reduce(acc, fn {sentence, group}, state ->
      distinct_labels = group |> Enum.map(& &1["sense_id"]) |> Enum.uniq()
      distinct_splits = group |> Enum.map(& &1["split"]) |> Enum.uniq()
      # A real target collision is the same normalized sentence and token span
      # assigned to more than one sense (not merely two targets in one sentence).
      same_target_conflict? =
        group
        |> Enum.group_by(& &1["target_span"])
        |> Enum.any?(fn {_span, span_rows} ->
          span_rows |> Enum.map(& &1["sense_id"]) |> Enum.uniq() |> length() > 1
        end)

      different_label? = length(distinct_labels) > 1
      cross_split? = length(distinct_splits) > 1

      if different_label? or cross_split? do
        classification =
          cond do
            cross_split? -> "cross_word_split"
            same_target_conflict? -> "same_target_conflicting_senses"
            true -> "different_targets_same_sentence"
          end

        sample = %{
          sentence: sentence,
          classification: classification,
          total_rows: length(group),
          involved_senses: distinct_labels,
          splits: distinct_splits,
          target_spans: Enum.map(group, & &1["target_span"])
        }

        %{state |
          conflicting_sentence_groups: state.conflicting_sentence_groups + 1,
          conflicting_sentence_rows: state.conflicting_sentence_rows + length(group),
          same_target_conflict_groups: state.same_target_conflict_groups +
            if(same_target_conflict?, do: 1, else: 0),
          different_target_only_groups: state.different_target_only_groups +
            if(different_label? and not same_target_conflict?, do: 1, else: 0),
          cross_word_split_groups: state.cross_word_split_groups +
            if(cross_split?, do: 1, else: 0),
          samples: if(length(state.samples) < sample_count,
            do: [sample | state.samples], else: state.samples)
        }
      else
        # Keep just one instance of repeated, identical normalized text for
        # each single sense. This guards against duplicate-context inflation.
        chosen = hd(Enum.sort_by(group, fn r -> {r["source_line"], r["source_example_index"]} end))
        %{state |
          retained: [chosen | state.retained],
          repeated_single_label_groups: state.repeated_single_label_groups +
            if(length(group) > 1, do: 1, else: 0),
          duplicate_single_label_rows: state.duplicate_single_label_rows + length(group) - 1
        }
      end
    end)
    |> then(fn state ->
      %{state | retained: Enum.reverse(state.retained),
                samples: Enum.reverse(state.samples)}
    end)
  end

  def eligible_groups(rows) do
    rows
    |> Enum.group_by(fn r -> {r["norm"], r["pos"]} end)
    |> Enum.sort_by(fn {key, _} -> key end)
    |> Enum.flat_map(fn {{norm, pos}, examples} ->
      senses =
        examples
        |> Enum.group_by(& &1["sense_id"])
        |> Enum.map(fn {sense_id, rs} ->
          gloss = rs |> hd() |> Map.fetch!("distinguishing_gloss")
          {sense_id, normalize(gloss), Enum.sort_by(rs, &sha(key(&1)))}
        end)
        |> Enum.filter(fn {_id, gloss, contexts} ->
          gloss != "" and length(contexts) >= 3
        end)

      # A group can include two dictionary senses with the *same* final gloss.
      # Such senses cannot serve as different classes in a gloss-matching pilot.
      unique_gloss_senses =
        senses
        |> Enum.group_by(fn {_id, gloss, _contexts} -> gloss end)
        |> Enum.map(fn {_gloss, group} ->
          Enum.min_by(group, fn {id, _gloss, contexts} -> {-length(contexts), id} end)
        end)
        |> Enum.sort_by(fn {id, _, _} -> id end)

      if length(unique_gloss_senses) >= 2 do
        [%{norm: norm, pos: pos, split: hd(examples)["split"],
           sense_records: unique_gloss_senses}]
      else
        []
      end
    end)
  end

  def row_for_output(row, subset) do
    row
    |> Map.put("dataset_version", "native_context_v2_pilot")
    |> Map.put("dataset_subset", subset)
    |> Map.put("label_status", "source_aligned_provisional_not_semantically_verified")
  end

  def make_splits(groups) do
    Enum.reduce(groups,
      %{train: [], seen_validation: [], unseen_validation: [], unseen_test: [],
        groups_by_split: %{}, group_count: 0, senses_used: 0},
      fn g, state ->
        state = %{state |
          group_count: state.group_count + 1,
          senses_used: state.senses_used + length(g.sense_records),
          groups_by_split: plus(state.groups_by_split, g.split)}

        Enum.reduce(g.sense_records, state, fn {_sense_id, _gloss, contexts}, acc ->
          case g.split do
            "train" ->
              # At least 3 distinct sentences: reserve exactly one per sense
              # as unseen *context* and retain >=2 distinct training contexts.
              [holdout | training] = contexts
              %{acc |
                train: Enum.map(training, &row_for_output(&1, "train")) ++ acc.train,
                seen_validation: [row_for_output(holdout, "seen_validation") |
                                  acc.seen_validation]}
            "validation" ->
              %{acc | unseen_validation:
                Enum.map(contexts, &row_for_output(&1, "unseen_validation")) ++
                acc.unseen_validation}
            "test" ->
              %{acc | unseen_test:
                Enum.map(contexts, &row_for_output(&1, "unseen_test")) ++
                acc.unseen_test}
          end
        end)
      end)
    |> then(fn state ->
      state
      |> Map.update!(:train, &Enum.reverse/1)
      |> Map.update!(:seen_validation, &Enum.reverse/1)
      |> Map.update!(:unseen_validation, &Enum.reverse/1)
      |> Map.update!(:unseen_test, &Enum.reverse/1)
    end)
  end

  def leakage_report(sets) do
    all = for subset <- [:train, :seen_validation, :unseen_validation, :unseen_test],
              row <- Map.fetch!(sets, subset), do: {subset, row}

    by_sentence = Enum.group_by(all, fn {_subset, row} -> key(row) end)
    by_norm = Enum.group_by(all, fn {_subset, row} -> row["norm"] end)
    by_sense = Enum.group_by(all, fn {_subset, row} -> row["sense_id"] end)

    %{
      normalized_sentence_across_subsets:
        Enum.count(by_sentence, fn {_s, pairs} ->
          pairs |> Enum.map(&elem(&1, 0)) |> Enum.uniq() |> length() > 1
        end),
      original_word_split_overlap:
        Enum.count(by_norm, fn {_n, pairs} ->
          subsets = pairs |> Enum.map(&elem(&1, 0)) |> MapSet.new()
          MapSet.member?(subsets, :train) and
            (MapSet.member?(subsets, :unseen_validation) or
             MapSet.member?(subsets, :unseen_test))
        end),
      original_sense_split_overlap:
        Enum.count(by_sense, fn {_s, pairs} ->
          subsets = pairs |> Enum.map(&elem(&1, 0)) |> MapSet.new()
          MapSet.member?(subsets, :train) and
            (MapSet.member?(subsets, :unseen_validation) or
             MapSet.member?(subsets, :unseen_test))
        end),
      seen_validation_senses_without_train:
        Enum.count(by_sense, fn {_s, pairs} ->
          subsets = pairs |> Enum.map(&elem(&1, 0)) |> MapSet.new()
          MapSet.member?(subsets, :seen_validation) and
            not MapSet.member?(subsets, :train)
        end)
    }
  end
end

alias Symbrella.NativeContextV2Pilot, as: V2

IO.puts("Native Context V2 pilot: source-aligned, provisional, NO DB access or model training")
state = V2.scan(input, progress)
collision = V2.classify_sentences(state.rows, collision_samples)
groups = V2.eligible_groups(collision.retained)
if groups == [], do: raise("No eligible groups after strict filtering; no outputs written")
sets = V2.make_splits(groups)
if Enum.any?([:train, :seen_validation, :unseen_validation, :unseen_test],
  &(Map.fetch!(sets, &1) == [])) do
  raise "At least one training/evaluation subset is empty; no outputs written"
end
leakage = V2.leakage_report(sets)

if Enum.any?(Map.values(leakage), &(&1 != 0)) do
  raise "V2 dataset guard failed: #{inspect(leakage)} — no outputs written"
end

# Write only after passing guards. Temp files and final filenames live in
# umbrella-root priv. Existing V1 input and diagnostics remain unchanged.
write_jsonl = fn path, rows ->
  tmp = path <> ".tmp"
  File.rm(tmp)
  {:ok, file} = File.open(tmp, [:write, :binary])
  try do
    Enum.each(rows, fn row -> IO.binwrite(file, Jason.encode!(row) <> "\n") end)
  after
    File.close(file)
  end
  File.rename!(tmp, path)
end

for subset <- [:train, :seen_validation, :unseen_validation, :unseen_test] do
  write_jsonl.(Map.fetch!(paths, subset), Map.fetch!(sets, subset))
end
write_jsonl.(paths.collisions, collision.samples)

sizes = Map.new([:train, :seen_validation, :unseen_validation, :unseen_test],
  fn subset -> {subset, length(Map.fetch!(sets, subset))} end)

report = %{
  version: "native_context_v2_pilot",
  mode: "source_aligned_provisional_training_manifest",
  input: input,
  input_sha256: state.input_sha256,
  input_rows: state.total,
  invalid_input_rows: state.invalid,
  normalized_sentence_groups: collision.groups,
  collision_groups_quarantined: collision.conflicting_sentence_groups,
  collision_rows_quarantined: collision.conflicting_sentence_rows,
  same_target_conflict_groups: collision.same_target_conflict_groups,
  different_target_only_groups: collision.different_target_only_groups,
  cross_word_split_groups: collision.cross_word_split_groups,
  repeated_single_label_groups: collision.repeated_single_label_groups,
  duplicate_single_label_rows_removed: collision.duplicate_single_label_rows,
  valid_rows_after_collision_filter: length(collision.retained),
  strict_pilot_policy: "Quarantine every normalized sentence with >1 sense label, even if it has different valid target spans. Preserve V1 for future review.",
  eligible_groups: sets.group_count,
  distinct_senses_used: sets.senses_used,
  groups_by_original_word_split: sets.groups_by_split,
  output_rows: sizes,
  leakage: leakage,
  outputs: paths,
  training_sense_minimum_examples: 2,
  seen_validation_sense_minimum_examples: 1,
  source_label_status: "PROVISIONAL, NOT HUMAN-VERIFIED",
  caveats: [
    "This is a conservatively filtered pilot, NOT a validated gold WSD benchmark.",
    "Each competing sense requires >=3 distinct contextual sentences and a distinct final gloss.",
    "One within-word context per seen sense is reserved for validation; at least two contexts remain for training.",
    "The existing word-level test and validation splits measure unseen-word generalization.",
    "Sentences with multiple labels are all quarantined for this pilot; many may be harmless distinct-target examples.",
    "Gloss similarity and syntax still require dedicated error analysis; no model has been trained."
  ],
  db_writes: false,
  vocabulary_writes: false,
  model_writes: false,
  new_directories: false
}

tmp = paths.report <> ".tmp"
File.rm(tmp)
File.write!(tmp, Jason.encode!(report, pretty: true) <> "\n")
File.rename!(tmp, paths.report)

IO.puts("\n=== Native Context V2 pilot ===")
IO.inspect(Map.take(report, [:input_rows, :invalid_input_rows,
  :collision_groups_quarantined, :collision_rows_quarantined,
  :same_target_conflict_groups, :different_target_only_groups,
  :duplicate_single_label_rows_removed, :eligible_groups,
  :distinct_senses_used, :groups_by_original_word_split,
  :output_rows, :leakage]), pretty: true, width: 120, limit: :infinity)
IO.puts("Reports and manifests written only under: #{priv}")
IO.puts("No database rows, vocabulary IDs, embeddings, model weights, or synapses were modified.")
