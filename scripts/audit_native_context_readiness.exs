# Symbrella / development: read-only training-readiness audit.
# Run from ~/Symbrella: mix run --no-start scripts/audit_native_context_readiness.exs
# Reads existing priv/native_context_v1_candidates.jsonl only.
# Writes ONE report to existing priv/native_context_v1_readiness.json.
# No database calls/writes, vocabulary changes, additional folders, or training.

{opts, rest, bad} = OptionParser.parse(System.argv(), strict: [input: :string, report: :string, progress: :integer])
if rest != [] or bad != [], do: raise("Usage: mix run --no-start scripts/audit_native_context_readiness.exs [--input PATH] [--report PATH] [--progress N]")

input = Path.expand(Keyword.get(opts, :input, "priv/native_context_v1_candidates.jsonl"))
report_path = Path.expand(Keyword.get(opts, :report, "priv/native_context_v1_readiness.json"))
progress = Keyword.get(opts, :progress, 20_000)
unless progress >= 0, do: raise("--progress must be >= 0")
unless File.regular?(input), do: raise("Missing candidate manifest: #{input}")
unless File.dir?(Path.expand("priv")) and File.dir?(Path.expand("scripts")),
  do: raise("Run from the existing ~/Symbrella umbrella root")
unless Path.dirname(report_path) == Path.expand("priv"),
  do: raise("Report must be saved in the existing umbrella-root priv/ directory")

# No DB application is needed: this is a dataset-level census.
defmodule Symbrella.ContextReadiness do
  @moduledoc false

  def initial do
    %{rows: 0, malformed: 0, sense_rows: %{}, sentences: %{}, norms: %{},
      splits: %{}, label_statuses: %{}, row_checks: %{}, sha: :crypto.hash_init(:sha256)}
  end

  def bump(map, key, amount \\ 1), do: Map.update(map, key, amount, &(&1 + amount))

  def consume(state, raw) do
    state = %{state | sha: :crypto.hash_update(state.sha, raw)}
    case Jason.decode(raw) do
      {:ok, %{"sense_id" => sense_id, "norm" => norm, "pos" => pos,
              "sentence" => sentence, "split" => split} = row}
      when is_binary(sense_id) and is_binary(norm) and is_binary(pos) and
           is_binary(sentence) and is_binary(split) ->
        state = %{state | rows: state.rows + 1,
          splits: bump(state.splits, split),
          label_statuses: bump(state.label_statuses, Map.get(row, "label_status", "missing"))}

        gloss = row |> Map.get("distinguishing_gloss", "") |> to_string_safe()
        current = %{sense_id: sense_id, norm: norm, pos: pos, split: split,
          gloss: String.trim(gloss), count: 1}
        senses = Map.update(state.sense_rows, sense_id, current, fn old ->
          %{old | count: old.count + 1}
        end)

        norm_splits = Map.update(state.norms, norm, MapSet.new([split]),
          &MapSet.put(&1, split))

        normalized_sentence =
          sentence
          |> String.downcase()
          |> String.replace(~r/\s+/u, " ")
          |> String.trim()
        sentence_hash = :crypto.hash(:sha256, normalized_sentence)
        sentence_info = %{splits: MapSet.new([split]), labels: MapSet.new([sense_id]),
          row_count: 1}
        sentence_index = Map.update(state.sentences, sentence_hash, sentence_info, fn old ->
          %{old | splits: MapSet.put(old.splits, split),
            labels: MapSet.put(old.labels, sense_id), row_count: old.row_count + 1}
        end)

        checks =
          state.row_checks
          |> inspect_span(row)
          |> inspect_gloss(row)

        %{state | sense_rows: senses, norms: norm_splits,
          sentences: sentence_index, row_checks: checks}

      _ -> %{state | malformed: state.malformed + 1}
    end
  end

  defp to_string_safe(v) when is_binary(v), do: v
  defp to_string_safe(_), do: ""

  defp inspect_gloss(checks, row) do
    if is_binary(row["distinguishing_gloss"]) and
      String.trim(row["distinguishing_gloss"]) != "" do
      checks
    else
      bump(checks, "missing_distinguishing_gloss")
    end
  end

  defp inspect_span(checks, row) do
    tokens = row["tokens"]
    span = row["target_span"]
    encoded = row["encoded_target_span"]
    cond do
      not is_list(tokens) or not is_list(span) or length(span) != 2 ->
        bump(checks, "missing_tokens_or_span")
      true ->
        [a, b] = span
        cond do
          not is_integer(a) or not is_integer(b) or a < 0 or b <= a or b > length(tokens) ->
            bump(checks, "invalid_token_span")
          encoded != [a + 1, b + 1] ->
            bump(checks, "encoded_span_offset_mismatch")
          true -> checks
        end
    end
  end

  def finalize(state, input) do
    senses = Map.values(state.sense_rows)
    senses_by_count = Enum.frequencies_by(senses, fn s ->
      cond do
        s.count == 1 -> "1_example"
        s.count == 2 -> "2_examples"
        s.count in 3..4 -> "3_to_4_examples"
        s.count in 5..9 -> "5_to_9_examples"
        true -> "10_plus_examples"
      end
    end)
    senses_by_norm_pos = Enum.group_by(senses, fn s -> {s.norm, s.pos} end)
    grouped = Enum.map(senses_by_norm_pos, fn {{norm, pos}, rows} ->
      supported_2 = Enum.filter(rows, &(&1.count >= 2 and &1.gloss != ""))
      supported_3 = Enum.filter(rows, &(&1.count >= 3 and &1.gloss != ""))
      distinct_2 = supported_2 |> Enum.map(&String.downcase(&1.gloss)) |> Enum.uniq() |> length()
      distinct_3 = supported_3 |> Enum.map(&String.downcase(&1.gloss)) |> Enum.uniq() |> length()
      %{norm: norm, pos: pos, all_senses: length(rows),
        usable_2: length(supported_2), usable_3: length(supported_3),
        distinct_glosses_2: distinct_2, distinct_glosses_3: distinct_3,
        eligible_for_training?: length(supported_2) >= 2 and distinct_2 >= 2,
        eligible_for_context_validation?: length(supported_3) >= 2 and distinct_3 >= 2,
        examples: Enum.sum(Enum.map(rows, & &1.count)),
        qualified_examples_2: Enum.sum(Enum.map(supported_2, & &1.count)),
        qualified_examples_3: Enum.sum(Enum.map(supported_3, & &1.count)),
        split: hd(rows).split,
        example_sense_ids: Enum.take(Enum.map(supported_3, & &1.sense_id), 5)}
    end)

    competing_groups = Enum.filter(grouped, &(&1.all_senses >= 2))
    multi_example_groups = Enum.filter(grouped, & &1.eligible_for_training?)
    eval_groups = Enum.filter(grouped, & &1.eligible_for_context_validation?)
    across_norm_groups = Enum.group_by(grouped, & &1.norm)

    sentence_info = Map.values(state.sentences)
    split_conflicts = Enum.count(sentence_info, &(MapSet.size(&1.splits) > 1))
    multi_labels = Enum.count(sentence_info, &(MapSet.size(&1.labels) > 1))
    norm_split_conflicts = Enum.count(state.norms, fn {_k, v} -> MapSet.size(v) > 1 end)

    %{
      audit_kind: "native_context_v1_training_readiness",
      source_manifest: input,
      input_sha256: state.sha |> :crypto.hash_final() |> Base.encode16(case: :lower),
      database_writes: false,
      provisional_label_warning: "Source aligned does not mean semantically verified.",
      row_count: state.rows,
      malformed_rows: state.malformed,
      split_rows: state.splits,
      label_statuses: state.label_statuses,
      sense_count: length(senses),
      norm_count: map_size(state.norms),
      norm_pos_groups: length(grouped),
      senses_by_example_support: senses_by_count,
      senses_with_2_plus_examples: Enum.count(senses, &(&1.count >= 2)),
      senses_with_3_plus_examples: Enum.count(senses, &(&1.count >= 3)),
      sense_examples_2_plus: senses |> Enum.filter(&(&1.count >= 2)) |> Enum.reduce(0, &(&1.count + &2)),
      sense_examples_3_plus: senses |> Enum.filter(&(&1.count >= 3)) |> Enum.reduce(0, &(&1.count + &2)),
      norms_with_multiple_senses: Enum.count(across_norm_groups, fn {_word, gs} ->
        Enum.sum(Enum.map(gs, & &1.all_senses)) >= 2
      end),
      norm_pos_groups_with_multiple_senses: length(competing_groups),
      trainable_2_per_sense_distinct_gloss_groups: length(multi_example_groups),
      trainable_2_per_sense_distinct_gloss_examples:
        Enum.sum(Enum.map(multi_example_groups, & &1.qualified_examples_2)),
      context_eval_3_per_sense_distinct_gloss_groups: length(eval_groups),
      context_eval_3_per_sense_distinct_gloss_examples:
        Enum.sum(Enum.map(eval_groups, & &1.qualified_examples_3)),
      trainable_groups_by_word_split: Enum.frequencies_by(multi_example_groups, & &1.split),
      context_eval_groups_by_word_split: Enum.frequencies_by(eval_groups, & &1.split),
      quality_checks: Map.merge(state.row_checks, %{
        "norms_in_multiple_splits" => norm_split_conflicts,
        "normalized_sentences_in_multiple_splits" => split_conflicts,
        "normalized_sentences_with_multiple_sense_labels" => multi_labels
      }),
      top_context_evaluable_groups:
        eval_groups
        |> Enum.sort_by(&(-&1.examples))
        |> Enum.take(15)
        |> Enum.map(&Map.take(&1, [:norm, :pos, :all_senses, :usable_3,
          :distinct_glosses_3, :examples, :split, :example_sense_ids])),
      caveats: [
        "Trainable counts only require source alignment, example support and nonidentical last glosses. They are NOT human-verified labels.",
        "The v1 split is by normalized word: test/validation norms are unseen in training. It cannot alone test held-out contexts of trained senses.",
        "Only normalized exact sentence overlap is inspected here; paraphrases and subtle semantic conflicts remain possible.",
        "Two examples per sense support training but not a separate positive-context validation example. Three or more is a conservative minimum for that.",
        "No dataset or model is modified by this audit."
      ]
    }
  end
end

IO.puts("Symbrella native context readiness — no DB access, no training")
report =
  input
  |> File.stream!([:read], :line)
  |> Stream.with_index(1)
  |> Enum.reduce(Symbrella.ContextReadiness.initial(), fn {line, index}, state ->
    if progress > 0 and rem(index, progress) == 0,
      do: IO.puts("Checked #{index} candidate rows")
    Symbrella.ContextReadiness.consume(state, line)
  end)
  |> Symbrella.ContextReadiness.finalize(input)

tmp = report_path <> ".tmp"
File.rm(tmp)
File.write!(tmp, Jason.encode!(report, pretty: true) <> "\n")
File.rename!(tmp, report_path)

IO.inspect(Map.take(report, [:row_count, :sense_count, :norm_count,
  :senses_by_example_support, :senses_with_2_plus_examples,
  :senses_with_3_plus_examples, :norm_pos_groups_with_multiple_senses,
  :trainable_2_per_sense_distinct_gloss_groups,
  :trainable_2_per_sense_distinct_gloss_examples,
  :context_eval_3_per_sense_distinct_gloss_groups,
  :context_eval_3_per_sense_distinct_gloss_examples,
  :trainable_groups_by_word_split,
  :context_eval_groups_by_word_split, :quality_checks]),
  pretty: true, width: 120, limit: :infinity)
IO.puts("Readiness report: #{report_path}")
IO.puts("No DB rows, vocabulary IDs, model weights, or synapses changed.")
