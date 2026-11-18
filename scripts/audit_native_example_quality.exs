# Symbrella / development — READ-ONLY database example quality and dataset pilot.
# Run from the EXISTING ~/Symbrella umbrella root:
#   mix run --no-start scripts/audit_native_example_quality.exs --limit-lines 20000
#
# No DB INSERT/UPDATE/DELETE; no vocab learning; no model training; no migrations.
# Writes exactly three named JSON/JSONL outputs to EXISTING umbrella root priv/.
# Candidate labels are SOURCE-ALIGNED PROVISIONAL, NOT HUMAN-VERIFIED GOLD.
# Source ID reconstruction must match current Db.JSONL.Populate and import order.

{opts, rest, invalid} =
  OptionParser.parse(System.argv(),
    strict: [source: :string, limit_lines: :integer, batch: :integer,
             max_candidates: :integer, max_review: :integer,
             max_examples_per_sense: :integer, max_tokens: :integer,
             progress: :integer]
  )

if rest != [] or invalid != [] do
  raise ArgumentError,
    "Usage: mix run --no-start scripts/audit_native_example_quality.exs " <>
    "[--source priv/english.jsonl] [--limit-lines 20000] [--batch 400] " <>
    "[--max-candidates 20000] [--max-review 1000] " <>
    "[--max-examples-per-sense 20] [--max-tokens 128] [--progress 10000]"
end

source = Path.expand(Keyword.get(opts, :source, "priv/english.jsonl"))
limit = Keyword.get(opts, :limit_lines, 20_000)
batch = Keyword.get(opts, :batch, 400)
max_candidates = Keyword.get(opts, :max_candidates, 20_000)
max_review = Keyword.get(opts, :max_review, 1_000)
max_examples_per_sense = Keyword.get(opts, :max_examples_per_sense, 20)
max_tokens = Keyword.get(opts, :max_tokens, 128)
progress = Keyword.get(opts, :progress, 10_000)

unless limit >= 0 and batch in 1..2_000 and
         max_candidates in 1..200_000 and max_review in 0..50_000 and
         max_examples_per_sense in 1..100 and max_tokens in 16..512 and
         progress >= 0 do
  raise ArgumentError, "Invalid limits or batch size"
end

unless File.regular?(source), do: raise("Wiktextract source missing: #{source}")
priv = Path.expand("priv")
unless File.dir?(priv), do: raise("Existing umbrella-root priv/ directory required")

# Don't accidentally run this while the umbrella's cwd is another directory.
unless File.dir?(Path.expand("scripts")),
  do: raise("Run from ~/Symbrella umbrella root (missing scripts/)")

{:ok, _} = Application.ensure_all_started(:db)
case Db.start_link() do
  {:ok, _pid} -> :ok
  {:error, {:already_started, _pid}} -> :ok
  other -> raise "Cannot start Db: #{inspect(other)}"
end

db_name =
  case Ecto.Adapters.SQL.query!(Db, "SELECT current_database()", []).rows do
    [[name]] -> name
  end

# These outputs contain only source-derived examples and audit metadata.
# Never write to the database; do not create directories.
candidates_path = Path.join(priv, "native_context_v1_candidates.jsonl")
review_path = Path.join(priv, "native_context_v1_review.jsonl")
report_path = Path.join(priv, "native_context_v1_report.json")

defmodule Symbrella.NativeExampleQuality do
  @moduledoc false
  import Ecto.Query
  alias Db.JSONL.{Populate, SenseEvidenceReconciliation, Util}
  alias Db.NativeVocab

  @det ~w(a an the this that these those my your his her its our their each every some any)

  def initial(max_candidates, max_review, max_examples_per_sense, max_tokens) do
    %{
      counters: %{}, pending: [], pending_count: 0,
      scanned_sha256_ctx: :crypto.hash_init(:sha256),
      lines: 0, english: 0, invalid_json: 0, skipped_non_english: 0,
      senses: 0, compared: 0, source_examples: 0,
      source_multi_examples: 0, source_multi_glosses: 0,
      verdicts: %{}, checks: %{}, candidates: [], candidate_count: 0,
      review: [], review_count: 0, review_total: 0,
      max_candidates: max_candidates, max_review: max_review,
      max_examples_per_sense: max_examples_per_sense, max_tokens: max_tokens,
      capped_candidates: 0, omitted_examples_per_sense: 0
    }
  end

  def increment(map, key, n \\ 1), do: Map.update(map, key, n, &(&1 + n))

  def consume(st, %{} = entry, line_no, line_sha, batch) do
    if entry["lang_code"] == "en" or entry["lang"] == "English" do
      word = Util.normalize_word(entry["word"] || "")
      if word == "" do
        %{st | english: st.english + 1}
      else
        meta = %{
          "word" => entry["word"], "pos" => entry["pos"],
          "etymology_number" => entry["etymology_number"],
          "entry_synonyms" => Util.flatten_term_list(entry["synonyms"]),
          "entry_antonyms" => Util.flatten_term_list(entry["antonyms"])
        }
        |> Enum.reject(fn {_k, v} -> is_nil(v) end)
        |> Map.new()

        senses =
          entry
          |> Map.get("senses", [])
          |> List.wrap()
          |> Enum.map(fn sense -> Map.put(Util.ensure_map(sense), "_meta", meta) end)

        {cells, counters} =
          Populate.populate_structs_from_senses_with_counters(senses, st.counters)

        new_pending =
          Enum.zip(cells, senses)
          |> Enum.map(fn {cell, sense} -> {cell, sense, line_no, line_sha} end)

        st = %{
          st | counters: counters, english: st.english + 1,
          senses: st.senses + length(cells),
          source_examples: st.source_examples + Enum.sum(Enum.map(senses, &(length(examples(&1))))),
          source_multi_examples: st.source_multi_examples + Enum.count(senses, &(length(examples(&1)) > 1)),
          source_multi_glosses: st.source_multi_glosses + Enum.count(senses, &(length(glosses(&1)) > 1)),
          pending: Enum.reverse(new_pending, st.pending),
          pending_count: st.pending_count + length(new_pending)
        }
        if st.pending_count >= batch, do: flush(st), else: st
      end
    else
      %{st | skipped_non_english: st.skipped_non_english + 1}
    end
  end

  def flush(%{pending: []} = st), do: st

  def flush(st) do
    records = Enum.reverse(st.pending)
    ids = Enum.map(records, fn {cell, _, _, _} -> cell.id end)
    stored =
      from(b in Db.BrainCell,
        where: b.id in ^ids,
        select: %{
          id: b.id, numeric_id: b.numeric_id, norm: b.norm, pos: b.pos,
          definition: b.definition, example: b.example,
          gram_function: b.gram_function, semantic_atoms: b.semantic_atoms
        }
      )
      |> Db.all()
      |> Map.new(&{&1.id, &1})

    Enum.reduce(records, %{st | pending: [], pending_count: 0}, fn
      {cell, sense, line_no, line_sha}, acc ->
        existing = Map.get(stored, cell.id)
        {verdict, _} = SenseEvidenceReconciliation.classify(cell, existing)
        acc = %{acc | compared: acc.compared + 1,
          verdicts: increment(acc.verdicts, Atom.to_string(verdict))}
        exs = examples(sense)
        selected = Enum.take(exs, acc.max_examples_per_sense)
        omitted = length(exs) - length(selected)
        acc = %{acc | omitted_examples_per_sense: acc.omitted_examples_per_sense + omitted}

        # IMPORTANT: only an exact current-source projection match qualifies.
        # Other verdicts are counted; they never get source-derived labels.
        if verdict == :matched_current do
          Enum.with_index(selected, 0)
          |> Enum.reduce(acc, fn {raw, ex_index}, state ->
            examine(state, cell, existing, sense, raw, ex_index, line_no, line_sha)
          end)
        else
          # Never treat a positional ID disagreement as an approved label.
          push_review(acc, %{
            sense_id: cell.id,
            source_line: line_no,
            source_line_sha256: line_sha,
            reconciliation: Atom.to_string(verdict),
            label_status: "rejected_source_database_alignment"
          }, "source_database_alignment_#{verdict}")
        end
    end)
  end

  defp examples(sense), do: List.wrap(sense["examples"])

  defp glosses(sense) do
    xs =
      case sense["glosses"] do
        [_ | _] = v -> v
        _ -> List.wrap(sense["raw_glosses"])
      end
    xs |> Enum.filter(&is_binary/1) |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp extract_text(%{"text" => s}) when is_binary(s), do: String.trim(s)
  defp extract_text(%{"example" => s}) when is_binary(s), do: String.trim(s)
  defp extract_text(s) when is_binary(s), do: String.trim(s)
  defp extract_text(_), do: ""

  defp span_matches(tokens, target) do
    width = length(target)
    if width == 0 or length(tokens) < width do
      []
    else
      tokens
      |> Enum.chunk_every(width, 1, :discard)
      |> Enum.with_index()
      |> Enum.filter(fn {window, _i} -> window == target end)
      |> Enum.map(fn {_window, i} -> [i, i + width] end)
    end
  end

  defp possible_inflection(tokens, lemma, "verb") do
    # Strictly REVIEW-only: never use a guessed inflection as an approved span.
    forms =
      if String.ends_with?(lemma, "y") and String.length(lemma) > 1 do
        [String.trim_trailing(lemma, "y") <> "ied", lemma <> "s", lemma <> "ing"]
      else
        [lemma <> "s", lemma <> "ed", lemma <> "ing", lemma <> "d"]
      end
    Enum.filter(forms, &(&1 in tokens))
  end
  defp possible_inflection(_tokens, _lemma, _pos), do: []

  defp syntactic_warning(tokens, [i, _j], "verb") when i > 0 do
    previous = Enum.at(tokens, i - 1)
    if previous in @det, do: "verb_target_after_determiner", else: nil
  end
  defp syntactic_warning(_tokens, _span, _pos), do: nil

  defp examine(st, cell, existing, sense, raw, ex_index, line_no, line_sha) do
    text = extract_text(raw)
    tokens = NativeVocab.tokenize(text)
    target = NativeVocab.tokenize(cell.norm)
    spans = span_matches(tokens, target)
    full_glosses = glosses(sense)

    reason =
      cond do
        text == "" -> "empty_example"
        length(tokens) + 2 > st.max_tokens -> "overlength_example"
        length(spans) == 0 -> "target_absent_or_inflected"
        length(spans) > 1 -> "multiple_target_occurrences"
        length(tokens) < 4 -> "insufficient_context"
        true -> syntactic_warning(tokens, hd(spans), cell.pos)
      end

    record = %{
      dataset_version: "native_context_v1",
      sense_id: cell.id,
      numeric_id: if(existing, do: existing.numeric_id, else: nil),
      norm: to_string(cell.norm), pos: cell.pos,
      sentence: text,
      tokens: tokens,
      target_spans: spans,
      target_span: if(length(spans) == 1, do: hd(spans), else: nil),
      # The native tokenizer adds BOS at 0, so both indices shift by +1.
      encoded_target_span: if(length(spans) == 1, do: Enum.map(hd(spans), &(&1 + 1)), else: nil),
      full_definition: cell.definition,
      distinguishing_gloss: List.last(full_glosses),
      source_gloss_path: full_glosses,
      source_line: line_no,
      source_line_sha256: line_sha,
      source_example_index: ex_index,
      split: split_for_norm(to_string(cell.norm)),
      reconciliation: "matched_current",
      label_status: "source_aligned_provisional_not_semantically_verified",
      example_sha256: digest(String.downcase(String.trim(text))),
      suspected_inflections: possible_inflection(tokens, to_string(cell.norm), cell.pos)
    }

    if is_nil(reason) do
      if st.candidate_count < st.max_candidates do
        %{st | candidates: [record | st.candidates], candidate_count: st.candidate_count + 1,
          checks: increment(st.checks, "aligned_single_target_candidates")}
      else
        %{st | capped_candidates: st.capped_candidates + 1,
          checks: increment(st.checks, "candidate_cap_reached")}
      end
    else
      push_review(st, Map.put(record, :reason, reason), reason)
    end
  end

  defp push_review(st, record, reason) do
    st = %{st | checks: increment(st.checks, reason), review_total: st.review_total + 1}
    if st.review_count < st.max_review do
      %{st | review: [record | st.review], review_count: st.review_count + 1}
    else
      st
    end
  end

  def digest(text), do: Base.encode16(:crypto.hash(:sha256, text), case: :lower)

  def split_for_norm(norm) do
    <<n::unsigned-big-16, _rest::binary>> = :crypto.hash(:sha256, norm)
    case rem(n, 100) do
      x when x < 80 -> "train"
      x when x < 90 -> "validation"
      _ -> "test"
    end
  end

  def finalize(st) do
    # Compare across all COLLECTED candidate rows, before deduplication.
    records = Enum.reverse(st.candidates)
    by_target = Enum.group_by(records, fn row ->
      {row.example_sha256, row.target_span}
    end)
    conflicting_target_keys =
      by_target
      |> Enum.filter(fn {_key, rows} ->
        rows |> Enum.map(& &1.sense_id) |> Enum.uniq() |> length() > 1
      end)
      |> Map.new()
      |> Map.keys()
      |> MapSet.new()

    by_sentence = Enum.group_by(records, & &1.example_sha256)
    cross_split_sentences =
      by_sentence
      |> Enum.filter(fn {_hash, rows} ->
        rows |> Enum.map(& &1.split) |> Enum.uniq() |> length() > 1
      end)
      |> Map.new()
      |> Map.keys()
      |> MapSet.new()

    {accepted, new_review, reasons} =
      Enum.reduce(records, {[], [], %{}}, fn row, {accepted, review, counts} ->
        cond do
          MapSet.member?(conflicting_target_keys, {row.example_sha256, row.target_span}) ->
            {accepted, [Map.put(row, :reason, "conflicting_sense_labels") | review],
             increment(counts, "conflicting_sense_labels")}
          MapSet.member?(cross_split_sentences, row.example_sha256) ->
            {accepted, [Map.put(row, :reason, "cross_split_sentence_leakage") | review],
             increment(counts, "cross_split_sentence_leakage")}
          true ->
            {[row | accepted], review, counts}
        end
      end)

    accepted =
      accepted
      |> Enum.reverse()
      |> Enum.uniq_by(fn row ->
        {row.sense_id, row.target_span, row.example_sha256}
      end)

    # Final cross-label/split conflicts receive priority in the bounded queue.
    # They should not be hidden by many earlier target-absent examples.
    new_review = Enum.reverse(new_review) |> Enum.take(st.max_review)
    remaining = max(st.max_review - length(new_review), 0)
    initial_review = Enum.reverse(st.review) |> Enum.take(remaining)
    reviewed = new_review ++ initial_review

    {accepted, reviewed, Map.merge(st.checks, reasons,
      fn _key, a, b -> a + b end),
     %{ambiguous_target_groups: MapSet.size(conflicting_target_keys),
       cross_split_sentence_groups: MapSet.size(cross_split_sentences),
       post_collision_review_total: Enum.sum(Map.values(reasons)),
       review_output_truncated?: st.review_total + Enum.sum(Map.values(reasons)) > length(reviewed)}}
  end
end

IO.puts("Native context v1 quality audit — DB READ ONLY")
IO.puts("DB: #{db_name}; SOURCE: #{source}")
IO.puts("Source lines: #{if(limit == 0, do: "all", else: limit)}; candidate cap: #{max_candidates}")

st =
  source
  |> File.stream!([:read], :line)
  |> then(fn stream -> if limit == 0, do: stream, else: Stream.take(stream, limit) end)
  |> Stream.with_index(1)
  |> Enum.reduce(
    Symbrella.NativeExampleQuality.initial(max_candidates, max_review,
      max_examples_per_sense, max_tokens),
    fn {line, i}, state ->
      state = %{state | lines: i,
        scanned_sha256_ctx: :crypto.hash_update(state.scanned_sha256_ctx, line)}
      state =
        case Jason.decode(line) do
          {:ok, %{} = entry} ->
            Symbrella.NativeExampleQuality.consume(state, entry, i,
              Symbrella.NativeExampleQuality.digest(line), batch)
          _ ->
            %{state | invalid_json: state.invalid_json + 1}
        end
      if progress > 0 and rem(i, progress) == 0 do
        IO.puts("Scanned #{i} lines; reconciled #{state.compared} senses; " <>
          "collected #{state.candidate_count} provisional candidates")
      end
      state
    end
  )
  |> Symbrella.NativeExampleQuality.flush()

{candidates, reviews, checks, cross_checks} =
  Symbrella.NativeExampleQuality.finalize(st)

# No training labels are ever marked gold/verified.
# Write temp files only inside existing umbrella-root priv, rename on success.
write_jsonl = fn target, rows ->
  tmp = target <> ".tmp"
  File.rm(tmp)
  {:ok, device} = File.open(tmp, [:write, :binary])
  try do
    Enum.each(rows, fn row -> IO.binwrite(device, Jason.encode!(row) <> "\n") end)
  after
    File.close(device)
  end
  File.rename!(tmp, target)
end

write_jsonl.(candidates_path, candidates)
write_jsonl.(review_path, reviews)

report = %{
  mode: "read_only_source_aligned_candidate_dataset",
  version: "native_context_v1",
  source: source,
  source_file_bytes: File.stat!(source).size,
  scanned_prefix_sha256: Base.encode16(:crypto.hash_final(st.scanned_sha256_ctx), case: :lower),
  complete_source_scan: limit == 0,
  db: db_name,
  lines_scanned: st.lines,
  english_entries: st.english,
  skipped_non_english: st.skipped_non_english,
  invalid_json: st.invalid_json,
  source_senses: st.senses,
  db_senses_compared: st.compared,
  reconciliation_verdicts: st.verdicts,
  source_examples: st.source_examples,
  source_senses_with_multiple_examples: st.source_multi_examples,
  source_senses_with_multiple_glosses: st.source_multi_glosses,
  examples_skipped_per_sense_cap: st.omitted_examples_per_sense,
  candidate_cap: max_candidates,
  candidates_omitted_due_to_cap: st.capped_candidates,
  candidates_before_conflict_filter: st.candidate_count,
  provisional_candidate_rows: length(candidates),
  review_rows_written: length(reviews),
  review_rows_total_before_cap: st.review_total + cross_checks.post_collision_review_total,
  checks: checks,
  collision_checks: cross_checks,
  splits: Enum.frequencies_by(candidates, & &1.split),
  candidate_lemma_count: candidates |> Enum.map(& &1.norm) |> Enum.uniq() |> length(),
  candidate_sense_count: candidates |> Enum.map(& &1.sense_id) |> Enum.uniq() |> length(),
  candidate_output: candidates_path,
  review_output: review_path,
  review_output_truncated?: cross_checks.review_output_truncated?,
  warnings: [
    "Candidate labels are weak source-aligned supervision, never human-verified gold.",
    "A matching positional BrainCell id is not independent proof of historical identity.",
    "Target presence and a simple grammatical filter do not prove contextual sense.",
    "Verb inflections and ambiguous target spans go to review, not training.",
    "Conflict checks cover collected candidates, not examples excluded by caps.",
    "Hash-by-norm splits prevent same-norm train/test overlap in collected candidates.",
    "Sentence duplicates spanning splits are excluded from candidates.",
    "This is a bounded pilot, not a production training manifest."
  ],
  db_writes: false,
  vocab_writes: false,
  model_writes: false
}

tmp_report = report_path <> ".tmp"
File.rm(tmp_report)
File.write!(tmp_report, Jason.encode!(report, pretty: true) <> "\n")
File.rename!(tmp_report, report_path)

IO.puts("\n=== Source-aligned example quality audit ===")
IO.inspect(Map.take(report, [:lines_scanned, :source_senses, :db_senses_compared,
  :reconciliation_verdicts, :source_examples, :candidates_before_conflict_filter,
  :provisional_candidate_rows, :review_rows_written, :review_rows_total_before_cap,
  :candidate_lemma_count, :candidate_sense_count, :splits, :checks, :collision_checks]),
  pretty: true, limit: :infinity, width: 120)
IO.puts("Candidate manifest: #{candidates_path}")
IO.puts("Review queue:       #{review_path}")
IO.puts("Audit report:       #{report_path}")
IO.puts("No DB rows, vocab IDs, embeddings, or synapses were modified.")
