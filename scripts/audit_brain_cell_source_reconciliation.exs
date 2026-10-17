# READ ONLY: batched Wiktextract -> existing BrainCell evidence reconciliation.
#
# Default: first 20,000 source JSONL entries, batches of 1,000 candidate senses.
#   mix run scripts/audit_brain_cell_source_reconciliation.exs
#
# First 100,000 entries:
#   mix run scripts/audit_brain_cell_source_reconciliation.exs --limit-lines 100000
#
# Full source file (heavy; source POS counters grow with distinct word/POS keys):
#   mix run scripts/audit_brain_cell_source_reconciliation.exs --limit-lines 0
#
# No insert/update/delete; this is NOT a bulk backfill.
#
# Reproduces Db.JSONL.Import's positional per-word/POS sense counters from
# the same source order. Even a matching ID and old gloss cannot independently
# guarantee that the source has never reordered senses.

import Ecto.Query
alias Db.JSONL.{Populate, SenseEvidenceReconciliation, Util}

args =
  case System.argv() do
    ["--" | rest] -> rest
    rest -> rest
  end

{opts, rest, invalid} =
  OptionParser.parse(args,
    strict: [limit_lines: :integer, batch: :integer, sample_limit: :integer,
             progress: :integer])

if rest != [] or invalid != [] do
  raise ArgumentError,
    "Usage: mix run scripts/audit_brain_cell_source_reconciliation.exs " <>
      "[--limit-lines N] [--batch N] [--sample-limit N] [--progress N]"
end

limit = Keyword.get(opts, :limit_lines, 20_000)
batch = Keyword.get(opts, :batch, 1_000)
sample_limit = Keyword.get(opts, :sample_limit, 5)
progress = Keyword.get(opts, :progress, 20_000)
path = Db.JSONL.IO.default_path()

if limit < 0 or batch < 1 or batch > 5_000 or sample_limit < 0 or progress < 0 do
  raise ArgumentError, "Require limit_lines >= 0, batch 1..5000, sample_limit >= 0 and progress >= 0"
end

unless File.regular?(path), do: raise("Missing Wiktextract source: #{path}")

defmodule Symbrella.SourceRowReconciliation do
  @moduledoc false

  import Ecto.Query
  alias Db.JSONL.{Populate, SenseEvidenceReconciliation, Util}

  def new(sample_limit) do
    %{
      counters: %{},
      pending: [],
      pending_count: 0,
      lines: 0,
      invalid_json: 0,
      source_senses: 0,
      candidates: 0,
      checked_candidates: 0,
      verdict_counts: %{},
      missing_extended_definitions: 0,
      missing_categories: 0,
      missing_usage: 0,
      missing_both: 0,
      examples: %{},
      sample_limit: sample_limit
    }
  end

  def consume(state, %{} = entry, batch) do
    word = entry["word"]

    if is_binary(word) and String.trim(word) != "" do
      meta =
        %{
          "word" => entry["word"],
          "pos" => entry["pos"],
          "etymology_number" => entry["etymology_number"],
          "entry_synonyms" => Util.flatten_term_list(entry["synonyms"]),
          "entry_antonyms" => Util.flatten_term_list(entry["antonyms"])
        }
        |> Enum.reject(fn {_k, v} -> is_nil(v) end)
        |> Map.new()

      senses =
        case entry["senses"] do
          v when is_list(v) -> Enum.map(v, fn s ->
            s = if is_map(s), do: s, else: %{}
            Map.put(s, "_meta", meta)
          end)
          _ -> []
        end

      {cells, counters} =
        Populate.populate_structs_from_senses_with_counters(senses, state.counters)

      selected =
        Enum.zip(cells, senses)
        |> Enum.filter(fn {cell, sense} ->
          # This is the SOURCE eligibility test, not proof of a stale DB row.
          multiple_glosses?(sense) or
            Enum.any?(cell.semantic_atoms, fn atom ->
              is_binary(atom) and
                (String.starts_with?(atom, "cat:") or
                 String.starts_with?(atom, "usage:"))
            end)
        end)
        |> Enum.map(&elem(&1, 0))

      state = %{
        state |
        counters: counters,
        source_senses: state.source_senses + length(cells),
        candidates: state.candidates + length(selected),
        pending: Enum.reverse(selected, state.pending),
        pending_count: state.pending_count + length(selected)
      }

      if state.pending_count >= batch, do: flush(state), else: state
    else
      state
    end
  end

  def multiple_glosses?(sense) do
    primary = glosses(sense["glosses"])
    result = if primary == [], do: glosses(sense["raw_glosses"]), else: primary
    length(result) > 1
  end

  defp glosses(values) do
    values
    |> List.wrap()
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  def flush(%{pending: []} = state), do: state

  def flush(state) do
    pending = Enum.reverse(state.pending)
    ids = Enum.map(pending, & &1.id)

    stored =
      from(b in Db.BrainCell,
        where: b.id in ^ids,
        select: %{
          id: b.id,
          norm: b.norm,
          pos: b.pos,
          definition: b.definition,
          example: b.example,
          gram_function: b.gram_function,
          semantic_atoms: b.semantic_atoms
        })
      |> Db.all()
      |> Map.new(&{&1.id, &1})

    state = %{state | pending: [], pending_count: 0}

    Enum.reduce(pending, state, fn source, acc ->
      {verdict, detail} =
        SenseEvidenceReconciliation.classify(source, Map.get(stored, source.id))

      old = detail.original_definition
      expected = detail.reconstructed_definition
      gloss_gap? =
        verdict == :missing_source_evidence and
          is_binary(old) and is_binary(expected) and old != expected

      category_gap? =
        verdict == :missing_source_evidence and detail.added_categories != []
      usage_gap? =
        verdict == :missing_source_evidence and detail.added_usage_atoms != []

      existing_examples = Map.get(acc.examples, verdict, [])
      examples =
        if length(existing_examples) < acc.sample_limit and
             verdict != :matched_current do
          Map.put(acc.examples, verdict, [detail | existing_examples])
        else
          acc.examples
        end

      %{
        acc |
        checked_candidates: acc.checked_candidates + 1,
        verdict_counts: Map.update(acc.verdict_counts, verdict, 1, &(&1 + 1)),
        missing_extended_definitions:
          acc.missing_extended_definitions + if(gloss_gap?, do: 1, else: 0),
        missing_categories:
          acc.missing_categories + if(category_gap?, do: 1, else: 0),
        missing_usage:
          acc.missing_usage + if(usage_gap?, do: 1, else: 0),
        missing_both:
          acc.missing_both + if(gloss_gap? and category_gap?, do: 1, else: 0),
        examples: examples
      }
    end)
  end
end

IO.puts("\n=== BrainCell vs Wiktextract source reconciliation ===")
IO.puts("Mode: READ ONLY; no database writes.")
IO.puts("Source: #{path}")
IO.puts("Source lines: #{if(limit == 0, do: "entire file", else: "first #{limit}")}")
IO.puts("Batch: #{batch}; compares per-sense IDs, old gloss and metadata.")
IO.puts("Full scan retains an index counter for each distinct source word/POS.")

stream = File.stream!(path, [], :line)
stream = if limit > 0, do: Stream.take(stream, limit), else: stream

state =
  stream
  |> Stream.with_index(1)
  |> Enum.reduce(Symbrella.SourceRowReconciliation.new(sample_limit),
    fn {line, idx}, state ->
      state = %{state | lines: idx}

      state =
        case Jason.decode(line) do
          {:ok, %{} = entry} ->
            Symbrella.SourceRowReconciliation.consume(state, entry, batch)

          _ ->
            %{state | invalid_json: state.invalid_json + 1}
        end

      if progress > 0 and rem(idx, progress) == 0 do
        IO.puts(:stderr,
          "Read #{idx} entries; compared #{state.checked_candidates} " <>
            "source candidates against brain_cells"
        )
      end

      state
    end)
  |> Symbrella.SourceRowReconciliation.flush()

IO.puts("\n=== Reconciliation totals ===")

IO.inspect(%{
  lines_scanned: state.lines,
  invalid_json_lines: state.invalid_json,
  source_senses_indexed: state.source_senses,
  senses_with_source_gloss_or_category_evidence: state.candidates,
  existing_rows_checked: state.checked_candidates -
    Map.get(state.verdict_counts, :absent_id, 0),
  source_candidate_ids_absent_from_db: Map.get(state.verdict_counts, :absent_id, 0),
  matched_current: Map.get(state.verdict_counts, :matched_current, 0),
  matched_but_missing_source_evidence:
    Map.get(state.verdict_counts, :missing_source_evidence, 0),
  among_matched_missing_extended_definitions: state.missing_extended_definitions,
  among_matched_missing_categories: state.missing_categories,
  among_matched_missing_usage_atoms: state.missing_usage,
  among_matched_missing_both: state.missing_both,
  different_stored_definition: Map.get(state.verdict_counts, :different_definition, 0),
  missing_definition_on_both_sides: Map.get(state.verdict_counts, :no_gloss, 0),
  usually_plural_grammar_derivation_drift:
    Map.get(state.verdict_counts, :grammar_derivation_drift, 0),
  metadata_mismatch: Map.get(state.verdict_counts, :metadata_mismatch, 0),
  wrong_word_or_pos: Map.get(state.verdict_counts, :wrong_word_pos, 0)
}, pretty: true, width: 120)

IO.puts("\n=== Examples requiring further review (bounded per verdict) ===")

state.examples
|> Enum.sort_by(fn {verdict, _} -> to_string(verdict) end)
|> Enum.each(fn {verdict, examples} ->
  IO.puts("#{verdict}:")
  Enum.each(Enum.reverse(examples),
    &IO.inspect(&1, pretty: true, limit: 20, width: 120))
end)

IO.puts("\nNo database rows were changed.")
IO.puts("Potentially recoverable matches are NOT approved for bulk repair.")
