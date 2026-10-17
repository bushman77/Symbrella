# READ-ONLY, sampled batch preflight for extending truncated Wiktextract glosses.
#
#   mix run scripts/audit_definition_batch_candidates.exs
#   mix run scripts/audit_definition_batch_candidates.exs --limit-lines 100000
#   mix run scripts/audit_definition_batch_candidates.exs --limit-lines 0
#
# Default: first 20,000 lines. The final word group is SKIPPED if the scan
# was line-limited, since it might be incomplete. Full scan tracks positional
# counters for every word/POS (may consume substantial RAM on mobile).
#
# NO writes. Results are PROVISIONAL because a normalized word can occur
# in multiple noncontiguous source runs (cat|noun is a real example).
# Run scripts/review_definition_word.exs or the full-source shortlist
# checker to verify ALL occurrences before treating an ID mismatch as real.
# For a chosen word/POS use the human-reviewed DefinitionOnlyBackfill.plan/4
# workflow, back up records and approve its individual fingerprint.

defmodule Symbrella.DefinitionBatchCandidateScript do
  @moduledoc false
  import Ecto.Query
  alias Db.JSONL.{DefinitionBatchAudit, Util}

  def run(argv) do
    argv =
      case argv do
        ["--" | rest] -> rest
        rest -> rest
      end

    {opts, remaining, invalid} =
      OptionParser.parse(argv,
        strict: [
          limit_lines: :integer,
          word_batch: :integer,
          samples: :integer,
          progress: :integer
        ]
      )

    if remaining != [] or invalid != [] do
      raise ArgumentError,
        "Usage: mix run scripts/audit_definition_batch_candidates.exs " <>
          "[--limit-lines N] [--word-batch N] [--samples N] [--progress N]"
    end

    limit = Keyword.get(opts, :limit_lines, 20_000)
    word_batch = Keyword.get(opts, :word_batch, 40)
    samples = Keyword.get(opts, :samples, 8)
    progress = Keyword.get(opts, :progress, 10_000)

    if limit < 0 or word_batch < 1 or word_batch > 200 or
         samples < 0 or samples > 30 or progress < 0 do
      raise ArgumentError, "Require limit >= 0, word_batch 1..200, samples 0..30, progress >= 0"
    end

    path = Db.JSONL.IO.default_path()
    unless File.regular?(path), do: raise("Missing source JSONL: #{path}")

    IO.puts("\n=== Definition recovery: safe batch candidate preflight ===")
    IO.puts("Mode: READ ONLY. No database modifications.")
    IO.puts("Source: #{path}")
    IO.puts("Lines: #{if(limit == 0, do: "entire file", else: "first #{limit}")}")
    IO.puts("Source ordering follows Db.JSONL.Import positional sense counters.")
    IO.puts("A matching positional ID never independently proves sense identity.")
    IO.puts("WARNING: contiguous word RUNS are not necessarily complete words.")
    IO.puts("Any ID mismatch may reflect later source entries; rerun full-source review.")

    stream = File.stream!(path, [], :line)
    stream = if limit == 0, do: stream, else: Stream.take(stream, limit)

    initial = %{
      current_norm: nil,
      current_entries: [],
      counters: %{},
      pending: [],
      pending_count: 0,
      processed_word_runs: 0,
      candidate_pos_groups: 0,
      source_senses: 0,
      skipped_bad_json: 0,
      skipped_blank_word: 0,
      lines: 0,
      audit: DefinitionBatchAudit.new(samples)
    }

    state =
      stream
      |> Stream.with_index(1)
      |> Enum.reduce(initial, fn {line, idx}, state ->
        state = %{state | lines: idx}

        state =
          case Jason.decode(line) do
            {:ok, %{} = entry} -> consume(state, entry, word_batch)
            _ -> %{state | skipped_bad_json: state.skipped_bad_json + 1}
          end

        if progress > 0 and rem(idx, progress) == 0 do
          IO.puts(:stderr,
            "Scanned #{idx} lines; classified #{state.audit.reviewed_groups} " <>
              "word/POS candidate groups")
        end

        state
      end)

    unfinished_word =
      if limit == 0 do
        nil
      else
        state.current_norm
      end

    # Without proof of the next word boundary, NEVER plan a partial word.
    state =
      if limit == 0 do
        flush_current(state, word_batch)
      else
        %{state | current_norm: nil, current_entries: []}
      end

    state = flush_pending(state)

    IO.puts("\n=== PROVISIONAL word-run definition candidate census ===")

    IO.inspect(%{
      source_lines_read: state.lines,
      skipped_bad_json_lines: state.skipped_bad_json,
      skipped_blank_word_entries: state.skipped_blank_word,
      complete_word_runs: state.processed_word_runs,
      source_senses_in_completed_runs: state.source_senses,
      pos_groups_with_extended_source_gloss: state.candidate_pos_groups,
      eligible_pos_groups: state.audit.candidate_groups,
      definitions_to_extend: state.audit.definitions_to_extend,
      already_repaired_pos_groups: state.audit.already_current,
      blocked_pos_groups: state.audit.blocked_groups,
      blocked_by_reason: state.audit.blocked_by_reason,
      pos_groups_without_extended_gloss: state.audit.no_extended_gloss,
      skipped_incomplete_last_word_at_limit: unfinished_word
    }, pretty: true, width: 120)

    IO.puts("\n=== Highest-yield examples (review required before any write) ===")
    Enum.each(state.audit.candidate_examples, fn sample ->
      IO.inspect(sample, pretty: true, width: 120, limit: :infinity)
    end)

    IO.puts("\n=== Provisional conflicts (full-source review required) ===")
    state.audit.conflict_examples
    |> Enum.reverse()
    |> Enum.each(&IO.inspect(&1, pretty: true))

    IO.puts("\nNO DATABASE ROWS CHANGED.")
    IO.puts("This audit deliberately has no --apply option.")
    IO.puts("All eligible/blocked counts are provisional until full-source word checks.")
  end

  defp consume(state, entry, word_batch) do
    norm = Util.normalize_word(Map.get(entry, "word") || "")

    cond do
      norm == "" ->
        %{state | skipped_blank_word: state.skipped_blank_word + 1}

      state.current_norm == nil ->
        %{state | current_norm: norm, current_entries: [entry]}

      state.current_norm == norm ->
        %{state | current_entries: [entry | state.current_entries]}

      true ->
        state
        |> flush_current(word_batch)
        |> Map.put(:current_norm, norm)
        |> Map.put(:current_entries, [entry])
    end
  end

  defp flush_current(%{current_norm: nil} = state, _word_batch), do: state

  defp flush_current(state, word_batch) do
    {cells, counters} =
      state.current_entries
      |> Enum.reverse()
      |> DefinitionBatchAudit.source_cells(state.counters)

    audit =
      cells
      |> Enum.group_by(& &1.pos)
      |> Enum.reduce(state.audit, fn {_pos, pos_cells}, acc ->
        if extended?(pos_cells) do
          acc
        else
          %{acc | no_extended_gloss: acc.no_extended_gloss + 1}
        end
      end)

    candidates =
      cells
      |> Enum.group_by(& &1.pos)
      |> Enum.flat_map(fn {pos, pos_cells} ->
        if extended?(pos_cells) do
          [%{word: state.current_norm, pos: pos, source: pos_cells}]
        else
          []
        end
      end)

    updated = %{
      state |
      counters: counters,
      processed_word_runs: state.processed_word_runs + 1,
      source_senses: state.source_senses + length(cells),
      current_norm: nil,
      current_entries: [],
      audit: audit,
      candidate_pos_groups: state.candidate_pos_groups + length(candidates),
      pending: Enum.reverse(candidates, state.pending),
      pending_count: state.pending_count + length(candidates)
    }

    if updated.pending_count >= word_batch,
      do: flush_pending(updated),
      else: updated
  end

  defp extended?(cells) do
    Enum.any?(cells, fn cell ->
      is_binary(cell.definition) and String.contains?(cell.definition, " — ")
    end)
  end

  defp flush_pending(%{pending: []} = state), do: state

  defp flush_pending(state) do
    pending = Enum.reverse(state.pending)
    norms = pending |> Enum.map(& &1.word) |> Enum.uniq()

    stored =
      from(cell in Db.BrainCell,
        where: cell.norm in ^norms,
        select: %{
          id: cell.id,
          norm: cell.norm,
          pos: cell.pos,
          definition: cell.definition,
          example: cell.example,
          synonyms: cell.synonyms,
          antonyms: cell.antonyms,
          gram_function: cell.gram_function,
          semantic_atoms: cell.semantic_atoms
        }
      )
      |> Db.all()
      |> Enum.group_by(&{&1.norm, &1.pos})

    audit =
      Enum.reduce(pending, state.audit, fn candidate, acc ->
        key = {candidate.word, candidate.pos}
        DefinitionBatchAudit.inspect_group(
          acc, candidate.word, candidate.pos, candidate.source, Map.get(stored, key, [])
        )
      end)

    %{state | pending: [], pending_count: 0, audit: audit}
  end
end

Symbrella.DefinitionBatchCandidateScript.run(System.argv())
