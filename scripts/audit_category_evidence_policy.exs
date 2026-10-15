# READ-ONLY: inspect the exact category evidence policy used by Db.JSONL.Populate.
#
#   mix run scripts/audit_category_evidence_policy.exs
#   mix run scripts/audit_category_evidence_policy.exs --limit-lines 100000 --top 20
#   mix run scripts/audit_category_evidence_policy.exs --limit-lines 0
#
# Counts category VALUES and distinct source SENSES, separately.
# No database inserts, updates or deletes; no implicit semantic promotion.

defmodule Symbrella.CategoryEvidencePolicyAudit do
  @moduledoc false

  alias Db.JSONL.CategoryEvidencePolicy

  def run(args) do
    args =
      case args do
        ["--" | rest] -> rest
        rest -> rest
      end

    {opts, rest, invalid} =
      OptionParser.parse(args, strict: [limit_lines: :integer, top: :integer, progress: :integer])

    if rest != [] or invalid != [] do
      raise ArgumentError, "Usage: mix run scripts/audit_category_evidence_policy.exs " <>
        "[--limit-lines N] [--top N] [--progress N]"
    end

    limit = Keyword.get(opts, :limit_lines, 20_000)
    top = Keyword.get(opts, :top, 25)
    progress = Keyword.get(opts, :progress, 20_000)

    if limit < 0 or top < 0 or top > 100 or progress < 0 do
      raise ArgumentError, "Require limit_lines >= 0, top in 0..100, progress >= 0"
    end

    path = Db.JSONL.IO.default_path()
    unless File.regular?(path), do: raise("Source unavailable: #{path}")

    IO.puts("\n=== Source category evidence policy audit ===")
    IO.puts("Mode: READ ONLY; policy version: #{CategoryEvidencePolicy.version()}")
    IO.puts("Source: #{path}")
    IO.puts("Source lines: #{if(limit == 0, do: "all", else: limit)}")
    IO.puts("Separate counts for category values and distinct source senses.")

    stream = File.stream!(path, [], :line)
    stream = if limit == 0, do: stream, else: Stream.take(stream, limit)

    initial = %{
      lines: 0,
      parse_errors: 0,
      entries: 0,
      senses: 0,
      senses_with_promoted_atoms: 0,
      senses_with_semantic_atoms: 0,
      senses_with_usage_only: 0,
      raw_category_values: 0,
      emitted_values: 0,
      decision_value_counts: %{},
      decision_sense_counts: %{},
      unreviewed_names: %{},
      emitted_names: %{}
    }

    totals =
      stream
      |> Stream.with_index(1)
      |> Enum.reduce(initial, fn {line, i}, state ->
        state =
          case Jason.decode(line) do
            {:ok, %{} = entry} -> ingest_entry(entry, state)
            _ -> %{state | parse_errors: state.parse_errors + 1}
          end

        if progress > 0 and rem(i, progress) == 0 do
          IO.puts(:stderr,
            "Scanned #{i} source lines; #{state.senses} senses; " <>
              "#{state.emitted_values} retained category values")
        end

        %{state | lines: i}
      end)

    IO.puts("\n=== Proposed retained evidence (source-side, not DB changes) ===")

    IO.inspect(%{
      lines_scanned: totals.lines,
      malformed_json: totals.parse_errors,
      entries_with_senses: totals.entries,
      source_senses: totals.senses,
      source_senses_with_any_retained_atoms: totals.senses_with_promoted_atoms,
      source_senses_with_semantic_atoms: totals.senses_with_semantic_atoms,
      source_senses_with_only_usage_atoms: totals.senses_with_usage_only,
      raw_category_values: totals.raw_category_values,
      retained_category_values: totals.emitted_values
    }, pretty: true)

    IO.puts("\nDecision totals by CATEGORY VALUE:")
    totals.decision_value_counts
    |> Enum.sort_by(fn {_kind, count} -> -count end)
    |> Enum.each(&IO.inspect/1)

    IO.puts("\nDecision totals by SOURCE SENSE (classes overlap):")
    totals.decision_sense_counts
    |> Enum.sort_by(fn {_kind, count} -> -count end)
    |> Enum.each(&IO.inspect/1)

    print_top("\nMost frequent UNREVIEWED category names:", totals.unreviewed_names, top)
    print_top("\nMost frequent RETAINED evidence names:", totals.emitted_names, top)

    IO.puts("\nNo database records changed. No bulk backfill is authorized by these counts.")
  end

  defp ingest_entry(%{"senses" => senses}, state) when is_list(senses) do
    Enum.reduce(senses, %{state | entries: state.entries + 1}, fn sense, acc ->
      if is_map(sense), do: ingest_sense(sense, acc), else: acc
    end)
  end

  defp ingest_entry(_, state), do: state

  defp ingest_sense(sense, state) do
    categories =
      case sense["categories"] do
        values when is_list(values) -> values
        _ -> []
      end

    decisions =
      Enum.map(categories, fn category ->
        {CategoryEvidencePolicy.decide(category), category_name(category)}
      end)

    state =
      Enum.reduce(decisions, state, fn {decision, name}, acc ->
        key =
          case decision do
            {:emit, class, _atom} -> {:emit, class}
            {:skip, reason} -> {:skip, reason}
          end

        acc = %{
          acc |
          raw_category_values: acc.raw_category_values + 1,
          emitted_values:
            acc.emitted_values + if(elem(decision, 0) == :emit, do: 1, else: 0),
          decision_value_counts:
            Map.update(acc.decision_value_counts, key, 1, &(&1 + 1))
        }

        cond do
          match?({:skip, :unreviewed_other}, decision) ->
            %{acc | unreviewed_names: Map.update(acc.unreviewed_names, name, 1, &(&1 + 1))}

          elem(decision, 0) == :emit ->
            %{acc | emitted_names: Map.update(acc.emitted_names, name, 1, &(&1 + 1))}

          true ->
            acc
        end
      end)

    distinct_decisions =
      decisions
      |> Enum.map(fn
        {{:emit, class, _atom}, _name} -> {:emit, class}
        {{:skip, reason}, _name} -> {:skip, reason}
      end)
      |> Enum.uniq()

    decision_sense_counts =
      Enum.reduce(distinct_decisions, state.decision_sense_counts, fn decision, map ->
        Map.update(map, decision, 1, &(&1 + 1))
      end)

    has_semantics? =
      Enum.any?(decisions, fn
        {{:emit, kind, _}, _} -> kind in [:semantic_domain, :place, :topic, :name_class]
        _ -> false
      end)

    has_usage? =
      Enum.any?(decisions, fn
        {{:emit, kind, _}, _} -> kind in [:regional_usage, :grammatical_usage]
        _ -> false
      end)

    %{
      state |
      senses: state.senses + 1,
      decision_sense_counts: decision_sense_counts,
      senses_with_promoted_atoms:
        state.senses_with_promoted_atoms + if(has_semantics? or has_usage?, do: 1, else: 0),
      senses_with_semantic_atoms:
        state.senses_with_semantic_atoms + if(has_semantics?, do: 1, else: 0),
      senses_with_usage_only:
        state.senses_with_usage_only + if(has_usage? and not has_semantics?, do: 1, else: 0)
    }
  end

  defp category_name(%{"name" => name}) when is_binary(name), do: name
  defp category_name(name) when is_binary(name), do: name
  defp category_name(_), do: "(missing)"

  defp print_top(heading, counts, top) do
    IO.puts(heading)

    counts
    |> Enum.sort_by(fn {name, count} -> {-count, name} end)
    |> Enum.take(top)
    |> Enum.with_index(1)
    |> Enum.each(fn {{name, count}, i} ->
      IO.puts("#{i}. #{name}: #{count}")
    end)
  end
end

Symbrella.CategoryEvidencePolicyAudit.run(System.argv())
