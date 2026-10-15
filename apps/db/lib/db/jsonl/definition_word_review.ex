defmodule Db.JSONL.DefinitionWordReview do
  @moduledoc """
  Read-only exact-word reconstruction for individual definition-only reviews.

  Walk the COMPLETE JSONL stream and collect ALL entries whose Wiktextract
  word normalizes to the requested norm, including non-contiguous/case
  variants. The importer's positional counters are then reconstructed from
  those entries in original source order.

  This is intentionally more conservative than fetching only one source
  capitalization or stopping at a line limit. Bad JSON aborts the scan: the
  source cannot be proven complete otherwise.
  """

  alias Db.JSONL.Util

  @type scan_stats :: %{
          total_lines: non_neg_integer(),
          source_entries: non_neg_integer(),
          source_senses: non_neg_integer(),
          normalized_spellings: map()
        }

  @spec collect(Enumerable.t(), String.t()) ::
          {:ok, list(map()), scan_stats()} | {:error, term()}
  def collect(lines, word) when is_binary(word) do
    norm = Util.normalize_word(word)

    if norm == "" do
      {:error, :blank_word}
    else
      result =
        lines
        |> Stream.with_index(1)
        |> Enum.reduce_while({[], 0, 0, 0, %{}}, fn {line, number},
                                                    {entries, _prev, found, senses, spellings} ->
          case Jason.decode(line) do
            {:ok, %{} = entry} ->
              raw_word = Map.get(entry, "word")
              if is_binary(raw_word) and Util.normalize_word(raw_word) == norm do
                count =
                  case Map.get(entry, "senses") do
                    list when is_list(list) -> length(list)
                    _ -> 0
                  end

                {:cont,
                  {[entry | entries], number, found + 1, senses + count,
                   Map.update(spellings, raw_word, 1, &(&1 + 1))}}
              else
                {:cont, {entries, number, found, senses, spellings}}
              end

            {:ok, _other} ->
              {:cont, {entries, number, found, senses, spellings}}

            {:error, error} ->
              {:halt, {:error, {:malformed_source_json, number, error}}}
          end
        end)

      case result do
        {:error, _} = error ->
          error

        {entries, total, found, senses, spellings} ->
          {:ok, Enum.reverse(entries), %{
            total_lines: total,
            source_entries: found,
            source_senses: senses,
            normalized_spellings: spellings
          }}
      end
    end
  end

  def collect(_, _), do: {:error, :invalid_word}
end
