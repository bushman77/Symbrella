defmodule Db.JSONL.DefinitionShortlistReview do
  @moduledoc """
  Read-only complete-source collection for a bounded group of normalized words.

  A *single* pass through the entire JSONL source collects every matching
  entry, including case variants and noncontiguous word runs. A malformed
  JSON line aborts the scan rather than silently making an ID census partial.

  Each word's entries are returned in source order. All other source entries
  are discarded immediately to keep memory bounded by shortlisted words.
  No database access or mutations occur here.
  """

  alias Db.JSONL.Util

  @max_words 12

  @spec collect(Enumerable.t(), [String.t()]) ::
          {:ok, %{String.t() => [map()]}, map()} | {:error, term()}
  def collect(lines, words) when is_list(words) do
    normalized =
      words
      |> Enum.map(fn
        word when is_binary(word) -> Util.normalize_word(word)
        _ -> ""
      end)

    cond do
      normalized == [] ->
        {:error, :no_targets}

      length(normalized) > @max_words ->
        {:error, {:too_many_words, @max_words}}

      Enum.any?(normalized, &(&1 == "")) ->
        {:error, :invalid_target}

      length(normalized) != length(Enum.uniq(normalized)) ->
        {:error, :duplicate_normalized_words}

      true ->
        initial = Map.new(normalized, &{&1, []})

        result =
          lines
          |> Stream.with_index(1)
          |> Enum.reduce_while({initial, 0, 0, 0}, fn {line, index},
                                                     {entries, _prev, matched, senses} ->
            case Jason.decode(line) do
              {:ok, %{"word" => word} = entry} when is_binary(word) ->
                norm = Util.normalize_word(word)

                if Map.has_key?(entries, norm) do
                  count =
                    case entry["senses"] do
                      list when is_list(list) -> length(list)
                      _ -> 0
                    end

                  {:cont,
                   {Map.update!(entries, norm, fn existing -> [entry | existing] end), index,
                    matched + 1, senses + count}}
                else
                  {:cont, {entries, index, matched, senses}}
                end

              {:ok, _} ->
                {:cont, {entries, index, matched, senses}}

              {:error, reason} ->
                {:halt, {:error, {:malformed_source_json, index, reason}}}
            end
          end)

        case result do
          {:error, _} = error ->
            error

          {entries, lines_scanned, matched, senses} ->
            {:ok, Map.new(entries, fn {word, rev} -> {word, Enum.reverse(rev)} end),
             %{
               lines_scanned: lines_scanned,
               selected_normalized_words: normalized,
               matching_source_entries: matched,
               matching_source_senses: senses
             }}
        end
    end
  end

  def collect(_, _), do: {:error, :invalid_targets}
end
