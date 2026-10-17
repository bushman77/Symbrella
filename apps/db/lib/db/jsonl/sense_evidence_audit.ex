defmodule Db.JSONL.SenseEvidenceAudit do
  @moduledoc """
  Read-only, bounded-memory audit of gloss and category evidence in a
  Wiktextract/Kaikki JSONL stream.

  This measures *source-side opportunities* to recover information. It does
  not assert that the corresponding existing BrainCell rows are missing data.

  Duplicate first-gloss groups are calculated for contiguous runs of the same
  normalized word, as in the current grouped JSONL importer. These are source
  gloss comparisons, NOT exact database-definition duplicate counts: tags and
  source-version differences can change the stored definition.
  """

  alias Db.JSONL.Util

  @type summary :: map()

  def new(opts \\ []) do
    %{
      entries: 0,
      source_senses: 0,
      senses_with_multiple_glosses: 0,
      senses_with_source_categories: 0,
      senses_with_both: 0,
      source_category_values: 0,
      shared_first_gloss_groups: 0,
      shared_first_gloss_senses: 0,
      groups_distinguished_by_full_gloss: 0,
      senses_in_distinguishable_groups: 0,
      groups_not_distinguished_by_full_gloss: 0,
      invalid_or_non_english_entries: 0,
      word_runs: 0,
      current_norm: nil,
      current_senses: [],
      sample_limit: max(0, Keyword.get(opts, :sample_limit, 8)),
      examples: []
    }
  end

  @doc "Account for one decoded source entry. Does not access the DB."
  def ingest(state, %{} = entry) do
    word = entry["word"]
    pos = entry["pos"]

    if is_binary(word) and String.trim(word) != "" and
         is_binary(pos) and
         (entry["lang_code"] == "en" or entry["lang"] == "English") do
      norm = Util.normalize_word(word)
      state = maybe_advance_word(state, norm)

      senses =
        case entry["senses"] do
          values when is_list(values) -> values
          _ -> []
        end

      Enum.reduce(senses, %{state | entries: state.entries + 1}, fn sense, acc ->
        if is_map(sense) do
          glosses = preferred_glosses(sense)
          categories = source_category_names(sense)
          multiple? = length(glosses) > 1
          has_categories? = categories != []

          evidence =
            case glosses do
              [first | _] ->
                [%{
                  pos: pos,
                  first: first,
                  full: Enum.join(glosses, " — ")
                } | acc.current_senses]

              [] ->
                acc.current_senses
            end

          %{
            acc |
            source_senses: acc.source_senses + 1,
            senses_with_multiple_glosses:
              acc.senses_with_multiple_glosses + if(multiple?, do: 1, else: 0),
            senses_with_source_categories:
              acc.senses_with_source_categories + if(has_categories?, do: 1, else: 0),
            senses_with_both:
              acc.senses_with_both + if(multiple? and has_categories?, do: 1, else: 0),
            source_category_values: acc.source_category_values + length(categories),
            current_senses: evidence
          }
        else
          acc
        end
      end)
    else
      %{state | invalid_or_non_english_entries: state.invalid_or_non_english_entries + 1}
    end
  end

  def ingest(state, _bad_entry),
    do: %{state | invalid_or_non_english_entries: state.invalid_or_non_english_entries + 1}

  @doc "Flush the final contiguous word group and return the summary."
  def finish(state) do
    state
    |> flush_word()
    |> Map.drop([:current_norm, :current_senses, :sample_limit])
  end

  defp preferred_glosses(sense) do
    # Match Db.JSONL.Populate: cleaned glosses first, raw glosses only as fallback.
    case nonempty_glosses(sense["glosses"]) do
      [] -> nonempty_glosses(sense["raw_glosses"])
      values -> values
    end
  end

  defp nonempty_glosses(values) do
    values
    |> List.wrap()
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp source_category_names(sense) do
    sense
    |> Map.get("categories")
    |> List.wrap()
    |> Enum.flat_map(fn
      %{"source" => "w", "name" => name} when is_binary(name) ->
        normalized = Util.normalize_atom(name)
        if normalized == "", do: [], else: [normalized]

      _ ->
        []
    end)
    |> Enum.uniq()
  end

  defp maybe_advance_word(%{current_norm: nil} = state, norm),
    do: %{state | current_norm: norm, word_runs: state.word_runs + 1}

  defp maybe_advance_word(%{current_norm: current} = state, norm) when current == norm,
    do: state

  defp maybe_advance_word(state, norm) do
    state
    |> flush_word()
    |> Map.put(:current_norm, norm)
    |> Map.update!(:word_runs, &(&1 + 1))
  end

  defp flush_word(%{current_norm: nil} = state), do: state

  defp flush_word(state) do
    groups =
      state.current_senses
      |> Enum.group_by(&{&1.pos, &1.first})
      |> Enum.filter(fn {_key, senses} -> length(senses) > 1 end)

    Enum.reduce(groups, %{state | current_senses: []}, fn {{pos, first}, senses}, acc ->
      different_full_glosses = senses |> Enum.map(& &1.full) |> Enum.uniq()
      distinguished? = length(different_full_glosses) > 1

      example = %{
        word: state.current_norm,
        source_pos: pos,
        shared_first_gloss: first,
        senses_sharing_first_gloss: length(senses),
        distinct_full_glosses: length(different_full_glosses),
        distinguished_by_remaining_glosses?: distinguished?,
        full_gloss_samples: Enum.take(different_full_glosses, 3)
      }

      samples =
        if length(acc.examples) < acc.sample_limit and distinguished? do
          [example | acc.examples]
        else
          acc.examples
        end

      %{
        acc |
        shared_first_gloss_groups: acc.shared_first_gloss_groups + 1,
        shared_first_gloss_senses: acc.shared_first_gloss_senses + length(senses),
        groups_distinguished_by_full_gloss:
          acc.groups_distinguished_by_full_gloss + if(distinguished?, do: 1, else: 0),
        senses_in_distinguishable_groups:
          acc.senses_in_distinguishable_groups + if(distinguished?, do: length(senses), else: 0),
        groups_not_distinguished_by_full_gloss:
          acc.groups_not_distinguished_by_full_gloss + if(distinguished?, do: 0, else: 1),
        examples: samples
      }
    end)
  end
end
