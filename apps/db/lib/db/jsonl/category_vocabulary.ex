defmodule Db.JSONL.CategoryVocabulary do
  @moduledoc """
  Streaming, read-only category vocabulary census.

  Counts each raw category value and also counts distinct source senses with
  at least one category in each provenance class. The per-sense counts may
  overlap; they must not be added to obtain a distinct sense count.

  This is inspection only: the :likely_maintenance classification is heuristic,
  and :other_review is deliberately not considered safe semantic evidence.
  """

  alias Db.JSONL.CategoryProvenance

  @spec new() :: map()
  def new do
    %{
      entries: 0,
      source_senses: 0,
      raw_category_values: 0,
      category_values_by_class: %{},
      senses_by_class: %{},
      senses_with_no_categories: 0,
      senses_with_only_disamb_or_maintenance: 0,
      category_name_counts: %{}
    }
  end

  @spec ingest(map(), map()) :: map()
  def ingest(acc, %{"senses" => senses}) when is_list(senses) do
    senses
    |> Enum.reduce(%{acc | entries: acc.entries + 1}, &ingest_sense/2)
  end

  def ingest(acc, _), do: acc

  @spec top_names(map(), atom(), non_neg_integer()) :: [{String.t(), non_neg_integer()}]
  def top_names(summary, class, limit) when is_integer(limit) and limit >= 0 do
    summary.category_name_counts
    |> Enum.flat_map(fn
      {{^class, name}, count} -> [{name, count}]
      _ -> []
    end)
    |> Enum.sort_by(fn {name, count} -> {-count, name} end)
    |> Enum.take(limit)
  end

  defp ingest_sense(sense, acc) when is_map(sense) do
    categories =
      case Map.get(sense, "categories") do
        values when is_list(values) -> values
        _ -> []
      end

    classes = categories |> Enum.map(&CategoryProvenance.classify/1) |> Enum.uniq()

    counts =
      Enum.reduce(classes, acc.senses_by_class, fn class, counts ->
        Map.update(counts, class, 1, &(&1 + 1))
      end)

    # Class labels are not exact semantic decisions: this simply identifies
    # senses for which no currently reviewed place/topic/other category exists.
    only_noise? =
      categories != [] and
        Enum.all?(classes, &(&1 in [:disambiguation_source, :likely_maintenance]))

    acc = %{
      acc |
      source_senses: acc.source_senses + 1,
      senses_by_class: counts,
      senses_with_no_categories:
        acc.senses_with_no_categories + if(categories == [], do: 1, else: 0),
      senses_with_only_disamb_or_maintenance:
        acc.senses_with_only_disamb_or_maintenance + if(only_noise?, do: 1, else: 0)
    }

    Enum.reduce(categories, acc, fn category, state ->
      class = CategoryProvenance.classify(category)
      name = category_name(category)

      %{
        state |
        raw_category_values: state.raw_category_values + 1,
        category_values_by_class:
          Map.update(state.category_values_by_class, class, 1, &(&1 + 1)),
        category_name_counts:
          Map.update(state.category_name_counts, {class, name}, 1, &(&1 + 1))
      }
    end)
  end

  defp ingest_sense(_, acc), do: acc

  defp category_name(%{"name" => name}) when is_binary(name),
    do: String.trim(name)

  defp category_name(name) when is_binary(name), do: String.trim(name)
  defp category_name(_), do: "(unknown)"
end
