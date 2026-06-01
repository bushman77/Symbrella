defmodule Db.BrainCellCorpus do
  @moduledoc """
  Corpus audit/export helpers for training sense/context embedding models.

  BrainCell rows are raw lexical material. This module measures which rows are
  useful enough to become Axon contrastive-learning examples and can export
  compact sense cards for offline experiments.
  """

  import Ecto.Query

  alias Db.BrainCell

  @useful_pos ~w(noun verb adjective adverb interjection phrase proper_noun)

  @type audit :: %{
          totals: map(),
          readiness: map(),
          by_pos: [map()],
          ambiguous_norms: map(),
          samples: [map()]
        }

  @doc """
  Summarizes how much of `brain_cells` is useful for sense encoder training.

  Options:
    * `:sample_limit` - number of representative sense cards to return.
    * `:only_active` - when true, samples are active-only. Aggregate counts still
      include active/inactive breakdowns.
  """
  @spec audit(keyword()) :: audit()
  def audit(opts \\ []) do
    sample_limit = Keyword.get(opts, :sample_limit, 8)
    only_active? = Keyword.get(opts, :only_active, true)

    %{
      totals: totals(),
      readiness: readiness(),
      by_pos: by_pos(),
      ambiguous_norms: ambiguous_norms(),
      samples: sample_cards(sample_limit, only_active?)
    }
  end

  @doc """
  Returns compact sense cards suitable for JSONL export or training manifests.
  """
  @spec sense_cards(keyword()) :: [map()]
  def sense_cards(opts \\ []) do
    limit = Keyword.get(opts, :limit, 1_000)
    only_active? = Keyword.get(opts, :only_active, true)

    base =
      from(b in BrainCell,
        where: b.pos in ^@useful_pos,
        where:
          fragment(
            "coalesce(nullif(btrim(?), ''), nullif(btrim(?), '')) is not null",
            b.definition,
            b.example
          ),
        order_by: [
          desc: b.activation,
          asc: b.norm,
          asc: b.pos,
          asc: b.id
        ],
        limit: ^limit
      )

    query =
      if only_active? do
        from(b in base, where: b.status == "active")
      else
        base
      end

    query
    |> Db.all()
    |> Enum.map(&sense_card/1)
  end

  @doc """
  Exports sense cards as newline-delimited JSON.
  """
  @spec export_jsonl(Path.t(), keyword()) :: {:ok, non_neg_integer()} | {:error, term()}
  def export_jsonl(path, opts \\ []) when is_binary(path) do
    cards = sense_cards(opts)

    body =
      cards
      |> Enum.map(&Jason.encode!/1)
      |> Enum.join("\n")

    body = if body == "", do: "", else: body <> "\n"

    case File.write(path, body) do
      :ok -> {:ok, length(cards)}
      {:error, reason} -> {:error, reason}
    end
  end

  defp totals do
    Db.one(
      from(b in BrainCell,
        select: %{
          rows: count(b.id),
          active_rows: fragment("count(*) FILTER (WHERE ? = 'active')", b.status),
          inactive_rows:
            fragment("count(*) FILTER (WHERE ? <> 'active' OR ? IS NULL)", b.status, b.status),
          with_definition:
            fragment("count(*) FILTER (WHERE nullif(btrim(?), '') IS NOT NULL)", b.definition),
          with_example:
            fragment("count(*) FILTER (WHERE nullif(btrim(?), '') IS NOT NULL)", b.example),
          with_synonyms:
            fragment("count(*) FILTER (WHERE coalesce(array_length(?, 1), 0) > 0)", b.synonyms),
          with_antonyms:
            fragment("count(*) FILTER (WHERE coalesce(array_length(?, 1), 0) > 0)", b.antonyms),
          with_embedding: fragment("count(*) FILTER (WHERE ? IS NOT NULL)", b.embedding)
        }
      )
    )
  end

  defp readiness do
    Db.one(
      from(b in BrainCell,
        select: %{
          active_useful_pos:
            fragment(
              "count(*) FILTER (WHERE ? = 'active' AND ? = ANY(?))",
              b.status,
              b.pos,
              ^@useful_pos
            ),
          active_with_definition:
            fragment(
              "count(*) FILTER (WHERE ? = 'active' AND nullif(btrim(?), '') IS NOT NULL)",
              b.status,
              b.definition
            ),
          active_with_example:
            fragment(
              "count(*) FILTER (WHERE ? = 'active' AND nullif(btrim(?), '') IS NOT NULL)",
              b.status,
              b.example
            ),
          definition_to_sense:
            fragment(
              "count(*) FILTER (WHERE ? = 'active' AND ? = ANY(?) AND nullif(btrim(?), '') IS NOT NULL)",
              b.status,
              b.pos,
              ^@useful_pos,
              b.definition
            ),
          example_to_sense:
            fragment(
              "count(*) FILTER (WHERE ? = 'active' AND ? = ANY(?) AND nullif(btrim(?), '') IS NOT NULL)",
              b.status,
              b.pos,
              ^@useful_pos,
              b.example
            ),
          strong_sense_cards:
            fragment(
              "count(*) FILTER (WHERE ? = 'active' AND ? = ANY(?) AND nullif(btrim(?), '') IS NOT NULL AND nullif(btrim(?), '') IS NOT NULL)",
              b.status,
              b.pos,
              ^@useful_pos,
              b.definition,
              b.example
            ),
          synonym_pairs:
            fragment(
              "coalesce(sum(coalesce(array_length(?, 1), 0)) FILTER (WHERE ? = 'active' AND ? = ANY(?)), 0)",
              b.synonyms,
              b.status,
              b.pos,
              ^@useful_pos
            )
        }
      )
    )
  end

  defp by_pos do
    Db.all(
      from(b in BrainCell,
        group_by: b.pos,
        order_by: [desc: count(b.id)],
        select: %{
          pos: b.pos,
          rows: count(b.id),
          active_rows: fragment("count(*) FILTER (WHERE ? = 'active')", b.status),
          strong_sense_cards:
            fragment(
              "count(*) FILTER (WHERE ? = 'active' AND nullif(btrim(?), '') IS NOT NULL AND nullif(btrim(?), '') IS NOT NULL)",
              b.status,
              b.definition,
              b.example
            )
        }
      )
    )
  end

  defp ambiguous_norms do
    query =
      from(b in BrainCell,
        where: b.status == "active",
        group_by: b.norm,
        having: count(b.id) > 1,
        select: %{
          norm: type(b.norm, :string),
          sense_count: count(b.id),
          pos_count: fragment("count(DISTINCT ?)", b.pos)
        }
      )

    rows = Db.all(query)

    %{
      norms: length(rows),
      senses: Enum.reduce(rows, 0, fn row, acc -> acc + row.sense_count end),
      multi_pos_norms: Enum.count(rows, &(&1.pos_count > 1)),
      examples: Enum.take(Enum.sort_by(rows, & &1.sense_count, :desc), 12)
    }
  end

  defp sample_cards(limit, only_active?) when is_integer(limit) and limit > 0 do
    sense_cards(limit: limit, only_active: only_active?)
  end

  defp sample_cards(_limit, _only_active?), do: []

  defp sense_card(%BrainCell{} = cell) do
    %{
      id: cell.id,
      norm: to_string(cell.norm || ""),
      word: to_string(cell.word || ""),
      pos: cell.pos,
      status: cell.status,
      activation: cell.activation,
      definition: compact_text(cell.definition),
      example: compact_text(cell.example),
      synonyms: compact_list(cell.synonyms),
      antonyms: compact_list(cell.antonyms),
      text: sense_text(cell)
    }
  end

  defp sense_text(%BrainCell{} = cell) do
    [
      "word: #{cell.word || cell.norm}",
      "part of speech: #{cell.pos}",
      maybe_line("meaning", cell.definition),
      maybe_line("example", cell.example),
      maybe_list_line("similar words", cell.synonyms),
      maybe_list_line("opposites", cell.antonyms)
    ]
    |> Enum.reject(&(&1 == nil or &1 == ""))
    |> Enum.join("\n")
  end

  defp maybe_line(_label, nil), do: nil
  defp maybe_line(label, value), do: "#{label}: #{compact_text(value)}"

  defp maybe_list_line(_label, values) when values in [nil, []], do: nil

  defp maybe_list_line(label, values) when is_list(values) do
    values = compact_list(values)
    if values == [], do: nil, else: "#{label}: #{Enum.join(values, ", ")}"
  end

  defp compact_text(nil), do: ""

  defp compact_text(value) do
    value
    |> to_string()
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
  end

  defp compact_list(values) when is_list(values) do
    values
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&compact_text/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp compact_list(_), do: []
end
