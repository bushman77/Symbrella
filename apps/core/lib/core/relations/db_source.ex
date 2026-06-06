defmodule Core.Relations.DbSource do
  @moduledoc """
  DB-backed boundary source for lexical relation rows.

  `Core.Relations` owns evidence shaping. This module owns Ecto queries and
  returns plain maps so downstream relation logic does not depend on schemas.
  """

  alias Db
  import Ecto.Query, only: [from: 2]

  @soft_activation 0.5

  @spec load_relation_rows([String.t()]) :: [map()]
  def load_relation_rows([]), do: []

  def load_relation_rows(norms) when is_list(norms) do
    Db.all(
      from(c in Db.BrainCell,
        where: c.norm in ^norms and c.status == "active",
        select: %{
          id: c.id,
          norm: c.norm,
          pos: c.pos,
          synonyms: c.synonyms,
          antonyms: c.antonyms
        }
      )
    )
  rescue
    _ -> []
  end

  @spec fetch_related_cells([String.t()]) :: [map()]
  def fetch_related_cells([]), do: []

  def fetch_related_cells(norms) when is_list(norms) do
    Db.all(
      from(c in Db.BrainCell,
        where: c.norm in ^norms and c.status == "active",
        select: %{
          id: c.id,
          norm: c.norm,
          pos: c.pos,
          word: c.word,
          definition: c.definition,
          example: c.example,
          synonyms: c.synonyms,
          antonyms: c.antonyms
        }
      )
    )
    |> Enum.map(&soften_related_cell/1)
  rescue
    _ -> []
  end

  defp soften_related_cell(%{} = row) do
    %{
      id: Map.get(row, :id),
      source: :relations,
      norm: Map.get(row, :norm),
      pos: Map.get(row, :pos),
      lemma: Map.get(row, :norm),
      word: Map.get(row, :word),
      definition: Map.get(row, :definition),
      example: Map.get(row, :example),
      synonyms: List.wrap(Map.get(row, :synonyms)),
      antonyms: List.wrap(Map.get(row, :antonyms)),
      score: @soft_activation,
      activation: @soft_activation,
      modulated_activation: @soft_activation
    }
  end
end
