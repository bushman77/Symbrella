defmodule Db.ConsolidatedConclusions do
  @moduledoc """
  Ecto boundary for durable reasoning results.

  Every row is immutable. Distinct contradictory proofs are allowed to coexist
  but Core must refuse to reuse the ambiguous result; no last-write-wins.
  """
  import Ecto.Query, only: [from: 2]

  alias Db.ConsolidatedConclusion

  @spec store(map()) :: {:ok, ConsolidatedConclusion.t()} | {:error, term()}
  def store(%{} = attrs) do
    changeset = ConsolidatedConclusion.changeset(%ConsolidatedConclusion{}, attrs)

    with true <- changeset.valid?,
         {:ok, _} <-
           Db.insert(changeset, on_conflict: :nothing, conflict_target: [:fingerprint]),
         %ConsolidatedConclusion{} = row <-
           Db.one(from(c in ConsolidatedConclusion,
             where: c.fingerprint == ^attrs.fingerprint, limit: 1)) do
      {:ok, row}
    else
      false -> {:error, changeset}
      {:error, _} = error -> error
      _ -> {:error, :not_stored}
    end
  end

  def store(_), do: {:error, :invalid_conclusion}

  @spec recent(String.t(), pos_integer()) :: [ConsolidatedConclusion.t()]
  def recent(pair_key, limit \\ 30)

  def recent(pair_key, limit)
      when is_binary(pair_key) and is_integer(limit) and limit > 0 do
    Db.all(
      from(c in ConsolidatedConclusion,
        where: c.pair_key == ^pair_key and c.kind == "population_comparison",
        order_by: [desc: c.reference_year, desc: c.inserted_at, desc: c.id],
        limit: ^min(limit, 100))
    )
  end

  def recent(_, _), do: []
end
