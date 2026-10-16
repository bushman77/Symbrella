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

  @doc """
  Find source-backed comparison premises mentioning one subject.

  The JSONB array stores compact original claims; querying through
  `unnest/1` avoids a full table download. Return :too_many_candidates
  rather than silently ignoring conflicting rows beyond the limit.
  """
  @spec for_subject(String.t(), pos_integer()) ::
          {:ok, [ConsolidatedConclusion.t()]} | {:error, :too_many_candidates}
  def for_subject(subject, limit \\ 50)

  def for_subject(subject, limit)
      when is_binary(subject) and byte_size(subject) > 0 and
             is_integer(limit) and limit > 0 do
    bounded = min(limit, 100)

    rows =
      Db.all(
        from(c in ConsolidatedConclusion,
          where: c.kind == "population_comparison" and
            fragment(
              "EXISTS (SELECT 1 FROM unnest(?) AS p WHERE p->>'subject' = ?)",
              c.premises, ^subject
            ),
          order_by: [desc: c.reference_year, desc: c.id],
          limit: ^(bounded + 1))
      )

    if length(rows) > bounded,
      do: {:error, :too_many_candidates},
      else: {:ok, rows}
  end

  def for_subject(_, _), do: {:ok, []}
end
