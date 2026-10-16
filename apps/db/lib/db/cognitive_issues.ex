defmodule Db.CognitiveIssues do
  @moduledoc """
  Durable issue ledger and a read-only prioritization surface for Curiosity.

  Opens are idempotent. No issue, regardless of priority or replay frequency,
  is evidence that its underlying proposition is true.
  """
  import Ecto.Query, only: [from: 2]

  alias Db.CognitiveIssue

  @spec open(map()) :: {:ok, CognitiveIssue.t()} | {:error, term()}
  def open(%{} = attrs) do
    changeset = CognitiveIssue.changeset(%CognitiveIssue{}, attrs)

    with true <- changeset.valid?,
         {:ok, _} <- Db.insert(changeset,
           on_conflict: :nothing, conflict_target: [:fingerprint]),
         %CognitiveIssue{} = row <- Db.one(
           from(i in CognitiveIssue,
             where: i.fingerprint == ^attrs.fingerprint, limit: 1)
         ) do
      {:ok, row}
    else
      false -> {:error, changeset}
      {:error, _} = error -> error
      _ -> {:error, :not_stored}
    end
  end

  def open(_), do: {:error, :invalid_issue}

  @doc "Bounded prioritization API; does not perform retrieval or mutate knowledge."
  @spec pending(pos_integer()) :: [CognitiveIssue.t()]
  def pending(limit \\ 12) when is_integer(limit) and limit > 0 do
    Db.all(
      from(i in CognitiveIssue,
        where: i.status in ["open", "investigating"],
        order_by: [desc: i.priority, asc: i.inserted_at],
        limit: ^min(limit, 50))
    )
  end

  @spec resolve(integer(), map()) :: {:ok, CognitiveIssue.t()} | {:error, term()}
  def resolve(id, %{verified_evidence: %{} = support})
      when is_integer(id) and id > 0 do
    case Db.get(CognitiveIssue, id) do
      %CognitiveIssue{} = issue ->
        issue
        |> CognitiveIssue.changeset(%{
          status: "resolved",
          resolution: %{verified_evidence: support},
          resolved_at: DateTime.utc_now()
        })
        |> Db.update()

      nil -> {:error, :not_found}
    end
  end

  def resolve(_, _), do: {:error, :verified_resolution_required}
end
