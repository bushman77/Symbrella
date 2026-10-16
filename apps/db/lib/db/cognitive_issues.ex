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

  @doc """
  A missing-year gap closes only after a NEW persisted source-supported
  symbolic conclusion exists for that exact subject pair and reference year.
  Contradictions are never resolved by selecting one competing proof.
  """
  @spec resolve_missing_year(Db.ConsolidatedConclusion.t()) :: :ok
  def resolve_missing_year(%Db.ConsolidatedConclusion{} = proof) do
    issues =
      Db.all(
        from(i in CognitiveIssue,
          where: i.subject_key == ^proof.pair_key and
                 i.reference_year == ^proof.reference_year and
                 i.kind == "missing_dated_evidence" and
                 i.status == "open")
      )

    Enum.each(issues, fn issue ->
      issue
      |> CognitiveIssue.changeset(%{
        status: "resolved",
        resolution: %{proof_fingerprint: proof.fingerprint,
          provenance: proof.provenance},
        resolved_at: DateTime.utc_now()
      })
      |> Db.update!()
    end)

    :ok
  end

  def resolve_missing_year(_), do: :ok
end
