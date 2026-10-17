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
  Narrow queue for internal symbolic investigation. Higher-priority
  unrelated uncertainties cannot starve missing two-hop links.
  """
  @spec pending_reasoning_links(pos_integer()) :: [CognitiveIssue.t()]
  def pending_reasoning_links(limit \\ 12)

  def pending_reasoning_links(limit) when is_integer(limit) and limit > 0 do
    Db.all(
      from(i in CognitiveIssue,
        where: i.kind == "missing_reasoning_link" and
               i.next_action == "find_linking_premise" and
               i.status in ["open", "investigating"],
        order_by: [desc: i.priority, asc: i.inserted_at, asc: i.id],
        limit: ^min(limit, 20))
    )
  end

  def pending_reasoning_links(_), do: []

  @doc """
  A missing-year gap closes only after a NEW persisted source-supported
  symbolic conclusion exists for that exact subject pair and reference year.
  Contradictions are never resolved by selecting one competing proof.
  """
  @spec resolve_missing_year(Db.ConsolidatedConclusion.t()) :: :ok
  def resolve_missing_year(%Db.ConsolidatedConclusion{} = proof) do
    persisted = Db.get_by(Db.ConsolidatedConclusion,
      id: proof.id, fingerprint: proof.fingerprint)

    if persisted != nil and persisted.provenance == "source_supported_symbolic_derivation" do
      close_missing_year_issues(persisted)
    end

    :ok
  end

  def resolve_missing_year(_), do: :ok

  @doc """
  Mark previously missing two-hop links resolved only after the verified
  composition path has identified TWO distinct persisted parent proofs.
  The logic that validates and composes their contents remains in Core.
  """
  @spec resolve_reasoning_link(String.t(), integer(), [integer()]) :: :ok
  def resolve_reasoning_link(subject_key, year, ids)
      when is_binary(subject_key) and is_integer(year) and is_list(ids) and
             length(ids) == 2 do
    unique_ids = Enum.uniq(ids)

    if length(unique_ids) == 2 do
      verified_parent_count =
        Db.aggregate(
          from(c in Db.ConsolidatedConclusion,
            where: c.id in ^unique_ids and
                   c.reference_year == ^year and
                   c.kind == "population_comparison" and
                   c.provenance == "source_supported_symbolic_derivation"),
          :count
        )

      if verified_parent_count == 2 do
        issues = Db.all(
          from(i in CognitiveIssue,
            where: i.subject_key == ^subject_key and
                   i.kind == "missing_reasoning_link" and
                   i.status in ["open", "investigating"] and
                   (is_nil(i.reference_year) or i.reference_year == ^year))
        )

        Enum.each(issues, fn issue ->
          issue
          |> CognitiveIssue.changeset(%{
            status: "resolved",
            resolution: %{
              reasoning_rule: "transitive_population_order",
              parent_proof_ids: Enum.sort(unique_ids),
              reference_year: year
            },
            resolved_at: DateTime.utc_now()
          })
          |> Db.update!()
        end)
      end
    end

    :ok
  end

  def resolve_reasoning_link(_, _, _), do: :ok

  @doc """
  Close a stale missing_reasoning_link when Core has independently revalidated
  a *direct* stored population comparison. This is not a two-hop deduction.

  The caller must first pass Consolidation.recall/1 and the authoritative
  answer planner. This DB boundary additionally checks durable identity,
  exact pair/year, and the absence of competing direct proofs for that year.
  An opaque hippocampal fingerprint alone never authorizes closure.
  """
  @spec resolve_direct_reasoning_link(integer(), integer(), String.t(), integer()) ::
          :ok | {:error, :direct_proof_not_unique_or_issue_mismatch}
  def resolve_direct_reasoning_link(issue_id, proof_id, fingerprint, year)
      when is_integer(issue_id) and issue_id > 0 and
             is_integer(proof_id) and proof_id > 0 and
             is_binary(fingerprint) and byte_size(fingerprint) == 64 and
             is_integer(year) do
    issue = Db.get(CognitiveIssue, issue_id)

    if match?(%CognitiveIssue{kind: "missing_reasoning_link",
                               next_action: "find_linking_premise"}, issue) and
         issue.status in ["open", "investigating"] and
         issue.reference_year == year do
      proof =
        Db.get_by(Db.ConsolidatedConclusion,
          id: proof_id,
          fingerprint: fingerprint,
          pair_key: issue.subject_key,
          reference_year: year,
          kind: "population_comparison",
          provenance: "source_supported_symbolic_derivation"
        )

      proof_count =
        Db.aggregate(
          from(c in Db.ConsolidatedConclusion,
            where: c.pair_key == ^issue.subject_key and
                   c.reference_year == ^year and
                   c.kind == "population_comparison"),
          :count
        )

      if proof != nil and proof_count == 1 do
        issue
        |> CognitiveIssue.changeset(%{
          status: "resolved",
          resolution: %{
            reasoning_rule: "direct_population_comparison",
            origin: "stored_symbolic_premises",
            proof_id: proof.id,
            proof_fingerprint: proof.fingerprint,
            reference_year: year
          },
          resolved_at: DateTime.utc_now()
        })
        |> Db.update!()

        :ok
      else
        {:error, :direct_proof_not_unique_or_issue_mismatch}
      end
    else
      {:error, :direct_proof_not_unique_or_issue_mismatch}
    end
  end

  def resolve_direct_reasoning_link(_, _, _, _),
    do: {:error, :direct_proof_not_unique_or_issue_mismatch}

  defp close_missing_year_issues(proof) do
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

end
