defmodule Core.Curiosity.InvestigationHistory do
  @moduledoc """
  Sanitized, best-effort durable audit for Curiosity issue attempts.

  Evidence remains in ConsolidatedConclusions, uncertainty in CognitiveIssues,
  and this operational ledger cannot authorize or change either one.
  A missing migration or temporary audit DB failure must not alter reasoning.
  """
  alias Db.CuriosityInvestigations

  @allowed_status [:resolved, :unresolved, :skipped, :failed, :disabled]
  @allowed_acquisition [:captured, :not_captured]
  @allowed_providers ["wikidata", "statistics_canada", "wikipedia", "other"]
  @allowed_provider_status ["source_supported", "unavailable", "partial_support", "error", "unknown"]

  @doc "Store a durable start marker for an actual issue attempt."
  @spec begin_attempt(pos_integer(), keyword()) :: pos_integer() | nil
  def begin_attempt(id, opts) when is_integer(id) and id > 0 do
    worker = if Keyword.get(opts, :name) == nil or
                Keyword.get(opts, :name) == Core.Curiosity.Investigator,
             do: "default", else: "isolated"

    case CuriosityInvestigations.begin_attempt(id, worker) do
      {:ok, row} -> row.id
      _ -> nil
    end
  rescue
    _ -> nil
  catch
    :exit, _ -> nil
  end

  def begin_attempt(_, _), do: nil

  @doc "Finish the durable attempt without persisting user text or provider payloads."
  @spec complete_attempt(pos_integer() | nil, map(), non_neg_integer()) :: :ok
  def complete_attempt(id, result, duration_ms)
      when is_integer(id) and id > 0 and is_map(result) do
    attrs = %{
      status: allowed_status(Map.get(result, :status)),
      reason: reason(Map.get(result, :reason)),
      acquisition: acquisition(Map.get(result, :acquisition)),
      acquisition_reason: reason(Map.get(result, :acquisition_reason)),
      acquired_proof_id: valid_id(Map.get(result, :acquired_proof_id)),
      parent_proof_ids: valid_ids(Map.get(result, :parent_proof_ids)),
      origin: reason(Map.get(result, :origin)),
      provider_attempts: providers(Map.get(result, :provider_attempts)),
      duration_ms: if(is_integer(duration_ms), do: max(duration_ms, 0), else: 0),
      completed_at: DateTime.utc_now()
    }

    _ = CuriosityInvestigations.finish_attempt(id, attrs)
    :ok
  rescue
    _ -> :ok
  catch
    :exit, _ -> :ok
  end

  def complete_attempt(_, _, _), do: :ok

  @doc "Bounded persisted audit; accepted source URLs remain in referenced proofs."
  def for_issue(id, limit \\ 20), do: CuriosityInvestigations.for_issue(id, limit)

  @doc "Bounded recent durable attempts, newest first."
  def recent(limit \\ 20), do: CuriosityInvestigations.recent(limit)

  defp allowed_status(status) when status in @allowed_status,
    do: Atom.to_string(status)
  defp allowed_status(_), do: "failed"

  defp acquisition(acq) when acq in @allowed_acquisition, do: Atom.to_string(acq)
  defp acquisition(_), do: "not_attempted"

  defp reason(value) when is_atom(value) and not is_nil(value) do
    text = Atom.to_string(value)
    if String.match?(text, ~r/\A[a-z][a-z0-9_]*\z/) and byte_size(text) <= 100,
      do: text, else: nil
  end
  defp reason(_), do: nil

  defp valid_id(value) when is_integer(value) and value > 0, do: value
  defp valid_id(_), do: nil

  defp valid_ids(ids) when is_list(ids) do
    ids |> Enum.filter(&(is_integer(&1) and &1 > 0)) |> Enum.uniq() |> Enum.take(2)
  end
  defp valid_ids(_), do: []

  defp providers(attempts) when is_list(attempts) do
    attempts
    |> Enum.take(4)
    |> Enum.flat_map(fn
      %{provider: provider, status: status}
        when provider in @allowed_providers and status in @allowed_provider_status ->
        [%{"provider" => provider, "status" => status}]
      _ -> []
    end)
  end
  defp providers(_), do: []
end
