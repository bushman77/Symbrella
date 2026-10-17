defmodule Db.CuriosityInvestigations do
  @moduledoc """
  Isolated append-only audit interface. Changes to an in-flight attempt are
  limited to completing that row once. Query methods never authorize facts.
  """
  import Ecto.Query, only: [from: 2]
  alias Db.CuriosityInvestigation

  @spec begin_attempt(pos_integer(), String.t()) ::
          {:ok, CuriosityInvestigation.t()} | {:error, term()}
  def begin_attempt(issue_id, worker)
      when is_integer(issue_id) and issue_id > 0 and worker in ["default", "isolated"] do
    %CuriosityInvestigation{}
    |> CuriosityInvestigation.changeset(%{issue_id: issue_id, worker: worker})
    |> Db.insert()
  end

  def begin_attempt(_, _), do: {:error, :invalid_attempt}

  @doc "Complete a started event, refusing repeat writes."
  @spec finish_attempt(pos_integer(), map()) ::
          {:ok, CuriosityInvestigation.t()} | {:error, term()}
  def finish_attempt(id, attrs) when is_integer(id) and id > 0 and is_map(attrs) do
    case Db.get(CuriosityInvestigation, id) do
      %CuriosityInvestigation{status: "started"} = row ->
        row
        |> CuriosityInvestigation.changeset(
          Map.take(attrs, [:status, :reason, :acquisition,
            :acquisition_reason, :acquired_proof_id, :parent_proof_ids,
            :origin, :provider_attempts, :duration_ms, :completed_at]))
        |> Db.update()

      _ -> {:error, :not_started}
    end
  end

  def finish_attempt(_, _), do: {:error, :invalid_completion}

  @doc "Bounded newest-first records for a single issue."
  @spec for_issue(pos_integer(), pos_integer()) :: [CuriosityInvestigation.t()]
  def for_issue(issue_id, limit \\ 20)
  def for_issue(issue_id, limit)
      when is_integer(issue_id) and issue_id > 0 and
           is_integer(limit) and limit > 0 do
    Db.all(from(i in CuriosityInvestigation,
      where: i.issue_id == ^issue_id,
      order_by: [desc: i.id], limit: ^min(limit, 100)))
  end
  def for_issue(_, _), do: []

  @doc "Bounded newest-first audit history across all attempted issues."
  @spec recent(pos_integer()) :: [CuriosityInvestigation.t()]
  def recent(limit \\ 20)
  def recent(limit) when is_integer(limit) and limit > 0 do
    Db.all(from(i in CuriosityInvestigation,
      order_by: [desc: i.id], limit: ^min(limit, 100)))
  end
  def recent(_), do: []
end
