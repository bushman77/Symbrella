defmodule Db.SelfSnapshots do
  @moduledoc """
  Storage helpers for Brain self-continuity snapshots.

  This module deliberately avoids restore policy. Callers should pass returned
  snapshot payloads into Brain.SelfContinuity for validation and warm-start.
  """

  import Ecto.Query, only: [from: 2]

  alias Db.SelfSnapshot

  @type create_result :: {:ok, SelfSnapshot.t()} | {:error, Ecto.Changeset.t()}

  @doc """
  Persist a self-model snapshot map.

  Options:
    * `:scope` - logical restore scope, default `"runtime"`
    * `:source` - source label, default `"runtime"`
    * `:self_model_v` - explicit self-model version override
    * `:snapshot_v` - explicit snapshot version override
  """
  @spec create_snapshot(map(), keyword()) :: create_result()
  def create_snapshot(snapshot, opts \\ [])

  def create_snapshot(%{} = snapshot, opts) when is_list(opts) do
    attrs = %{
      scope: opts |> Keyword.get(:scope, "runtime") |> to_string(),
      source: opts |> Keyword.get(:source, "runtime") |> to_string(),
      self_model_v: Keyword.get(opts, :self_model_v, snapshot_value(snapshot, :self_model_v, 1)),
      snapshot_v: Keyword.get(opts, :snapshot_v, snapshot_value(snapshot, :v, 1)),
      snapshot: snapshot
    }

    %SelfSnapshot{}
    |> SelfSnapshot.changeset(attrs)
    |> Db.insert()
  end

  def create_snapshot(_snapshot, _opts) do
    changeset =
      %SelfSnapshot{}
      |> SelfSnapshot.changeset(%{})
      |> Ecto.Changeset.add_error(:snapshot, "must be a map")

    {:error, changeset}
  end

  @doc """
  Fetch the latest snapshot for a scope.
  """
  @spec latest_snapshot(keyword()) :: {:ok, SelfSnapshot.t()} | {:error, :not_found}
  def latest_snapshot(opts \\ []) when is_list(opts) do
    scope = opts |> Keyword.get(:scope, "runtime") |> to_string()

    query =
      from(s in SelfSnapshot,
        where: s.scope == ^scope,
        order_by: [desc: s.inserted_at, desc: s.id],
        limit: 1
      )

    case Db.one(query) do
      %SelfSnapshot{} = snapshot -> {:ok, snapshot}
      nil -> {:error, :not_found}
    end
  end

  defp snapshot_value(snapshot, key, default) when is_atom(key) do
    Map.get(snapshot, key, Map.get(snapshot, Atom.to_string(key), default))
  end
end
