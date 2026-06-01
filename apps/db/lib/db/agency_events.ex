defmodule Db.AgencyEvents do
  @moduledoc """
  Storage helpers for the self-agency ledger.

  This module records events only. Brain/Core own interpretation, reflection,
  and learning policy.
  """

  import Ecto.Query, only: [from: 2]

  alias Db.AgencyEvent

  @type create_result :: {:ok, AgencyEvent.t()} | {:error, Ecto.Changeset.t()}

  @doc """
  Persist one agency event.
  """
  @spec create_event(map()) :: create_result()
  def create_event(%{} = attrs) do
    attrs =
      attrs
      |> normalize_string(:session_id, "global")
      |> normalize_string(:actor, "symbrella")
      |> normalize_string(:source, "runtime")
      |> normalize_string(:status, "observed")
      |> normalize_string(:action, nil)
      |> Map.put_new(:agency_v, 1)

    %AgencyEvent{}
    |> AgencyEvent.changeset(attrs)
    |> Db.insert()
  end

  def create_event(_attrs) do
    changeset =
      %AgencyEvent{}
      |> AgencyEvent.changeset(%{})
      |> Ecto.Changeset.add_error(:input, "must be a map")

    {:error, changeset}
  end

  @doc """
  Fetch recent agency events for a session, newest first.
  """
  @spec recent(keyword()) :: [AgencyEvent.t()]
  def recent(opts \\ []) when is_list(opts) do
    session_id = opts |> Keyword.get(:session_id, "global") |> to_string()
    limit = opts |> Keyword.get(:limit, 20) |> normalize_limit()

    query =
      from(e in AgencyEvent,
        where: e.session_id == ^session_id,
        order_by: [desc: e.inserted_at, desc: e.id],
        limit: ^limit
      )

    Db.all(query)
  end

  defp normalize_string(attrs, key, default) do
    value = Map.get(attrs, key, Map.get(attrs, to_string(key), default))

    cond do
      is_binary(value) and String.trim(value) != "" -> Map.put(attrs, key, String.trim(value))
      is_nil(value) -> attrs
      is_atom(value) -> Map.put(attrs, key, Atom.to_string(value))
      is_nil(default) -> attrs
      true -> Map.put(attrs, key, default)
    end
  end

  defp normalize_limit(limit) when is_integer(limit) and limit > 0, do: min(limit, 100)
  defp normalize_limit(_), do: 20
end
