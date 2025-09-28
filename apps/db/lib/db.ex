defmodule Db do
  @moduledoc "Umbrella-wide Repo. One DB, one config source."

  use Ecto.Repo,
    otp_app: :db,
    adapter: Ecto.Adapters.Postgres,
    priv: "priv/repo"

  import Ecto.Query, only: [from: 2]
  alias Db.BrainCell

  @payload %{delta: 1.0, decay: 0.98}

  def ltm(%{tokens: toks} = si) do
    norms =
      toks
      |> Enum.map(&norm(&1.phrase))
      |> Enum.reject(&(&1 == ""))
      |> Enum.uniq()

    rows =
      case norms do
        [] -> []
        _  -> from(b in BrainCell, where: b.norm in ^norms) |> Db.all()
      end

    # 🔥 Activate with full rows (Brain already handles rows or ids)
    if rows != [] do
      GenServer.cast(Brain, {:activate_cells, rows, @payload})
    end

    # Always treat :active_cells as a LIST (coerce if someone stored a map earlier)
    existing = ensure_list(Map.get(si, :active_cells, []))
    new_rows = ensure_list(rows)

    active_cells =
      existing
      |> Kernel.++(new_rows)
      |> Enum.uniq_by(& &1.id)

    db_hits = MapSet.new(for r <- new_rows, do: r.norm)

    activation_summary =
      Map.update(si.activation_summary || %{}, :db_hits, db_hits, fn existing_hits ->
        MapSet.union(existing_hits, db_hits)
      end)

    si
    |> Map.put(:active_cells, active_cells)
    |> Map.put(:activation_summary, activation_summary)
  end

  @doc """
  Return `true` if a *word* exists; guards invalid inputs without hitting the DB.
  Accepts a binary; trims whitespace. Empty/invalid → false immediately.
  """
  @spec word_exists?(term) :: boolean()
  def word_exists?(term)
  def word_exists?(term) when is_binary(term) do
    word = String.trim(term)
    if word == "" do
      false
    else
      exists?(word)
    end
  end

  def word_exists?(_), do: false

  defp ensure_list(nil), do: []
  defp ensure_list(l) when is_list(l), do: l
  defp ensure_list(m) when is_map(m), do: Map.values(m)
  defp ensure_list(other), do: List.wrap(other)

  defp norm(nil), do: ""
  defp norm(s) when is_binary(s), do: s |> String.trim() |> String.downcase(:default)
end

