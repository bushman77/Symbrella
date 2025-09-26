defmodule Db do
  @moduledoc "Umbrella-wide Repo. One DB, one config source."
  use Ecto.Repo, otp_app: :db, adapter: Ecto.Adapters.Postgres, priv: "priv/repo"
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

    # Update SI fields WITHOUT struct expansion
    db_cells =
      (si.db_cells || [])
      |> Kernel.++(rows)
      |> Enum.uniq_by(& &1.id)

    db_hits = MapSet.new(for r <- rows, do: r.norm)

    activation_summary =
      Map.update(si.activation_summary || %{}, :db_hits, db_hits, fn existing ->
        MapSet.union(existing, db_hits)
      end)

    si
    |> Map.put(:db_cells, db_cells)
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
    # If you already have Db.exists?/1 that takes a word, delegate to it:
    exists?(word)
    # If your exists?/1 expects a query instead, use whatever helper you wrote
    # to check by :word or :norm (this keeps the test intent intact).
  end
end
def word_exists?(_), do: false


  defp norm(nil), do: ""
  defp norm(s) when is_binary(s), do: s |> String.trim() |> String.downcase(:default)
end

