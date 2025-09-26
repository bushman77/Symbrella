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

  defp norm(nil), do: ""
  defp norm(s) when is_binary(s), do: s |> String.trim() |> String.downcase(:default)
end

