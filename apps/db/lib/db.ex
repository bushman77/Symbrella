defmodule Db do
  use Ecto.Repo, otp_app: :db, adapter: Ecto.Adapters.Postgres, priv: "priv/repo"
  import Ecto.Query, only: [from: 2]
  alias Db.BrainCell
  require Logger

  def ltm(%{tokens: toks} = si) do
    norms =
      toks
      |> Enum.map(&norm(&1.phrase))
      |> Enum.uniq()

    rows =
      from(b in BrainCell, where: b.norm in ^norms)
      |> Db.all()

    # 🔥 Kick off cell starts + activation (your Brain handles this)
    GenServer.cast(Brain, {:activate_cells, rows, %{delta: 0.12, decay: 0.98}})

    # Debug hits, and keep DB results for visibility (don’t clobber active_cells)
    db_hits = rows |> Enum.map(& &1.norm) |> MapSet.new()
    debug   = Map.update(si[:debug] || %{}, :db_hits, db_hits, &MapSet.union(&1, db_hits))

    %{si | cells: rows, debug: debug}
  end

  defp norm(s) when is_binary(s), do: s |> String.trim() |> String.downcase(:default)
  defp norm(_), do: ""
end

