defmodule Db do
  @moduledoc "Umbrella-wide Repo. One DB, one config source."
  use Ecto.Repo, otp_app: :db, adapter: Ecto.Adapters.Postgres, priv: "priv/repo"
  import Ecto.Query, only: [from: 2]
  alias Db.BrainCell

  @doc """
  Long-term memory lookup: batch by normalized phrases, return rows as `si.cells`,
  and mark `si.debug.db_hits`. No Brain messaging here.
  """
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
rows
|> Enum.reduce([], fn cell, acc -> [cell.id] ++acc end)
|> IO.inspect
    

    %{si | active_cells:
      rows
      |> Enum.reduce([], fn cell, acc ->
        acc ++ [cell.id]
      end)
    }
  end

  defp norm(nil), do: ""
  defp norm(s) when is_binary(s) do
    s |> String.trim() |> String.downcase(:default)
  end
end

