defmodule Db do
  @moduledoc """
  Umbrella-wide Repo. One DB, one config source.
  """

  use Ecto.Repo,
    otp_app: :db,
    adapter: Ecto.Adapters.Postgres,
    priv: "priv/repo"

  import Ecto.Query, only: [from: 2]
  alias Db.BrainCell

  def ltm(si) do
    %{si| 
      active_cells: 
      si.tokens
      |>Enum.reduce([], fn token, acc -> 
        query = from b in Db.BrainCell, where: b.word == ^token.phrase 
        acc++Db.all(query)  
      end)
    }
  end

end


