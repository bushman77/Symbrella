defmodule Core do
  alias Core.{Lexicon, SemanticInput, Token}
  alias Core.Brain, as: CBrain

  def resolve_input(phrase) do
    phrase
    |> Token.tokenize()
    |> CBrain.stm()
    |> Db.ltm()
    |> Lexicon.all()
    |> activate_cells()
  end

# Read a cell’s status (id is a string)
  def cell_status(id), do: GenServer.call(CBrain, {:cell, id, :status})

  # Send any cast message to a cell (e.g., :activate, :stop)
  def cell_cast(id, msg), do: GenServer.cast(CBrain, {:cell, id, msg})

@doc "Public facade: activate a batch of cells (rows or ids)."
  def activate_cells(si, opts \\ [delta: 1]) do
    payload = Map.new(opts)
    GenServer.cast(Brain, {:activate_cells, si, payload})
si
  end
end
