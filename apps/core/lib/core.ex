defmodule Core do
  alias Core.{Brain, Lexicon, SemanticInput, Token}

  def resolve_input(phrase) do
    phrase
    |> Token.tokenize()
    |> Brain.stm()
    |> Db.ltm()
    |> Lexicon.all()
  end

# Read a cell’s status (id is a string)
  def cell_status(id), do: GenServer.call(Brain, {:cell, id, :status})

  # Send any cast message to a cell (e.g., :activate, :stop)
  def cell_cast(id, msg), do: GenServer.cast(Brain, {:cell, id, msg})

@doc "Public facade: activate a batch of cells (rows or ids)."
  def activate_cells(rows_or_ids, opts \\ [delta: 1]) do
    payload = Map.new(opts)
    GenServer.cast(Brain, {:activate_cells, rows_or_ids, payload})
    :ok
  end
end
