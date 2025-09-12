defmodule Symbrella.PostgrexTypes do
  Postgrex.Types.define(
    __MODULE__,
    [Pgvector.Extensions.Vector] ++ Ecto.Adapters.Postgres.extensions(),
    []
  )
end

