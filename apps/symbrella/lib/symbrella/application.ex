defmodule Symbrella.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    children = [
      # DB
      Db,

      # Registry for cells (must start before Brain/Cell)
      {Registry, keys: :unique, name: Brain.Registry},

      # Dynamic supervisor for cells
      {DynamicSupervisor, strategy: :one_for_one, name: Brain.CellSup},

      # Your Brain process
      Brain

      # Add/keep any other children you already run (Lexicon, etc.)
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Symbrella.Supervisor)
  end
end

