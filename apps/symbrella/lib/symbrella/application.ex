defmodule Symbrella.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {DNSCluster, query: Application.get_env(:symbrella, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: Symbrella.PubSub},
      {Task.Supervisor, name: Symbrella.TaskSup},

      # --- Brain infra (start before Brain) ---
      {Registry, keys: :unique, name: Brain.Registry},
      {DynamicSupervisor, name: Brain.CellSup, strategy: :one_for_one},

      # --- Storage ---
      Db,                        # Repo

      # --- Manager (the “Brain” GenServer) ---
      Brain                      # must register itself as `name: Brain`
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Symbrella.Supervisor)
  end
end

