defmodule Symbrella.Application do
  use Application

  @impl true
  def start(_type, _args) do
    # Ensure DETS dir exists for NegCache
    neg_path = Application.app_dir(:core, "priv/negcache/negcache.dets")
    File.mkdir_p!(Path.dirname(neg_path))

    children = [
      # DB first
      Db,

      # Shared infra
      {Registry, keys: :unique, name: Brain.Registry},
      {DynamicSupervisor, name: Brain.CellSup, strategy: :one_for_one},
      # Caches / services
      {Phoenix.PubSub, name: Symbrella.PubSub},
      {Task.Supervisor, name: Symbrella.TaskSup},
      {Finch, name: Lexicon.Finch},
      {Core.NegCache, dets_path: neg_path, ttl: 30 * 24 * 60 * 60},

      # Servers
      Lexicon,
      Brain,

      # Web (if you’re using Phoenix Endpoint here, add it)
      # SymbrellaWeb.Telemetry,
      # {Phoenix.PubSub, name: Symbrella.PubSub},
      # SymbrellaWeb.Endpoint
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Symbrella.Supervisor)
  end
end
