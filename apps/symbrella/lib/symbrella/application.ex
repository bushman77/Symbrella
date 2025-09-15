# apps/symbrella/lib/symbrella/application.ex
defmodule Symbrella.Application do
  use Application

  @impl true
  def start(_type, _args) do
    neg_path = Application.app_dir(:core, "priv/negcache/negcache.dets")
    File.mkdir_p!(Path.dirname(neg_path)) # ensure DETS dir exists

    children = [
      {DNSCluster, query: Application.get_env(:symbrella, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: Symbrella.PubSub},
      {Task.Supervisor, name: Symbrella.TaskSup},

      # DB first
      Db,

      # Brain infra that cells expect:
{Registry, keys: :unique, name: Brain.Registry},
{DynamicSupervisor, name: Brain.CellSup, strategy: :one_for_one},

      # Caches / services
      {Core.NegCache, dets_path: neg_path, ttl: 30 * 24 * 60 * 60},

      # HTTP client pool for Lexicon (if you switch Tesla to Finch)
      {Finch, name: Lexicon.Finch},

      # Your servers
      Lexicon,
      Brain
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Symbrella.Supervisor)
  end
end

