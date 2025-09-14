# apps/symbrella/lib/symbrella/application.ex

defmodule Symbrella.Application do
  use Application

  @impl true
  def start(_type, _args) do
    neg_path = Application.app_dir(:core, "priv/negcache/negcache.dets")

    children = [
      {DNSCluster, query: Application.get_env(:symbrella, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: Symbrella.PubSub},
      {Task.Supervisor, name: Symbrella.TaskSup},
      Db,
      # 30 days in **seconds** (the NegCache expects seconds, not ms)
      {Core.NegCache, dets_path: neg_path, ttl: 30 * 24 * 60 * 60},
      {Brain, []},
      {Finch, name: Lexicon.Finch},
      Lexicon
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Symbrella.Supervisor)
  end
end

