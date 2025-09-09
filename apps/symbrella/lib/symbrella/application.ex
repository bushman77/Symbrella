defmodule Symbrella.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {DNSCluster, query: Application.get_env(:symbrella, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: Symbrella.PubSub}
      # Start a worker by calling: Symbrella.Worker.start_link(arg)
      # {Symbrella.Worker, arg}
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Symbrella.Supervisor)
  end
end
