defmodule SymbrellaWeb.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    app_children = [
      SymbrellaWeb.Telemetry,
      SymbrellaWeb.ChatHistory,
      SymbrellaWeb.Endpoint
    ]

    children = maybe_pubsub_child() ++ app_children

    opts = [strategy: :one_for_one, name: SymbrellaWeb.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def config_change(changed, _new, removed) do
    SymbrellaWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  defp maybe_pubsub_child do
    case Process.whereis(Symbrella.PubSub) do
      nil -> [{Phoenix.PubSub, name: Symbrella.PubSub}]
      _pid -> []
    end
  end
end
