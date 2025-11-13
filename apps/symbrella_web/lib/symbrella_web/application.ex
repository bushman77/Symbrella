defmodule SymbrellaWeb.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      SymbrellaWeb.Telemetry,
      SymbrellaWeb.Endpoint
    ]

    opts = [strategy: :one_for_one, name: SymbrellaWeb.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def config_change(changed, _new, removed) do
    SymbrellaWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
