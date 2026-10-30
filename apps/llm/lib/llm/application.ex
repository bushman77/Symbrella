defmodule Llm.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    # TokenCache holds only temporary bearer tokens in memory. Bootstrap
    # safely provisions a missing permanent key only when explicitly opted in.
    # Neither worker publishes content or modifies Symbrella cognition.
    children = [Llm.Colony.TokenCache, Llm.Colony.Bootstrap]
    Supervisor.start_link(children, strategy: :one_for_one, name: Llm.Supervisor)
  end
end
