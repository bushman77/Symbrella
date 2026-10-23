defmodule Llm.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    # Holds only temporary bearer credentials; no polling or social writes.
    children = [Llm.Colony.TokenCache]
    Supervisor.start_link(children, strategy: :one_for_one, name: Llm.Supervisor)
  end
end
