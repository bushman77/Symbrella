defmodule Brain.AG do
  @moduledoc ~S"""
  Angular Gyrus / pMTG (composition). Binds roles & composes phrase/frames.
  - Input:  `{:compose, si, ctx}`                        - Output: `si` with role frames, collapsed spans (stub: passthrough)
  """
  use GenServer
                                                         @type si :: map()
  @type ctx :: map()

  def start_link(opts \\ []), do: GenServer.start_link(__MODULE__, %{}, Keyword.merge([name: __MODULE__], opts))
  def compose(si, ctx \\ %{}), do: GenServer.call(__MODULE__, {:compose, si, ctx})

  @impl true
  def init(state), do: {:ok, state}

  @impl true
  def handle_call({:compose, si, _ctx}, _from, state) do
    {:reply, si, state}
  end
end

