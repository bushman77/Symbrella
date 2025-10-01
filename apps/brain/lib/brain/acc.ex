defmodule Brain.ACC do
  @moduledoc ~S"""
  Anterior Cingulate Cortex (conflict monitor). Detects ambiguity/competition.
  - Input:  `{:conflict_check, si, ctx}`
  - Output: `{:ok, si} | {:reanalyse, hints}` (stub: always `{:ok, si}`)
  Also a good place for LIFG char-gram tripwire/telemetry.
  """
  use GenServer

  @type si :: map()
  @type ctx :: map()

  # --- Public API ---
  def start_link(opts \\ []),
    do: GenServer.start_link(__MODULE__, %{}, Keyword.merge([name: __MODULE__], opts))

  def conflict_check(si, ctx \\ %{}),
    do: GenServer.call(__MODULE__, {:conflict_check, si, ctx})

  # --- GenServer callbacks ---
  @impl true
  def init(state), do: {:ok, state}

  @impl true
  def handle_call({:conflict_check, si, _ctx}, _from, state) do
    # TODO: emit telemetry if char-grams leak into LIFG path
    {:reply, {:ok, si}, state}
  end
end

