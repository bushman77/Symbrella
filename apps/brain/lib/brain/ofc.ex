defmodule Brain.OFC do
  @moduledoc ~S"""
  Orbitofrontal / vmPFC (valuation). Computes utility/cost for options.

  API:
    - value(options, ctx) :: list of {option, score}
  Stub: scores all options equally at 0.0.
  """
  use GenServer

  @type option :: any()
  @type ctx :: map()

  # --- Public API ---
  def start_link(opts \\ []),
    do: GenServer.start_link(__MODULE__, %{}, Keyword.merge([name: __MODULE__], opts))

  def value(options, ctx \\ %{}) when is_list(options),
    do: GenServer.call(__MODULE__, {:value, options, ctx})

  # --- GenServer callbacks ---
  @impl true
  def init(state), do: {:ok, state}

  @impl true
  def handle_call({:value, options, _ctx}, _from, state) do
    scores = Enum.map(options, &{&1, 0.0})
    {:reply, scores, state}
  end
end

