defmodule Brain.Region do
  @moduledoc """
  Mixin for brain-region GenServers.
  Each region:
    • registers via `Brain.Registry` under a short atom (e.g. :atl)
    • holds a plain map state (empty by default)
    • supports `{:stimulus, si}` casts and `:snapshot` calls
  """

  defmacro __using__(opts) do
    region = Keyword.fetch!(opts, :region)

    quote bind_quoted: [region: region] do
      use GenServer

      @region region

      # ----- public API -----
      @doc "Start the region server registered via Brain.Registry."
      def start_link(init_arg \\ %{}) do
        name = {:via, Registry, {Brain.Registry, @region}}
        GenServer.start_link(__MODULE__, init_arg, name: name)
      end

      @doc "Whereis helper (nil if not running)."
      def whereis(), do: Registry.whereis_name({Brain.Registry, @region})

      @doc "Get current region state."
      def snapshot(), do: GenServer.call(via(), :snapshot)

      @doc "Send a stimulus (e.g., SI after LIFG)."
      def stimulate(si), do: GenServer.cast(via(), {:stimulus, si})

      # ----- GenServer -----
      @impl true
      def init(_arg), do: {:ok, %{}}

      @impl true
      def handle_cast({:stimulus, si}, state) do
        # For now: keep last SI; future: compute + store derived features.
        {:noreply, Map.put(state, :last_si, si)}
      end

      @impl true
      def handle_call(:snapshot, _from, state), do: {:reply, state, state}

      @impl true
      def child_spec(arg) do
        %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [arg]},
          restart: :permanent,
          shutdown: 5_000,
          type: :worker
        }
      end

      # ----- helpers -----
      defp via(), do: {:via, Registry, {Brain.Registry, @region}}
    end
  end
end

