defmodule Brain.MTL do
  @moduledoc """
  Medial Temporal Lobe / Hippocampal complex (episodic binding).

  Responsibilities
  ----------------
  • Bind the current semantic input (SI) into persistent `episodes`.
  • Provide read API for hybrid recall (token prefilter + vector ANN + rerank).
  """

  use GenServer
  require Logger
  # Keep Region mixin if you rely on its helpers/macros.
  # IMPORTANT: It likely defines start_link/1 with defaults, so our start_link/1 must NOT.
  use Brain.Region, region: :mtl

  alias Db.Episodes

  # -- Public API --------------------------------------------------------------

  @doc "Start the MTL server. Options: :name (default Brain.MTL), :async_embedding (bool)."
  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc "Bind an SI map into an episode. Options forwarded to Db.Episodes.write_episode/2."
  def bind(si, opts \\ []) when is_map(si) do
    GenServer.cast(server(opts), {:bind, si, opts})
  end

  @doc "Recall episodes for cues and optional query (embedding list or query string)."
  def recall(cues, query \\ nil, opts \\ []) when is_list(cues) do
    GenServer.call(server(opts), {:recall, cues, query, opts})
  end

  @doc "Health check. Returns :pong."
  def ping(opts \\ []), do: GenServer.call(server(opts), :ping)

  # -- GenServer callbacks -----------------------------------------------------

  @impl GenServer
  def init(opts) do
    state = %{
      async_embedding: Keyword.get(opts, :async_embedding, false)
    }

    Logger.metadata(region: :mtl)
    Logger.debug("Brain.MTL init: #{inspect(state)}")
    {:ok, state}
  end

  @impl GenServer
  def handle_cast({:bind, si, opts}, state) do
    # Merge default policy (e.g., async embedding toggle) with per-call opts
    opts = Keyword.merge([async_embedding: state.async_embedding], opts)

    case Episodes.write_episode(si, opts) do
      {:ok, _ep} -> :ok
      {:error, reason} -> Logger.warning("MTL bind failed: #{inspect(reason)}")
    end

    {:noreply, state}
  end

  @impl GenServer
  def handle_call({:recall, cues, query, opts}, _from, state) do
    reply = Episodes.recall_hybrid(cues, query, opts)
    {:reply, reply, state}
  end

  @impl GenServer
  def handle_call(:ping, _from, state), do: {:reply, :pong, state}

  # -- Helpers -----------------------------------------------------------------

  defp server(opts), do: Keyword.get(opts, :name, __MODULE__)
end

