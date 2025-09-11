defmodule Brain do
  @moduledoc """
  Central chat coordinator.

  • Public API: `chat/2` (blocking) and `snapshot/0`.
  • Internals: calls Core.resolve_input/2 directly (start of semantic pipeline),
    then updates in-memory history and token counts.
  """

  use GenServer
  require Logger

  @name __MODULE__
  @history_max 50
  @call_timeout 15_000

  # ───────────────────────── Start / Init ─────────────────────────

  def start_link(_args), do: GenServer.start_link(__MODULE__, :ok, name: @name)

  @impl true
  def init(:ok) do
    {:ok,
     %{
       attention: MapSet.new(),
       activation_log: [],
       active_cells: %{},
       llm_ctx: nil,
       llm_model: nil,
       llm_ctx_updated_at: nil,
       llm_max: 8192,
       turn_seq: 0,
       history: [],
       token_counts: %{}
     }}
  end

  # ───────────────────────── Public API ─────────────────────────

  @doc """
  Primary chat entrypoint (blocking). Returns a reply with at least `:text`.
  """
  @spec chat(binary, keyword) :: map | struct
  def chat(text, opts \\ []) when is_binary(text) do
    GenServer.call(@name, {:chat, text, opts}, opts[:timeout] || @call_timeout)
  end

  @doc "Peek at minimal state (for debugging)."
  def snapshot, do: GenServer.call(@name, :snapshot)

  # ───────────────────────── GenServer ─────────────────────────

  @impl true
  def handle_call({:chat, raw_text, opts}, _from, state) do
    raw_text
    |> Core.resolve_input 
    |> IO.inspect
    {:reply, "reply", state}
  end

  @impl true
  def handle_call(:snapshot, _from, state) do
    view =
      state
      |> Map.take([:turn_seq, :history, :token_counts])
      |> Map.update!(:history, &Enum.take(&1, 5))

    {:reply, view, state}
  end
end

