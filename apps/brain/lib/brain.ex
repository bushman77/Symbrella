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
    # tiny inline normalize
    text =
      raw_text
      |> to_string()
      |> String.trim_trailing()
      |> String.replace(~r/(?:\r\n|\r|\n){3,}/, "\n\n")

    # 🔹 single, strict call into Core (no wrappers / no shape juggling)
    {input, reply} =
      try do
        Core.resolve_input(text, opts)
      rescue
        e ->
          Logger.warning("Core.resolve_input/2 error: " <> Exception.format(:error, e, __STACKTRACE__))
          {%{tokens: []}, %{text: text, intent: :echo, confidence: 1.0, placeholder: true}}
      end

    # inline token text extraction
    token_texts =
      case input do
        %{tokens: tokens} when is_list(tokens) ->
          tokens
          |> Enum.map(fn
            %{text: t} when is_binary(t) -> t
            t when is_binary(t) -> t
            _ -> nil
          end)
          |> Enum.reject(&is_nil/1)

        _ -> []
      end

    # inline reply text fetch
    output_text = Map.get(reply, :text) || Map.get(reply, "text") || "(no text)"

    # inline log_turn
    turn_id = state.turn_seq + 1

    turn = %{
      id: turn_id,
      at: System.system_time(:millisecond),
      session_id: opts[:session_id],
      source: opts[:source] || :ui,
      input: text,
      output: output_text,
      token_texts: token_texts
    }

    history = [turn | state.history] |> Enum.take(@history_max)

    token_counts =
      Enum.reduce(token_texts, state.token_counts, fn tok, acc ->
        Map.update(acc, tok, 1, &(&1 + 1))
      end)

    new_state =
      state
      |> Map.put(:turn_seq, turn_id)
      |> Map.put(:history, history)
      |> Map.put(:token_counts, token_counts)

    {:reply, reply, new_state}
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

