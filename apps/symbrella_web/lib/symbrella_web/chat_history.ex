defmodule SymbrellaWeb.ChatHistory do
  @moduledoc """
  In-memory bounded chat history for the LiveView chat window.

  Keeps up to 100 displayed messages per browser conversation across LiveView
  remounts while Phoenix runs, with a global cap on remembered browsers. The session key is supplied by the signed
  browser cookie. This deliberately does not persist private chat text to disk.

  Legacy zero/one-argument APIs remain for internal callers and older tests;
  browser-facing callers must pass an explicit session identifier.
  """

  use GenServer

  @name __MODULE__
  @default_limit 100
  @default_max_sessions 128

  @type message :: %{
          required(:id) => String.t(),
          required(:role) => atom(),
          required(:text) => String.t(),
          optional(:tone) => atom(),
          optional(:meta) => map(),
          optional(:mods) => map(),
          optional(:session_id) => term(),
          optional(:si) => map(),
          optional(:explain_text) => String.t(),
          optional(:explain_payload) => map()
        }

  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: @name)
  end

  @doc "Legacy in-process session key. Browser callers must use their cookie key."
  @spec session_id() :: String.t()
  def session_id, do: GenServer.call(@name, :session_id)

  @spec list(String.t() | nil) :: [message()]
  def list(session_id \\ nil) do
    GenServer.call(@name, {:list, session_id})
  end

  @spec append(message() | [message()]) :: :ok
  def append(messages), do: append(nil, messages)

  @spec append(String.t() | nil, message() | [message()]) :: :ok
  def append(session_id, messages) when is_list(messages) do
    GenServer.cast(@name, {:append, session_id, messages})
  end

  def append(session_id, %{} = message), do: append(session_id, [message])

  @spec clear() :: :ok
  def clear, do: GenServer.call(@name, :clear)

  @spec clear(String.t()) :: :ok
  def clear(session_id) when is_binary(session_id) do
    GenServer.call(@name, {:clear, session_id})
  end

  @impl true
  def init(opts) do
    limit = opts |> Keyword.get(:limit, @default_limit) |> normalize_limit()
    max_sessions = opts |> Keyword.get(:max_sessions, @default_max_sessions) |> normalize_limit()
    {:ok,
     %{
       limit: limit,
       max_sessions: max_sessions,
       histories: %{},
       order: [],
       session_id: new_session_id()
     }}
  end

  @impl true
  def handle_call(:session_id, _from, state), do: {:reply, state.session_id, state}

  def handle_call({:list, session_id}, _from, state) do
    id = session_id || state.session_id
    {:reply, state.histories |> Map.get(id, []) |> Enum.reverse(), state}
  end

  def handle_call(:clear, _from, state) do
    {:reply, :ok, %{state | histories: %{}, order: [], session_id: new_session_id()}}
  end

  def handle_call({:clear, session_id}, _from, state) do
    {:reply, :ok,
     %{
       state
       | histories: Map.delete(state.histories, session_id),
         order: List.delete(state.order, session_id)
     }}
  end

  @impl true
  def handle_cast({:append, session_id, messages}, state) when is_list(messages) do
    id = session_id || state.session_id

    normalized =
      messages
      |> Enum.map(&normalize_message/1)
      |> Enum.reject(&is_nil/1)

    previous = Map.get(state.histories, id, [])

    updated =
      normalized
      |> Enum.reduce(previous, fn message, acc -> [message | acc] end)
      |> Enum.take(state.limit)

    order = List.delete(state.order, id) ++ [id]
    histories = Map.put(state.histories, id, updated)

    {order, histories} =
      if length(order) > state.max_sessions do
        [expired | remaining] = order
        {remaining, Map.delete(histories, expired)}
      else
        {order, histories}
      end

    {:noreply, %{state | histories: histories, order: order}}
  end

  defp normalize_message(%{} = message) do
    id = message[:id] || message["id"]
    role = normalize_role(message[:role] || message["role"])
    text = message[:text] || message["text"]

    cond do
      not is_binary(id) or id == "" -> nil
      is_nil(role) -> nil
      not is_binary(text) -> nil
      true -> normalize_optional(%{id: id, role: role, text: text}, message)
    end
  end

  defp normalize_message(_), do: nil

  defp normalize_optional(out, message) do
    out
    |> maybe_put(:tone, message[:tone] || message["tone"])
    |> maybe_put(:meta, message[:meta] || message["meta"])
    |> maybe_put(:mods, message[:mods] || message["mods"])
    |> maybe_put(:session_id, message[:session_id] || message["session_id"])
    |> maybe_put(:si, message[:si] || message["si"])
    |> maybe_put(:explain_text, message[:explain_text] || message["explain_text"])
    |> maybe_put(:explain_payload, message[:explain_payload] || message["explain_payload"])
  end

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, :meta, value) when not is_map(value), do: map
  defp maybe_put(map, :mods, value) when not is_map(value), do: map
  defp maybe_put(map, :si, value) when not is_map(value), do: map
  defp maybe_put(map, :explain_payload, value) when not is_map(value), do: map
  defp maybe_put(map, :explain_text, value) when not is_binary(value), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp normalize_role(role) when role in [:user, :assistant, :system], do: role
  defp normalize_role("user"), do: :user
  defp normalize_role("assistant"), do: :assistant
  defp normalize_role("system"), do: :system
  defp normalize_role(_), do: nil

  defp new_session_id do
    "s-" <> Base.encode16(:crypto.strong_rand_bytes(16), case: :lower)
  end

  defp normalize_limit(limit) when is_integer(limit) and limit > 0, do: limit
  defp normalize_limit(_), do: @default_limit
end
