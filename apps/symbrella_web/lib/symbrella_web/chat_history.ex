defmodule SymbrellaWeb.ChatHistory do
  @moduledoc """
  In-memory bounded chat history for the LiveView chat window.

  This keeps the last 100 displayed messages across LiveView remounts while the
  Phoenix application is running. It intentionally does not persist private chat
  text to disk.
  """

  use GenServer

  @name __MODULE__
  @default_limit 100

  @type message :: %{
          required(:id) => String.t(),
          required(:role) => atom(),
          required(:text) => String.t(),
          optional(:tone) => atom(),
          optional(:meta) => map(),
          optional(:mods) => map(),
          optional(:explain_text) => String.t(),
          optional(:explain_payload) => map()
        }

  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: @name)
  end

  @spec list() :: [message()]
  def list do
    GenServer.call(@name, :list)
  end

  @spec append(message() | [message()]) :: :ok
  def append(messages) when is_list(messages) do
    GenServer.cast(@name, {:append, messages})
  end

  def append(%{} = message) do
    append([message])
  end

  @spec clear() :: :ok
  def clear do
    GenServer.call(@name, :clear)
  end

  @impl true
  def init(opts) do
    limit = opts |> Keyword.get(:limit, @default_limit) |> normalize_limit()
    {:ok, %{limit: limit, messages: []}}
  end

  @impl true
  def handle_call(:list, _from, state) do
    {:reply, Enum.reverse(state.messages), state}
  end

  def handle_call(:clear, _from, state) do
    {:reply, :ok, %{state | messages: []}}
  end

  @impl true
  def handle_cast({:append, messages}, state) when is_list(messages) do
    normalized =
      messages
      |> Enum.map(&normalize_message/1)
      |> Enum.reject(&is_nil/1)

    messages =
      normalized
      |> Enum.reduce(state.messages, fn message, acc -> [message | acc] end)
      |> Enum.take(state.limit)

    {:noreply, %{state | messages: messages}}
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
    |> maybe_put(:explain_text, message[:explain_text] || message["explain_text"])
    |> maybe_put(:explain_payload, message[:explain_payload] || message["explain_payload"])
  end

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, :meta, value) when not is_map(value), do: map
  defp maybe_put(map, :mods, value) when not is_map(value), do: map
  defp maybe_put(map, :explain_payload, value) when not is_map(value), do: map
  defp maybe_put(map, :explain_text, value) when not is_binary(value), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp normalize_role(role) when role in [:user, :assistant, :system], do: role
  defp normalize_role("user"), do: :user
  defp normalize_role("assistant"), do: :assistant
  defp normalize_role("system"), do: :system
  defp normalize_role(_), do: nil

  defp normalize_limit(limit) when is_integer(limit) and limit > 0, do: limit
  defp normalize_limit(_), do: @default_limit
end
