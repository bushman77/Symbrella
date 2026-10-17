defmodule SymbrellaWeb.Plugs.ChatSession do
  @moduledoc """
  Give each browser cookie its own conversational identifier.

  This value lives in Phoenix's signed session cookie and survives LiveView
  remounts, but is deliberately unrelated to the shared cognitive processes.
  """

  @behaviour Plug
  import Plug.Conn

  @session_key "symbrella_chat_session_id"

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    case get_session(conn, @session_key) do
      "s-" <> suffix when byte_size(suffix) == 32 ->
        conn

      _ ->
        put_session(conn, @session_key, new_session_id())
    end
  end

  defp new_session_id do
    "s-" <> Base.encode16(:crypto.strong_rand_bytes(16), case: :lower)
  end
end
