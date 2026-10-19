defmodule SymbrellaWeb.Plugs.WebAuth do
  @moduledoc """
  Protects sensitive browser routes with HTTP Basic authentication and ensures
  connected LiveViews came from an authenticated, signed browser session.

  Credentials are supplied at runtime, never compiled into the application.
  When disabled for local development, routes retain their previous behaviour.
  """

  @behaviour Plug
  import Plug.Conn

  @session_key "symbrella_web_auth"

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, opts) do
    case auth_config(opts) do
      %{enabled?: true, username: username, password: password} = cfg ->
        conn = Plug.BasicAuth.basic_auth(conn, username: username, password: password)

        if conn.halted do
          conn
        else
          put_session(conn, @session_key, session_proof(cfg))
        end

      _ ->
        conn
    end
  end

  @doc "Verify a session on both disconnected and connected LiveView mounts."
  def on_mount(:require_authenticated, _params, session, socket) do
    if authorized_session?(session) do
      {:cont, socket}
    else
      {:halt, Phoenix.LiveView.redirect(socket, to: "/")}
    end
  end

  @doc "Checks the signed-cookie session proof against current credentials."
  def authorized_session?(session) when is_map(session) do
    case auth_config([]) do
      %{enabled?: true} = cfg ->
        expected = session_proof(cfg)
        actual = Map.get(session, @session_key)

        is_binary(actual) and byte_size(actual) == byte_size(expected) and
          Plug.Crypto.secure_compare(actual, expected)

      _ ->
        true
    end
  end

  def authorized_session?(_), do: false

  defp auth_config(opts) do
    config = Keyword.merge(Application.get_env(:symbrella_web, :web_auth, []), opts)

    if Keyword.get(config, :enabled?, false) do
      username = Keyword.get(config, :username)
      password = Keyword.get(config, :password)

      if is_binary(username) and byte_size(username) > 0 and
           is_binary(password) and byte_size(password) >= 16 do
        %{enabled?: true, username: username, password: password}
      else
        raise "Symbrella web authentication is enabled without valid credentials"
      end
    else
      %{enabled?: false}
    end
  end

  # HMAC keys the proof to the Phoenix secret as well as the configured user
  # and password. The signed cookie does not reveal a password verifier.
  defp session_proof(%{username: username, password: password}) do
    endpoint_config = Application.get_env(:symbrella_web, SymbrellaWeb.Endpoint, [])
    secret = Keyword.fetch!(endpoint_config, :secret_key_base)

    :crypto.mac(:hmac, :sha256, secret, [username, 0, password])
    |> Base.url_encode64(padding: false)
  end
end
