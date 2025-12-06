defmodule SymbrellaWeb.Plugs.DevFileApiAuth do
  @moduledoc """
  Dev-only auth gate for the read-only file API.

  Requires: Authorization: Bearer <token>
  Token source: SYMBRELLA_DEV_FILE_API_TOKEN

  Fails closed if token is not configured.
  """

  @behaviour Plug

  import Plug.Conn

  @token_env "SYMBRELLA_DEV_FILE_API_TOKEN"

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    expected = System.get_env(@token_env)

    cond do
      not is_binary(expected) or byte_size(expected) < 16 ->
        conn
        |> put_status(:service_unavailable)
        |> Phoenix.Controller.json(%{error: "dev_file_api_token_not_configured"})
        |> halt()

      true ->
        provided = bearer(conn) || conn.params["token"]

        if is_binary(provided) and secure_eq?(provided, expected) do
          conn
        else
          conn
          |> put_status(:unauthorized)
          |> Phoenix.Controller.json(%{error: "unauthorized"})
          |> halt()
        end
    end
  end

  defp bearer(conn) do
    case get_req_header(conn, "authorization") do
      ["Bearer " <> token] -> String.trim(token)
      ["bearer " <> token] -> String.trim(token)
      _ -> nil
    end
  end

  defp secure_eq?(a, b) when is_binary(a) and is_binary(b) do
    if byte_size(a) == byte_size(b) do
      Plug.Crypto.secure_compare(a, b)
    else
      false
    end
  end
end

