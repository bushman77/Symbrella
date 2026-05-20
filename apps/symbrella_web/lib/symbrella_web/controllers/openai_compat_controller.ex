defmodule SymbrellaWeb.OpenAICompatController do
  use SymbrellaWeb, :controller

  @model_id "symbrella-rp"

  def models(conn, _params) do
    now = System.system_time(:second)

    json(conn, %{
      object: "list",
      data: [
        %{
          id: @model_id,
          object: "model",
          created: now,
          owned_by: "symbrella"
        }
      ]
    })
  end

  def chat_completions(conn, params) do
    stream? = Map.get(params, "stream", false)

    if stream? do
      conn
      |> put_status(:bad_request)
      |> json(%{
        error: %{
          message:
            "Streaming is not implemented yet. Send stream: false for this first gateway slice.",
          type: "invalid_request_error",
          param: "stream",
          code: "streaming_not_supported"
        }
      })
    else
      case Core.RoleplayTurn.run(params) do
        {:ok, response} ->
          json(conn, response)

        {:error, error} ->
          conn
          |> put_status(:bad_gateway)
          |> json(%{error: error})
      end
    end
  end
end
