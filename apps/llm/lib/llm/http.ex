# apps/llm/lib/llm/http.ex
defmodule Llm.Http do
  @moduledoc false
  alias Llm.Const
  alias Llm.Util

  def client(state, opts) do
    timeout = Keyword.get(opts, :timeout, state.timeout)

    Tesla.client(
      [
        {Tesla.Middleware.BaseUrl, state.base_url},
        {Tesla.Middleware.JSON, engine: Jason},
        {Tesla.Middleware.Headers, [{"content-type", "application/json"}]}
      ],
      {Tesla.Adapter.Finch, name: state.finch, receive_timeout: timeout, pool_timeout: timeout}
    )
  end

  def get_json(state, path, opts) do
    case Tesla.get(client(state, opts), path) do
      {:ok, %Tesla.Env{status: code, body: resp}} when code in 200..299 -> {:ok, resp}
      {:ok, %Tesla.Env{status: code, body: resp}} -> {:error, {:http_error, code, resp}}
      {:error, reason} -> {:error, {:transport, reason}}
    end
  end

  def post_json(state, path, body, opts) do
    case Tesla.post(client(state, opts), path, body) do
      {:ok, %Tesla.Env{status: code, body: resp}} when code in 200..299 -> {:ok, resp}
      {:ok, %Tesla.Env{status: code, body: resp}} -> {:error, {:http_error, code, resp}}
      {:error, reason} -> {:error, {:transport, reason}}
    end
  end

  def build_request_body(base_map, state, opts) do
    base_map
    |> Util.maybe_put_keep_alive(opts)
    |> Util.maybe_put_format(opts)
    |> Util.put_temperature(Keyword.get(opts, :temperature, state.temperature))
    |> Util.put_options(common_ollama_options(opts))
  end

  def common_ollama_options(opts) do
    user = Keyword.get(opts, :options, %{}) |> Map.new() |> Map.drop([:num_ctx])
    Map.merge(Const.stable_runner_opts(), user)
  end
end

