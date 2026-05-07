# apps/llm/lib/llm/http.ex
defmodule Llm.Http do
  @moduledoc false
  alias Llm.Const
  alias Llm.Util

  def get_json(state, path, opts) do
    timeout = Keyword.get(opts, :timeout, state.timeout)

    case Req.get(state.base_url <> path,
           headers: [{"accept", "application/json"}],
           receive_timeout: timeout
         ) do
      {:ok, %{status: code, body: resp}} when code in 200..299 -> {:ok, resp}
      {:ok, %{status: code, body: resp}} -> {:error, {:http_error, code, resp}}
      {:error, reason} -> {:error, {:transport, reason}}
    end
  end

  def post_json(state, path, body, opts) do
    timeout = Keyword.get(opts, :timeout, state.timeout)

    case Req.post(state.base_url <> path,
           json: body,
           headers: [{"accept", "application/json"}],
           receive_timeout: timeout
         ) do
      {:ok, %{status: code, body: resp}} when code in 200..299 -> {:ok, resp}
      {:ok, %{status: code, body: resp}} -> {:error, {:http_error, code, resp}}
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
