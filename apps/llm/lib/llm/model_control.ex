# apps/llm/lib/llm/model_control.ex
defmodule Llm.ModelControl do
  @moduledoc false
  alias Llm.{Http, Const}

  # /api/ps
  def list_loaded_models(state, opts) do
    case Http.get_json(state, "/api/ps", opts) do
      {:ok, %{"models" => _} = body} -> {:ok, body}
      {:ok, other} -> {:error, {:unexpected_response, other}}
      other -> other
    end
  end

  # Warm model by a tiny no-op generate; memoize warmed set in caller.
  def maybe_warm_model(%{warmed_models: warmed} = state, model, http_timeout) do
    m = model || state.model

    if MapSet.member?(warmed, m) do
      {:ok, state}
    else
      body = %{
        "model" => m,
        "prompt" => "ok",
        "stream" => false,
        "keep_alive" => Const.default_keep_alive(),
        "options" => Const.stable_runner_opts()
      }

      case Http.post_json(state, "/api/generate", body, timeout: http_timeout) do
        {:ok, %{"response" => _}} ->
          {:ok, %{state | warmed_models: MapSet.put(warmed, m)}}

        {:ok, %{"done" => true}} ->
          {:ok, %{state | warmed_models: MapSet.put(warmed, m)}}

        _ ->
          {:error, :warm_failed}
      end
    end
  end

  # Strict unload via keep_alive: 0 + verification polling.
  def unload_model_strict(state, model, opts) do
    http_timeout = Keyword.get(opts, :timeout, state.timeout)
    body = %{
      "model" => model,
      "prompt" => "",
      "stream" => false,
      "keep_alive" => 0,
      "options" => Const.stable_runner_opts()
    }

    _ = Http.post_json(state, "/api/generate", body, timeout: http_timeout)
    verify_absent?(state, model, Keyword.get(opts, :attempts, 12), Keyword.get(opts, :sleep_ms, 150), http_timeout)
  end

  def verify_absent?(state, model, attempts, sleep_ms, http_timeout) when attempts > 0 do
    case list_loaded_models(state, timeout: http_timeout) do
      {:ok, %{"models" => ms}} ->
        if Enum.any?(ms, &(&1["name"] == model)) do
          Process.sleep(sleep_ms)
          verify_absent?(state, model, attempts - 1, sleep_ms, http_timeout)
        else
          :ok
        end

      _ ->
        Process.sleep(sleep_ms)
        verify_absent?(state, model, attempts - 1, sleep_ms, http_timeout)
    end
  end
  def verify_absent?(_state, _model, 0, _sleep_ms, _timeout), do: {:error, :unload_not_confirmed}

  def pull_model(model) do
    case System.find_executable("ollama") do
      nil -> false
      path ->
        {_, code} = System.cmd(path, ["pull", model], env: System.get_env(), stderr_to_stdout: true)
        code == 0
    end
  end
end

