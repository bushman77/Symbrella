# apps/llm/lib/llm/daemon.ex
defmodule Llm.Daemon do
  @moduledoc false
  alias Llm.Http

  def ensure_serving(state, timeout_ms) do
    if reachable?(state, timeout_ms) do
      {:ok, state, false}
    else
      case spawn_ollama() do
        {:ok, port} ->
          state2 = %{state | ollama_port: port, served_by_us?: true}

          if wait_until(fn -> reachable?(state2, timeout_ms) end, 60, 500) do
            {:ok, state2, true}
          else
            {:error, state2, :serve_not_ready}
          end

        {:error, reason} ->
          {:error, state, reason}
      end
    end
  end

  def reachable?(state, timeout_ms) do
    case Http.get_json(state, "/api/version", timeout: timeout_ms) do
      {:ok, %{"version" => _}} -> true
      _ -> false
    end
  end

  def spawn_ollama do
    case System.find_executable("ollama") do
      nil ->
        {:error, :ollama_not_found}

      path ->
        port =
          Port.open({:spawn_executable, path}, [
            :binary,
            :exit_status,
            args: ["serve"],
            env:
              Enum.map(System.get_env(), fn {k, v} ->
                {String.to_charlist(k), String.to_charlist(v)}
              end)
          ])

        {:ok, port}
    end
  end

  def wait_until(fun, attempts, sleep_ms) when attempts > 0 do
    if fun.() do
      true
    else
      Process.sleep(sleep_ms)
      wait_until(fun, attempts - 1, sleep_ms)
    end
  end

  def wait_until(_fun, 0, _sleep_ms), do: false
end
