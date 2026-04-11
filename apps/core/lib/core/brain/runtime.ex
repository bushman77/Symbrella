defmodule Core.Brain.Runtime do
  @moduledoc """
  Shared runtime guards for Core -> Brain interactions.
  """

  @spec pid_alive?(atom()) :: boolean()
  def pid_alive?(name) when is_atom(name) do
    case Process.whereis(name) do
      pid when is_pid(pid) -> Process.alive?(pid)
      _ -> false
    end
  end

  @spec apply_if_exported(module(), atom(), list(), term()) :: term()
  def apply_if_exported(mod, fun, args, default)
      when is_atom(mod) and is_atom(fun) and is_list(args) do
    case Code.ensure_loaded?(mod) and function_exported?(mod, fun, length(args)) do
      true -> apply(mod, fun, args)
      false -> default
    end
  end

  @spec emit([atom()], map(), map()) :: :ok
  def emit(event, measurements, metadata)
      when is_list(event) and is_map(measurements) and is_map(metadata) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(event, measurements, metadata)
    else
      :ok
    end
  end

  @spec put_if_present(keyword(), atom(), term()) :: keyword()
  def put_if_present(kvs, _key, nil), do: kvs

  def put_if_present(kvs, key, value) when is_list(kvs) and is_atom(key),
    do: Keyword.put(kvs, key, value)
end
