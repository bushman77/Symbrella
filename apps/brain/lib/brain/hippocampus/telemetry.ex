defmodule Brain.Hippocampus.Telemetry do
  @moduledoc "Small wrappers for Hippocampus telemetry + test echo."

  @spec emit_write(map(), map()) :: :ok
  def emit_write(meas, meta) do
    execute([:brain, :hippocampus, :write], meas, meta)
  end

  @spec emit_recall(map(), map()) :: :ok
  def emit_recall(meas, meta) do
    execute([:brain, :hippocampus, :recall], meas, meta)
  end

  @spec maybe_echo_to_caller({pid(), term()}, [atom()], map(), map()) :: :ok
  def maybe_echo_to_caller({pid, _tag}, event, meas, meta) when is_pid(pid) do
    if test_env?(), do: send(pid, {:telemetry, event, meas, meta})
    :ok
  end

  def maybe_echo_to_caller(_from, _event, _meas, _meta), do: :ok

  ## ── internal

  defp execute(ev, meas, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(ev, meas, meta)
    end

    :ok
  end

  defp test_env? do
    mix_env = (Code.ensure_loaded?(Mix) and function_exported?(Mix, :env, 0) and Mix.env()) || :prod
    mix_env == :test
  end
end

