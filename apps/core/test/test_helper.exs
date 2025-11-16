# UTF-8, LF endings

ExUnit.start()

# --- Helper module: keeps sandbox allowances stable for background GenServers
# Avoids undefined-function errors by defining helpers inside a module.
# Works across restarts of the named processes.

defmodule Core.TestSandboxHelper do
  @moduledoc false

  def start_allow_loop(repo, owner_pid, names) when is_list(names) do
    Enum.each(names, fn name ->
      spawn_link(fn -> loop_allow(repo, owner_pid, name) end)
    end)
  end

  defp loop_allow(repo, owner_pid, name) when is_atom(name) do
    pid = Process.whereis(name)

    if is_pid(pid) do
      safe_allow(repo, owner_pid, pid)
      ref = Process.monitor(pid)

      receive do
        {:DOWN, ^ref, :process, ^pid, _reason} -> :ok
      after
        1_000 -> :ok
      end
    else
      Process.sleep(200)
    end

    loop_allow(repo, owner_pid, name)
  end

  defp safe_allow(repo, owner_pid, pid) do
    try do
      Ecto.Adapters.SQL.Sandbox.allow(repo, owner_pid, pid)
    rescue
      _ -> :ok
    end
  end
end

# --- Boot deps needed for Ecto SQL sandbox (idempotent) ---------------
{:ok, _} = Application.ensure_all_started(:ecto_sql)
_ = Application.ensure_all_started(:symbrella)

# --- Repo + Sandbox setup ----------------------------------------------
if Code.ensure_loaded?(Db) do
  # Ensure the Repo is running even when tests run with --no-start
  repo_alive? =
    try do
      pid = Ecto.Repo.Registry.lookup(Db)
      is_pid(pid) and Process.alive?(pid)
    rescue
      _ -> false
    end

  unless repo_alive? do
    case Db.start_link() do
      {:ok, _pid} ->
        :ok

      {:error, {:already_started, _pid}} ->
        :ok

      other ->
        raise "Failed to ensure Db repo started: #{inspect(other)}"
    end
  end

  # Manual sandbox + a suite-long shared owner
  :ok = Ecto.Adapters.SQL.Sandbox.mode(Db, :manual)

  owner_pid =
    case :persistent_term.get({:symbrella, :sandbox_owner}, :undefined) do
      :undefined ->
        pid = Ecto.Adapters.SQL.Sandbox.start_owner!(Db, shared: true)
        :persistent_term.put({:symbrella, :sandbox_owner}, pid)

        ExUnit.after_suite(fn _ ->
          if Process.alive?(pid) do
            try do
              Ecto.Adapters.SQL.Sandbox.stop_owner(pid)
            rescue
              _ -> :ok
            end
          end
        end)

        pid

      pid when is_pid(pid) ->
        pid
    end

  # Continuously (re)allow known background processes to use the owner's connection.
  allow_names = [Brain.PMTG, Brain.Hippocampus]
  Core.TestSandboxHelper.start_allow_loop(Db, owner_pid, allow_names)
end

