# apps/core/test/test_helper.exs
# UTF-8, LF endings

ExUnit.start()

defmodule Core.TestSandboxHelper do
  @moduledoc """
  Manages database sandbox ownership for Core tests.

  Supports:
    - Standalone Core test execution
    - Umbrella-wide test execution
    - Shared sandbox owner reuse
    - Background GenServer allowances
    - Recovery when supervised processes restart
    - Safe cleanup of Core-owned resources
  """

  alias Ecto.Adapters.SQL.Sandbox

  @owner_key {:symbrella, :sandbox_owner}

  @poll_interval_ms 200
  @check_interval_ms 1_000

  # ----------------------------------------------------------
  # Repository initialization
  # ----------------------------------------------------------

  def ensure_repo_started(repo) do
    alive? =
      try do
        case Ecto.Repo.Registry.lookup(repo) do
          pid when is_pid(pid) ->
            Process.alive?(pid)

          _ ->
            false
        end
      rescue
        _ -> false
      end

    if alive? do
      :ok
    else
      case repo.start_link() do
        {:ok, _pid} ->
          :ok

        {:error, {:already_started, _pid}} ->
          :ok

        other ->
          raise "Failed to start #{inspect(repo)}: #{inspect(other)}"
      end
    end
  end

  # ----------------------------------------------------------
  # Shared sandbox ownership
  # ----------------------------------------------------------

  @doc """
  Returns {owner_pid, created_here?}.

  Reuses a live owner established by Brain.
  Otherwise creates a new owner for standalone Core tests.
  """
  def ensure_owner(repo) do
    case :persistent_term.get(@owner_key, :undefined) do
      pid when is_pid(pid) ->
        if Process.alive?(pid) do
          {pid, false}
        else
          :persistent_term.erase(@owner_key)
          start_new_owner(repo)
        end

      _ ->
        start_new_owner(repo)
    end
  end

  defp start_new_owner(repo) do
    pid = Sandbox.start_owner!(repo, shared: true)

    :persistent_term.put(@owner_key, pid)

    {pid, true}
  end

  # ----------------------------------------------------------
  # Background process allowances
  # ----------------------------------------------------------

  @doc """
  Maintains sandbox allowances for named processes.

  Reapplies permissions after supervised process restarts.
  Stops monitoring when the owner terminates.
  """
  def start_allow_loop(repo, owner_pid, names)
      when is_pid(owner_pid) and is_list(names) do
    Enum.each(names, fn
      name when is_atom(name) ->
        spawn(fn ->
          allow_loop(repo, owner_pid, name)
        end)

      _ ->
        :ok
    end)

    :ok
  end

  defp allow_loop(repo, owner_pid, name) do
    if Process.alive?(owner_pid) do
      case Process.whereis(name) do
        pid when is_pid(pid) ->
          safe_allow(repo, owner_pid, pid)
          monitor_process(pid)

        _ ->
          Process.sleep(@poll_interval_ms)
      end

      allow_loop(repo, owner_pid, name)
    else
      :ok
    end
  end

  defp monitor_process(pid) do
    ref = Process.monitor(pid)

    receive do
      {:DOWN, ^ref, :process, ^pid, _reason} ->
        :ok
    after
      @check_interval_ms ->
        :ok
    end

    Process.demonitor(ref, [:flush])
    :ok
  end

  defp safe_allow(repo, owner_pid, pid) do
    Sandbox.allow(repo, owner_pid, pid)
  rescue
    _ -> :ok
  catch
    :exit, _ -> :ok
  end

  # ----------------------------------------------------------
  # Owner cleanup
  # ----------------------------------------------------------

  @doc """
  Stops the sandbox owner only when Core created it.

  Never terminates an owner inherited from Brain.
  """
  def cleanup_owner(_pid, false), do: :ok

  def cleanup_owner(pid, true) do
    try do
      if Process.alive?(pid) do
        Sandbox.stop_owner(pid)
      end
    rescue
      _ -> :ok
    catch
      :exit, _ -> :ok
    after
      if :persistent_term.get(@owner_key, :undefined) == pid do
        :persistent_term.erase(@owner_key)
      end
    end

    :ok
  end
end

# ============================================================
# Application initialization
# ============================================================

{:ok, _} = Application.ensure_all_started(:ecto_sql)
{:ok, _} = Application.ensure_all_started(:symbrella)

# ============================================================
# Database sandbox initialization
# ============================================================

if Code.ensure_loaded?(Db) do
  # Ensure the repository is available.
  :ok = Core.TestSandboxHelper.ensure_repo_started(Db)

  # Reuse Brain's owner in umbrella tests.
  # Create our own in standalone Core tests.
  {owner_pid, created_here?} =
    Core.TestSandboxHelper.ensure_owner(Db)

  # ----------------------------------------------------------
  # Background process allowances
  # ----------------------------------------------------------

  allow_names = [
    Brain.PMTG,
    Brain.Hippocampus
  ]

  :ok =
    Core.TestSandboxHelper.start_allow_loop(
      Db,
      owner_pid,
      allow_names
    )

  # ----------------------------------------------------------
  # Suite cleanup
  # ----------------------------------------------------------

  ExUnit.after_suite(fn _results ->
    Core.TestSandboxHelper.cleanup_owner(
      owner_pid,
      created_here?
    )
  end)
end
