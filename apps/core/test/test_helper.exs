# apps/core/test/test_helper.exs
ExUnit.start()

# 1) Bring up ecto_sql for Sandbox
{:ok, _} = Application.ensure_all_started(:ecto_sql)

# 2) Ensure the Repo (Db) is running in this test VM.
repo_started? =
  try do
    case Ecto.Repo.Registry.lookup(Db) do
      pid when is_pid(pid) -> true
      _ -> false
    end
  rescue
    _ -> false
  end

unless repo_started? do
  {:ok, _pid} = Db.start_link()
end

# 3) Suite-wide sandbox owner (manual mode, shared).
Ecto.Adapters.SQL.Sandbox.mode(Db, :manual)
owner = Ecto.Adapters.SQL.Sandbox.start_owner!(Db, shared: true)

# 4) Idempotent teardown — won't blow up if already stopped elsewhere.
ExUnit.after_suite(fn _ ->
  if Process.alive?(owner) do
    try do
      Ecto.Adapters.SQL.Sandbox.stop_owner(owner)
    catch
      :exit, _ -> :ok
    rescue
      _ -> :ok
    end
  end
end)

