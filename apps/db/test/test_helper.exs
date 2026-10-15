# apps/db/test/test_helper.exs
ExUnit.start()

# The :db OTP application does not supervise the Ecto Repo: Db.MixProject
# declares only extra_applications. Starting :db alone does not register Db.
{:ok, _} = Application.ensure_all_started(:db)

case Db.start_link() do
  {:ok, _pid} -> :ok
  {:error, {:already_started, _pid}} -> :ok
  {:error, reason} -> raise "Cannot start Db test repository: #{inspect(reason)}"
end

# Use SQL Sandbox for isolated DB tests
Ecto.Adapters.SQL.Sandbox.mode(Db, :manual)
