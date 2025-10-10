# apps/db/test/test_helper.exs
ExUnit.start()

# Ensure the :db application (and Repo) are started in tests
{:ok, _} = Application.ensure_all_started(:db)

# Use SQL Sandbox for isolated DB tests
Ecto.Adapters.SQL.Sandbox.mode(Db, :manual)
