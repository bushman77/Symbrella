# apps/core/test/test_helper.exs
ExUnit.start()

# Start the DB app (Repo) and the Brain app tree that defines the `Brain` process
{:ok, _} = Application.ensure_all_started(:db)
{:ok, _} = Application.ensure_all_started(:brain)

# Put Repo in manual sandbox mode for tests
Ecto.Adapters.SQL.Sandbox.mode(Db, :manual)

