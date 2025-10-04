# apps/core/test/test_helper.exs
ExUnit.start()

# Start the DB app (Repo) and the Brain app tree
{:ok, _} = Application.ensure_all_started(:db)
{:ok, _} = Application.ensure_all_started(:brain)

# Put Repo in manual sandbox mode and create a shared owner for the whole suite
Ecto.Adapters.SQL.Sandbox.mode(Db, :manual)
_owner = Ecto.Adapters.SQL.Sandbox.start_owner!(Db, shared: true)

