# apps/symbrella_web/test/test_helper.exs

ExUnit.start()

alias Ecto.Adapters.SQL.Sandbox

# Ensure the database repository is running.
{:ok, _} = Application.ensure_all_started(:db)

# Brain and Core may have used an umbrella-wide shared
# sandbox owner. Their tests have finished by the time
# the Web test helper executes.
#
# Web tests use ConnCase, which manages a separate
# sandbox connection for each test.

owner_key = {:symbrella, :sandbox_owner}

case :persistent_term.get(owner_key, :undefined) do
  pid when is_pid(pid) ->
    if Process.alive?(pid) do
      Sandbox.stop_owner(pid)
    end

    :persistent_term.erase(owner_key)

  _ ->
    :ok
end

# Restore manual ownership before Web tests begin.
# ConnCase will check out a connection for each test.
:ok = Sandbox.mode(Db, :manual)
