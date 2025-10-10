# UTF-8, LF endings

ExUnit.start()

# --- Bring up dependencies needed for Ecto's SQL sandbox (idempotent) ----
{:ok, _} = Application.ensure_all_started(:ecto_sql)

# Prefer starting the umbrella application once; it should start Repo and other children.
# If Symbrella isn't an OTP app in your env, this call simply does nothing.
_ = Application.ensure_all_started(:symbrella)

# --- DB/Sandbox setup (guarded so repeated loads won't crash) -------------
if Code.ensure_loaded?(Db) do
  # Is the Repo process already alive?
  repo_alive? =
    try do
      pid = Ecto.Repo.Registry.lookup(Db)
      is_pid(pid) and Process.alive?(pid)
    rescue
      _ -> false
    end

  # If for some reason the Repo isn't running (e.g., tests with --no-start), try to start it.
  unless repo_alive? do
    case Db.start_link() do
      {:ok, _pid} -> :ok
      {:error, {:already_started, _pid}} -> :ok
      other -> raise "Failed to ensure Db repo started: #{inspect(other)}"
    end
  end

  # Put Repo in manual sandbox mode for the suite
  Ecto.Adapters.SQL.Sandbox.mode(Db, :manual)

  # Create or reuse a single suite-wide sandbox owner using :persistent_term
  owner_pid =
    case :persistent_term.get({:symbrella, :sandbox_owner}, :undefined) do
      :undefined ->
        pid = Ecto.Adapters.SQL.Sandbox.start_owner!(Db, shared: true)
        :persistent_term.put({:symbrella, :sandbox_owner}, pid)

        # Teardown only once, tied to the pid we created
        ExUnit.after_suite(fn _ ->
          if Process.alive?(pid) do
            try do
              Ecto.Adapters.SQL.Sandbox.stop_owner(pid)
            catch
              :exit, _ -> :ok
            rescue
              _ -> :ok
            end
          end
        end)

        pid

      pid when is_pid(pid) ->
        pid
    end

  # Optional: keep a local reference to avoid "unused variable" warnings if you later expand
  _ = owner_pid
end

