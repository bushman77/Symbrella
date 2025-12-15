ExUnit.start()

# Ensure the umbrella root is running for brain integration tests.
# This boots singletons like Brain.Thalamus/PMTG/PFC under the preferred topology.
_ = Application.ensure_all_started(:symbrella)

# Start a single shared Sandbox owner for the whole test run.
# (Do NOT call Sandbox.mode/2 when using the owner API.)
_ = Ecto.Adapters.SQL.Sandbox.start_owner!(Db, shared: true)

defmodule Brain.TestHooks do
  @moduledoc false

  # Telemetry handlers must be module-qualified to avoid the "local function" warning.
  # We forward the event into the test process mailbox.
  def lifg_handle(event, meas, meta, pid) do
    if is_pid(pid) do
      send(pid, {:telemetry, event, meas, meta})
    end

    :ok
  end
end

:telemetry.attach_many(
  "lifg-test-hooks",
  [
    [:brain, :lifg, :chargram_violation],
    [:brain, :lifg, :boundary_drop],
    [:brain, :pmtg, :no_mwe_senses]
  ],
  &Brain.TestHooks.lifg_handle/4,
  self()
)

