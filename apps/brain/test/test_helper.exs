# test/test_helper.exs
ExUnit.start()

# Start a single shared Sandbox owner for the whole test run.
# (Do NOT call Sandbox.mode/2 when using the owner API.)
_ = Ecto.Adapters.SQL.Sandbox.start_owner!(Db, shared: true)

:telemetry.attach_many(
  "lifg-test-hooks",
  [
    [:brain, :lifg, :chargram_violation],
    [:brain, :lifg, :boundary_drop],
    [:brain, :pmtg, :no_mwe_senses]
  ],
  fn event, meas, meta, _ ->
    send(self(), {:telemetry, event, meas, meta})
  end,
  nil
)
