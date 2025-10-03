ExUnit.start()

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

