defmodule Support.TelemetryHelpers do
  @moduledoc false

  # Bring in flunk/1 for assertion-style failures
  import ExUnit.Assertions, only: [flunk: 1]

  @doc """
  Capture exactly one telemetry event while running `fun`.

  ## Params
    * `event` — a telemetry event name (list of atoms), e.g. `[:brain, :pmtg, :mwe_fallback_emitted]`
    * `fun` — a zero-arity function to run
    * `timeout_ms` — how long to wait (ms) for an event (default 100)

  ## Returns
    `{meas, meta}` from the first matching event.

  ## Examples
      {m, meta} =
        Support.TelemetryHelpers.capture([:brain, :acc, :conflict], fn ->
          # run code that should emit the event
        end, 200)
  """
  @spec capture([atom], (() -> any), non_neg_integer) :: {map, map}
  def capture(event, fun, timeout_ms \\ 100)
      when is_list(event) and is_function(fun, 0) and is_integer(timeout_ms) do
    capture_opts(event, fun, timeout_ms: timeout_ms)
  end

  # Internal opt-style version (supports :timeout_ms and optional :tag)
  defp capture_opts(event, fun, opts) do
    timeout_ms = Keyword.get(opts, :timeout_ms, 100)
    tag        = Keyword.get(opts, :tag, :telemetry_event)

    handler_id = "telemetry-test-#{System.unique_integer([:positive])}"

    :ok =
      :telemetry.attach_many(
        handler_id,
        [event],
        fn ev, meas, meta, pid -> send(pid, {tag, ev, meas, meta}) end,
        self()
      )

    try do
      fun.()

      receive do
        {^tag, ^event, meas, meta} ->
          {meas, meta}
      after
        timeout_ms ->
          flunk("did not receive telemetry for #{inspect(event)} in #{timeout_ms}ms")
      end
    after
      :telemetry.detach(handler_id)
    end
  end
end

