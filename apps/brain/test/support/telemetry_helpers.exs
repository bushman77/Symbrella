defmodule Support.TelemetryHelpers do
  @moduledoc """
  Test-only helpers for capturing and asserting `:telemetry` events.

  ## Quick use
      import Support.TelemetryHelpers

      {meas, meta} =
        capture([:brain, :acc, :conflict], fn ->
          # run code that should emit the event
        end, 200)

      # Or attach manually and assert_receive yourself:
      ref = attach([[:brain, :lifg, :chargram_violation]])
      # ...run code...
      assert_receive {:telemetry_event, [:brain, :lifg, :chargram_violation], meas, meta}
      detach(ref)

  The helpers ensure handlers are detached even on failures.
  """

  import ExUnit.Assertions, only: [assert: 1, flunk: 1, refute: 1]

  @default_tag :telemetry_event

  @type event_path :: [atom()]

  @doc """
  Attach a handler for one or many telemetry events.

  Returns a `handler_id` which you must pass to `detach/1` (unless you use `capture*` helpers).

  Messages delivered to the calling process have shape:
      {tag, event_path, measurements :: map(), metadata :: map()}
  """
  @spec attach([event_path()] | event_path(), atom()) :: String.t()
  def attach(events, tag \\ @default_tag) do
    evs =
      case events do
        [h | _] = list when is_list(h) -> list
        list when is_list(list) -> [list]
      end

    handler_id = "telemetry-test-#{System.unique_integer([:positive])}"

    :ok =
      :telemetry.attach_many(
        handler_id,
        evs,
        fn ev, meas, meta, pid -> send(pid, {tag, ev, meas, meta}) end,
        self()
      )

    handler_id
  end

  @doc "Detach a previously attached handler."
  @spec detach(String.t()) :: :ok
  def detach(handler_id) when is_binary(handler_id),
    do: :telemetry.detach(handler_id)

  @doc """
  Drain all pending telemetry messages with the given `tag` from the mailbox.
  Useful to avoid cross-test leakage.
  """
  @spec drain(atom()) :: :ok
  def drain(tag \\ @default_tag) do
    receive do
      {^tag, _ev, _meas, _meta} -> drain(tag)
    after
      0 -> :ok
    end
  end

  @doc """
  Capture exactly one telemetry `event` while running `fun`.

  Returns `{measurements, metadata}`. Fails if no event arrives within `timeout_ms`.
  """
  @spec capture(event_path(), (-> any), non_neg_integer()) :: {map(), map()}
  def capture(event, fun, timeout_ms \\ 150)
      when is_list(event) and is_function(fun, 0) and is_integer(timeout_ms) do
    capture_opts([event], fun, timeout_ms: timeout_ms)
    |> case do
      {^event, meas, meta} -> {meas, meta}
      other -> flunk("unexpected event captured: #{inspect(other)}")
    end
  end

  @doc """
  Capture the first event from `events` while running `fun`.

  Returns `{event_path, measurements, metadata}`.
  """
  @spec capture_any([event_path()] | event_path(), (-> any), non_neg_integer()) ::
          {event_path(), map(), map()}
  def capture_any(events, fun, timeout_ms \\ 150)
      when is_function(fun, 0) and is_integer(timeout_ms) do
    capture_opts(events, fun, timeout_ms: timeout_ms)
  end

  @doc """
  Capture zero or more events from `events` while running `fun`, returning a list
  of `{event_path, measurements, metadata}` seen within `timeout_ms`.

  Uses a short rolling mailbox window after `fun` finishes to scoop up bursts.
  """
  @spec capture_many([event_path()] | event_path(), (-> any), non_neg_integer()) ::
          list({event_path(), map(), map()})
  def capture_many(events, fun, timeout_ms \\ 150)
      when is_function(fun, 0) and is_integer(timeout_ms) do
    tag = @default_tag
    ref = attach(events, tag)

    try do
      fun.()
      collect_many(tag, timeout_ms, [])
    after
      :ok = detach(ref)
    end
  end

  @doc """
  Assert that **no** `event` is emitted while running `fun` (within `timeout_ms`).
  """
  @spec refute_event(event_path(), (-> any), non_neg_integer()) :: :ok
  def refute_event(event, fun, timeout_ms \\ 150)
      when is_list(event) and is_function(fun, 0) and is_integer(timeout_ms) do
    tag = @default_tag
    ref = attach([event], tag)

    try do
      fun.()

      receive do
        {^tag, ^event, meas, meta} ->
          flunk(
            "unexpected telemetry #{inspect(event)}: meas=#{inspect(meas)} meta=#{inspect(meta)}"
          )
      after
        timeout_ms -> :ok
      end
    after
      :ok = detach(ref)
    end
  end

  # ----- Internals ----------------------------------------------------

  defp capture_opts(events, fun, opts) do
    tag = Keyword.get(opts, :tag, @default_tag)
    timeout_ms = Keyword.get(opts, :timeout_ms, 150)
    ref = attach(events, tag)

    try do
      fun.()

      receive do
        {^tag, ev, meas, meta} ->
          {ev, meas, meta}
      after
        timeout_ms ->
          flunk("did not receive telemetry for #{inspect(events)} in #{timeout_ms}ms")
      end
    after
      :ok = detach(ref)
    end
  end

  defp collect_many(tag, timeout_ms, acc) do
    receive do
      {^tag, ev, meas, meta} ->
        collect_many(tag, timeout_ms, [{ev, meas, meta} | acc])
    after
      timeout_ms ->
        Enum.reverse(acc)
    end
  end
end
