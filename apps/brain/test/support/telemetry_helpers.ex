defmodule Support.TelemetryHelpers do
  @moduledoc """
  Tiny helpers to capture `:telemetry` events in tests.

  Place this file at: `apps/brain/test/support/telemetry_helpers.ex`
  Make sure your `apps/brain/mix.exs` sets:
      def project do
        [
          elixirc_paths: elixirc_paths(Mix.env()),
          # ...rest
        ]
      end

      defp elixirc_paths(:test), do: ["lib", "test/support"]
      defp elixirc_paths(_), do: ["lib"]

  ## Quick reference

      import Support.TelemetryHelpers

      # Single event — returns {meas, meta}
      {meas, meta} =
        capture([:brain, :lifg, :boundary_drop], fn ->
          # ...run code...
        end, 200)

      # Many events — returns {result, list}
      {res, captured} =
        capture([[:a, :b], [:c, :d]], fn ->
          # ...run code...
        end, 200)

      # Assert no event
      :ok =
        refute_event([:brain, :lifg, :boundary_drop], fn ->
          # ...run code...
        end, 200)
  """

  # -------------- Public API --------------

  @doc """
  Capture telemetry while running `fun` for up to `timeout_ms`.

  * If `events` is a **single event path** (e.g., `[:brain, :lifg, :boundary_drop]`),
    this returns `{measurements, metadata}` for the **first** matching event, or raises
    `ExUnit.AssertionError` if none were seen.

  * If `events` is a **list of event paths** (e.g., `[[:a,:b],[:c,:d]]`),
    this returns `{result, captured}` where `captured` is a list of
    `{event, measurements, metadata}` for all events seen within the window.
  """
  @spec capture([atom()] | [[atom()]], (-> any()), non_neg_integer()) ::
          {map(), map()} | {any(), [{[atom()], map(), map()}]}
  def capture(events, fun, timeout_ms \\ 100)
      when is_list(events) and is_function(fun, 0) and is_integer(timeout_ms) do
    case normalize_events(events) do
      {:single, ev} -> capture_single(ev, fun, timeout_ms)
      {:multi, evs} -> capture_multi(evs, fun, timeout_ms)
    end
  end

  @doc """
  Asserts that **no** telemetry event in `events` is emitted while running `fun`,
  for up to `timeout_ms`. Returns `:ok` on success, raises `ExUnit.AssertionError` otherwise.

  Accepts either a single event path (list of atoms) or a list of paths.
  """
  @spec refute_event([atom()] | [[atom()]], (-> any()), non_neg_integer()) :: :ok
  def refute_event(events, fun, timeout_ms \\ 100)
      when is_list(events) and is_function(fun, 0) and is_integer(timeout_ms) do
    case normalize_events(events) do
      {:single, ev} -> do_refute([ev], fun, timeout_ms)
      {:multi, evs} -> do_refute(evs, fun, timeout_ms)
    end
  end

  @doc """
  Attach a probe to one or more telemetry `events` and return its handler id.
  """
  @spec attach_probe([[atom()]], pid(), String.t()) :: String.t()
  def attach_probe(events, pid \\ self(), name \\ default_name()) when is_list(events) do
    id = name

    :ok =
      :telemetry.attach_many(
        id,
        events,
        fn event, measurements, metadata, pid ->
          send(pid, {:telemetry, event, measurements, metadata})
        end,
        pid
      )

    id
  end

  @doc """
  Detach a previously attached probe by its `id`.
  """
  @spec detach_probe(String.t()) :: :ok
  def detach_probe(id) when is_binary(id) do
    _ = :telemetry.detach(id)
    :ok
  end

  @doc """
  Clear the current process mailbox (non-blocking). Useful to remove stale messages before a capture.
  """
  @spec drain_mailbox() :: [term()]
  def drain_mailbox do
    do_drain([])
  end

  # -------------- Internal --------------

  defp capture_single(event, fun, timeout_ms) do
    drain_mailbox()
    id = attach_probe([event])

    try do
      _result = fun.()
      msg = wait_for_event(event, timeout_ms)

      case msg do
        {:telemetry, ^event, meas, meta} ->
          {meas, meta}

        nil ->
          raise ExUnit.AssertionError,
                "Expected telemetry #{inspect(event)} but none was emitted within #{timeout_ms}ms"
      end
    after
      :ok = detach_probe(id)
    end
  end

  defp capture_multi(events, fun, timeout_ms) do
    drain_mailbox()
    id = attach_probe(events)

    try do
      result = fun.()
      msgs = collect_telemetry(timeout_ms)

      formatted =
        for {:telemetry, ev, meas, meta} <- msgs do
          {ev, meas, meta}
        end

      {result, formatted}
    after
      :ok = detach_probe(id)
    end
  end

  defp do_refute(events, fun, timeout_ms) do
    drain_mailbox()
    id = attach_probe(events)

    try do
      _ = fun.()
      msgs = collect_telemetry(timeout_ms)

      any? =
        Enum.any?(msgs, fn
          {:telemetry, ev, _m, _md} -> ev in events
          _ -> false
        end)

      if any? do
        raise ExUnit.AssertionError,
              "Expected NO telemetry in #{inspect(events)}, but some were emitted"
      else
        :ok
      end
    after
      :ok = detach_probe(id)
    end
  end

  defp normalize_events(events) do
    cond do
      events == [] ->
        {:multi, []}

      is_list(events) and Enum.all?(events, &is_list/1) ->
        {:multi, events}

      is_list(events) and Enum.all?(events, &is_atom/1) ->
        {:single, events}

      true ->
        raise ArgumentError, "events must be a path [:a,:b] or a list of paths [[:a,:b], [:c,:d]]"
    end
  end

  defp wait_for_event(event, timeout_ms) do
    receive do
      {:telemetry, ^event, _m, _md} = msg -> msg
      _other -> wait_for_event(event, timeout_ms)
    after
      timeout_ms -> nil
    end
  end

  defp collect_telemetry(timeout_ms) when is_integer(timeout_ms) and timeout_ms >= 0 do
    first_window = wait_for_msgs(timeout_ms, [])
    first_window ++ drain_new_msgs([])
  end

  defp wait_for_msgs(timeout_ms, acc) do
    receive do
      {:telemetry, _e, _m, _md} = msg -> wait_for_msgs(timeout_ms, [msg | acc])
    after
      timeout_ms -> Enum.reverse(acc)
    end
  end

  defp drain_new_msgs(acc) do
    receive do
      {:telemetry, _e, _m, _md} = msg -> drain_new_msgs([msg | acc])
    after
      0 -> Enum.reverse(acc)
    end
  end

  defp do_drain(acc) do
    receive do
      msg -> do_drain([msg | acc])
    after
      0 -> Enum.reverse(acc)
    end
  end

  defp default_name do
    "telemetry-probe-" <> Base.encode16(:crypto.strong_rand_bytes(6), case: :lower)
  end
end
