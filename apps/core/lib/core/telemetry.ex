defmodule Core.Telemetry do
  @moduledoc """
  Safe wrapper around :telemetry with zero compile-time coupling.

  - `emit/3` degrades to no-op if :telemetry isn't present
  - `attach/4`, `attach_many/4`, `detach/1` mirror :telemetry API safely
  """

  @type event :: [atom()]
  @type handler_id :: term()
  @type handler_fun :: (event(), map(), map(), term() -> any())

  @spec emit(event(), map(), map()) :: :ok
  def emit(event, measurements, metadata \\ %{}) when is_list(event) and is_map(measurements) do
    if Code.ensure_loaded?(:telemetry) and :erlang.function_exported(:telemetry, :execute, 3) do
      _ = apply(:telemetry, :execute, [event, measurements, metadata])
    end
    :ok
  end

  @doc "Safely attach a single handler to one event."
  @spec attach(handler_id(), event(), handler_fun(), term()) :: :ok | {:error, :already_exists}
  def attach(handler_id, event, fun, config \\ nil)
      when is_list(event) and is_function(fun, 4) do
    if Code.ensure_loaded?(:telemetry) and :erlang.function_exported(:telemetry, :attach, 4) do
      apply(:telemetry, :attach, [handler_id, event, fun, config])
    else
      :ok
    end
  end

  @doc "Safely attach a handler to many events."
  @spec attach_many(handler_id(), [event()], handler_fun(), term()) ::
          :ok | {:error, :already_exists}
  def attach_many(handler_id, events, fun, config \\ nil)
      when is_list(events) and is_function(fun, 4) do
    if Code.ensure_loaded?(:telemetry) and :erlang.function_exported(:telemetry, :attach_many, 4) do
      apply(:telemetry, :attach_many, [handler_id, events, fun, config])
    else
      :ok
    end
  end

  @doc "Safely detach a handler."
  @spec detach(handler_id()) :: :ok
  def detach(handler_id) do
    if Code.ensure_loaded?(:telemetry) and :erlang.function_exported(:telemetry, :detach, 1) do
      _ = apply(:telemetry, :detach, [handler_id])
    end
    :ok
  end
end

