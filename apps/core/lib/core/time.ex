defmodule Core.Time do
  @moduledoc """
  Clock helpers.

  • Use `mono_ms/0` for durations, budgets, and elapsed time.
  • Use `system_ms/0` for persisted/logged timestamps.
  """

  @spec mono_ms() :: integer
  def mono_ms do
    System.monotonic_time(:millisecond)
  end

  @spec system_ms() :: integer
  def system_ms do
    System.system_time(:millisecond)
  end
end
