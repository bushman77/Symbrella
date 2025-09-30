defmodule Core.Brain do
  @moduledoc """
  Core-side helpers related to Brain integration.

  We avoid wrappers in the main pipeline; callers should use direct GenServer calls, e.g.:

      GenServer.cast(Brain, {:activate_cells, rows, payload})

  This module exists for small, pure utility helpers only.
  """

  @doc false
  def normalize_id(nil), do: nil
  def normalize_id(id) when is_binary(id), do: id
  def normalize_id(id), do: to_string(id)

  @doc false
  def now_ms, do: System.system_time(:millisecond)
end

