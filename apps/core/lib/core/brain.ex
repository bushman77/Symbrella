defmodule Core.Brain do
  @moduledoc """
  Core-side helpers related to Brain integration.

  Core treats Brain as a service boundary. Keep direct process messages out of
  pipeline modules and prefer the named functions here or on the `Brain` public
  API.
  """

  @doc false
  def normalize_id(nil), do: nil
  def normalize_id(id) when is_binary(id), do: id
  def normalize_id(id), do: to_string(id)

  @doc false
  def now_ms, do: System.system_time(:millisecond)

  @spec activate_cells(list() | map(), map()) :: :ok
  def activate_cells(rows_or_ids, payload \\ %{delta: 1}) when is_map(payload) do
    Core.Brain.Runtime.apply_if_exported(Brain, :activate_cells, [rows_or_ids, payload], :ok)
  end
end
