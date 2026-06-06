defmodule Core.Response.OverrideSkills.Decision do
  @moduledoc false

  @spec put(map(), keyword()) :: map()
  def put(decision, kvs) when is_list(kvs) do
    Enum.reduce(kvs, decision, fn {k, v}, acc -> Map.put(acc, k, v) end)
  end

  @spec add_override(map(), atom()) :: map()
  def add_override(decision, flag) do
    existing = Map.get(decision, :overrides)
    Map.put(decision, :overrides, add_override_value(existing, flag))
  end

  defp add_override_value(nil, flag), do: [flag]
  defp add_override_value(list, flag) when is_list(list), do: Enum.uniq([flag | list])
  defp add_override_value(map, flag) when is_map(map), do: Map.put(map, flag, true)
  defp add_override_value(other, flag), do: Enum.uniq([flag, other])
end
