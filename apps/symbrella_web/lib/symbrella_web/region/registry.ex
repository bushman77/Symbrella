defmodule SymbrellaWeb.Region.Registry do
  @moduledoc false

  # List of intended region modules. It's OK if some aren't defined yet.
  @modules [
    SymbrellaWeb.Region.Frontal,
    SymbrellaWeb.Region.Temporal,
    SymbrellaWeb.Region.Cerebellum,
    SymbrellaWeb.Region.Prefrontal,
    SymbrellaWeb.Region.Dlpfc,
    SymbrellaWeb.Region.Vmpfc,
    SymbrellaWeb.Region.Dmpfc,
    SymbrellaWeb.Region.Fpc,
    SymbrellaWeb.Region.Lifg,
    SymbrellaWeb.Region.Ofc,
    SymbrellaWeb.Region.Acc,
    SymbrellaWeb.Region.Thalamus,
    SymbrellaWeb.Region.BasalGanglia,
    SymbrellaWeb.Region.Hippocampus,
    SymbrellaWeb.Region.Pmtg,
    SymbrellaWeb.Region.Atl,
    SymbrellaWeb.Region.Salience
  ]

  # Runtime-only discovery (no compile-time calls to region modules)
  def modules do
    Enum.filter(@modules, fn mod ->
      # Don’t force compile; just include if it's already loaded or compilable
      Code.ensure_compiled?(mod) and
        function_exported?(mod, :key, 0) and
        function_exported?(mod, :path, 0) and
        function_exported?(mod, :colors, 0) and
        function_exported?(mod, :anchor, 0) and
        function_exported?(mod, :tweak, 0)
    end)
  end

  defp by_key_map do
    Enum.reduce(modules(), %{}, fn mod, acc ->
      key =
        if function_exported?(mod, :key, 0) do
          apply(mod, :key, [])
        else
          nil
        end

      if key, do: Map.put(acc, key, mod), else: acc
    end)
  end

  def module_for(key), do: Map.get(by_key_map(), key)
  def available?(key),  do: Map.has_key?(by_key_map(), key)
  def keys,             do: Map.keys(by_key_map())

  # Safe definition fetch (with sensible defaults if a module is missing)
  def defn(key) do
    case module_for(key) do
      nil ->
        %{
          path: "",
          colors: {"#94A3B8", "#64748B"},
          anchor: {0, 0},
          tweak: %{dx: 0, dy: 0, s: 1.0}
        }

      mod ->
        safe = fn fun, default ->
          if function_exported?(mod, fun, 0), do: apply(mod, fun, []), else: default
        end

        %{
          path:   safe.(:path,   ""),
          colors: safe.(:colors, {"#94A3B8", "#64748B"}),
          anchor: safe.(:anchor, {0, 0}),
          tweak:  safe.(:tweak,  %{dx: 0, dy: 0, s: 1.0})
        }
    end
  end
end

