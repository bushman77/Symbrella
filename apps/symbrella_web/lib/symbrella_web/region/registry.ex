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

  # ----- Public: SVG Region discovery (unchanged) -----------------------------

  # Runtime-only discovery (no compile-time calls to region modules)
  def modules do
    Enum.filter(@modules, fn mod ->
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

  # ----- New: Labels & Brain process mapping ---------------------------------

  @labels %{
    lifg: "LIFG",
    pmtg: "pMTG",
    atl: "ATL",
    acc: "ACC",
    ofc: "OFC",
    dlpfc: "DLPFC",
    vmpfc: "VMPFC",
    dmpfc: "DMPFC",
    fpc: "FPC",
    bg: "Basal Ganglia",
    basal_ganglia: "Basal Ganglia"
  }

  @doc """
  Human-friendly label for a region key.
  - Keeps acronyms UPPERCASE.
  - Falls back to Capitalized words for normal keys.
  """
  def label_for(key) when is_atom(key) do
    Map.get(@labels, key, key |> Atom.to_string() |> String.replace("_", " ") |> titlecase())
  end

  def label_for(key) when is_binary(key) do
    try do
      label_for(String.to_existing_atom(key))
    rescue
      _ -> key |> String.replace("_", " ") |> titlecase()
    end
  end

  defp titlecase(s) do
    s
    |> String.downcase()
    |> String.split(" ", trim: true)
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end

  @brain_process_map %{
    lifg:        Brain.LIFG,
    pmtg:        Brain.PMTG,
    atl:         Brain.ATL,
    acc:         Brain.ACC,
    ofc:         Brain.OFC,
    dlpfc:       Brain.DLPFC,
    vmpfc:       Brain.VMPFC,
    dmpfc:       Brain.DMPFC,
    fpc:         Brain.FPC,
    bg:          Brain.BasalGanglia,
    basal_ganglia: Brain.BasalGanglia,
    hippocampus: Brain.Hippocampus,
    cerebellum:  Brain.Cerebellum,
    thalamus:    Brain.Thalamus,
    occipital:   Brain.Occipital,
    parietal:    Brain.Parietal,
    frontal:     Brain.Frontal,
    prefrontal:  Brain.Prefrontal,
    temporal:    Brain.Temporal,
    salience:    Brain.Salience
  }

  @doc """
  Returns the **Brain** process module for a region key (used by BrainLive).
  - Uses explicit overrides for acronyms (e.g., :lifg -> Brain.LIFG).
  - Falls back to `Module.concat(Brain, Macro.camelize(key))` for normal names.
  """
  def process_for(key) when is_atom(key) do
    Map.get(@brain_process_map, key, Module.concat(Brain, Macro.camelize(Atom.to_string(key))))
  end

  def process_for(key) when is_binary(key) do
    atom =
      try do
        String.to_existing_atom(key)
      rescue
        ArgumentError ->
          # Careful with atoms; but keys come from our own set, so bounded.
          String.to_atom(key)
      end

    process_for(atom)
  end
end

