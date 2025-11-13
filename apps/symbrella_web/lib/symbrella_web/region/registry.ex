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

  # ── Public: SVG Region discovery ───────────────────────────────────────────

  # Runtime-only discovery (no compile-time calls to region modules).
  # Accepts modules that expose either:
  #   • defn/0   OR
  #   • key/0, path/0, colors/0, anchor/0, tweak/0
  def modules do
    @modules
    |> Enum.uniq()
    |> Enum.filter(&region_module?/1)
  end

  def keys, do: Map.keys(by_key_map())
  def available?(key), do: Map.has_key?(by_key_map(), key)
  def module_for(key), do: Map.get(by_key_map(), key)

  # Safe definition fetch (with sensible defaults if a module is missing).
  # If a region module implements defn/0 we use it directly; otherwise we
  # assemble a defn map from the individual functions.
  def defn(key) when is_atom(key) do
    case module_for(key) do
      nil ->
        %{
          path: "",
          colors: {"#94A3B8", "#64748B"},
          anchor: {0, 0},
          tweak: %{dx: 0, dy: 0, s: 1.0}
        }

      mod ->
        if function_exported?(mod, :defn, 0) do
          safe_apply(mod, :defn, []) ||
            %{
              path: "",
              colors: {"#94A3B8", "#64748B"},
              anchor: {0, 0},
              tweak: %{dx: 0, dy: 0, s: 1.0}
            }
        else
          %{
            path: safe_apply(mod, :path, "") || "",
            colors: safe_apply(mod, :colors, {"#94A3B8", "#64748B"}) || {"#94A3B8", "#64748B"},
            anchor: safe_apply(mod, :anchor, {0, 0}) || {0, 0},
            tweak: safe_apply(mod, :tweak, %{dx: 0, dy: 0, s: 1.0}) || %{dx: 0, dy: 0, s: 1.0}
          }
        end
    end
  end

  # ── Labels & Brain process mapping ─────────────────────────────────────────

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

  @brain_process_map %{
    lifg: Brain.LIFG,
    pmtg: Brain.PMTG,
    atl: Brain.ATL,
    acc: Brain.ACC,
    ofc: Brain.OFC,
    dlpfc: Brain.DLPFC,
    vmpfc: Brain.VMPFC,
    dmpfc: Brain.DMPFC,
    fpc: Brain.FPC,
    bg: Brain.BasalGanglia,
    basal_ganglia: Brain.BasalGanglia,
    hippocampus: Brain.Hippocampus,
    cerebellum: Brain.Cerebellum,
    thalamus: Brain.Thalamus,
    occipital: Brain.Occipital,
    parietal: Brain.Parietal,
    frontal: Brain.Frontal,
    prefrontal: Brain.Prefrontal,
    temporal: Brain.Temporal,
    salience: Brain.Salience
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
        # bounded by our own keys
        ArgumentError -> String.to_atom(key)
      end

    process_for(atom)
  end

  # ── internal helpers ───────────────────────────────────────────────────────

  defp by_key_map do
    modules()
    |> Enum.reduce(%{}, fn mod, acc ->
      key =
        cond do
          function_exported?(mod, :key, 0) ->
            safe_apply(mod, :key, nil)

          function_exported?(mod, :defn, 0) ->
            case safe_apply(mod, :defn, %{}) do
              %{key: k} when is_atom(k) -> k
              _ -> nil
            end

          true ->
            nil
        end

      if is_atom(key), do: Map.put(acc, key, mod), else: acc
    end)
  end

  defp region_module?(mod) when is_atom(mod) do
    loaded? =
      try do
        # strictly boolean; avoids {:error, :nofile}
        Code.ensure_loaded?(mod)
      rescue
        _ -> false
      end

    loaded? and
      (function_exported?(mod, :defn, 0) or
         (function_exported?(mod, :key, 0) and
            function_exported?(mod, :path, 0) and
            function_exported?(mod, :colors, 0) and
            function_exported?(mod, :anchor, 0) and
            function_exported?(mod, :tweak, 0)))
  end

  defp region_module?(_), do: false

  defp titlecase(s) do
    s
    |> String.downcase()
    |> String.split(" ", trim: true)
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end

  defp safe_apply(mod, fun, default) do
    try do
      apply(mod, fun, [])
    rescue
      _ -> default
    catch
      _, _ -> default
    end
  end
end
