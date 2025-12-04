# apps/symbrella_web/lib/symbrella_web/region/registry.ex
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

  @shape_defaults %{
    path: "",
    colors: {"#94A3B8", "#64748B"},
    anchor: {0, 0},
    tweak: %{dx: 0, dy: 0, s: 1.0}
  }

  @meta_defaults %{
    subtitle: "Module Summary",
    desc: "Overview of the selected module.",
    modules: [],
    telemetry: [],
    config_examples: []
  }

  # Registry-owned meta. These values override module-provided meta (if any).
  @meta_by_key %{
    lifg: %{
      subtitle: "Competitive Sense Selection",
      desc:
        "Selects the winning sense per token from si.sense_candidates with boundary guards and scoring " <>
          "(lex_fit, rel_prior, activation, intent_bias).",
      modules: [Brain.LIFG, Brain.LIFG.Stage1],
      telemetry: [
        [:brain, :lifg, :stage1, :score],
        [:brain, :lifg, :stage1, :winner],
        [:brain, :lifg, :stage1, :boundary_drop],
        [:brain, :lifg, :stage1, :chargram_violation]
      ],
      config_examples: [
        "config :brain, :lifg_defaults, margin_threshold: 0.15",
        "config :brain, :lifg_stage1_scores_mode, :all",
        "config :brain, :lifg_stage1_mwe_fallback, true"
      ]
    },
    pmtg: %{
      subtitle: "Lexical Retrieval Boost",
      desc:
        "Boosts/recovers sense candidates before LIFG using retrieval heuristics when confidence is low " <>
          "or tokens are needy.",
      modules: [Brain.PMTG],
      telemetry: [
        [:brain, :pmtg, :boost],
        [:brain, :pmtg, :no_mwe_senses]
      ],
      config_examples: [
        "config :brain, pmtg_mode: :boost",
        "config :brain, pmtg_margin_threshold: 0.15"
      ]
    },
    hippocampus: %{
      subtitle: "Episodic Recall",
      desc:
        "Stores and recalls episodes; can attach recall evidence to bias selection and planning. " <>
          "Supports memory/db/hybrid recall modes.",
      modules: [Brain.Hippocampus],
      telemetry: [
        [:brain, :hippocampus, :recall],
        [:brain, :hippocampus, :write]
      ],
      config_examples: [
        "config :brain, episodes_mode: :on",
        "config :brain, :hippo_db_defaults, recall_source: :memory, recall_k: 8"
      ]
    },
    acc: %{
      subtitle: "Conflict Monitor",
      desc:
        "Tracks competition/conflict signals and helps decide when to escalate or tighten thresholds.",
      modules: [Brain.ACC],
      telemetry: [[:brain, :acc, :conflict]],
      config_examples: ["config :brain, acc_conflict_tau: 0.50"]
    },
    thalamus: %{
      subtitle: "Routing & Gating",
      desc:
        "Routes signals between regions and participates in gating policies (attention/WM routing).",
      modules: [Brain.Thalamus],
      telemetry: [[:brain, :thalamus, :gate]],
      config_examples: []
    },
    basal_ganglia: %{
      subtitle: "Action Selection",
      desc:
        "Action selection and gating signals (what enters focus/WM; inhibit/permit competing actions).",
      modules: [Brain.BasalGanglia],
      telemetry: [[:brain, :basal_ganglia, :gate]],
      config_examples: []
    },
    bg: %{
      subtitle: "Action Selection",
      desc:
        "Action selection and gating signals (what enters focus/WM; inhibit/permit competing actions).",
      modules: [Brain.BasalGanglia],
      telemetry: [[:brain, :basal_ganglia, :gate]],
      config_examples: []
    }
  }

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
    base = @shape_defaults

    result =
      case module_for(key) do
        nil ->
          base

        mod ->
          if function_exported?(mod, :defn, 0) do
            safe_apply(mod, :defn, %{}) || %{}
          else
            %{
              path: safe_apply(mod, :path, "") || "",
              colors: safe_apply(mod, :colors, base.colors) || base.colors,
              anchor: safe_apply(mod, :anchor, base.anchor) || base.anchor,
              tweak: safe_apply(mod, :tweak, base.tweak) || base.tweak
            }
          end
      end

    result
    |> ensure_shape_defaults()
    |> ensure_identity(key)
    |> ensure_meta_defaults()
    |> apply_meta_override(key)
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
    basal_ganglia: "Basal Ganglia",
    hippocampus: "Hippocampus",
    thalamus: "Thalamus",
    cerebellum: "Cerebellum",
    frontal: "Frontal",
    temporal: "Temporal",
    prefrontal: "Prefrontal",
    salience: "Salience"
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

  defp ensure_shape_defaults(%{} = defn) do
    defn
    |> Map.put_new(:path, @shape_defaults.path)
    |> Map.put_new(:colors, @shape_defaults.colors)
    |> Map.put_new(:anchor, @shape_defaults.anchor)
    |> Map.put_new(:tweak, @shape_defaults.tweak)
  end

  defp ensure_identity(%{} = defn, key) do
    lbl = label_for(key)

    defn
    |> Map.put_new(:key, key)
    |> Map.put_new(:label, lbl)
    |> Map.put_new(:title, lbl)
  end

  defp ensure_meta_defaults(%{} = defn) do
    defn
    |> Map.put_new(:subtitle, @meta_defaults.subtitle)
    |> Map.put_new(:desc, @meta_defaults.desc)
    |> Map.put_new(:modules, @meta_defaults.modules)
    |> Map.put_new(:telemetry, @meta_defaults.telemetry)
    |> Map.put_new(:config_examples, @meta_defaults.config_examples)
  end

  defp apply_meta_override(%{} = defn, key) do
    # allow :bg to shadow :basal_ganglia (or other alias keys)
    meta =
      Map.get(@meta_by_key, key) ||
        Map.get(@meta_by_key, alias_key(key)) ||
        %{}

    Map.merge(defn, meta)
  end

  defp alias_key(:bg), do: :basal_ganglia
  defp alias_key(other), do: other

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
