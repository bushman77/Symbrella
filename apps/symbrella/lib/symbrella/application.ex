defmodule Symbrella.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    # Ensure DETS dir exists for NegCache
    neg_path = Application.app_dir(:core, "priv/negcache/negcache.dets")
    File.mkdir_p!(Path.dirname(neg_path))

    # LLM boot options (env-driven where available)
    llm_opts =
      [
        base_url: System.get_env("OLLAMA_API_BASE"),
        model: System.get_env("OLLAMA_MODEL"),
        timeout: 60_000,
        auto_start_on_boot?: true,
        pull_on_boot?: true,
        warm_on_boot?: true,
        boot_timeout: 60_000
      ]
      |> Enum.reject(fn {_k, v} -> is_nil(v) end)

    children = [
      # ── DB (start first) ──────────────────────────────────────────────
      Db,

      # ── Foundations / infra (order matters) ───────────────────────────
      {Registry, keys: :unique, name: Brain.Registry},
      {DynamicSupervisor, name: Brain.CellSup, strategy: :one_for_one},

      # ── Caches / services ─────────────────────────────────────────────
      {Task.Supervisor, name: Symbrella.TaskSup},
      {Finch, name: Lexicon.Finch},
      {Core.NegCache, dets_path: neg_path, ttl: 30 * 24 * 60 * 60},

      # ── Autonomous workers (first-class, decoupled) ───────────────────
      Curiosity,        # standalone, emits [:curiosity, :proposal]
      Core.Curiosity,   # Core sweeper that re-probes NegCache

      # ── Brain timing & phase coordination ─────────────────────────────

      # ── Brain servers ─────────────────────────────────────────────────
      Brain,
      Brain.LIFG,
      Brain.PMTG,
      {Brain.ATL, keep: 300},
      {Brain.Hippocampus, keep: 300},
      Brain.Thalamus,
      Brain.OFC,
      {Brain.DLPFC, act_on_thalamus: true},
      # Brain.AG,
      # Brain.MTL,
      {Brain.ACC, keep: 300},
      # Curiosity is already started above (standalone)
      # Brain.BasalGanglia
      # --- Global brain clock + neuromodulators ---
      {Brain.CycleClock, Application.get_env(:brain, Brain.CycleClock, [])},
      {Brain.MoodCore, []}

    ]

    # Start the supervision tree
    {:ok, sup} = Supervisor.start_link(children, strategy: :one_for_one, name: Symbrella.Supervisor)

    # Attach telemetry handlers AFTER the tree is live
    Brain.Telemetry.attach!()
    # Bridge Curiosity proposals → Core.Curiosity sweeps (no UI coupling)
    Core.Curiosity.Bridge.attach()
    # Optionally: feed more misses automatically (LLM/brain → NegCache)
    # Core.Curiosity.Events.attach()

    {:ok, sup}
  end
end

