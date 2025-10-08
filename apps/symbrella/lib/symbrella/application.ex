defmodule Symbrella.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    # Ensure DETS dir exists for NegCache
    neg_path = Application.app_dir(:core, "priv/negcache/negcache.dets")
    File.mkdir_p!(Path.dirname(neg_path))

    children = [
      # ── DB (start first) ──────────────────────────────────────────────
      Db,

      # ── Brain infra (order matters) ───────────────────────────────────
      {Registry, keys: :unique, name: Brain.Registry},
      {DynamicSupervisor, name: Brain.CellSup, strategy: :one_for_one},

      # ── Caches / services ─────────────────────────────────────────────
      {Task.Supervisor, name: Symbrella.TaskSup},
      {Finch, name: Lexicon.Finch},
      {Core.NegCache, dets_path: neg_path, ttl: 30 * 24 * 60 * 60},

      # ── Brain servers ─────────────────────────────────────────────────
      Brain,
      Brain.LIFG,
      Brain.PMTG,
      {Brain.ATL, keep: 300},
      {Brain.Hippocampus, keep: 300},
      # Brain.AG,
      # Brain.MTL,
      {Brain.ACC, keep: 300}
      # Brain.BasalGanglia
    ]

    # Attach telemetry handlers
    Brain.Telemetry.attach!()

    Supervisor.start_link(children, strategy: :one_for_one, name: Symbrella.Supervisor)
  end
end
