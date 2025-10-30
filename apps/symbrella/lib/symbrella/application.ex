defmodule Symbrella.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    # Ensure DETS dir exists for NegCache
    neg_path = Application.app_dir(:core, "priv/negcache/negcache.dets")
    File.mkdir_p!(Path.dirname(neg_path))

    # Optional: LLM boot options (env-driven; reserved for future use)
    _llm_opts =
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
      # ── DB ────────────────────────────────────────────────────────────────
      Db,

      # ── Foundations / infra (order matters) ───────────────────────────────
      {Registry, keys: :unique, name: Brain.Registry},
      {DynamicSupervisor, name: Brain.CellSup, strategy: :one_for_one},

      # ── Caches / services ────────────────────────────────────────────────
      {Task.Supervisor, name: Symbrella.TaskSup},
      {Finch, name: Lexicon.Finch},
      {Core.NegCache, dets_path: neg_path, ttl: 30 * 24 * 60 * 60},

      # ── Autonomous workers (first-class, decoupled) ──────────────────────
      Curiosity,        # emits [:curiosity, :proposal]
      Core.Curiosity,   # sweeps NegCache

      # ── Mood / policy should boot before LIFG.Stage1 to feed mood events ─
      {Brain.MoodCore, []},
      {Brain.MoodPolicy, []},

      # ── Stage-1 scoring server (mood-nudged) ─────────────────────────────
      {Brain.LIFG.Stage1, []},

      # ── Brain servers / timing ───────────────────────────────────────────
      Brain,
      Brain.Cerebellum,
      Brain.LIFG,
      Brain.PMTG,
      {Brain.ATL, keep: 300},
      {Brain.Hippocampus, keep: 300},
      Brain.PFC,
      Brain.Thalamus,
      Brain.Temporal,
      Brain.OFC,
      {Brain.DLPFC, act_on_thalamus: true},
      {Brain.ACC, keep: 300},
      {Brain.CycleClock, Application.get_env(:brain, Brain.CycleClock, [])},
      {Brain.Blackboard, []},

      # ── Web endpoint LAST (owns PubSub; do NOT start Phoenix.PubSub here) ─
      SymbrellaWeb.Endpoint
    ]

    {:ok, sup} =
      Supervisor.start_link(children, strategy: :one_for_one, name: Symbrella.Supervisor)

    # Attach telemetry handlers AFTER the tree is live (safe-guarded)
    safe_attach(fn -> Brain.Telemetry.attach!() end)
    safe_attach(fn -> Core.Curiosity.Bridge.attach() end)

    {:ok, sup}
  end

  @impl true
  def config_change(changed, _new, removed) do
    if function_exported?(SymbrellaWeb.Endpoint, :config_change, 2) do
      SymbrellaWeb.Endpoint.config_change(changed, removed)
    end

    :ok
  end

  # ── helpers ────────────────────────────────────────────────────────────────

  defp safe_attach(fun) when is_function(fun, 0) do
    try do
      fun.()
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end
end

