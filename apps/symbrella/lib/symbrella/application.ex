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
      {Phoenix.PubSub, name: Symbrella.PubSub},

      # ── DB ────────────────────────────────────────────────────────────────
      Db,

      # ── Foundations / infra (order matters) ───────────────────────────────
      {Registry, keys: :unique, name: Brain.Registry},
      {DynamicSupervisor, name: Brain.CellSup, strategy: :one_for_one},

      # ── Caches / services ────────────────────────────────────────────────
      {Task.Supervisor, name: Symbrella.TaskSup},
      {Finch, name: Lexicon.Finch},
      {Core.NegCache, dets_path: neg_path, ttl: 30 * 24 * 60 * 60},

      # ── Background maintenance (decoupled) ───────────────────────────────
      # NOTE: The standalone Curiosity app has been removed (Checklist item 1).
      # Keep Core.Curiosity (NegCache sweeps) for now; fold later into Brain.Idle.

      # ── Mood / policy should boot before LIFG.Stage1 to feed mood events ─
      {Brain.MoodCore, []},
      {Brain.MoodPolicy, []},

      # ── Stage-1 scoring server (mood-nudged) ─────────────────────────────
      {Brain.LIFG.Stage1, []},

      # ── Brain servers / timing ───────────────────────────────────────────
      Brain,
      Brain.Amygdala,
      Brain.Cerebellum,
      Brain.LIFG,
      Brain.PMTG,
      {Brain.ATL, keep: 300},
      Brain.Curiosity,
      {Brain.Hippocampus, keep: 300},
      Brain.PFC,
      Brain.Thalamus,
      Brain.Temporal,
      Brain.OFC,
      {Brain.DLPFC, act_on_thalamus: true},
      {Brain.ACC, keep: 300},
      {Brain.CycleClock, Application.get_env(:brain, Brain.CycleClock, [])},

      # Event bridge first, then ML consumer that finalizes turn records
      {Brain.Blackboard, []},
      Brain.ML

      # 🚫 Do NOT start SymbrellaWeb.Endpoint here.
      # The web app owns its endpoint under SymbrellaWeb.Application.
    ]

    {:ok, sup} =
      Supervisor.start_link(children, strategy: :one_for_one, name: Symbrella.Supervisor)

    # Attach telemetry handlers AFTER the tree is live (safe-guarded)
    safe_attach(fn -> Brain.Telemetry.attach!() end)
    # NOTE: Bridge stays for now; when telemetry topics migrate, we'll remove/replace.
    safe_attach(fn -> Core.Curiosity.Bridge.attach() end)

    {:ok, sup}
  end

  @impl true
  def config_change(_changed, _new, _removed) do
    # Root doesn’t own the Endpoint; nothing to forward here.
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

