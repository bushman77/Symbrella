# apps/symbrella/lib/symbrella/application.ex
defmodule Symbrella.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    # Ensure DETS dir exists for NegCache
    # neg_path = Application.app_dir(:core, "priv/negcache/negcache.dets")
    # File.mkdir_p!(Path.dirname(neg_path))

    app_children = [
      # ── DB ────────────────────────────────────────────────────────────────
      Db,

      # ── Foundations / infra (order matters) ───────────────────────────────
      {Registry, keys: :unique, name: Brain.Registry},
      {DynamicSupervisor, name: Brain.CellSup, strategy: :one_for_one},

      # ── Caches / services ────────────────────────────────────────────────
      {Task.Supervisor, name: Symbrella.TaskSup},
      {Finch, name: Lexicon.Finch},
      # {Core.NegCache, dets_path: neg_path, ttl: 30 * 24 * 60 * 60},

      # ── Local LLM runner (llama.cpp / llama-server) ──────────────────────
      # NOTE: This is intentionally owned by the umbrella-root supervisor.
      {Llm, []},

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
      Brain.Meta,
      Brain.PFC,
      Brain.Thalamus,
      Brain.Temporal,
      Brain.OFC,
      {Brain.DLPFC, act_on_thalamus: true},
      {Brain.ACC, keep: 300},
      {Brain.CycleClock, Application.get_env(:brain, Brain.CycleClock, [])},

      # Event bridge first, then consumers
      {Brain.Blackboard, []},

      # Self model (subscribes to Blackboard topic)
      {Brain.SelfPortrait, []},

      # ML consumer that finalizes turn records
      Brain.ML

      # 🚫 Do NOT start SymbrellaWeb.Endpoint here.
      # The web app owns its endpoint under SymbrellaWeb.Application.
    ]

    children = maybe_pubsub_child() ++ app_children

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

  defp maybe_pubsub_child do
    case Process.whereis(Symbrella.PubSub) do
      nil -> [{Phoenix.PubSub, name: Symbrella.PubSub}]
      _pid -> []
    end
  end
end
