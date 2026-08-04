# apps/symbrella/lib/symbrella/application.ex
defmodule Symbrella.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    # Ensure DETS dir exists for NegCache
    # neg_path = Application.app_dir(:core, "priv/negcache/negcache.dets")
    # File.mkdir_p!(Path.dirname(neg_path))

    app_children =
      [
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
        {Llm.BootGate, []},

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
        {Brain.GoalStack, []},
        Brain.Thalamus,
        Brain.Temporal,
        Brain.OFC,
        {Brain.DLPFC, act_on_thalamus: true},
        {Brain.ACC, keep: 300},
        {Brain.CycleClock, Application.get_env(:brain, Brain.CycleClock, [])},

        # Event bridge first, then consumers
        {Brain.Blackboard, []}
      ] ++
        maybe_camera_child() ++
        [
          # Self model (subscribes to Blackboard topic)
          {Brain.SelfPortrait, []},
          Brain.DriveLoop,
          # ML consumer that finalizes turn records
          Brain.ML

          # 🚫 Do NOT start SymbrellaWeb.Endpoint here.
          # The web app owns its endpoint under SymbrellaWeb.Application.
        ]

    children = maybe_pubsub_child() ++ app_children

    {:ok, sup} =
      Supervisor.start_link(children, strategy: :one_for_one, name: Symbrella.Supervisor)

    # Attach telemetry handlers AFTER the tree is live. Attach failures should
    # fail boot instead of disappearing; duplicate attach is handled by the
    # attaching modules.
    :ok = Brain.Telemetry.attach!()

    # NOTE: Bridge stays for now; when telemetry topics migrate, we'll remove/replace.
    :ok = Core.Curiosity.Bridge.attach()

    _ =
      Brain.SelfContinuity.warm_start(
        scope: Application.get_env(:brain, :self_snapshot_scope, "runtime")
      )

    {:ok, sup}
  end

  @impl true
  def config_change(_changed, _new, _removed) do
    # Root doesn’t own the Endpoint; nothing to forward here.
    :ok
  end

  # ── helpers ────────────────────────────────────────────────────────────────
  defp maybe_pubsub_child do
    case Process.whereis(Symbrella.PubSub) do
      nil -> [{Phoenix.PubSub, name: Symbrella.PubSub}]
      _pid -> []
    end
  end

  defp maybe_camera_child do
    opts = Application.get_env(:brain, Brain.Camera, [])

    if camera_enabled?(opts) do
      bridge_opts = Application.get_env(:brain, Brain.Camera.ObservationBridge, [])
      decoder_opts = Application.get_env(:brain, Brain.Visual.DebugDecoder, [])

      [
        {Brain.Camera, opts},
        {Brain.Camera.ObservationBridge, bridge_opts},
        {Brain.Visual.DebugDecoder, decoder_opts}
      ]
    else
      []
    end
  end

  defp camera_enabled?(opts) when is_list(opts) do
    Keyword.get(opts, :enabled?, false) in [true, "true", "1", 1, :on, "on", :yes, "yes"]
  end

  defp camera_enabled?(opts) when is_map(opts) do
    opts
    |> Map.get(:enabled?, false)
    |> camera_enabled_value?()
  end

  defp camera_enabled?(_), do: false

  defp camera_enabled_value?(value) do
    value in [true, "true", "1", 1, :on, "on", :yes, "yes"]
  end
end
