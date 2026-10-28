defmodule Symbrella.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children =
      maybe_pubsub_child() ++
        infrastructure_children() ++
        cognitive_children() ++
        consumer_children() ++
        maybe_ml_child() ++
        maybe_camera_child() ++
        clock_children()

    {:ok, supervisor} =
      Supervisor.start_link(
        children,
        strategy: :one_for_one,
        name: Symbrella.Supervisor
      )

    # Register global telemetry handlers after the
    # supervision tree has successfully started.
    :ok = Brain.Telemetry.attach!()
    :ok = Core.Curiosity.Bridge.attach()
    :ok = Core.Agency.AutonomyLoop.attach()

    # Restore persistent self-state only after its
    # supporting processes are available.
    _ =
      Brain.SelfContinuity.warm_start(
        scope:
          Application.get_env(
            :brain,
            :self_snapshot_scope,
            "runtime"
          )
      )

    {:ok, supervisor}
  end

  @impl true
  def config_change(_changed, _new, _removed) do
    :ok
  end

  # --------------------------------------------------
  # 1. PubSub
  # --------------------------------------------------

  defp maybe_pubsub_child do
    case Process.whereis(Symbrella.PubSub) do
      nil ->
        [{Phoenix.PubSub, name: Symbrella.PubSub}]

      _pid ->
        []
    end
  end

  # --------------------------------------------------
  # 2. Infrastructure
  # --------------------------------------------------

  defp infrastructure_children do
    [
      # Database must be available before memory,
      # lexical hydration, and persistent stores.
      Db,

      # Dynamic brain-cell infrastructure.
      {Registry, keys: :unique, name: Brain.Registry},
      {DynamicSupervisor, name: Brain.CellSup, strategy: :one_for_one},

      # Shared asynchronous work.
      {Task.Supervisor, name: Symbrella.TaskSup},

      # HTTP client infrastructure.
      {Finch, name: Lexicon.Finch},

      # The ETS chat-history owner must be supervised. Lazily starting it via
      # start_link/1 from a request/test process links its lifetime to that
      # caller and can terminate concurrent history reads.
      Core.Response.LlmChatHistoryOwner,

      # A separate protected semantic timeline, supervised independently of
      # the LLM prompt-history owner and individual request processes.
      Core.Discourse.Ledger.Owner,

      # Explicit, temporary human approvals for Colony replies. No polling or
      # outbound writes occur from this supervised store.
      Core.Social.ColonyDraftStore,

      # Local LLM ownership and boot control.
      {Llm, []},
      {Llm.BootGate, []},

      # Start the event bridge before the cognitive
      # regions begin publishing runtime activity.
      {Brain.Blackboard, []}
    ]
  end

  # --------------------------------------------------
  # 3. Cognitive foundations and processing
  # --------------------------------------------------

  defp cognitive_children do
    [
      # Neuromodulation and intent policy.
      {Brain.MoodCore, []},
      {Brain.MoodPolicy, []},

      # Working memory and brain coordination.
      Brain,

      # Receive learned Axon state predictions as observations only.
      # The receptor never actuates mood, memory, decisions or neural weights.
      Brain.Neural.SignalReceptor,

      # Ephemeral, experience-driven synaptic weights in shadow mode only.
      Brain.Synapses.Shadow,

      # Cognitive state and learning support.
      Brain.Amygdala,
      Brain.Cerebellum.Store,
      Brain.Cerebellum,
      {Brain.Hippocampus, keep: 300},
      Brain.Meta,

      # Executive control and goal management.
      Brain.PFC,
      {Brain.GoalStack, []},

      # Attention and evaluation.
      Brain.Thalamus,
      Brain.Temporal,
      Brain.OFC,
      {Brain.DLPFC, act_on_thalamus: true},
      {Brain.ACC, keep: 300},

      # Lexical and semantic infrastructure.
      # Initialize supporting regions before LIFG.
      Brain.PMTG,
      {Brain.ATL, keep: 300},

      # Stage1 uses mood and Cerebellum adjustments.
      {Brain.LIFG.Stage1, []},
      Brain.LIFG
    ]
  end

  # --------------------------------------------------
  # 4. Cognitive consumers
  # --------------------------------------------------

  defp consumer_children do
    [
      # Observes blackboard events.
      {Brain.SelfPortrait, []},

      # Processes curiosity proposals.
      Brain.Curiosity,

      # Bounded silent semantic investigation. Supervised at boot but inert
      # unless independently enabled; it never sends unsolicited chat.
      {Core.Curiosity.Investigator, []},

      # Separate session-scoped clarification receipts and verified lexical
      # feedback. Never rewrites symbolic facts or proof-based Curiosity.
      {Core.Curiosity.ClarificationLoop, []},

      # Observes self-portrait salience.
      Brain.DriveLoop
    ]
  end

  # --------------------------------------------------
  # 5. Optional machine learning
  # --------------------------------------------------

  defp maybe_ml_child do
    if Application.get_env(:brain, :ml_enabled, true) == true do
      [Brain.ML]
    else
      []
    end
  end

  # --------------------------------------------------
  # 6. Optional camera pipeline
  # --------------------------------------------------

  defp maybe_camera_child do
    camera_opts = Application.get_env(:brain, Brain.Camera, [])
    cortex_opts = Application.get_env(:brain, Brain.Visual.Cortex, [])

    # The visual cortex is a permanent cognitive region, not a camera driver.
    # Keep it supervised and queryable even without any camera configured.
    cortex = [{Brain.Visual.Cortex, cortex_opts}]

    if camera_enabled?(camera_opts) do
      bridge_opts = Application.get_env(:brain, Brain.Camera.ObservationBridge, [])

      # Attach both telemetry consumers BEFORE starting camera capture.
      cortex ++
        [
          {Brain.Camera.ObservationBridge, bridge_opts},
          {Brain.Camera, camera_opts}
        ]
    else
      cortex
    end
  end

  # --------------------------------------------------
  # 7. Periodic cognitive activity
  # --------------------------------------------------

  defp clock_children do
    [
      {Brain.CycleClock,
       Application.get_env(
         :brain,
         Brain.CycleClock,
         []
       )}
    ]
  end

  # --------------------------------------------------
  # Camera configuration helpers
  # --------------------------------------------------

  defp camera_enabled?(opts) when is_list(opts) do
    opts
    |> Keyword.get(:enabled?, false)
    |> camera_enabled_value?()
  end

  defp camera_enabled?(opts) when is_map(opts) do
    opts
    |> Map.get(:enabled?, false)
    |> camera_enabled_value?()
  end

  defp camera_enabled?(_), do: false

  defp camera_enabled_value?(value) do
    value in [
      true,
      "true",
      "1",
      1,
      :on,
      "on",
      :yes,
      "yes"
    ]
  end
end
