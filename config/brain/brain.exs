# config/brain/brain.exs
import Config

# ─────────────────────────── Brain (central) ──────────────────────
config :brain,
  pubsub: Symbrella.PubSub,
  pmtg_mode: :boost,
  pmtg_margin_threshold: 0.15,
  pmtg_window_keep: 50,
  lifg_defaults: [inject_child_unigrams?: true],
  lifg_stage1_weights: %{
    lex_fit: 0.20,
    context_fit: 0.40,
    rel_prior: 0.20,
    activation: 0.10,
    intent_bias: 0.10
  },
  lifg_stage1_scores_mode: :all,
  lifg_min_margin: 0.05,
  lifg_stage1_mwe_fallback: true,
  lifg_slate_filter_rules: [
    %{lemma: "a", allow: [:det, :article, :particle], drop_others?: true},
    %{lemma: "A", allow: [:det, :article, :particle], drop_others?: true},
    %{lemma: "eat", allow: [:verb], drop_others?: true}
  ],
  acc_conflict_tau: 0.50,
  wm_decay_lambda: 0.12,
  wm_score_min: 0.0,
  wm_score_max: 1.0,
  lifg_mood_weights: %{expl: 0.02, inhib: -0.03, vigil: 0.02, plast: 0.00},
  lifg_mood_cap: 0.05,
  hpc_half_life_ms: 300_000,
  hpc_window_keep: 300,
  hpc_min_jaccard: 0.0,
  hpc_recall_limit: 3,
  hippo_priming: :on,
  hippo_priming_vectors: %{
    success: %{da: 0.02, "5ht": -0.01, glu: 0.02, ne: 0.01},
    failure: %{da: -0.01, "5ht": 0.02, glu: 0.00, ne: 0.02}
  },

  # Gating adjustments to allow first WM inserts
  gate_threshold: 0.25,
  prefer_sources: [:curiosity, :hippocampus, :pmtg, :lifg, :runtime, :recency, :intent],
  fullness_penalty_mult: 0.10,
  thalamus_acc_alpha: 0.15,
  thalamus_mood_weights: %{expl: 0.10, inhib: -0.02, vigil: -0.01, plast: 0.08},
  thalamus_mood_cap: 0.30,
  capacity: 12

config :brain, :blackboard_window_size, 100
config :brain, :log_cognition_pipeline?, false
config :brain, :log_lifg_stage1?, false

config :brain, Brain.DriveLoop, idle_status_interval_ms: 0

config :brain, Brain.MoodCore,
  half_life_ms: %{da: 30_000, "5ht": 60_000, glu: 90_000, ne: 45_000},
  clock: :cycle,
  init: %{da: 0.35, "5ht": 0.50, glu: 0.40, ne: 0.50}
