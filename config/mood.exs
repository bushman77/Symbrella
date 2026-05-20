# config/mood.exs
import Config

# -------------------------------------------------------------------
# Core neuromodulator dynamics (Brain.MoodCore)
# -------------------------------------------------------------------
config :brain, Brain.MoodCore,
  # Baseline neuromodulators (more “chill” + slightly optimistic)
  baseline: %{
    # a bit more reward / exploration
    da: 0.40,
    # more serotonin = calmer default
    "5ht": 0.60,
    glu: 0.40,
    # slightly *less* default vigilance
    ne: 0.40
  },
  # Half-life (ms) per neuromodulator — how fast it drifts back to baseline
  half_life_ms: %{
    da: 30_000,
    "5ht": 60_000,
    glu: 90_000,
    ne: 45_000
  },
  # Max delta we allow per “tick” (MoodCore will clamp)
  max_delta_per_tick: 0.06,
  saturation_ticks: 45,
  shock_threshold: 0.35,
  # Let CycleClock drive ticks (hooked via [:brain, :cycle, :tick])
  clock: :cycle,
  # Optional immediate override of baseline (we’ll keep empty for now)
  init: %{}

# -------------------------------------------------------------------
# Canonical affect runtime tuning surface (P-301)
# -------------------------------------------------------------------
# This is intentionally config-only for now. Runtime modules still read their
# existing settings until the P-302/P-303 migration steps wire them in.
config :brain, :affect_runtime,
  neuromodulators: [
    aliases: %{
      dopamine: :da,
      serotonin: :"5ht",
      glutamate: :glu,
      norepinephrine: :ne
    },
    baseline: %{da: 0.40, "5ht": 0.60, glu: 0.40, ne: 0.40},
    init: %{},
    half_life_ms: %{da: 30_000, "5ht": 60_000, glu: 90_000, ne: 45_000},
    max_delta_per_tick: 0.06,
    saturation_ticks: 45,
    shock_threshold: 0.35,
    trace_limit: 12
  ],
  derived_indices: %{
    exploration: %{da: 0.60, ne: 0.40},
    inhibition: %{"5ht": 1.0},
    vigilance: %{ne: 1.0},
    plasticity: %{da: 0.50, glu: 0.50}
  },
  intent_deltas: %{
    abuse: %{ne: 0.25, "5ht": -0.20, da: -0.05},
    insult: %{ne: 0.20, "5ht": -0.14, da: -0.04},
    gratitude: %{"5ht": 0.12, da: 0.15, ne: -0.05},
    greeting: %{"5ht": 0.08, ne: -0.02},
    greet: %{"5ht": 0.10, ne: -0.04, da: 0.03, glu: 0.02},
    question: %{da: 0.07, ne: 0.07},
    ask: %{da: 0.05, glu: 0.02},
    translate: %{da: 0.04, glu: 0.02},
    default: %{"5ht": 0.03, glu: 0.03}
  },
  load_deltas: [
    activation_denominator: 120.0,
    activation: %{ne: 0.08, glu: 0.04, da: 0.03},
    wm_density_denominator: 7.0,
    wm: %{ne: 0.05, glu: 0.03}
  ],
  tone: [
    neutral_band: 0.10,
    cautious: %{vigilance_min: 0.50, inhibition_max: 0.55},
    warm: %{exploration_min: 0.65, vigilance_max: 0.85, inhibition_min: 0.55},
    cool: %{inhibition_min: 0.70, exploration_max: 0.50},
    deescalate: %{vigilance_min: 0.88, inhibition_max: 0.60}
  ],
  pressure: [
    deescalation: %{vigilance_min: 0.65, inhibition_max: 0.55},
    cautious_emergency: %{vigilance_min: 0.50, inhibition_max: 0.55},
    heightened_attention: %{vigilance_min: 0.50},
    steady_restraint: %{inhibition_min: 0.60, vigilance_max: 0.45},
    engaged_adaptation: %{exploration_min: 0.60, plasticity_min: 0.60}
  ]

# -------------------------------------------------------------------
# Intent → mood bumps (Brain.MoodPolicy)
# -------------------------------------------------------------------
config :brain, Brain.MoodPolicy,
  # Debounce: don’t hammer MoodCore if intents fire rapidly
  min_interval_ms: 200,
  # Global gain on all bump_for/2 mappings
  gain: 0.75

# -------------------------------------------------------------------
# Tone mapping thresholds (for Brain.MoodCore.choose_tone/1)
# -------------------------------------------------------------------
# These control how raw indices → :warm / :cool / :deescalate / :neutral.
#
# “Good morning” style inputs should normally land in :warm
# (high exploration, moderate vigilance), not :deescalate.
config :brain, :mood_tone,
  neutral_band: 0.10,
  warm: %{
    exploration_min: 0.65,
    vigilance_max: 0.85,
    # NEW: only warm if serotonin isn't sagging
    inhibition_min: 0.55
  },
  cool: %{
    inhibition_min: 0.70
  },
  deescalate: %{
    vigilance_min: 0.88
    # exploration_max no longer used in the core logic
  }

# -------------------------------------------------------------------
# Mood HUD UI smoothing (SymbrellaWeb.BrainLive.MoodHud)
# -------------------------------------------------------------------
config :brain, :mood_ui,
  # Ignore super-short deescalate spikes when we were just calm/warm
  ignore_transient_deescalate_ms: 300,
  # Minimum time before we allow a more "intense" tone to visually flip
  min_tone_dwell_ms: 200

# -------------------------------------------------------------------
# TRCS affect latents (Brain.AffectLatents)
# -------------------------------------------------------------------
# Soft bounds for threat / reward / control / safety latents.
# For now these are just caps; later we can add per-axis gains here too.
config :brain, :trcs,
  max_threat: 1.0,
  max_reward: 1.0,
  max_control: 1.0,
  max_safety: 1.0
