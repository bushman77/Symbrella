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
    da: 18_000,
    "5ht": 14_000,
    glu: 55_000,
    ne: 14_000
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
