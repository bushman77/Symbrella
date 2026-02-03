# P-201 — Tokenizer defaults (dev): words-only, no char-grams
import Config
config :brain, :ml_enabled, false
  
config :symbrella_web, SymbrellaWeb.Endpoint,
  server: true,
  http: [ip: {0, 0, 0, 0}, port: 4000],
  url: [host: "localhost", port: 4000],
  code_reloader: true,
  debug_errors: true,
  check_origin: false,
  secret_key_base: "0123456789012345678901234567890123456789012345678901234567890123",
  live_reload: [
    patterns: [
      ~r"priv/static/.*(js|css|png|jpeg|jpg|gif|svg)$",
      ~r"lib/symbrella_web/(controllers|live|components)/.*(ex|heex)$",
      ~r"priv/gettext/.*(po)$"
    ]
  ]

# Dev-only routes (LiveDashboard/mailbox)
config :symbrella_web, :dev_routes, true

# Dev ergonomics
config :logger, :console, format: "[$level] $message\n"
config :phoenix, :plug_init_mode, :runtime

config :phoenix_live_view,
  debug_heex_annotations: true,
  debug_attributes: true,
  enable_expensive_runtime_checks: true

# Disable Swoosh API client in dev
config :swoosh, :api_client, false

# Helpful stacktraces in dev
config :phoenix, :stacktrace_depth, 20

# ---- Core tokenizer defaults (dev) -----------------------------------
config :core, :tokenizer_defaults,
  mode: :words,
  emit_chargrams: false

config :brain, Brain.CycleClock, hz: 20

config :brain, Brain.MoodCore,
  clock: :cycle,
  half_life_ms: 12_000,
  init: %{da: 0.35, "5ht": 0.50, glu: 0.40, ne: 0.50}

# config/dev.exs (and prod.exs)
config :brain, Brain.MoodPolicy,
  # overall strength (try 0.5..1.5)
  gain: 1.0,
  # rate-limit bumps from rapid-fire intents
  min_interval_ms: 150

# Optional extra LIFG outer weights in dev (kept from your original)
config :brain, :lifg_weights, prime: 0.05
