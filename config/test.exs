# P-201 — Tokenizer defaults (test): words-only, no char-grams
import Config

# -------------------------------
# Database (umbrella app: :db)
# -------------------------------
config :db, ecto_repos: [Db]

config :db, Db,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "symbrella_db_test",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10,
  show_sensitive_data_on_connection_error: true

# -------------------------------
# Web (no server during tests)
# -------------------------------
config :symbrella_web, SymbrellaWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "14hiEvqKbnYtZ+2qDPg34F/YD7XKAZIhRV2/+Tu0m00q/a2ba7en3+E9Dr010Ze7",
  server: false

# -------------------------------
# Mailer (test adapter)
# -------------------------------
config :symbrella, Symbrella.Mailer, adapter: Swoosh.Adapters.Test
config :swoosh, :api_client, false

# -------------------------------
# Logger / ExUnit
# -------------------------------
config :logger, level: :warning
config :ex_unit, capture_log: true

# -------------------------------
# Phoenix runtime knobs for tests
# -------------------------------
config :phoenix, :plug_init_mode, :runtime

config :phoenix_live_view,
  enable_expensive_runtime_checks: true

# -------------------------------
# Core tokenizer defaults (tests)
# -------------------------------
config :core, :tokenizer_defaults,
  mode: :words,
  emit_chargrams: false

# -------------------------------
# Brain (LIFG / pMTG / Hippocampus)
# -------------------------------
config :brain,
  # Episodic mode during tests
  # set :off if you want zero writes
  episodes_mode: :on,
  # LIFG test stability (thresholds & outputs)
  lifg_min_score: 0.6,
  lifg_min_margin: 0.12,
  lifg_min_p_top1: 0.65,
  lifg_stage1_scores_mode: :all,
  lifg_stage1_weights: %{
    lex_fit: 0.40,
    rel_prior: 0.30,
    activation: 0.20,
    intent_bias: 0.10
  },
  # pMTG defaults (kept simple for unit tests)
  pmtg_mode: :boost,
  pmtg_margin_threshold: 0.15,
  pmtg_window_keep: 50,
  # Hippocampus: hide dup counter in test assertions
  hippo_meta_dup_count: false
