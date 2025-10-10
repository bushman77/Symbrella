import Config

# ───────── Mailer ─────────
config :symbrella, Symbrella.Mailer, adapter: Swoosh.Adapters.Local

# ───────── Generators ─────────
config :symbrella_web, generators: [context_app: :symbrella]

# ───────── App-specific (example) ─────────
config :symbrella,
  resolve_input_opts: [mode: :prod, enrich_lexicon?: true, lexicon_stage?: true]

# ───────── Phoenix Endpoint (base/static config) ─────────
config :symbrella_web, SymbrellaWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [
    formats: [html: SymbrellaWeb.ErrorHTML, json: SymbrellaWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: Symbrella.PubSub,
  live_view: [signing_salt: "mkK1WujO"]

# ───────── Build tools ─────────
config :esbuild,
  version: "0.25.4",
  default: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../apps/symbrella_web/assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

config :tailwind,
  version: "3.4.10",
  default: [
    args:
      ~w(--config=tailwind.config.js --input=css/app.css --output=../priv/static/assets/app.css),
    cd: Path.expand("../apps/symbrella_web/assets", __DIR__)
  ]

# ───────── Logger (global) ─────────
# Show only :info / :warn / :error at runtime (can override in runtime.exs via LOG_LEVEL)
config :logger,
  backends: [:console],
  level: :info,
  compile_time_purge_matching: [
    # Purge anything below :info (i.e., :debug) at compile time
    [level_lower_than: :info]
  ]

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# JSON
config :phoenix, :json_library, Jason

# ───────── Ecto (db app) ─────────
config :db, ecto_repos: [Db]

config :db, Db,
  username: "postgres",
  password: "postgres",
  database: "brain",
  hostname: "localhost",
  show_sensitive_data_on_connection_error: true,
  pool_size: 10,
  types: Db.PostgrexTypes,
  # silence SQL logs by default (opt in at runtime via DB_LOG=true)
  log: false

config :db, :embedding_dim, 1536
# implement MyEmbeddings.embed/1 -> {:ok, [float()]}
config :db, :embedder, MyEmbeddings

# ───────── Core defaults ─────────
config :core,
  recall_budget_ms: :infinity,
  recall_max_items: :infinity

# ───────── Tesla ─────────
config :tesla, disable_deprecated_builder_warning: true

# ───────── Brain (central defaults) ─────────
config :brain,
  # :boost | :rerun | :none
  pmtg_mode: :boost,
  pmtg_margin_threshold: 0.15,
  pmtg_window_keep: 50,
  lifg_stage1_weights: %{lex_fit: 0.40, rel_prior: 0.35, activation: 0.15, intent_bias: 0.10},
  lifg_stage1_scores_mode: :all,
  # per-second exponential decay (≈5.8s half-life)
  wm_decay_lambda: 0.12,
  wm_score_min: 0.0,
  wm_score_max: 1.0

# Env-specific at the very end
import_config "#{config_env()}.exs"
