# config/config.exs
import Config

# ───────────────────────────── Assistant ─────────────────────────────
config :symbrella, :assistant,
  name: "Symbrella",
  norm: "symbrella",
  aliases: ["symbrella"]

# ───────────────────────────── Mailer ─────────────────────────────
config :symbrella, Symbrella.Mailer, adapter: Swoosh.Adapters.Local

# ───────────────────────────── Core ───────────────────────────────
config :core, Core.Recall.Synonyms,
  provider: Core.Recall.Synonyms.Providers.External,
  cache?: true,
  ttl_ms: 60_000,
  top_k: 12

config :core,
  recall_budget_ms: :infinity,
  recall_max_items: :infinity,
  agency_ledger_enabled?: true,
  agency_execute_commands?: true,
  agency_require_permission?: false,
  agency_record_commands?: true,
  llm_client: Llm,
  mwe_greet_phrase_bump: 0.02,
  mwe_general_bump: 0.01

config :core, :llm_synthesis,
  timeout_ms: 10_000,
  ready_timeout_ms: 2_500,
  max_tokens: 220,
  history_turn_pairs: 3,
  max_item_chars: 900,
  max_system_chars: 8_000,
  max_user_chars: 2_000,
  degraded_acc_conflict_min: 0.5

config :core, Core.Curiosity.Bridge,
  threshold: 0.60,
  min_gap_ms: 30_000

config :core, Core.Agency.AutonomyLoop,
  enabled?: true,
  record_commands?: true

config :core, Core.Curiosity.EpisodeProbe,
  enabled?: true,
  every_turns: 2,
  min_gap_ms: 15_000,
  max_vigilance: 0.75,
  min_uncertainty: 0.2,
  max_recent: 40

# ─────────────────────────── Brain ────────────────────────────────
import_config "brain/brain.exs"

# ───────────────────────────── LLM ────────────────────────────────
import_config "llm.exs"

config :symbrella,
  resolve_input_opts: [mode: :prod, enrich_lexicon?: true, lexicon_stage?: true]

config :symbrella_web, generators: [context_app: :symbrella]

config :symbrella_web, SymbrellaWeb.HomeLive,
  curiosity_idle_enabled?: false,
  curiosity_idle_ms: 180_000

config :symbrella_web, SymbrellaWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [
    formats: [html: SymbrellaWeb.ErrorHTML, json: SymbrellaWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: Symbrella.PubSub,
  live_view: [signing_salt: "mkK1WujO"]

# ─────────────────────────── Build tools ──────────────────────────
config :esbuild,
  version: "0.25.4",
  default: [
    args: [
      "js/app.js",
      "--bundle",
      "--target=es2017",
      "--outdir=../priv/static/assets",
      "--external:/fonts/*",
      "--external:/images/*"
    ],
    cd: Path.expand("../apps/symbrella_web/assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

config :tailwind,
  version: "4.1.7",
  default: [
    args: [
      "--input=css/app.css",
      "--output=../priv/static/assets/app.css"
    ],
    cd: Path.expand("../apps/symbrella_web/assets", __DIR__)
  ]

# ───────────────────────────── Logger ─────────────────────────────
config :logger,
  level: :info,
  compile_time_purge_matching: [
    [level_lower_than: :info]
  ]

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# ───────────────────────────── Phoenix ────────────────────────────
config :phoenix, :json_library, Jason

# ───────────────────────────── DB ───────────────────────────────
config :db, ecto_repos: [Db]

config :db, Db,
  username: System.get_env("PGUSER", "postgres"),
  password: System.get_env("PGPASSWORD", "postgres"),
  database: System.get_env("PGDATABASE", "brain"),
  hostname: System.get_env("PGHOST", "127.0.0.1"),
  port: String.to_integer(System.get_env("PGPORT", "5432")),
  show_sensitive_data_on_connection_error: true,
  pool_size: 10,
  types: Db.PostgrexTypes,
  log: System.get_env("DB_LOG", "false") in ["true", "1", "on", "yes"]

config :db, :embedding_dim, 1536
config :db, :brain_cell_embedding_dim, 768
config :db, :embedder, MyEmbeddings

# ─────────────────────────── Per-env tail ─────────────────────────
import_config "mood.exs"
import_config "#{config_env()}.exs"
