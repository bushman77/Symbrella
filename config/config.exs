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
  recall_budget_ms: 2000,
  recall_max_items: 12,
  agency_ledger_enabled?: true,
  agency_execute_commands?: true,
  agency_require_permission?: false,
  agency_record_commands?: true,
  llm_client: Llm,
  mwe_greet_phrase_bump: 0.02,
  mwe_general_bump: 0.01

# When enabled, logs aggregate estimated prompt tokens without including messages
# or recalled content. It can also be toggled from IEx during a running session.
# Durable symbolic consolidation uses its own proof store, not raw RAG answers.
# Intentionally disabled until the new migration and regression tests pass.
# Unlike Curiosity's independent OTP loop, capture only runs after an
# authoritative symbolic response is fully verified.
config :core, :symbolic_consolidation_enabled?, false

# Experimental two-hop composition of already source-backed stored proofs.
# Kept separate so the existing direct consolidation path stays unchanged.
config :core, :symbolic_composition_enabled?, false

config :core, :log_prompt_budget?, false

config :core, :llm_synthesis,
  timeout_ms: 120_000,
  ready_timeout_ms: 10_000,
  max_tokens: 150,
  news_first_max_tokens: 280,
  evidence_revision_enabled?: true,
  evidence_revision_max_tokens: 360,
  history_turn_pairs: 6,
  # Full history remains default; opt into :selective_recall only after evaluating metrics.
  history_mode: :full,
  selective_recent_pairs: 2,
  max_item_chars: 500,
  max_system_chars: 4_000,
  max_user_chars: 1_500,
  degraded_acc_conflict_min: 0.5

config :core, :web_retrieval,
  enabled?: config_env() != :test,
  provider: Core.Tools.WebRetrieval.Wikipedia,
  news_provider: Core.Tools.WebRetrieval.NewsRss,
  timeout_ms: 3_500,
  max_attempts: 3

# Automatically append completed LiveView conversations and memory evidence
# to the private local training archive in development. This is data capture,
# NOT online Axon training. Keep production and tests off by default because
# the JSONL records may contain private chat text and recalled memories.
# Change enabled?: false here to pause recording; no CLI env flag is needed.
config :core, Core.TrainingArchive,
  enabled?: config_env() == :dev,
  dir: "tmp/training"

# Admit bounded, session-scoped comprehension frames to working memory after
# semantic interpretation. Disabled in tests unless explicitly opted in;
# this does not change the evidence gate for LIFG senses or recalled facts.
config :core, Core.Brain.WM.Comprehension,
  enabled?: config_env() != :test

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
  pool_size: 3,
  types: Db.PostgrexTypes,
  log: System.get_env("DB_LOG", "false") in ["true", "1", "on", "yes"]

config :db, :embedding_dim, 1536
config :db, :brain_cell_embedding_dim, 768
config :db, :embedder, MyEmbeddings

# ─────────────────────────── Per-env tail ─────────────────────────
import_config "mood.exs"
import_config "#{config_env()}.exs"
