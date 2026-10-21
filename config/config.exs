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

# ──────────────── Autonomous symbolic Curiosity ────────────────
# The reasoning and evidence-acquisition experiments passed their dedicated
# regression suite and unattended development trial. Start them automatically
# in the development environment; keep test and production inert by default.
# These settings replace the former IEx Application.put_env/3 setup.
# They are feature gates, not separate processes: Investigator is already
# supervised by Symbrella.Application, and evidence acquisition runs within
# its scheduled issue-investigation cycle.
#
# Prerequisite: run mix db.migrate before starting Phoenix so the durable
# curiosity_investigations audit table exists. Neither captured web evidence
# nor a hippocampal cue alone is authorized knowledge.
curiosity_enabled? = config_env() == :dev

# Persist independently verified, source-backed population comparisons.
config :core, :symbolic_consolidation_enabled?, curiosity_enabled?

# Compose two stored population comparisons only after Core revalidates both.
config :core, :symbolic_composition_enabled?, curiosity_enabled?

# Use Hippocampus only as a proof index; never as a factual authority.
config :core, :semantic_associative_recall_enabled?, curiosity_enabled?

# One supervised investigator is started by the application on boot.
# Acquisition is independently gated and uses fixed, bounded public-data
# providers (no Qwen calls or unsolicited chat). First tick after 60 seconds
# in development, then at most one eligible issue per minute; an unresolved
# issue enters a one-hour retry cooldown. Production remains opt-in.
config :core, Core.Curiosity.Investigator,
  enabled?: curiosity_enabled?,
  acquire_evidence?: curiosity_enabled?,
  interval_ms: if(curiosity_enabled?, do: 60_000, else: 300_000),
  retry_ms: 3_600_000

config :core, :response_prompt_profile, :mythomax

config :core, :log_prompt_budget?, false

config :core, :llm_synthesis,
  timeout_ms: 120_000,
  ready_timeout_ms: 10_000,
  max_tokens: 240,
  news_first_max_tokens: 280,
  evidence_revision_enabled?: true,
  evidence_revision_max_tokens: 360,
  # Retain a longer same-session conversation; prompt assembly separately
  # caps the history to a safe portion of the local model's context.
  history_turn_pairs: 32,
  history_mode: :full,
  selective_recent_pairs: 2,
  max_item_chars: 900,
  max_input_estimated_tokens: 2_800,
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
  dir: "priv"

# Admit bounded, session-scoped comprehension frames to working memory after
# semantic interpretation. Disabled in tests unless explicitly opted in;
# this does not change the evidence gate for LIFG senses or recalled facts.
config :core, Core.Brain.WM.Comprehension,
  enabled?: config_env() != :test

# Feed Core-verified response semantics into Brain WM as bounded session-local
# proof pointers. Each admission revalidates persisted symbolic evidence;
# neither WM nor model-generated prose can establish factual authority.
# Off in tests unless individually opted in; enabled on development startup.
config :core, Core.Brain.WM.VerifiedSemantics,
  enabled?: config_env() == :dev

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

# ────────────── Durable experience-backed synaptic shadow ──────────────
# Private, local receipts + review decisions survive BEAM restarts in dev.
# Never feeds LIFG, neuron firing, or authoritative memory; isolated in tests.
# Umbrella-root priv/ is git-ignored. Back up BOTH receipts.json and secret.key together.
config :brain, Brain.Synapses.Shadow,
  durable?: config_env() == :dev,
  dir: "priv"

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
