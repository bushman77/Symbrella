# config/config.exs
import Config

import Config

config :symbrella, :assistant,
  name: "Symbrella",
  norm: "symbrella",
  aliases: ["symbrella"]

# ───────────────────────────── Mailer ─────────────────────────────
config :symbrella, Symbrella.Mailer, adapter: Swoosh.Adapters.Local

# ───────────────────────────── Core ───────────────────────────────
# Synonyms (decoupled from :db, via external provider that calls Db.Lexicon)
config :core, Core.Recall.Synonyms,
  provider: Core.Recall.Synonyms.Providers.External,
  cache?: true,
  ttl_ms: 60_000,
  top_k: 12

config :core, Core.Recall.Synonyms.Providers.External,
  # You implement this in :db
  mfa: {Db.Lexicon, :lookup_synonyms, []}

# Core defaults
config :core,
  recall_budget_ms: :infinity,
  recall_max_items: :infinity,
  mwe_greet_phrase_bump: 0.02,
  mwe_general_bump: 0.01

# Curiosity bridge (kept as-is)
config :core, Core.Curiosity.Bridge,
  threshold: 0.60,
  min_gap_ms: 30_000

# ─────────────────────────── Brain (central) ──────────────────────
# Single consolidated block; preserves your effective weights and options.
config :brain,
  pubsub: Symbrella.PubSub,
  # pMTG
  # :boost | :rerun | :none
  pmtg_mode: :boost,
  pmtg_margin_threshold: 0.15,
  pmtg_window_keep: 50,
  # LIFG (Stage-1)
  lifg_defaults: [inject_child_unigrams?: true],
  lifg_stage1_weights: %{lex_fit: 0.40, rel_prior: 0.35, activation: 0.15, intent_bias: 0.10},
  # or :top2 | :none
  lifg_stage1_scores_mode: :all,
  lifg_min_margin: 0.05,
  lifg_stage1_mwe_fallback: true,
  lifg_slate_filter_rules: [
    %{lemma: "a", allow: ~w(det article particle), drop_others?: true},
    %{lemma: "A", allow: ~w(det article particle), drop_others?: true},
    %{lemma: "eat", allow: ~w(verb), drop_others?: true}
  ],
  # ACC gate
  acc_conflict_tau: 0.50,
  # Working memory shaping
  # per-second exponential decay (≈5.8s half-life)
  wm_decay_lambda: 0.12,
  wm_score_min: 0.0,
  wm_score_max: 1.0,
  lifg_mood_weights: %{expl: 0.02, inhib: -0.03, vigil: 0.02, plast: 0.00},
  lifg_mood_cap: 0.05,
  # Hippocampus defaults
  hpc_half_life_ms: 300_000,
  hpc_window_keep: 300,
  hpc_min_jaccard: 0.0,
  hpc_recall_limit: 3,
  # Priming knobs
  hippo_priming: :on,
  hippo_priming_vectors: %{
    success: %{da: 0.02, "5ht": -0.01, glu: 0.02, ne: 0.01},
    failure: %{da: -0.01, "5ht": 0.02, glu: 0.00, ne: 0.02}
  }

config :brain, :blackboard_window_size, 100
# MoodCore — explicit module config (kept separate on purpose)
config :brain, Brain.MoodCore,
  half_life_ms: 12_000,
  clock: :cycle,
  init: %{da: 0.35, "5ht": 0.50, glu: 0.40, ne: 0.50}

# ───────────────────────────── Web ────────────────────────────────
config :llm, Llm,
  model_path: Path.expand("~/models/Qwen2.5-0.5B-Instruct-Q4_K_M.gguf"),
  llama_server: "llama-server",
  auto_start_on_boot?: true,
  allow_lazy_start?: true,       # ok to leave true; still autostarts anyway
  auto_restart_on_crash?: true,
  host: "127.0.0.1",
  port: 0,
  ctx: 2048,
  threads: 4,
  heartbeat_ms: 15_000

# Symbrella app opts
config :symbrella,
  resolve_input_opts: [mode: :prod, enrich_lexicon?: true, lexicon_stage?: true]

# Generators
config :symbrella_web, generators: [context_app: :symbrella]

# Endpoint (base/static config)
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

# ───────────────────────────── Logger ─────────────────────────────
config :logger,
  backends: [:console],
  level: :info,
  compile_time_purge_matching: [
    [level_lower_than: :info]
  ]

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# ───────────────────────────── Phoenix ────────────────────────────
config :phoenix, :json_library, Jason

# ─────────────────────────────  DB  ───────────────────────────────
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
  # silence SQL logs by default (opt in at runtime via DB_LOG=true)
  log: System.get_env("DB_LOG", "false") in ~w(true 1 on yes)

# Embeddings (placeholders; wire up your module)
config :db, :embedding_dim, 1536
config :db, :embedder, MyEmbeddings

# ───────────────────────────── Tesla ──────────────────────────────
config :tesla, disable_deprecated_builder_warning: true

# ─────────────────────────── Per-env tail ─────────────────────────
import_config "mood.exs"
import_config "#{config_env()}.exs"
