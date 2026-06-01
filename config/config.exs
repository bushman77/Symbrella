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
  llm_client: Llm,
  mwe_greet_phrase_bump: 0.02,
  mwe_general_bump: 0.01

config :core, :llm_synthesis,
  timeout_ms: 15_000,
  history_turn_pairs: 6,
  max_item_chars: 1_600,
  max_system_chars: 8_000,
  max_user_chars: 2_000,
  degraded_acc_conflict_min: 0.5

config :core, Core.Curiosity.Bridge,
  threshold: 0.60,
  min_gap_ms: 30_000

config :core, Core.Curiosity.EpisodeProbe,
  enabled?: true,
  every_turns: 2,
  min_gap_ms: 15_000,
  max_vigilance: 0.75,
  min_uncertainty: 0.2,
  max_recent: 40

# ─────────────────────────── Brain (central) ──────────────────────
config :brain,
  pubsub: Symbrella.PubSub,
  pmtg_mode: :boost,
  pmtg_margin_threshold: 0.15,
  pmtg_window_keep: 50,
  lifg_defaults: [inject_child_unigrams?: true],
  lifg_stage1_weights: %{lex_fit: 0.40, rel_prior: 0.35, activation: 0.15, intent_bias: 0.10},
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

config :brain, Brain.MoodCore,
  half_life_ms: %{da: 30_000, "5ht": 60_000, glu: 90_000, ne: 45_000},
  clock: :cycle,
  init: %{da: 0.35, "5ht": 0.50, glu: 0.40, ne: 0.50}

# ───────────────────────────── Web ────────────────────────────────
config :llm, Llm,
  # model_path: Path.expand("\~/models/qwen3-8b/Qwen3-8B-Q4_K_M.gguf"),
  model_path: Path.expand("\~/models/mythomax-l2-13b.Q4_K_M.gguf"),
  llama_server: "llama-server",
  # Synchronous boot is handled by Llm.BootGate. Keep the GenServer's own
  # handle_continue autostart off so there is a single startup path.
  auto_start_on_boot?: false,
  allow_lazy_start?: true,
  auto_restart_on_crash?: true,
  host: "127.0.0.1",
  port: 0,
  ctx: 2048,
  threads: 4,
  heartbeat_ms: 15_000

config :llm, Llm.BootGate,
  # Blocks the umbrella root supervisor until llama-server answers /v1/models.
  # SymbrellaWeb depends on :symbrella, so Phoenix starts only after this passes.
  enabled?: true,
  timeout: 120_000

config :llm, :runner,
  host: "127.0.0.1",
  ctx: 2048,
  threads: 4,
  temperature: 0.4,
  call_timeout_ms: 60_000,
  heartbeat_ms: 15_000,
  ready_poll_attempts: 80,
  ready_poll_sleep_ms: 250,
  models_timeout_cap_ms: 8_000,
  ready_probe_timeout_ms: 1_250,
  log_ring_max: 200,
  backoff_min_ms: 250,
  backoff_max_ms: 10_000,
  line_buffer: 16_384,
  body_preview_chars: 2_000,
  log_line_chars: 4_000

config :llm, :generation,
  chat_model: "local",
  embedding_model: "local",
  stream?: false,
  temperature: 0.4,
  keep_alive: "10m",
  stable_runner_opts: %{
    num_ctx: 1024,
    top_k: 1,
    top_p: 1.0,
    repeat_penalty: 1.0,
    seed: 42,
    num_predict: 80
  }

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
  version: "3.4.10",
  default: [
    args: [
      "--config=tailwind.config.js",
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
config :db, :embedder, MyEmbeddings

# ─────────────────────────── Per-env tail ─────────────────────────
import_config "mood.exs"
import_config "#{config_env()}.exs"
