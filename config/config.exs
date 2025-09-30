import Config

# Mailer
config :symbrella, Symbrella.Mailer, adapter: Swoosh.Adapters.Local

# Generators
config :symbrella_web, generators: [context_app: :symbrella]

# config/config.exs (or runtime.exs)
config :symbrella, resolve_input_opts: [mode: :prod, enrich_lexicon?: true, lexicon_stage?: true]


# Endpoint
config :symbrella_web, SymbrellaWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [
    formats: [html: SymbrellaWeb.ErrorHTML, json: SymbrellaWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: Symbrella.PubSub,
  live_view: [signing_salt: "mkK1WujO"]

# Build tools
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

# Logger / JSON
config :logger, :default_formatter,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

config :phoenix, :json_library, Jason

# >>> Single source of truth for Ecto (db app)
config :db, ecto_repos: [Db]

config :db, Db,
  username: "postgres",
  password: "postgres",
  database: "brain",
  hostname: "localhost",
  show_sensitive_data_on_connection_error: true,
  pool_size: 10,
  # keep if you defined custom types (e.g., pgvector)
  types: Db.PostgrexTypes

config :core,
  recall_budget_ms: :infinity,
  recall_max_items: :infinity

config :tesla, disable_deprecated_builder_warning: true

# Import env-specific at the end (OK to keep empty)
import_config "#{config_env()}.exs"
