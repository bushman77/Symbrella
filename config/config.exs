import Config

# This file configures your umbrella and all apps within it.

# Mailer
config :symbrella, Symbrella.Mailer, adapter: Swoosh.Adapters.Local

# Generators (scope contexts to the umbrella app)
config :symbrella_web,
  generators: [context_app: :symbrella]

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

# Esbuild profile (does not run automatically—use your tasks/aliases)
config :esbuild,
  version: "0.25.4",
  default: [
    args: ~w(
      js/app.js
      --bundle
      --target=es2017
      --outdir=../priv/static/assets
      --external:/fonts/*
      --external:/images/*
    ),
    cd: Path.expand("../apps/symbrella_web/assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

# Tailwind profile (does not run automatically—use your tasks/aliases)
config :tailwind,
  version: "3.4.10",
  default: [
    args: ~w(
      --config=tailwind.config.js
      --input=css/app.css
      --output=../priv/static/assets/app.css
    ),
    cd: Path.expand("../apps/symbrella_web/assets", __DIR__)
  ]

# Logger
config :logger, :default_formatter,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# JSON library
config :phoenix, :json_library, Jason

config :symbrella, ecto_repos: [Db]

config :symbrella, Db,
  username: "postgres",
  password: "postgres",
  database: "brain",
  hostname: "localhost",
  show_sensitive_data_on_connection_error: true,
  pool_size: 10,
  types: Db.PostgrexTypes

# Import environment-specific config at the very end
import_config "#{config_env()}.exs"

