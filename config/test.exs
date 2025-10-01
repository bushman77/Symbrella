import Config

config :db, ecto_repos: [Db]

config :db, Db,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "symbrella_db_test",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10,
  show_sensitive_data_on_connection_error: true


# We don't run a server during test. If one is required,
# you can enable the server option below.
config :symbrella_web, SymbrellaWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "14hiEvqKbnYtZ+2qDPg34F/YD7XKAZIhRV2/+Tu0m00q/a2ba7en3+E9Dr010Ze7",
  server: false

# Print only warnings and errors during test
config :logger, level: :warning

# In test we don't send emails
config :symbrella, Symbrella.Mailer, adapter: Swoosh.Adapters.Test

# Disable swoosh api client as it is only required for production adapters
config :swoosh, :api_client, false

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

# Enable helpful, but potentially expensive runtime checks
config :phoenix_live_view,
  enable_expensive_runtime_checks: true

config :core, :tokenizer_defaults,
  mode: :words,
  emit_chargrams: false
